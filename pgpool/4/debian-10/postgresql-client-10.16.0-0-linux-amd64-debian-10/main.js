'use strict';

const _ = require('lodash');
const utilsFunctions = require('./lib/utils');
const postgresqlFunctions = require('./lib/databases/postgresql')({
  binDir: $app.binDir,
  logger: utilsFunctions.getLoggerFromApp($app),
});
const networkFunctions = require('./lib/network')($app);

$app.postInstallation = function() {
  const dbProperties = [$app.database, $app.password, $app.username];
  const userConnection = {
    host: $app.host,
    port: $app.port,
    user: $app.username,
    password: $app.password,
  };
  const adminConnection = {
    host: $app.host,
    port: $app.port,
    user: $app.databaseAdminUser,
    password: $app.databaseAdminPassword,
  };
  if (!dbProperties.some(_.isEmpty)) {
    networkFunctions.waitForService($app.host, $app.port);

    // Wait for a valid postgres connection
    postgresqlFunctions.checkConnection(adminConnection);

    // Check whether or not the user credentials are valid
    const connected = postgresqlFunctions.canConnect(userConnection).connection;
    // In case the user cannot connect, we will check if it exists before creating it
    if (!connected) {
      if (postgresqlFunctions.existsUser($app.username, adminConnection)) {
        throw new Error('The credentials provided for the user are not valid.');
      } else {
        postgresqlFunctions.createUser($app.username, _.merge({newUserPassword: $app.password}, adminConnection));
      }
    }

    // Possible states at this point:
    // connected = 0 ; existsDatabase = 0 ; visibleDatabase = 0
    // connected = 1 ; existsDatabase = 0 ; visibleDatabase = 0
    // connected = 1 ; existsDatabase = 1 ; visibleDatabase = 0
    // connected = 1 ; existsDatabase = 1 ; visibleDatabase = 1

    // Check whether or not the database truly exists
    const existsDatabase = postgresqlFunctions.existsDatabase($app.database, adminConnection);
    // In case the database does not exist it will be created.
    if (!existsDatabase) {
      $app.info(`==> Creating database ${$app.database}...`);
      postgresqlFunctions.createDatabase($app.database, adminConnection);
      $app.info(`==> Granting access to ${$app.username} to the database ${$app.database}...`);
      postgresqlFunctions.grantPrivileges($app.database, $app.username, adminConnection);
    } else {
      // Check whether or not the user is able to see the database
      const visibleDatabase = postgresqlFunctions.existsDatabase($app.database, userConnection);
      if (!visibleDatabase) {
        throw new Error('The credentials provided have no privileges over the specified database');
      } else $app.debug('Reusing database and credentials');
    }
  } else if (!dbProperties.every(_.isEmpty)) {
    throw new Error('You should specify user, password and name parameters for the database creation');
  }
};

$app.exports = {
  createDatabase: postgresqlFunctions.createDatabase,
  dropDatabase: postgresqlFunctions.dropDatabase,
  createUser: postgresqlFunctions.createUser,
  dropUser: postgresqlFunctions.dropUser,
  grantPrivileges: postgresqlFunctions.grantPrivileges,
  execute: postgresqlFunctions.execute,
  set: postgresqlFunctions.set,
  get: postgresqlFunctions.get,
  insert: postgresqlFunctions.insert,
  checkConnection: postgresqlFunctions.checkConnection,
  canConnect: postgresqlFunctions.canConnect,
  showDatabases: postgresqlFunctions.showDatabases,
  showUsers: postgresqlFunctions.showUsers,
  existsUser: postgresqlFunctions.existsUser,
  existsDatabase: postgresqlFunctions.existsDatabase,
};
