'use strict';

const _ = require('lodash');

const mysqlCore = $modules['com.qingcloud.mysql-client'];
const MysqlDatabase = require('./database');
const MysqlUser = require('./user');
const DatabaseHandler = require('../handler');

/**
 * MySQL handler functions.
 * @namespace handler.databases.mysql
 */
class MysqlHandler extends DatabaseHandler {
  constructor(properties, options) {
    super(options);
    _.assign(this, {
      connection: _.defaults(properties || {}, {
        user: 'root',
        password: '',
        host: '127.0.0.1',
        port: 3306,
        socket: '',
        sslCAFile: '',
        defaultsFile: '',
      }),
      appDatabase: {},
    }, _.pick(
      mysqlCore,
      ['installdir', 'confDir', 'binDir', 'dataDir', 'logsDir', 'libDir', 'persistDir']
    ));
    this._database = new MysqlDatabase(this.connection);
    this._user = new MysqlUser(this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~execute execute}
   * @function handler.databases.mysql~execute
   */
  execute(command, options) {
    const _options = _.defaults(options || {}, {cwd: this.cwd});
    mysqlCore.execute(command, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mysql~executeSqlFile executeSqlFile}
   * @function handler.databases.mysql~executeFile
   */
  executeFile(file, options) {
    const _options = _.defaults(options || {}, {cwd: this.cwd});
    if (!_.startsWith(file, '/')) file = $file.join(options.cwd, file);
    mysqlCore.executeSqlFile(file, _options);
  }

  database(name) {
    return _.assign(this._database, {name});
  }

  user(name) {
    return _.assign(this._user, {name});
  }

  /**
   * Create a database, and grant privileges to a given user with a random password.
   * An object with the database name, user and password will be returned.
   * @function handler.databases.mysql~createDatabaseForApp
   * @param {string} name - Application name
   * @param {Object} options - Credentials object
   * @param {boolean} [options.dropIfExists=false] - Will drop any database with the same name
   * @param {string[]} [options.fromHosts=['localhost', '%']] - Granted hosts for the new database user
   * @returns {Object} - Object containing the database attributes: user, name, password, host and port.
   * @example
   * // Create a database for the application in the default database server with a random password
   * createDatabaseForApp($app.name);
   */
  createDatabaseForApp(name, options) {
    options = _.defaults(options || {}, {id: 'appDatabase'});
    const databaseSettings = {
      name: `qingcloud_${name}`,
      user: `bn_${name}`,
      password: $crypt.rand({size: 10, alphanumeric: true}),
    };
    if (options.dropIfExists) {
      this.logger.debug(`Dropping database '${databaseSettings.name}'`);
      mysqlCore.dropDatabase(databaseSettings.name, this.connection);
    }
    const previousDatabases = mysqlCore.execute('SHOW DATABASES;', this.connection);
    while (previousDatabases.match(databaseSettings.name)) {
      databaseSettings.name = `qingcloud_${name}_${$crypt.rand({size: 5, alphanumeric: true})}`;
    }
    this.logger.debug(`Creating database '${databaseSettings.name}'`
               + `for '${databaseSettings.user}' with '${databaseSettings.password}'`);
    mysqlCore.createDatabase(databaseSettings.name, this.connection);
    mysqlCore.grantPrivileges(
      databaseSettings.name,
      databaseSettings.user,
      _.extend(options, this.connection, {databaseUserPassword: databaseSettings.password})
    );
    this[options.id] = _.assign(new MysqlDatabase(this.connection), databaseSettings);
  }

  /**
   * Wait for a connection with the database server
   * @function handler.databases.mysql~checkConnection
   * @example
   * checkConnection()
   */
  checkConnection() {
    mysqlCore.checkConnection(this.connection);
  }

  /**
   * Check the connection with the database server
   * @function handler.databases.mysql~checkConnection
   * @returns {boolean}
   * @example
   * canConnect()
   */
  canConnect() {
    mysqlCore.canConnect(this.connection);
  }
}

module.exports = MysqlHandler;
