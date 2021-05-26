'use strict';

const _ = require('lodash');

const mongodbCore = $modules['com.qingcloud.mongodb-client'];
const MongoDBDatabase = require('./database');
const MongoDBUser = require('./user');
const DatabaseHandler = require('../handler');

/**
 * MongoDB handler functions.
 * @namespace handler.databases.mongodb
 */
class MongoDBHandler extends DatabaseHandler {
  constructor(properties, options) {
    super(options);
    _.assign(this, {
      connection: _.defaults(properties || {}, {
        user: 'root',
        password: '',
        host: '127.0.0.1',
        port: 27017,
      }),
      appDatabase: {},
    }, _.pick(
      mongodbCore,
      ['installdir', 'confDir', 'binDir', 'dataDir', 'logsDir', 'libDir', 'persistDir']
    ));
    this._database = new MongoDBDatabase(this.connection);
    this._user = new MongoDBUser(this.connection);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~execute execute}
   * @function handler.databases.mongodb~execute
   */
  execute(command, options) {
    const _options = _.defaults(options || {}, {cwd: this.cwd});
    return mongodbCore.execute(command, _options);
  }

  /**
   * Wrapper of {@link base-functions.database.mongodb~executeFile executeFile}
   * @function handler.databases.mongodb~executeFile
   */
  executeFile(file, options) {
    const _options = _.defaults(options || {}, {cwd: this.cwd});
    if (!_.startsWith(file, '/')) file = $file.join(options.cwd, file);
    mongodbCore.executeFile(file, _options);
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
   * @function createDatabaseForApp
   * @param {string} name - Application name
   * @param {Object} options - Credentials object
   * @param {boolean} [options.dropIfExists=false] - Will drop any database with the same name
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
      collection: `${name}_collection`,
      password: $crypt.rand({size: 10, alphanumeric: true}),
    };
    if (options.dropIfExists) {
      $app.debug(`Dropping database '${databaseSettings.name}'`);
      mongodbCore.dropDatabase(databaseSettings.name, this.connection);
    }
    const previousDatabases = mongodbCore.execute('db.adminCommand(\'listDatabases\')', this.connection);
    if (previousDatabases.code !== 0) {
      throw new Error(`There was an error (${previousDatabases.code}) ${previousDatabases.stderr}`);
    }
    // Look for a valid database name
    while (previousDatabases.stdout.match(databaseSettings.name)) {
      databaseSettings.name = `qingcloud_${name}_${$crypt.rand({size: 5, alphanumeric: true})}`;
    }

    this.logger.debug(`Creating database '${databaseSettings.name}'`
               + `for '${databaseSettings.user}' with '${databaseSettings.password}'`);
    mongodbCore.createDatabase(databaseSettings.name, databaseSettings.collection, this.connection);
    mongodbCore.createUser(
      databaseSettings.user,
      databaseSettings.password,
      databaseSettings.name,
      {role: 'readWrite', db: databaseSettings.name},
      this.connection
    );
    this[options.id] = _.assign(new MongoDBDatabase(this.connection), databaseSettings);
  }

  /**
   * Check the connection with the database server
   * @function handler.databases.mongodb~checkConnection
   * @example
   * checkConnection()
   */
  checkConnection() {
    mongodbCore.checkConnection(this.connection);
  }
}

module.exports = MongoDBHandler;
