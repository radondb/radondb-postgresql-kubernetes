'use strict';

const _ = require('lodash');

const postgresqlCore = $modules['com.qingcloud.postgresql-client'];
const PostgresqlDatabase = require('./database');
const PostgresqlUser = require('./user');
const DatabaseHandler = require('../handler');

class PostgresqlHandler extends DatabaseHandler {
  constructor(properties, options) {
    super(options);
    _.assign(this, {
      connection: _.defaults(properties || {}, {
        user: 'postgres',
        password: '',
        host: '127.0.0.1',
        port: 5432,
      }),
      appDatabase: {},
    });
    this._database = new PostgresqlDatabase(this.connection);
    this._user = new PostgresqlUser(this.connection);
  }

  execute(command, options) {
    const _options = _.defaults(options || {}, {cwd: this.cwd});
    postgresqlCore.execute(command, _options);
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
   * // Create a database for the application in the default database server with a random password
   * createDatabaseForApp($app.name);
   */
  createDatabaseForApp(name, options) {
    options = options || {id: 'appDatabase'};
    const databaseSettings = {
      name: `qingcloud_${name}`,
      user: `bn_${name}`,
      password: $crypt.rand({size: 10, alphanumeric: true}),
    };
    const previousDatabases = postgresqlCore.execute('\\list', this.connection);
    if (options.dropIfExists) {
      $app.debug(`Dropping database '${databaseSettings.name}'`);
      postgresqlCore.dropDatabase(databaseSettings.name, this.connection);
    }

    while (previousDatabases.match(databaseSettings.name)) {
      databaseSettings.name = `qingcloud_${name}_${$crypt.rand({size: 5, alphanumeric: true}).toLowerCase()}`;
    }
    $app.debug(`Creating database '${databaseSettings.name}' \
               for '${databaseSettings.user}' with '${databaseSettings.password}'`);
    postgresqlCore.createDatabase(databaseSettings.name, this.connection);
    postgresqlCore.createUser(
      databaseSettings.user,
      _.extend({}, this.connection, {newUserPassword: databaseSettings.password}, options)
    );
    postgresqlCore.grantPrivileges(
      databaseSettings.name,
      databaseSettings.user,
      _.extend(options, this.connection)
    );

    this[options.id] = _.assign(new PostgresqlDatabase(this.connection), databaseSettings);
  }

  checkConnection() {
    postgresqlCore.checkConnection(this.connection);
  }

  /**
 * Check the connection with the database server
 * @function handler.databases.postgresql~checkConnection
 * @returns {boolean}
 * @example
 * canConnect()
 */
  canConnect() {
    postgresqlCore.canConnect(this.connection);
  }
}

module.exports = PostgresqlHandler;
