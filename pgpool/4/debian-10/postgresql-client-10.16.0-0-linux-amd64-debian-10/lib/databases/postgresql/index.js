'use strict';

const _ = require('lodash');
const $os = require('harpoon-utils/os');
const Logger = require('harpoon-logger');
const networkFunctions = require('../../network')();

function PostgresqlFunctions(opts) {
  function _findBinDir() {
    if (!_.isUndefined(opts.binDir)) {
      return opts.binDir;
    } if ($os.isInPath('psql')) {
      return $file.dirname($os.findInPath('psql'));
    }
    throw new Error('psql client binary not found');
  }
  const binDir = _findBinDir();
  const logger = _.isEmpty(opts.logger) ? new Logger() : opts.logger;

  /**
   * Execute a command in a database server.
   * @function execute
   * @param {string} command - Command to execute
   * @param {Object} options - Options object
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.database] - Database affected by the query
   * @example
   * // Select the column usename in the table pg_users from the postgres database in the default database server
   * // identified by 'password123'
   * execute('SELECT rolname FROM pg_roles;', {database: 'postgres', password: 'password123'});
   */
  function execute(command, options) {
    const _opts = _.defaults({}, options, {
      user: 'postgres', host: '127.0.0.1', port: '5432', database: 'postgres',
    });
    const commandArguments = ['-t'];
    const commandOptions = {
      port: '-p',
      host: '-h',
      database: '-d',
      user: '-U',
    };

    _.each(commandOptions, (value, key) => {
      if (!_.isEmpty(_opts[key])) commandArguments.push(`${value}${_opts[key]}`);
    });
    commandArguments.push(`-c${command}`);
    logger.trace(`[execute] Executing psql with parameters ${commandArguments}`);
    return $os.runProgram($file.join(binDir, 'psql'), commandArguments, {env: {PGPASSWORD: _opts.password}});
  }

  /**
   * Create a database.
   * @function createDatabase
   * @param {string} database - Database to create
   * @param {Object} options - Database connection parameters
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Create database 'test' in the default database server (postgres@127.0.0.1 at port 5432)
   * identified by 'password123'
   * createDatabase('test', {password: 'password123'})
   * @example
   * // Create database 'test' in the database server admin@54.23.5.12 indentified by 'password123' at port 5000
   * createDatabase('test', {
   *   user: 'admin',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 5000
   * });
   */
  function createDatabase(database, options) {
    const _opts = _.defaults({}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    const command = `CREATE DATABASE ${database}`;
    const checkDatabase = `SELECT 1 FROM pg_database WHERE datname='${database}'`;
    if (_.includes(execute(checkDatabase, _opts), '1')) {
      throw new Error(`Database ${database} already exists`);
    } else {
      execute(command, _opts);
    }
  }

  /**
   * Drop a database.
   * @function dropDatabase
   * @param {string} database - Database to drop
   * @param {Object} options - Datatabase connection parameters
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Drop database 'test' in the default database server
   * (postgres@127.0.0.1 at port 5432) identified by 'password123'
   * dropDatabase('test', {password: 'password123'})
   * @example
   * // Drop database 'test' in the database server admin@54.23.5.12 at port 5000 identified by 'password123'
   * dropDatabase('test', {
   *   user: 'admin',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 5000
   * });
   */
  function dropDatabase(database, options) {
    const command = `DROP DATABASE IF EXISTS "${database}"`;
    execute(command, options);
  }

  /**
   * Create a user with a minimum set of privileges for a production environment or full access
   * for development environment.
   * @function createUser
   * @param {string} username - Username to create
   * @param {Object} options - Options object
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.newUserPassword] - Password of the new user
   * @param {boolean} [options.canCreateDB] - The new user can/cannot create new databases. False by default
   * @param {boolean} [options.canReplicate] - The new user can/cannot initiate streaming replication or put
   * the system in and out of backup mode. False by default.
   * @example
   * // Create the user 'user' with no password in the default database server (postgres@127.0.0.1 at port 5432)
   * // identified by 'password123'
   * createUser('user', {password: 'password123'})
   * @example
   * // Create the user 'user' identified by 'userpassword' in the database server admin@54.23.5.12 identified by
   * // 'password123' at port 5000
   * createUser('user', {
   *   user: 'postgres',
   *   newUserPassword: 'userpassword',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 5000
   * });
   */
  function createUser(username, options) {
    const _opts = _.defaults({}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    let command = `CREATE USER "${username}"`;
    if (!_.isEmpty(_opts.newUserPassword)) command += ` PASSWORD '${_opts.newUserPassword}'`;
    if (options.canCreateDB) command += ' CREATEDB';
    if (options.canReplicate) command += ' REPLICATION';
    const checkUser = `SELECT 1 FROM pg_roles WHERE rolname='${username}'`;
    if (_.includes(execute(checkUser, _opts), '1')) {
      throw new Error(`User ${username} already exists`);
    } else {
      execute(command, _opts);
    }
  }

  /**
   * Drop a user.
   * @function dropUser
   * @param {string} username - Username to drop
   * @param {Object} options - Options object
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Drop user 'test' in the default database server (postgres@127.0.0.1 at port 5432)
   * dropUser('test');
   * @example
   * // Drop user 'test' in the database server admin@54.23.5.12 at port 5000
   * // identified by 'password123'
   * dropUser('test', {
   *   user: 'postgres',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 5000
   * });
   */
  function dropUser(username, options) {
    const command = `DROP USER IF EXISTS "${username}"`;
    execute(command, options);
  }

  /**
   * Grant privileges to a user with a minimum set of privileges for a production environment or full access
   * for development environment.
   * @function grantPrivileges
   * @param {string} database - Database to grant privileges
   * @param {string} databaseUser - Owner to grant privileges
   * @param {Object} options - Options object
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Grant permissions on 'test' to 'user' in the default database server
   * // (postgres@127.0.0.1 at port 5432) identified by 'password123'
   * grantPrivileges('test', 'user', {password: 'password123'})
   * @example
   * // Grant permissions on 'test' to 'user'  in the database server
   * // admin@54.23.5.12 identified by 'password123' at port 5000
   * grantPrivileges('test', 'user', {
   *   user: 'postgres',
   *   databaseUserPassword: 'userpassword',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 5000
   * });
   */
  function grantPrivileges(database, databaseUser, options) {
    const _opts = _.defaults({}, {database: database}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    const command = `GRANT ALL PRIVILEGES ON DATABASE "${database}" TO "${databaseUser}"`;
    execute(command, _opts);
  }

  /**
   * Revoke privileges from a user.
   * @function revokePrivileges
   * @param {string} database - Database to revoke privileges
   * @param {string} databaseUser - Owner to revoke privileges
   * @param {Object} options - Options object
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Revoke permissions on 'test' from 'user' in the default database server
   * // (postgres@127.0.0.1 at port 5432) identified by 'password123'
   * revokePrivileges('test', 'user', {password: 'password123'})
   * @example
   * // Revoke permissions on 'test' to 'user' in the database server
   * // admin@54.23.5.12 identified by 'password123' at port 5000
   * revokePrivileges('test', 'user', {
   *   user: 'postgres',
   *   databaseUserPassword: 'userpassword',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 5000
   * });
   */
  function revokePrivileges(database, databaseUser, options) {
    const _opts = _.defaults({}, {database: database}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    const command = `REVOKE ALL ON DATABASE "${database}" TO "${databaseUser}"`;
    execute(command, _opts);
  }

  /**
   * Update a key value from a table in a database server.
   * @function set
   * @param {string} database - Database to use
   * @param {string} table - Table from where select
   * @param {string} key - Key to update
   * @param {string} value - Value to set
   * @param {Object} options - Options object
   * @param {string} [options.databaseUserPassword] - Password for the username@database
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Update the column rolname in the table pg_user where rolname='postgres' by 'newusername'
   * // from the database postgres in the default
   * // database server (postgres@127.0.0.1 at port 5432) identified by 'password123'
   * set({'postgres', 'pg_roles', 'rolname', 'newusername',
   * {password: 'password123', 'condition: 'rolname="postgres"'});
   */
  function update(database, table, key, value, options) {
    const _opts = _.defaults(
      {},
      {database: database},
      options,
      {user: 'postgres', host: '127.0.0.1', port: '5432'}
    );
    let command = `UPDATE ${table} SET ${key}='${value}'`;
    if (!_.isEmpty(_opts.condition)) command += ` WHERE ${_opts.condition}`;
    execute(command, _opts);
  }

  /**
   * Select a key from a table in a database server.
   * @function get
   * @param {string} database - Database to use
   * @param {string} table - Table from where select
   * @param {string} key - Key to select
   * @param {Object} options - Options object
   * @param {string} [options.condition] - Condition for the query
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Select the column usename in the table pg_users from the postgres database in the default database server
   * // (postgres@127.0.0.1 at port 5432) identified by 'password13'
   * select('postgres', 'pg_roles', 'rolname', {password: 'password123'});
   */
  function select(database, table, key, options) {
    const _opts = _.defaults({}, {database: database}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    let command = `SELECT ${key} FROM ${table}`;
    if (!_.isEmpty(_opts.condition)) command += ` WHERE ${_opts.condition}`;
    const result = _.compact(execute(command, _opts).split('\n'));
    return result;
  }

  /**
   * Insert new row in a table from a database.
   * @function insert
   * @param {string} database - Database to use
   * @param {string} table - Table where to insert
   * @param {Object} data - Data object (key:value)(column:value)
   * @param {Object} options - Options object
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // Insert a new row in the table 'companies' of 'postgres' database with values 'Qingcloud, SF'
   * // for the columns 'name, place' and setting the remaining columns to default value in
   * // the default database server identified by 'password123'
   * insert('postgres', 'companies', {'name': 'Qingcloud', 'place': 'SF'}, {password: 'password123'});
   */
  function insert(database, table, data, options) {
    const _opts = _.defaults({}, {database: database}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    let command = '';

    function _getInsertCommand(dataToInsert) {
      let result = '';
      const keys = [];
      const values = [];

      switch (typeof dataToInsert) {
        case 'object':
          if (_.isArray(dataToInsert)) {
            _.each(dataToInsert, (item) => {
              if (_.isString(item)) values.push(`'${item}'`);
              else if (_.isNull(item)) values.push('NULL');
              else values.push(item);
            });
            result = `INSERT INTO ${table} VALUES (${values.join(', ')});`;
          } else if (_.isPlainObject(dataToInsert)) {
            _.each(dataToInsert, (value, key) => {
              keys.push(key);
              if (_.isString(value)) values.push(`'${value}'`);
              else if (_.isNull(value)) values.push('NULL');
              else values.push(value);
            });
            result = `INSERT INTO ${table} (${keys.join(', ')}) VALUES (${values.join(', ')});`;
          } else if (_.isNull(dataToInsert)) {
            result = `INSERT INTO ${table} VALUES (NULL);`;
          } else {
            throw new Error('Object data not recognised');
          }
          break;
        case 'string':
          result = `INSERT INTO ${table} VALUES ('${dataToInsert}');`;
          break;
        default:
          result = `INSERT INTO ${table} VALUES (${dataToInsert});`;
          break;
      }
      return result;
    }

    if (_.isArray(data) && _.some(data, _.isObject)) {
      _.each(data, (item) => {
        command += _getInsertCommand(item);
      });
    } else {
      command += _getInsertCommand(data);
    }
    return execute(command, _opts);
  }

  /**
   * Show existing databases
   * @function base-functions.database.postgresql~showDatabases
   * @param {string} [options.user=postgres] - Database server user
   * @param {string} [options.password] - Database server user password
   * @param {string} [options.host=127.0.0.1] - Database server host
   * @param {string|number} [options.port=5432] - Database server port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @returns {string} String that contains all names of the existing databases
   */
  function showDatabases(options) {
    const _opts = _.defaults({}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    const command = 'SELECT datname FROM pg_database;';
    // options.name specifies the name of the application db, use it here because
    // the query will fail if app user does not have access to 'postgres' db
    if (!_.isUndefined(options.name)) _opts.database = options.name;
    logger.trace(`[showDatabases] Getting databases for '${_opts.user}' user`);
    const streams = execute(command, _opts);
    logger.trace(`[showDatabases] Databases: ${JSON.stringify(streams)}`);
    return streams;
  }

  /**
  * Checks if database exists
  * @function base-functions.database.postgresql~existsDatabase
  * @param {string} database - Name of the database to check
  * @param {Object} options - Options object
  * @param {string} [options.user=postgres] - Database server admin user
  * @param {string} [options.password] - Database server admin password
  * @param {string} [options.host=127.0.0.1] - Database server admin host
  * @param {string|number} [options.port=5432] - Database server admin port
  * @param {string} [options.socket] - Database server socket
  * @returns {boolean} Boolean explaining whether a DB exists or not.
  * @example
  * // checks if qingcloudRocks database exists
  * existsDatabase('qingcloudRocks', {user: 'postgres',  host: 'postgresql'});
  */
  function existsDatabase(database, options) {
    const _opts = _.defaults({}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    logger.trace(`[existsDatabase] Checking database existance for '${_opts.user}' user`);
    return _.includes(showDatabases(_opts), database);
  }

  /**
   * Checks if a user can connect to a server
   * @function base-functions.database.postgresql~canConnect
   * @function base-functions.database.postgresql~canConnect
   * @param {string} [options.user=postgres] - Database server user
   * @param {string} [options.password] - Database server user password
   * @param {string} [options.host=127.0.0.1] - Database server host
   * @param {string|number} [options.port=5432] - Database server port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @returns {Object} Object describing the connection and any related error
   */
  function canConnect(options) {
    const _opts = _.defaults({}, options, {user: 'postgres', host: '127.0.0.1', port: '5432'});
    let connection = false;
    let error = '';
    try {
      logger.trace(`[canConnect] Testing connection with '${_opts.user}' user`);
      showDatabases(_opts);
      logger.trace(`[canConnect] Connection with '${_opts.user}' user is successful`);
      connection = true;
    } catch (err) {
      logger.error(`[canConnect] Connection with '${_opts.user}' user is unsuccessful`);
      error = err;
    }
    return {connection: connection, err: error};
  }

  /**
   * Check connection with a PostgreSQL database.
   * @function base-functions.database.postgresql~checkConnection
   * @param {Object} options - Datatabase connection parameters
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * checkConnection({
   *   user: 'postgres',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function checkConnection(options) {
    const _opts = _.defaults({}, options, {user: 'postgres', host: '127.0.0.1', port: 5432});
    $app.info('Trying to connect to PostgreSQL server');
    networkFunctions.waitForService(_opts.host, _opts.port);
    $app.info(`Found PostgreSQL server listening at ${_opts.host}:${_opts.port}`);
    // The database will take some time to do the first migration
    let status = '';
    let connected = false;
    let retries = 5;
    while (!connected && retries > 0) {
      // Check can connect to PostgreSQL
      status = canConnect(_opts);
      connected = status.connection;
      // Connection refused
      retries -= 1;
      if (connected === false) {
        logger.debug('Cannot connect to PostgreSQL server. Retrying in 5 seconds...');
        if (retries === 0) throw new Error(`Cannot connect to PostgreSQL server:\n${status.err.message}`);
        $util.sleep(5);
      }
    }
    $app.info(`PostgreSQL server listening and working at ${_opts.host}:${_opts.port}`);
  }

  /**
   * Show existing users as an array
   * @function base-functions.database.postgresql~showUsers
   * @function base-functions.database.postgresql~showUsers
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @returns {string} String containing all the user names in a database
   */
  function showUsers(options) {
    return select('postgres', 'pg_roles', 'rolname', options);
  }

  /**
   * Check whether or not a user exists
   * @function base-functions.database.postgresql~existsUser
   * @function base-functions.database.postgresql~existsUser
   * @param {string} [options.user=postgres] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=5432] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @param {string} username - Name of the user to check
   * @returns {boolean} Boolean that explains whether an user exists or not
   */
  function existsUser(username, options) {
    if (_.isEmpty(username)) {
      throw new Error('User is empty');
    }
    return _.includes(showUsers(options), username);
  }

  return {
    execute,
    createDatabase,
    dropDatabase,
    createUser,
    dropUser,
    grantPrivileges,
    revokePrivileges,
    set: update,
    get: select,
    insert,
    existsDatabase,
    showDatabases,
    canConnect,
    showUsers,
    existsUser,
    checkConnection,
  };
}

module.exports = PostgresqlFunctions;
