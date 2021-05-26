'use strict';

const _ = require('lodash');
const $os = require('harpoon-utils/os');
const Logger = require('harpoon-logger');
const networkFunctions = require('../../network')();

/**
 * MySQL functions.
 * @namespace base-functions.database.mysql
 */

function MysqlFunctions(opts) {
  function _findBinDir() {
    if (!_.isUndefined(opts.binDir)) {
      return opts.binDir;
    } if ($os.isInPath('mysql')) {
      return $file.dirname($os.findInPath('mysql'));
    }
    throw new Error('mysql client binary not found');
  }
  const binDir = _findBinDir();
  const logger = _.isEmpty(opts.logger) ? new Logger() : opts.logger;

  /**
   * Execute a command in a database server.
   * @function base-functions.database.mysql~execute
   * @param {string} [options.user=root] - Database server user
   * @param {string} [options.password] - Database server user password
   * @param {string} [options.host=localhost] - Database server host
   * @param {string|number} [options.port=3306] - Database server port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @param {string} [options.database] - Database affected by the query
   * @param {string} command - Command to execute
   * @param {Object} options - Options object
   * @example
   * // Select the column Users in the table user from the mysql database in the default database server
   * // identified by 'password123'
   * execute('SELECT User FROM user;', {database: 'mysql', password: 'password123'});
   */
  function execute(command, options) {
    const _runOptions = _.defaults({}, options, {retrieveStdStreams: false});
    const _mysqlOptions = _.defaults({}, _.pick(options, [
      'user', 'password', 'host',
      'port', 'socket', 'defaultsFile',
      'database', 'command', 'sslCAFile',
    ]), {user: 'root', host: 'localhost', port: '3306'});
    if (_.isEmpty(command)) {
      throw new Error('Command to execute is empty');
    }
    const commandArguments = ['-N'];
    const commandOptions = {
      port: '-P ',
      host: '-h',
      database: '-D',
      defaultsFile: '--defaults-file=',
      user: '-u',
      password: '-p',
      sslCAFile: '--ssl-ca=',
    };

    // Populate mysql command from options and command arguments
    _.each(commandOptions, (value, key) => {
      if (!_.isEmpty(_mysqlOptions[key])) commandArguments.push(`${value}${_mysqlOptions[key]}`);
    });
    commandArguments.push(`-e ${command}`);

    // Execute command and retrieve streams
    logger.trace(`[execute] Executing: mysql ${commandArguments.join(' ')}`);
    const streams = $os.runProgram(
      $file.join(binDir, 'mysql'),
      commandArguments,
      _.merge({}, _runOptions, {retrieveStdStreams: true})
    );
    logger.trace(`[execute] Result: ${JSON.stringify(streams)}`);
    if (streams.code !== 0) {
      throw new Error(`MySQL command failed to run. Error: \n ${streams.stderr}`);
    }
    return _runOptions.retrieveStdStreams ? streams : streams.stdout;
  }

  /**
   * Update a key value from a table in a database server.
   * @function base-functions.database.mysql~set
   * @param {string} database - Database to use
   * @param {string} table - Table from where select
   * @param {string} key - Key to update
   * @param {string} value - Value to set
   * @param {Object} options - Options object
   * @param {Object} [options.where] - Conditions for the query
   * @param {string} [options.databaseUserPassword] - Password for the username@database
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Update the column Users in the table user where User='root' by 'test' from the mysql database in the default
   * // database server (root@localhost at port 3306) identified by 'password13'
   * set('mysql', 'user', 'User', 'test', {password: 'password123', where: { User: 'root'}});
   * @example
   * // Update the column Users in the table user where User='root' AND Host='localhost' by 'test' from the mysql
   * // database in the default database server (root@localhost at port 3306) identified by 'password13'
   * set('mysql', 'user', 'User', 'test', {password: 'password123', where: { User: 'root', Host: 'localhost'}});
   */
  function sqlSet(database, table, key, value, options) {
    options = _.defaults(options || {}, {database});
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    if (_.isEmpty(table)) {
      throw new Error('Table name is empty');
    }
    if (_.isEmpty(key)) {
      throw new Error('Key to select is empty');
    }
    if (_.isEmpty(value) && !_.isNumber(value)) {
      throw new Error('Value to set is empty');
    }
    let command = `UPDATE ${table} SET ${key}='${value}'`;
    if (!_.isEmpty(options.where)) {
      command += ` WHERE ${_.map(options.where, (v, k) => `${k}='${v}'`).join(' AND ')}`;
    }
    if (!_.isEmpty(options.limit)) command += ` LIMIT ${options.limit}`;
    command += ';';
    execute(command, options);
  }

  /**
   * Select a key from a table in a database server.
   * @function base-functions.database.mysql~get
   * @param {string} database - Database to use
   * @param {string} table - Table from where select
   * @param {string} key - Key to select
   * @param {Object} options - Options object
   * @param {string} [options.where] - Condition for the query
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Select the column Users in the table user from the mysql database in the default database server
   * // (root@localhost at port 3306) identified by 'password13'
   * get('mysql', 'user', 'User', {password: 'password123', where: { User: 'root'}});
   */
  function sqlGet(database, table, key, options) {
    options = _.defaults(options || {}, {database});
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    if (_.isEmpty(table)) {
      throw new Error('Table name is empty');
    }
    if (_.isEmpty(key)) {
      throw new Error('Key to select is empty');
    }
    let command = `SELECT ${key} FROM ${table}`;
    if (!_.isEmpty(options.where)) {
      command += ` WHERE ${_.map(options.where, (v, k) => `${k}='${v}'`).join(' AND ')}`;
    }
    if (!_.isEmpty(options.limit)) command += ` LIMIT ${options.limit}`;
    command += ';';
    return _.compact(execute(command, options).split('\n'));
  }

  /**
   * Insert new row in a table from a database.
   * @function base-functions.database.mysql~insert
   * @param {string} database - Database to use
   * @param {string} table - Table where to insert
   * @param {Object|Array} data - Data object (column:value) or array of values
   * @param {Object} options - Options object
   * @param {string} [options.condition] - Condition for the query
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Insert a new row in the table 'companies' with values 'Qingcloud, SF' for the columns 'name, place'
   * // and setting the remaining columns to default value in the default database server identified by 'password123'
   * insert('mysql', 'user', {'name': 'Qingcloud', 'place': 'SF'}, {password: 'password123'});
   * @example
   * // Insert a new row in the table 'companies' with values 'Qingcloud, SF' for the columns 'name, place'
   * // and setting the remaining columns to default value in the default database server identified by 'password123'
   * insert('mysql', 'user', ['Qingcloud', 'SF'], {password: 'password123'});
   */
  function insert(database, table, data, options) {
    options = _.defaults(options || {}, {database});
    let command = '';
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    if (_.isEmpty(table)) {
      throw new Error('Table name is empty');
    }
    if (_.isEmpty(data)) {
      throw new Error('Data object is empty');
    }
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
              else if (_.isUndefined(item)) throw new Error(`Some value from your 'insert' command is undefined`);
              else values.push(item);
            });
            result = `INSERT INTO ${table} VALUES (${values.join(', ')});`;
          } else if (_.isPlainObject(dataToInsert)) {
            _.each(dataToInsert, (value, key) => {
              keys.push(key);
              if (_.isString(value)) values.push(`'${value}'`);
              else if (_.isNull(value)) values.push('NULL');
              else if (_.isUndefined(value)) throw new Error(`Some value from your 'insert' command is undefined`);
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
    return execute(command, options);
  }

  /**
   * Create a database.
   * @function base-functions.database.mysql~createDatabase
   * @param {string} database - Database to create
   * @param {Object} options - Datatabase connection parameters
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @param {Object} options - Options object
   * @example
   * // Create database 'test' in the default database server (root@localhost at port 3306) identified by 'password123'
   * createDatabase('test', {password: 'password123'})
   * @example
   * // Create database 'test' in the database server admin@54.23.5.12 indentified by 'password123' at port 3300
   * createDatabase('test', {
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function createDatabase(database, options) {
    const command = `CREATE DATABASE IF NOT EXISTS \`${database}\``;
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    return execute(command, options);
  }

  /**
   * Drop a database.
   * @function base-functions.database.mysql~dropDatabase
   * @param {string} database - Database to drop
   * @param {Object} options - Datatabase connection parameters
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Drop database 'test' in the default database server (root@localhost at port 3306) identified by 'password123'
   * dropDatabase('test', {password: 'password123'})
   * @example
   * // Drop database 'test' in the database server admin@54.23.5.12 at port 3300 identified by 'password123'
   * dropDatabase('test', {
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function dropDatabase(database, options) {
    const command = `DROP DATABASE IF EXISTS \`${database}\``;
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    execute(command, options);
  }

  /**
   * Show existing users as an array
   * @function base-functions.database.mysql~showUsers
   * @function base-functions.database.mysql~showUsers
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   */
  function showUsers(options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: '3306'});
    return sqlGet('mysql', 'user', 'User', _opts);
  }

  /**
   * Check whether or noot a user exists
   * @function base-functions.database.mysql~existsUser
   * @function base-functions.database.mysql~existsUser
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @param {string} username - Name of the user to check
   */
  function existsUser(username, options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: '3306'});
    if (_.isEmpty(username)) {
      throw new Error('User is empty');
    }
    return _.includes(showUsers(_opts), username);
  }

  /**
   * Create a user with a minimum set of privileges for a production environment or full access
   * for development environment.
   * @function base-functions.database.mysql~createUser
   * @param {string} username - Username to create
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.authPlugin] - Autentication plugin to be used
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Create the user 'user' in the default database server (root@localhost at port 3306) identified by 'password123'
   * createUser('user', {password: 'password123'})
   * @example
   * // Create the user 'user' identified by 'userpassword' in the database server admin@54.23.5.12 identified by
   * // 'password123' at port 3300
   * createUser('user', {
   *   user: 'root',
   *   newUserPassword: 'userpassword',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * }, 'user');
   */
  function createUser(username, options) {
    // If the user is created using %, it's not necessary to create another one for localhost
    options = _.defaults(options || {}, {fromHosts: ['%']});
    if (username !== 'root' && !_.includes(options.fromHosts, '%')) {
      options.fromHosts = _.union(options.fromHosts, ['localhost']);
    }
    if (_.isEmpty(username)) {
      throw new Error('User is empty');
    }
    _.forEach(_.flatten(options.fromHosts), function(fromHost) {
      let command = `CREATE USER '${username}'@'${fromHost}'`;
      if (!_.isEmpty(options.newUserPassword)) {
        command += ' IDENTIFIED';
        if (!_.isEmpty(options.authPlugin)) command += ` WITH '${options.authPlugin}'`;
        command += ` BY '${options.newUserPassword}'`;
      }
      execute(command, options);
    });
  }

  /**
   * Drop a user.
   * @function base-functions.database.mysql~dropUser
   * @param {string} username - Username to drop
   * @param {Object} options - Options object
   * @param {string} [options.newUserPassword] - Password for the username
   * @param {string[]} [options.fromHosts=localhost, %] - Granted hosts for username
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Drop user 'test' in the default database server (root@localhost at port 3306)
   * dropUser('test');
   * @example
   * // Drop user 'test' from the specific host mariadb in the database server admin@54.23.5.12 at port 3300
   * // identified by 'password123'
   * dropUser('test', {
   *   user: 'root',
   *   password: 'password123',
   *   fromHosts: ['mariadb'],
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function dropUser(username, options) {
    options = _.defaults(options || {}, {fromHosts: ['%']});
    if (username !== 'root' && !_.includes(options.fromHosts, '%')) {
      options.fromHosts = _.union(options.fromHosts, ['localhost']);
    }
    if (_.isEmpty(username)) {
      throw new Error('User is empty');
    }
    const users = sqlGet(options, 'mysql', 'user', 'user');
    if (_.includes(users, username)) {
      _.forEach(_.flatten(options.fromHosts), (fromHost) => {
        const command = `DROP USER '${username}'@'${fromHost}'`;
        execute(command, options);
      });
    }
  }

  /**
   * Grant privileges to a user with a minimum set of privileges for a production environment or full access
   * for development environment.
   * @function base-functions.database.mysql~grantPrivileges
   * @param {string} database - Database to create
   * @param {string} username - Owner of the database
   * @param {Object} options - Options object
   * @param {string[]} [options.fromHosts=localhost, %] - Granted hosts for username
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.authPlugin] - Autentication plugin to be used
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @param {string[]} [options.privileges=ALL] - Array of privileges to grant
   * @example
   * // Create the user 'user' and grant permissions on 'test' in the default database server
   * // (root@localhost at port 3306) identified by 'password123'
   * grantPrivileges('test', 'user', {password: 'password123'})
   * @example
   * // Create the user 'user' identified by 'userpassword' and grant permissions on 'test' in the database server
   * // admin@54.23.5.12 identified by 'password123' at port 3300
   * grantPrivileges('test', 'user', {
   *   user: 'root',
   *   databaseUserPassword: 'userpassword',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function grantPrivileges(database, username, options) {
    options = _.defaults(options || {}, {
      fromHosts: ['%'],
      isPattern: false,
      withGrantOption: false,
      privileges: ['ALL'],
    });
    if (username !== 'root' && !_.includes(options.fromHosts, '%')) {
      options.fromHosts = _.union(options.fromHosts, ['localhost']);
    }
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    if (_.isEmpty(username)) {
      throw new Error('Database user is empty');
    }
    if (!options.isPattern) {
      database = `\`${database}\``;
    }
    _.forEach(_.flatten(options.fromHosts), (fromHost) => {
      let command = `GRANT ${options.privileges.join(', ')} ON ${database}.* TO '${username}'@'${fromHost}'`;
      if (!_.isEmpty(options.databaseUserPassword)) {
        command += ' IDENTIFIED';
        if (!_.isEmpty(options.authPlugin)) command += ` WITH '${options.authPlugin}'`;
        command += ` BY '${options.databaseUserPassword}'`;
      }
      if (options.withGrantOption) command += ' WITH GRANT OPTION';
      execute(command, options);
    });
  }

  /**
   * Show existing databases
   * @function base-functions.database.mysql~showDatabases
   * @function base-functions.database.mysql~showDatabases
   * @param {string} [options.user=root] - Database server user
   * @param {string} [options.password] - Database server user password
   * @param {string} [options.host=localhost] - Database server host
   * @param {string|number} [options.port=3306] - Database server port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   */
  function showDatabases(options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: '3306'});
    const command = 'SHOW DATABASES;';
    logger.trace(`[showDatabases] Getting databases for '${_opts.user}' user`);
    const streams = execute(command, _opts);
    logger.trace(`[showDatabases] Databases: ${JSON.stringify(streams.stdout) || streams}`);
    return streams;
  }

  /**
   * Checks if a user can connect to a server
   * @function base-functions.database.mysql~canConnect
   * @function base-functions.database.mysql~canConnect
   * @param {string} [options.user=root] - Database server user
   * @param {string} [options.password] - Database server user password
   * @param {string} [options.host=localhost] - Database server host
   * @param {string|number} [options.port=3306] - Database server port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @returns {Object} Object describing the connection and any related error
   */
  function canConnect(options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: '3306'});
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
   * Check connection with a MySQL database.
   * @function base-functions.database.mysql~checkConnection
   * @param {Object} options - Datatabase connection parameters
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * checkConnection({
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function checkConnection(options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: 3306});
    logger.info('Trying to connect to MySQL server');
    networkFunctions.waitForService(_opts.host, _opts.port);
    logger.info(`Found MySQL server listening at ${_opts.host}:${_opts.port}`);
    // The database will take some time to do the first migration
    let status = '';
    let connected = false;
    let retries = 5;
    while (!connected && retries > 0) {
      // Check can connect to MySQL
      status = canConnect(_opts);
      connected = status.connection;
      // Connection refused
      retries -= 1;
      if (connected === false) {
        logger.debug('Cannot connect to MySQL server. Retrying in 5 seconds...');
        if (retries === 0) throw new Error(`Cannot connect to MySQL server:\n${status.err.message}`);
        $util.sleep(5);
      }
    }
    logger.info(`MySQL server listening and working at ${_opts.host}:${_opts.port}`);
  }

  /**
   * Flush privileges in a database server
   * @function base-functions.database.mysql~flushPrivileges
   * @function base-functions.database.mysql~flushPrivileges
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {Object} options - Options object
   * @example
   * // frush privileges
   * flushPrivileges({user: 'root', password: 'qingcloudRocks', host: 'mariadb'});
   */
  function flushPrivileges(options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: '3306'});
    const command = 'FLUSH PRIVILEGES;';
    return execute(command, _opts);
  }

  /**
   * Execute sql file in a database server.
   * @function base-functions.database.mysql~executeSqlFile
   * @param {string} sqlFile - SQL file containing the commands
   * @param {Object} options - Options object
   * @param {string} [options.database] - Database to execute the queries on
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} options.password - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @param {string} [options.sslCAFile] - SSL CA certificate for connections
   * @param {string} [options.defaultsFile] - Database server defaults file
   * @example
   * // Execute the sql file 'mycommands.sql' in the default database server
   * // (root@localhost at port 3306) identified by 'password123'
   * executeSqlFile('mycommands.sql', {password: 'password123'})
   */
  function executeSqlFile(sqlFile, options) {
    if (_.isEmpty(sqlFile)) {
      throw new Error('SQL file path is empty');
    }
    const command = `source ${sqlFile}`;
    return execute(command, options);
  }

  /**
   * Checks if database exists
   * @function base-functions.database.mysql~existsDatabase
   * @param {string} database - Name of the database to check
   * @param {Object} options - Options objecti
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=localhost] - Database server admin host
   * @param {string|number} [options.port=3306] - Database server admin port
   * @param {string} [options.socket] - Database server socket
   * @example
   * // checks if qingcloudRocks database exists
   * existsDatabase('qingcloudRocks', {user: 'root',  host: 'mariadb'});
   */
  function existsDatabase(database, options) {
    const _opts = _.defaults({}, options, {user: 'root', host: 'localhost', port: '3306'});
    if (_.isEmpty(database)) {
      throw new Error('Database name is empty');
    }
    logger.trace(`[existsDatabase] Checking database existance for '${_opts.user}' user`);
    return _.includes(showDatabases(_opts), database);
  }

  return {
    execute,
    executeSqlFile,
    set: sqlSet,
    get: sqlGet,
    createDatabase,
    createUser,
    dropUser,
    dropDatabase,
    grantPrivileges,
    insert,
    existsDatabase,
    checkConnection,
    canConnect,
    showDatabases,
    showUsers,
    existsUser,
    flushPrivileges,
  };
}

module.exports = MysqlFunctions;
