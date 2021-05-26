'use strict';

const _ = require('lodash');
const $os = require('harpoon-utils/os');
const Logger = require('harpoon-logger');
const networkFunctions = require('../../network')();

/**
 * MongoDB functions.
 * @namespace base-functions.database.mongodb
 */

function MongoDBFunctions(opts) {
  function _findBinDir() {
    if (!_.isUndefined(opts.binDir)) {
      return opts.binDir;
    } if ($os.isInPath('mongo')) {
      return $file.dirname($os.findInPath('mongo'));
    }
    throw new Error('mongodb client binary not found');
  }
  const binDir = _findBinDir();
  const logger = _.isEmpty(opts.logger) ? new Logger() : opts.logger;

  /**
   * Execute a command in a database server.
   * @function base-functions.database.mongodb~execute
   * @param {string} [command] - Command to execute
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Initiate replica set mode in the default database server.
   * execute('rs.initiate()');
   */
  function execute(command, options) {
    options = _.defaults(options || {}, {database: 'admin'});
    const commandArguments = ['--quiet', options.database];
    const commandOptions = {
      host: '--host',
      port: '--port',
      user: '-u',
      password: '-p',
    };

    // If password is empty it means no auth, do not specify user
    if (_.isEmpty(options.password)) {
      delete commandOptions.user;
      delete commandOptions.password;
    }

    _.each(commandOptions, (value, key) => {
      if (!_.isEmpty(options[key])) {
        commandArguments.push(value, options[key]);
      }
    });
    commandArguments.push('--eval');
    commandArguments.push(command);

    const streams = $os.runProgram($file.join(binDir, 'mongo'), commandArguments, {retrieveStdStreams: true, logger});
    logger.trace(JSON.stringify(streams, null, 2));
    return streams;
  }

  /**
   * Execute a javascript file in database.
   * @function base-functions.database.mongodb~executeFile
   * @param {string} file - Javascript file to execute
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Execute a javascript file
   * executeFile('newUser.js');
   */
  function executeFile(file, options) {
    options = _.defaults(options || {}, {database: 'admin'});
    const commandArguments = ['--quiet'];
    const commandOptions = {
      host: '--host',
      port: '--port',
      database: '--authenticationDatabase',
      user: '-u',
      password: '-p',
    };
    _.each(commandOptions, (value, key) => {
      if (!_.isEmpty(options[key])) {
        commandArguments.push(value);
        commandArguments.push(options[key]);
      }
    });
    commandArguments.push(file);
    return $os.runProgram($file.join(binDir, 'mongo'), commandArguments, {logger});
  }

  /**
   * Create a collection.
   * @function base-functions.database.mongodb~createCollection
   * @param {string} collection - Collection to create
   * @param {string} database - Database where the collection belongs
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Create collection 'test' inside database 'testdb' database server (root@127.0.0.1 at port 27017)
   * // identified by 'password123'
   * createCollection('test','testdb', {password: 'password123'})
   * @example
   * // Create collection 'test' inside database 'testdb' database server 54.23.5.12 indentified by 'root/password123'
   * createCollection('test', 'testdb',{
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12'
   * });
   */
  function createCollection(collection, database, options) {
    return execute(`db = db.getSiblingDB('${database}');db.createCollection('${collection}')`, options);
  }

  /**
   * Drop a collection.
   * @function base-functions.database.mongodb~dropCollection
   * @param {string} collection - Collection to drop
   * @param {string} database - Database where the collection belongs
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Drop collection 'test' from database 'testdb' in the default database server
   * (root@127.0.0.1 at port 27017) identified by 'password123'
   * dropDatabase('test', 'testdb')
   * @example
   * // Drop collection 'test' from database 'testdb' server admin@54.23.5.12 at port 27018 identified by 'password123'
   * dropDatabase('test', 'testdb', {
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 27018
   * });
   */
  function dropCollection(collection, database, options) {
    return execute(`db = db.getSiblingDB('${database}'); db.${collection}.drop()`, options);
  }

  /**
   * Create a database.
   * @function base-functions.database.mongodb~createDatabase
   * @param {string} database - New database to create
   * @param {string} collection - New collection to create
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Create database 'test' in the default database server (root@127.0.0.1 at port 27017) identified by 'password123'
   * createDatabase('test', 'test', {password: 'password123'})
   * @example
   * // Create database 'test' in the database server admin@54.23.5.12 indentified by 'password123' at port 27018
   * createDatabase('test', 'test', {
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 27018
   * });
   */
  function createDatabase(database, collection, options) {
    return execute(`db = db.getSiblingDB('${database}'); db.createCollection('${collection}')`, options);
  }

  /**
   * Drop a database
   * @function base-functions.database.mongodb~dropDatabase
   * @param {string} database - Database where the collection belongs
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Drop database 'test' in the default database server (root@127.0.0.1 at port 27017) identified by 'password123'
   * dropDatabase('test', {password: 'password123'})
   * @example
   * // Drop database 'test' in the database server admin@54.23.5.12 at port 27018 identified by 'password123'
   * dropDatabase('test', {
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 27018
   * });
   */
  function dropDatabase(database, options) {
    return execute(`db = db.getSiblingDB('${database}'); db.dropDatabase()`, options);
  }

  /**
   * Create a user with a minimum set of privileges for a production environment or full access
   * for development environment.
   * @function base-functions.database.mongodb~createUser
   * @param {string} username - Username to create
   * @param {string} password - Password for the newly created password
   * @param {string} database - Database where the user will have privileges
   * @param {Object} roles - Object describing role or roles and the database where it takes effect.
   * If the database is empty, it will be resolved to the database where the command is running from.
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Create the user 'user' with role in database 'test' identified by 'password123'
   * createUser('user', 'password123', 'userdb', { role: 'readWrite', db: 'test' })
   * // Create user 'user' with 'readWrite' role in database 'userdb' (where the command is running from).
   * createUser('user', 'password123', 'userdb', 'readWrite')
   * // Create user 'user' with 'readWrite' role in database 'userdb' and 'dbAdmin' role in database 'test'
   * createUser('user', 'password123', 'userdb', [{ role: 'readWrite', db: 'userdb'}, { role: 'dbAdmin', db: 'test'}])
   */
  function createUser(username, password, database, roles, options) {
    const user = {
      user: username,
      pwd: password,
      roles: _.flatten([roles]),
    };
    const command = `db.getSiblingDB('${database}').createUser(${JSON.stringify(user)})`;
    return execute(command, options);
  }

  /**
   * Drop a user.
   * @function base-functions.database.mongodb~dropUser
   * @param {string} username - Username to drop
   * @param {string} database - Database where the user belogs
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Drop user 'test' from database 'testdb' the default database server (root@127.0.0.1 at port 27017)
   * dropUser('test', 'testdb');
   */
  function dropUser(username, database, options) {
    return execute(`db = db.getSiblingDB('${database}');db.dropUser('${username}')`, options);
  }

  /**
   * Grant privileges to a user with a minimum set of privileges for a production environment or full access
   * for development environment.
   * @function base-functions.database.mongodb~grantRoles
   * @param {string} username -Database user
   * @param {string} database - Database where the user belongs
   * @param {Object} roles - Object describing role or roles and the database where it takes effect.
   * If the database is empty, it will be resolved to the database where the command is running from.
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Grant 'dbOwner' and 'readWrite' roles to the user 'user' on 'userdb' database.
   * grantRoles('user', 'userdb', '[{role: 'dbOwner', db: 'userdb'}, {role: 'readWrite', db: 'userdb'}]);
   * // Grant 'root' permissions to the user 'user' on 'userdb' database in the default database server
   * // (root@127.0.0.1 at port 27017) identified by 'password123'
   * grantRoles('user', 'userdb', '{ role: 'root', db: 'admin' }', {user: 'root', password: 'qwer1234'});
   */
  function grantRoles(username, database, roles, options) {
    roles = _.flatten([roles]);
    const command = `db.getSiblingDB('${database}').grantRolesToUser('${username}', ${JSON.stringify(roles)})`;
    return execute(command, options);
  }

  /**
   * Update a whole document from a specific collection
   * @function base-functions.database.mongodb~updateDocuments
   * @param {string} database - Database to use
   * @param {string} collection - Collection from where select
   * @param {string} query - Query to match document
   * @param {string} update - Data object
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Update the document from collection 'collection' in database  'database' matching 'query' selector
   * // with the content of the object 'update'
   * updateDocument('testdb', 'test', {message: "qingcloud"}, {message: "qingcloud is awesome!"});
   */
  function updateDocuments(database, collection, query, update, options) {
    const command = `db = db.getSiblingDB('${database}');`
            + `db.${collection}.update(${JSON.stringify(query, null, 2)}, ${JSON.stringify(update, null, 2)})`;
    return execute(command, options);
  }

  /**
   * Update specific field inside a document.
   * @function base-functions.database.mongodb~updateFields
   * @param {string} database - Database to use
   * @param {string} collection - Table from where select
   * @param {Object} query - Query to select
   * @param {Object} fields - Fields object containing the pairs key-value to update
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Based on the following document:
   *{
   *  "_id" : ObjectId("573ef26268e613a775a1de89"),
   *  "name" : "qingcloud",
   *  "values" : {
   *      "first" : "make it so",
   *      "second" : "stay curious"
   *  }
   *}
   * // Update fields 'name' and 'second' using admin database credentials
   * updateFields('qingclouddb', 'qingcloudcol', {name: "qingcloud"},
   *               {"name":"qingcloud", "lastname.second": "value simplicity"},
   *               {user: 'root', password: 'qwer1234', database: 'admin'})
   */
  function updateFields(database, collection, query, fields, options) {
    const command = `db = db.getSiblingDB('${database}');`
            + `db.${collection}.update(${JSON.stringify(query, null, 2)}, {$set: ${JSON.stringify(fields, null, 2)}})`;
    return execute(command, options);
  }

  /**
   * Select a key from a table in a database server.
   * @function base-functions.database.mongodb~getDocuments
   * @param {string} database - Database to use
   * @param {string} collection - Collection where make the query
   * @param {Object} query - Query object to select
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Select the column Users in the table user from the mysql database in the default database server
   * // (root@127.0.0.1 at port 27017) identified by 'password13'
   * getDocuments('qingclouddb', 'qingcloudcol', {name: "qingcloud"}, {password: 'password123', where: { User: 'root'}});
   */
  function getDocuments(database, collection, query, options) {
    const command = `db = db.getSiblingDB('${database}'); db.${collection}.find(${JSON.stringify(query, null, 2)})`;
    return execute(command, options);
  }

  /**
   * Insert new document in a collection from a database.
   * @function base-functions.database.mongodb~insert
   * @param {string} database - Database to use
   * @param {string} collection - Table where to insert
   * @param {Object} data - Data object (column:value)
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @param {string} [options.database=admin] - Database where the command will be executed
   * @example
   * // Insert a new row in the table 'companies' with values 'Qingcloud, SF' for the columns 'name, place'
   * // and setting the remaining columns to default value in the default database server identified by 'password123'
   * insert('qingclouddb', 'qingcloudcol', {'name': 'Qingcloud', 'place': 'SF'}, {password: 'password123'});
   */
  function insert(database, collection, data, options) {
    const command = `db = db.getSiblingDB('${database}'); db.${collection}.insert(${JSON.stringify(data, null, 2)})`;
    return execute(command, options);
  }

  /**
   * Check connection with a MongoDB database.
   * @function base-functions.database.mongodb~checkConnection
   * @param {Object} options - Options object
   * @param {string} [options.user=root] - Database server admin user
   * @param {string} [options.password] - Database server admin password
   * @param {string} [options.host=127.0.0.1] - Database server admin host
   * @param {string|number} [options.port=27017] - Database server admin port
   * @example
   * checkConnection({
   *   user: 'root',
   *   password: 'password123',
   *   host: '54.23.5.12',
   *   port: 3300
   * });
   */
  function checkConnection(options) {
    options = _.defaults(options || {}, {host: '127.0.0.1', port: 27017});
    logger.info('Trying to connect to MongoDB server');
    networkFunctions.waitForService(options.host, options.port);
    logger.info(`Found MongoDB server listening at ${options.host}:${options.port}`);
    let connected = false;
    let retries = 30;
    while (!connected && retries > 0) {
      // Check can execute a query in MongoDB
      const result = execute('db.getUsers()', options);
      connected = (result.code === 0);
      if (!connected) {
        retries -= 1;
        logger.info(`Cannot connect to MongoDB server. Retrying in 5 seconds...:
${result.stdout}\n\n${result.stderr}`);
        if (retries === 0) {
          throw new Error(`Cannot connect to MongoDB server. Aborting:\n${result.stdout}\n${result.stderr}`);
        }
        $util.sleep(5);
      }
    }
    logger.info(`MongoDB server listening and working at ${options.host}:${options.port}`);
  }

  return {
    execute,
    executeFile,
    createUser,
    dropUser,
    grantRoles,
    createDatabase,
    dropDatabase,
    createCollection,
    dropCollection,
    getDocuments,
    updateDocuments,
    updateFields,
    insert,
    checkConnection,
  };
}

module.exports = MongoDBFunctions;
