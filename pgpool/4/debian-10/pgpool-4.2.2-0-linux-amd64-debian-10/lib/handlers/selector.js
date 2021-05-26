'use strict';

const _ = require('lodash');
const MysqlHandler = require('./databases/mysql');
const MongoDBHandler = require('./databases/mongodb');
const PostgresqlHandler = require('./databases/postgresql');
const ApacheHandler = require('./webservers/apache');
const TomcatHandler = require('./jspservers/tomcat');
const NginxHandler = require('./webservers/nginx');

/**
 * Handler selector.
 * @namespace handler.selector
 */

function getDatabaseHandler(variation, properties, options) {
  let result = {};
  switch (variation) {
    case 'mysql':
    case 'mariadb':
      result = new MysqlHandler(properties, options);
      break;
    case 'postgresql':
      result = new PostgresqlHandler(properties, options);
      break;
    case 'mongodb':
      result = new MongoDBHandler(properties, options);
      break;
    default:
      throw new Error(`Handler for ${variation} not found`);
  }
  return result;
}

function getWebServerHandler(variation, options) {
  let result = {};
  switch (variation) {
    case 'apache':
      result = new ApacheHandler(options);
      break;
    case 'nginx':
      result = new NginxHandler(options);
      break;
    default:
      throw new Error(`Handler for ${variation} not found`);
  }
  return result;
}

function getJspServerHandler(variation, options) {
  let result = {};
  switch (variation) {
    case 'tomcat':
      result = new TomcatHandler(options);
      break;
    default:
      throw new Error(`Handler for ${variation} not found`);
  }
  return result;
}

/**
 * Returns a handler for a specific type with its variation
 * @function handler.selector~getHandler
 * @param {string} type - Selector type. Supported values: 'database', 'webServer' and 'jspServer'
 * @param {string|Object} properties - Variation of the handler or object defining its parameters.
 * @param {string} [properties.variation] - Variation of the handler
 * @param {string} [properties.user] - (Database type only) User of the database server
 * @param {string} [properties.password] - (Database type only) Password of the user
 * @param {string} [properties.host] - (Database type only) Host of the database
 * @param {string} [properties.port] - (Database type only) Port of the database
 * @param {Object} [options]
 * @param {string} [options.cwd] - Working directory for the handler
 * @example
 * // Create handler for Apache
 * getHandler('webServer', 'apache');
 * @example
 * // Create handler for MySQL
 * getHandler('webServer', {
 *  variation: 'mariadb',
 *  user: $app.databaseAdminUser,
 *  password: $app.databaseAdminPassword,
 *  host: $app.databaseServerHost,
 *   port: $app.databaseServerPort
 * }, { cwd: $app.installdir }););
 */
function getHandler(type, properties, options) {
  let result = {};
  options = options || {};
  const variation = _.isPlainObject(properties) ? properties.variation : properties;
  switch (type) {
    case 'database':
      result = getDatabaseHandler(variation, properties, options);
      break;
    case 'webServer':
      result = getWebServerHandler(variation, options);
      break;
    case 'jspServer':
      result = getJspServerHandler(variation, options);
      break;
    default:
      throw new Error(`Handler for ${type} not found`);
  }
  return result;
}

module.exports = {
  getHandler,
};
