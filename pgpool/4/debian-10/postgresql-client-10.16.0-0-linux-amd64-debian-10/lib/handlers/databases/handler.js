'use strict';

const Handler = require('../handler');

/**
 * Database handler functions.
 * @namespace handler.databases
 */
class DatabaseHandler extends Handler {
  createDatabaseForApp() {
    throw new Error('This method should be implemented');
  }

  checkConnection() {
    throw new Error('This method should be implemented');
  }

  database() {
    throw new Error('This method should be implemented');
  }
}

module.exports = DatabaseHandler;
