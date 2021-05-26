'use strict';

const Handler = require('../handler');

/**
 * Web server handler functions.
 * @namespace handler.webservers
 */

class WebServerHandler extends Handler {
  addAppVhost() {
    throw new Error('This method should be implemented');
  }
}

module.exports = WebServerHandler;
