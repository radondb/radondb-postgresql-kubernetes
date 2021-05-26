'use strict';

const Handler = require('../handler');

/**
 * JSP server handler functions.
 * @namespace handler.jspservers
 */

class JspServerHandler extends Handler {
  // Application management
  setInstallingPage() {
    throw new Error('This method should be implemented');
  }

  removeInstallingPage() {
    throw new Error('This method should be implemented');
  }

  addEnvironmentVar() {
    throw new Error('This method should be implemented');
  }

  waitForLogEntry() {
    throw new Error('This method should be implemented');
  }

  deploy() {
    throw new Error('This method should be implemented');
  }

  // Service management
  start() {
    throw new Error('This method should be implemented');
  }

  stop() {
    throw new Error('This method should be implemented');
  }

  restart() {
    throw new Error('This method should be implemented');
  }
}

module.exports = JspServerHandler;
