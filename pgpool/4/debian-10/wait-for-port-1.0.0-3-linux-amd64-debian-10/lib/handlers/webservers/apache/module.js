'use strict';

const apacheCore = $modules['com.qingcloud.apache'];
const _ = require('lodash');

/**
 * Apache handler functions.
 * @namespace handler.webservers.apache.module
 */
class Module {
  constructor() {
    this.name = '';
    this.id = '';
  }

  /**
   * Wrapper of {@link servers.apache~configureModules}
   * Will restart webserver
   * @function handler.webservers.apache.module~activate
   * @example
   * handler.module('mod_rewrite').activate()
   */
  activate() {
    return apacheCore.configureModules([{name: this.name, id: this.id}], 'enable', _.assign({restartIfRunning: true}));
  }

  /**
   * Wrapper of {@link servers.apache~configureModule}
   * Will restart webserver
   * @function handler.webservers.apache.module~deactivate
   * @example
   * handler.module('mod_rewrite').deactivate()
   */
  deactivate() {
    return apacheCore.configureModules([{name: this.name, id: this.id}], 'disable', _.assign({restartIfRunning: true}));
  }

  /**
   * Wrapper of {@link servers.apache~configureModules}
   * @function handler.webservers.apache.module~configureModules
   * @example
   * handler.module('mod_rewrite').configure('deactivate');
   * @example
   * handler.module('mod_rewrite').configure('activate', {id: 'rewrite_module', restartIfRunning: true});
   */
  configure(action, options) {
    return apacheCore.configureModules([{name: this.name, id: this.id}], action, options);
  }
}

module.exports = Module;
