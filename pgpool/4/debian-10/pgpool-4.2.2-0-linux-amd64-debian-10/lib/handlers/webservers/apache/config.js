'use strict';

const apacheCore = $modules['com.qingcloud.apache'];

/**
 * Apache handler functions.
 * @namespace handler.webservers.apache.config
 */
class Config {
  /**
   * Wrapper of {@link servers.apache~configureEntry} to add an entry
   * @function handler.webservers.apache.config~add
   * @example
   * handler.config.add('AddType application/x-httpd-php .php')
   */
  add(entry, options) {
    return apacheCore.configureEntry(entry, 'add', options);
  }

  /**
   * Wrapper of {@link servers.apache~configureEntry} to remove an entry
   * @function handler.webservers.apache.config~remove
   * @example
   * handler.config.remove(AddType application/x-httpd-php .php')
   */
  remove(entry, options) {
    return apacheCore.configureEntry(entry, 'remove', options);
  }
}

module.exports = Config;
