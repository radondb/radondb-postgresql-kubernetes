'use strict';

const _ = require('lodash');
const WebServerHandler = require('../handler');
const Module = require('./module');
const Config = require('./config');

const apacheCore = $modules['com.qingcloud.apache'];

/**
 * Apache handler functions.
 * @namespace handler.webservers.apache
 */
class ApacheHandler extends WebServerHandler {
  constructor(options) {
    super(options);
    _.assign(this, {
      user: apacheCore.systemUser,
      group: apacheCore.systemGroup,
      httpPort: apacheCore.httpPort,
      httpsPort: apacheCore.httpsPort,
      configureModules: apacheCore.configureModules,
      config: new Config(),
      moduleObject: new Module(),
    }, _.pick(
      apacheCore,
      ['installdir', 'confDir', 'binDir', 'dataDir', 'logsDir', 'libDir', 'persistDir']
    ));
  }

  module(mod) {
    const _module = _.isPlainObject(mod) ? {name: mod.name, id: mod.id} : {name: mod, id: mod};
    return _.assign(this.moduleObject, _module);
  }

  /**
   * Restart the Apache server
   * @function handler.webservers.apache~restart
   * @example
   * restart();
   */
  restart() {
    this.logger.debug('Restarting apache...');
    return apacheCore.restart();
  }

  /**
   * Reload configuration
   * @function handler.webservers.apache~reload
   * @example
   * reload();
   */
  reload() {
    this.logger.debug('Reloading apache configuration...');
    return apacheCore.reload();
  }

  /**
   * Start the Apache server
   * @function handler.webservers.apache~start
   * @example
   * start();
   */
  start() {
    this.logger.debug('Starting apache...');
    return apacheCore.start();
  }

  /**
   * Stop the Apache server
   * @function handler.webservers.apache~stop
   * @example
   * stop();
   */
  stop() {
    this.logger.debug('Stopping apache...');
    return apacheCore.stop();
  }

  /**
   * Get the status of the Apache server
   * @function handler.webservers.apache~status
   * @example
   * status();
   */
  status() {
    return apacheCore.status();
  }

  _getHostText(hosts, port) {
    if (!_.isArray(hosts)) hosts = [hosts];
    return _.map(hosts, (h) => ` ${h}:${port}`).join(' ');
  }

  /**
   * Add a virtual host for an application
   * @function handler.webservers.apache~addAppVhost
   * @param {string} name - Application name for which configure the virtual host
   * @param {Object} [options] - Options object
   * @param {string} [options.type] - Application runtime type (php, java, node, python, ruby)
   * @param {string[]} [options.hosts=127.0.0.1, _default_] - Server hosts
   * @param {number} [options.port] - HTTP port
   * @param {string} [options.documentRoot] - Application document root
   * @param {string} [options.requireOption] - Application Require option
   * @param {boolean} [options.moveHtaccess=true] - Move application htaccess to a configuration file
   * @param {boolean} [options.enableHttps=true] - Create another virtual Host using HTTPS certificates
   * @param {number} [options.httpsPort] - HTTPS port
   * @param {string} [options.certificatePath='conf/qingcloud/certs/'] - Path to server.crt and server.key files
   * @param {string} [options.extraDirectoryConfiguration] - Additional configuration in the document root directory
   * @example
   * // Configure default vhost file
   * addAppVhost('drupal');
   */
  addAppVhost(name, options) {
    options = _.defaults(options || {}, {
      type: 'php',
      hosts: ['127.0.0.1', '_default_'],
      port: apacheCore.httpPort,
      documentRoot: $file.join($file.dirname(this.cwd), name),
      requireOption: 'all granted',
      moveHtaccess: true,
      enableHttps: true,
      httpsPort: apacheCore.httpsPort,
      certificatePath: $file.join(apacheCore.confDir, 'qingcloud/certs/'),
      extraDirectoryConfiguration: '',
      additionalConfiguration: '',
    });
    if (options.moveHtaccess) apacheCore.replaceHtaccessFiles(name);
    if ($file.exists($file.join(apacheCore.htaccessDir, `${name}-htaccess.conf`))) {
      options.allowOverride = 'None';
      options.htaccess = `Include ${$file.join(apacheCore.htaccessDir, `${name}-htaccess.conf`)}`;
    } else {
      options.allowOverride = 'All';
      options.htaccess = '';
    }
    apacheCore.addVhost(name, {
      vhostText: $hb.render(
        $file.join(__dirname, 'vhosts', `httpd-vhost-${options.type}.conf.tpl`),
        _.defaults({hostText: this._getHostText(options.hosts, options.port)}, options)
      ),
    });
    if (options.enableHttps) {
      options.sslInfo = `SSLEngine on
  SSLCertificateFile "${options.certificatePath}/server.crt"
  SSLCertificateKeyFile "${options.certificatePath}/server.key"`;
      options.port = options.httpsPort;
      apacheCore.addVhost(`${name}-https`, {
        vhostText: $hb.render(
          $file.join(__dirname, 'vhosts', `httpd-vhost-${options.type}.conf.tpl`),
          _.defaults({hostText: this._getHostText(options.hosts, options.port)}, options)
        ),
      });
    }
    apacheCore.reload();
  }

  /**
   * Set the application installing page
   * @function handler.webservers.apache~setInstallingPage
   * @example
   * // Add installing page
   * setInstallingPage();
   */
  setInstallingPage() {
    const tmpDir = $os.createTempDir();
    $file.copy($file.join(__dirname, '../../loading_page/*'), tmpDir);
    apacheCore.addVhost('__loading', {
      vhostText: $hb.render(
        $file.join(__dirname, 'vhosts/loading-page-vhost.conf.tpl'),
        {hostText: `_default_:${apacheCore.httpPort}`, documentRoot: tmpDir}
      ),
    });
    apacheCore.reload();
  }

  /**
   * Remove the application installing page
   * @function handler.webservers.apache~removeInstallingPage
   * @example
   * // Remove installing page
   * removeInstallingPage();
   */
  removeInstallingPage() {
    apacheCore.removeVhost('__loading');
    apacheCore.reload();
  }
}
module.exports = ApacheHandler;
