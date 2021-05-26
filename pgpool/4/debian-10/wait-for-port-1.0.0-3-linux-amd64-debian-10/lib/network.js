'use strict';

const _ = require('lodash');
const deasync = require('deasync');
const net = require('net');
const dns = require('dns');
const os = require('os');
const http = require('http');
const delegate = require('harpoon-utils').delegate;
const Logger = require('harpoon-logger');
const url = require('url');

/**
 * Network functions.
 * @namespace base-functions.network
 */
function networkFunctions(opts) {
  opts = opts || {};
  let logger = {};
  const loggerRequirements = ['error', 'warn', 'info', 'debug', 'trace', 'trace1', 'trace2',
    'trace3', 'trace4', 'trace5', 'trace6', 'trace7', 'trace8'];
  if (_.every(loggerRequirements, (r) => { return _.has(opts, r); })) {
    delegate(logger, loggerRequirements, opts);
  } else {
    logger = new Logger({prefix: 'network'});
  }

  /**
   * Wait for a service to start
   * @function base-functions.network~waitForService
   * @param {string} host - Service host
   * @param {number} port - Service port
   * @param {Object} [options]
   * @param {number} [options.tries=36] - Number of tries before giving up
   * @param {number} [options.step=10] - Seconds to wait between tries
   * @example
   * waitForService('domain.com', '1234');
   */
  function waitForService(host, port, options) {
    options = _.defaults(options || {}, {
      tries: 36,
      step: 10,
    });
    logger.debug(`Waiting for service at ${host}:${port}...`);
    let bound = false;
    let cont = 0;
    let done = false;
    let connection = {};
    const createConnection = function() {
      connection = net.createConnection({host, port}, () => {
        connection.destroy();
        done = true;
        bound = true;
      });
      return connection;
    };
    const onTimeOut = function() {
      connection.destroy();
      done = true;
    };
    const onError = function() {
      $util.sleep(options.step);
      done = true;
    };
    const checkDone = function() {
      return !done;
    };
    while (!bound && cont <= options.tries) {
      done = false;
      connection = createConnection();
      connection.setTimeout(1000);
      connection.on('timeout', onTimeOut);
      connection.on('error', onError);
      deasync.loopWhile(checkDone);
      cont += 1;
    }
    if (!bound) throw new Error(`Failed to connect to ${host}:${port} after ${options.tries} tries`);
  }

  /**
   * Obtains the public IP of the machine where the module is running
   * @function base-functions.network~getPublicIp
   * @param {string} [options.url='myip.qingcloud.com'] - Server's url to obtain the ip from.
   * @param {number} [options.port=80] - Server's port to obtain the ip from.
   * @return {string} - String containing the public IP
   * @example
   * getPublicIP()
   * // => "50.50.50.50"
   */
  function getPublicIp(options) {
    options = _.defaults(options || {}, {host: 'myip.qingcloud.com', port: 80});
    let foundIp = '';
    http.get(options, function(res) {
      res.setEncoding('utf8');
      res.on('data', function(ip) {
        foundIp = ip;
      });
    });
    deasync.loopWhile(function() {
      return _.isEmpty(foundIp);
    });
    return foundIp;
  }

  /** Returns a normalized URL given a list of parameters. Acts as a wrapper over URL module.
 * @function base-functions.network~getURL
 * @see https://nodejs.org/api/url.html#url_url_strings_and_url_objects
 * @param {string} hostname - URL hostname.
 * @param {Object} [urlOptions] - Object containing the basic URL parameters.
 * @param {string} [urlOptions.protocol=http] - URL protocol.
 * @param {string} [urlOptions.host] - Host to assign to the URL. Overwrites hostname and port.
 * @param {string} [urlOptions.auth] - Basic HTTP authentication parameters.
 * @param {string} [urlOptions.hostname] - URL hostname. Will overwrite the first parameter provided if set.
 * @param {Number|string} [urlOptions.port] - Port to assign to the URL.
 * @param {string} [urlOptions.pathname] - Entire path section of the URL.
 * @param {string} [urlOptions.search] - Entire query section of the URL.
 * @param {string} [urlOptions.path] - A concatenation of pathname and search components.
 * @return {string} - A string with the normalized URL.
 *
 * @example
 * getURL('192.168.0.1')
 * // => 'http://192.168.0.1'
 * @example
 * getURL('192.168.0.1', {port: 80})
 * // => 'http://192.168.0.1'
 * @example
 * getURL('192.168.0.1', {port: 8080})
 * // => 'http://192.168.0.1:8080'
 * @example
 * getURL('192.168.0.1', {protocol: ''})
 * // => '192.168.0.1'
 * @example
 * getURL('192.168.0.1', {protocol:'https'})
 * // => 'https://192.168.0.1'
 * @example
 * getURL('192.168.0.1', {protocol:'https', port: 8443})
 * // => 'https://192.168.0.1:8443'
 * @example
 * getURL('192.168.0.1', {protocol:'ftp', port: '9999'})
 * // => 'ftp://192.168.0.1:9999'
 * @example
 * getURL('192.168.0.1', {protocol:'ssh'})
 * // => 'ssh://192.168.0.1'
 * @example
 * getURL('192.168.0.1', {protocol:'mongodb', port: 27017, pathname: 'database'})
 * // => 'mongodb://192.168.0.1:27017/database'
 * @example
 * getURL('example.com', {protocol: 'https', auth: 'user:qingcloud'})
 * // => 'https://user:qingcloud@qingcloud.com'
 * @example
 * getURL('example.com', {path: '/p/a/t/h?query=string'})
 * // => 'http://example.com/p/a/t/h?query=string'
 * @example
 * getURL('example.com', {pathname: '/p/a/t/h', 'query': 'query=string'})
 * // => 'http://example.com/p/a/t/h?query=string'
 */

  function getURL(hostname, urlOptions) {
    urlOptions = _.defaults(urlOptions || {}, {
      protocol: 'http',
      auth: '',
      hostname: hostname,
      port: '',
      pathname: '',
      search: '',
      path: '',
      hash: '',
    });

    // This intends to solve all type issues with the port parameter
    urlOptions.port = urlOptions.port.toString();

    // Remove port if it is the default protocol's port
    const knownProtocols = {
      'http': '80',
      'http:': '80',
      'https': '443',
      'https:': '443',
    };
    if (urlOptions.port === knownProtocols[urlOptions.protocol]) urlOptions = _.omit(urlOptions, 'port');

    // Postfix '//' to known protocols that we want to
    const slashedProtocol = {
      'mongodb': true,
      'mongodb:': true,
      'ssh': true,
      'ssh:': true,
    };
    urlOptions.slashes = slashedProtocol[urlOptions.protocol];

    // Remove '//' if the protocol is empty
    const urlResult = _.isEmpty(urlOptions.protocol) ? url.format(urlOptions).slice(2) : url.format(urlOptions);

    return urlResult;
  }

  /**
   * Resolves a domain (e.g. 'google.com') into the first found A (IPv4) or AAAA (IPv6) record.
   * @function base-functions.network~lookup
   * @param {string} - Domain to resolve
   * @return {string} - A string with the domain IP
   * @example
   * lookup('app_master')
   * // => "172.17.0.2"
   * @example
   * lookup('www.google.com')
   * // => "216.58.217.132"
   */
  function lookup(domain) {
    let foundIp = '';
    dns.lookup(domain, (e, address) => {
      foundIp = address;
    });
    deasync.loopWhile(function() {
      return _.isEmpty(foundIp);
    });
    return foundIp;
  }

  /**
   * Checks if the string is a valid IP
   * @function base-functions.network~isIPv4
   * @param {string} ipaddress - IP address
   * @return {boolean}
   * @example
   * isIPv4("192.168.0.1")
   * // => true
   */
  function isIPv4(ipaddress) {
    const blocks = ipaddress.split('.');
    if (blocks.length === 4) {
      return blocks.every((b) => Number(b) >= 0 && Number(b) <= 255);
    }
    return false;
  }

  /**
   * Obtains the private IP of the machine where the module is running
   * @function base-functions.network~getMachineIp
   * @return {string} - A string with the machine IP
   * @example
   * getMachineIP()
   * // => "192.168.0.1"
   */
  function getMachineIp() {
    return this.lookup(os.hostname());
  }

  /**
   * Makes a request to a specific uri
   * @function base-functions.network~httpRequest
   * @param {string} path - Request path
   * @param {Object} [options]
   * @param {string} [options.method=GET] - HTTP request method
   * @param {string} [options.hostname=127.0.0.1] - Server's hostname
   * @param {string} [options.port=80] - Server's port
   * @param {Object} [options.headers] - HTTP request headers
   * @param {Object} [options.data] - Data to send
   * @return {Object} Object Contains the html document and the request
   * @return {string} Object.html HTML document
   * @return {Object} Object.request HTTP request
   * @example
   * httpRequest('/ghost/api/v0.1/authentication/setup/', {method: 'POST'});
   * // Result => {html, request}
   * @example
   * let optionsPost = {
   *   hostname: '127.0.0.1',
   *   port: '80',
   *   method: 'POST',
   *   headers: {
   *     'content-type': 'application/x-www-form-urlencoded',
   *     'set-cookie': `${cookie}`,
   *     'content-length': Buffer.byteLength(postData)
   *   },
   *   data: JSON.stringify({
   *     'txtScreen': '5',
   *     'actionResponse': 'CONFIRMED'
   *   });
   * };
   * httpRequest('/ghost/api/v0.1/authentication/setup/', optionsPost);
   * // Result => {html, request}
   */
  function httpRequest(path, options) {
    options = _.defaults(options || {}, {hostname: '127.0.0.1', port: '80', method: 'GET'});
    options.path = path;
    let madeReq = false;
    let html = '';
    logger.debug(`Making ${options.method} request at ${options.hostname}:${options.port}${options.path}`);
    const request = http.request(options, (res) => {
      logger.trace(options.method);
      logger.trace(`  Status: ${res.statusCode}`);
      logger.trace(`  Cookie: ${res.headers['set-cookie']}`);
      logger.trace(`  Headers: ${JSON.stringify(res.headers)}`);
      if (options.method === 'POST' && options.data) {
        logger.trace(`  Data to send: ${options.data}`);
      }
      res.setEncoding('utf8');
      res.on('data', (chunk) => {
        logger.trace(`  Body: ${chunk}`);
        html = chunk;
      });
      res.on('end', () => {
        madeReq = true;
      });
    });
    request.on('error', (e) => {
      throw e;
    });
    if (options.method === 'POST' && options.data) {
      request.write(options.data);
    }
    if (options.method === 'PUT' && options.data) {
      request.end(options.data);
    } else {
      request.end();
    }
    deasync.loopWhile(function() {
      return !madeReq;
    });
    return {html, request};
  }

  return {
    waitForService,
    getPublicIp,
    getURL,
    getMachineIp,
    isIPv4,
    lookup,
    httpRequest,
  };
}

module.exports = networkFunctions;
