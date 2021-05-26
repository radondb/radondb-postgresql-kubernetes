'use strict';

const _ = require('lodash');
const JspServerHandler = require('../handler');
const logFunctions = require('../../../log');

const tomcatCore = $modules['com.qingcloud.tomcat'];

/**
 * Tomcat handler functions.
 * @namespace handler.jspservers.tomcat
 */
class TomcatHandler extends JspServerHandler {
  constructor(options) {
    super(options);
    _.assign(this, {
      user: tomcatCore.systemUser,
      group: tomcatCore.systemGroup,
      httpPort: tomcatCore.httpPort,
      ajpPort: tomcatCore.ajpPort,
    }, _.pick(
      tomcatCore,
      ['installdir', 'confDir', 'binDir', 'dataDir', 'logsDir', 'libDir', 'persistDir']
    ));
  }

  // Service managament
  /**
   * Restart the Tomcat server
   * @function handler.jspservers.tomcat~restart
   * @example
   * restart();
   */
  restart() {
    return tomcatCore.restart();
  }

  /**
   * Start the Tomcat server
   * @function handler.jspservers.tomcat~start
   * @example
   * restart();
   */
  start() {
    return tomcatCore.start();
  }

  /**
   * Stop the Tomcat server
   * @function handler.jspservers.tomcat~stop
   * @example
   * restart();
   */
  stop() {
    return tomcatCore.stop();
  }

  // Application management
  /**
   * Set the application installing page
   * @function handler.jspservers.tomcat~setInstallingPage
   * @example
   * // Add installing page
   * setInstallingPage();
   */
  setInstallingPage() {
    const rootDir = $file.join(this.dataDir, 'ROOT');
    const indexFile = $file.join(rootDir, 'index.jsp');
    const webFile = $file.join(rootDir, 'WEB-INF', 'web.xml');
    $file.copy($file.join(__dirname, '../../loading_page/*'), rootDir);
    const header = '<%@ '
      + 'page session="false" '
      + 'pageEncoding="UTF-8" '
      + 'contentType="text/html; '
      + 'charset=UTF-8" %>\n'
      + '<%-- If request has a query parameter in the form `index.jsp?404`, then return 404, else return 503 --%>\n'
      + '<%\n'
      + '  if(request.getQueryString()!=null && request.getQueryString().equals("404")) {\n'
      + '    response.setStatus(404);\n'
      + '  } else {\n'
      + '    response.setStatus(503);\n'
      + '  }\n'
      + '%>\n';
    const body = $file.read($file.join(rootDir, 'index.html'));
    $file.delete($file.join(rootDir, 'index.html'));
    if (!$file.exists(indexFile.concat('.back'))) $file.rename(indexFile, indexFile.concat('.back'));
    $file.write(indexFile, header.concat(body));

    if (!$file.exists(webFile.concat('.back'))) $file.copy(webFile, webFile.concat('.back'));
    // Set `index.jsp?404` as the location for 404 error page
    $file.substitute(
      webFile, /<\/web-app>/,
      '  <error-page>\n'
      + '    <error-code>404</error-code>\n'
      + '    <location>/index.jsp?404</location>\n'
      + '  </error-page>\n'
      + '</web-app>'
    );
  }

  /**
   * Remove the application installing page
   * @function handler.jspservers.tomcat~removeInstallingPage
   * @param {Object} [options] - Options object
   * @param {string} [options.redirectTo] - Additionally, add a redirect to the ROOT page
   * @example
   * // Remove installing page
   * removeInstallingPage();
   * @example
   * // Remove installing page and redirect to 'Jenkins'
   * removeInstallingPage({redirectTo: '/jenkins'});
   */
  removeInstallingPage(options) {
    options = options || {};
    const rootDir = $file.join(this.dataDir, 'ROOT');
    const indexFile = $file.join(rootDir, 'index.jsp');
    const webFile = $file.join(rootDir, 'WEB-INF', 'web.xml');
    $file.delete($file.join(rootDir, 'img'));
    $file.rename(indexFile.concat('.back'), indexFile);
    $file.rename(webFile.concat('.back'), webFile);
    if (!_.isEmpty(options.redirectTo)) tomcatCore.redirectRoot(options.redirectTo);
  }

  /**
   * Wait until the Tomcat log file contains a given pattern
   * @function handler.jspservers.tomcat~waitForLogEntry
   * @param {string|RegExp} pattern - Glob like pattern or regexp to match
   * @param {Object} [options] - Options object
   * @param {string} [options.encoding] - Encoding used to read the file
   * @param {string} [options.timeout] - Time to wait
   * @example
   * // Wait until the Tomact log file matches 'Jenkins is fully up and running'
   * waitForLogEntry(/Jenkins is fully up and running/);
   */
  waitForLogEntry(pattern, options) {
    // Patch for 3dc3788
    return logFunctions.waitForEntry($file.join(tomcatCore.logsDir, 'catalina.out'), pattern, options);
    // TODO: return logFunctions.waitForEntry(tomcatCore.logFile, pattern, options);
  }

  /**
   * Add a variable to the setenv.sh file
   * @function handler.jspservers.tomcat~addEnvironmentVar
   * @param {Object} vars - Variables to set
   * @param {Object} [options] - Options object
   * @param {string} [options.comment] - Comment to prepend
   * @example
   * addEnvironmentVar({
   *   JENKINS_HOME: '/qingcloud/jenkins',
   *   JAVA_OPTS: `"-Xms256M -Xmx512M $JAVA_OPTS"`
   * });
   */
  addEnvironmentVar(vars, options) {
    return tomcatCore.addEnvironmentVar(vars, options);
  }

  /**
   * Link a web root folder to the Tomcat webapps folder and set it to ROOT
   * @function handler.jspservers.tomcat~deploy
   * @param {string} webroot - Application webroot
   * @example
   * // $app.installdir: /opt/qingcloud/jenkins
   * deploy($app.installdir);
   * // webapps/jenkins is removed first and a link with the same name is created pointing to $app.installdir:
   * // /opt/qingcloud/tomcat/webapps/jenkins -> /opt/qingcloud/jenkins (link)
   * @example
   * // $app.installdir: /opt/qingcloud/jenkins
   * deploy($app.installdir, {as: 'ROOT'});
   * // webapps/ROOT is removed first and a link with the same name is created pointing to $app.installdir:
   * // /opt/qingcloud/tomcat/webapps/ROOT => /opt/qingcloud/jenkins (link)
   */
  deploy(webroot, options) {
    return tomcatCore.deploy(webroot, options);
  }

  /**
   * Rotate logs of tomcat
   * @function handler.jspservers.tomcat~rotateLog
   * @param {string} suffix - suffix for the naming of the logfile stored
   * @example
   * rotateLogs({suffix: 'firstBoot'})
   */
  rotateLog(options) {
    return tomcatCore.rotateLog(options);
  }
}
module.exports = TomcatHandler;
