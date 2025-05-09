process.env.CHROME_BIN = require('puppeteer').executablePath()
module.exports = function (config) {

  config.set({
    browsers: ['ChromeHeadless'],
    basePath: 'out',
    files: [
      // Add global polyfill first
      { pattern: '../test/karma-setup.js', included: true },
      'puppeteer-tests.js'
    ],
    frameworks: ['cljs-test'],
    plugins: [
        'karma-cljs-test',
        'karma-chrome-launcher'],
    colors: true,
    logLevel: config.LOG_INFO,
    client: {
      args: ['shadow.test.karma.init']
    },

  })
}
