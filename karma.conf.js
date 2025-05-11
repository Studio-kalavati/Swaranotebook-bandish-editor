process.env.CHROME_BIN = require('puppeteer').executablePath()
module.exports = function (config) {

  config.set({
    browsers: ['ChromeHeadless'],
    basePath: './',
    files: [
      // Add global polyfill first
      { pattern: './test/karma-setup.js', included: true },
      './out/puppeteer-tests.js',
      // Serve the recordings directory
      { pattern: './puppeteer_recordings/*.json', included: false, served: true, nocache: true }
    ],
    proxies: {
      '/puppeteer_recordings/': '/base/../puppeteer_recordings/'
    },
    frameworks: ['cljs-test'],
    plugins: [
        'karma-cljs-test',
        //'karma-puppeteer-launcher',
        'karma-chrome-launcher'],
    colors: true,
    logLevel: config.LOG_INFO,
    client: {
      args: ['shadow.test.karma.init']
    },

  })
}
