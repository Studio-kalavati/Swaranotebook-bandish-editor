process.env.CHROME_BIN = require('puppeteer').executablePath()
module.exports = function (config) {

  config.set({
    browsers: ['ChromeHeadless'],
    basePath: 'out',
      files: ['puppeteer-tests.js'],
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
