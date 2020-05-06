require('chromedriver');
const assert = require('assert');
const {Builder, Key, By, until, Capabilities} = require('selenium-webdriver');
const chrome = require('selenium-webdriver/chrome');

// let chromeCapabilities = Capabilities.chrome();
const chromeOptions = new chrome.Options();
chromeOptions.addArguments('--headless');
chromeOptions.addArguments('--no-sandbox');
chromeOptions.setChromeBinaryPath('/usr/bin/chromium-browser');
//   {
//   'args': ['--headless', '--no-sandbox', '--disable-dev-shm-usage']
// };
// chromeCapabilities.set('chromeOptions', chromeOptions);

describe('Testing OpenChronology', function () {
  let driver;

  before(async function() {
    driver = await new Builder()
               .forBrowser('chrome')
               .setChromeOptions(chromeOptions)
              // .withCapabilities(chromeCapabilities)
               .build();
  });

  it('Should Have The Title', async function() {
    await driver.get('file://' + process.cwd() + '/static.html');

    let title = await driver.getTitle();
    assert.equal(title, 'OpenChronology');
  });

  after(() => driver && driver.quit());
});
