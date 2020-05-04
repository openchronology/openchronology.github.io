require('chromedriver');
const assert = require('assert');
const {Builder, Key, By, until} = require('selenium-webdriver');

describe('Testing OpenChronology', function () {
  let driver;

  before(async function() {
    driver = await new Builder().forBrowser('chrome').build();
  });

  it('Should Have The Title', async function() {
    await driver.get('file:///home/athan/dev/openchronology.github.io/static.html');

    let title = await driver.getTitle();
    assert.equal(title, 'OpenChronology');
  });

  after(() => driver && driver.quit());
});
