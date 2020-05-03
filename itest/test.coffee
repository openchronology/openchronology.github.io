selenium = require 'selenium-webdriver'
chai = require 'chai'
chai.use require 'chai-as-promised'
expect = chai.expect

before ->
  @timeout 10000
  @driver = new selenium.Builder()
    .withCapabilities(selenium.Capabilities.chrome())
    .build()
  @driver.getWindowHandle()

after ->
  @driver.quit()

describe 'OpenChronology Tests', ->
  beforeEach ->
    @driver.get './static.html'

  it 'has the expected title', ->
    expect(@driver.getTitle()).to.eventually.contain 'OpenChronology'
