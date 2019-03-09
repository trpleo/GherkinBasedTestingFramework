
import cucumber.api.CucumberOptions
import cucumber.api.junit.Cucumber
import org.junit.runner.RunWith

@RunWith(classOf[Cucumber])
@CucumberOptions(
  features = Array("classpath:features/WordTesting.feature"),
  tags = Array("@currdev"),
  glue = Array("classpath:steps"),
  plugin =
    Array(
      "pretty",
      "html:target/cucumber/html",
      "json:target/cucumber/test-report.json",
      "junit:target/cucumber/test-report.xml"))
class WorldTestRunner
