package aml.cim.generators

import amf.client.AMF
import amf.client.model.document.Dialect
import amf.client.render.Aml10Renderer
import amf.core.unsafe.PlatformSecrets
import aml.cim.Utils
import aml.cim.model.SchemasModel
import org.scalatest.FunSuite

class DialectGeneratorTests extends FunSuite with Utils with PlatformSecrets {

  test("It should generate a AML dialect for each functional area in a schemas model") {
    AMF.init().get()

    val jsonld = loadExample("schemas")
    val schemasModel = new SchemasModel(jsonld)
    val functionalArea = schemasModel.functionalAreas.head
    val dialect = new DialectGenerator(schemasModel, functionalArea).generate()

    val txt = new Aml10Renderer("application/yaml").generateString(Dialect(dialect)).get().trim
    val cs = platform.fs.syncFile("src/test/resources/schemas_example/schemas.yaml").read()
    val target = cs.subSequence(0, cs.length()).toString
    assert(txt == target)
  }
}
