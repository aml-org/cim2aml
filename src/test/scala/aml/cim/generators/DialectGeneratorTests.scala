package aml.cim.generators

import amf.client.model.document.Dialect
import amf.client.render.Aml10Renderer
import amf.core.AMF
import amf.core.unsafe.PlatformSecrets
import aml.cim.Utils
import aml.cim.model.{SchemasModel}
import org.scalatest.AsyncFunSuite


class DialectGeneratorTests extends AsyncFunSuite with Utils with PlatformSecrets {

  test("It should generate a AML dialect for each functional area in a schemas model") {
    AMF.init() map { _ =>
      val jsonld = loadExample("schemas")
      val schemasModel = new SchemasModel(jsonld, Nil)
      val entityGroup = schemasModel.entityGroups.head.copy(location = Some("schema.jsonld"))
      val dialect = new DialectGenerator(schemasModel, entityGroup, "1.0", Nil).generate()

      val txt = new Aml10Renderer("application/yaml").generateString(Dialect(dialect)).get().trim
      val cs = platform.fs.syncFile("src/test/resources/schemas_example/schemas.yaml").read()
      val target = cs.subSequence(0, cs.length()).toString
      assert(txt == target)
    }
  }
}
