package aml.cim.generators

import amf.client.model.document.Vocabulary
import amf.client.render.Aml10Renderer
import amf.core.AMF
import amf.core.unsafe.PlatformSecrets
import aml.cim.Utils
import aml.cim.model.ConceptualModel
import org.scalatest.{AsyncFunSuite, FunSuite}

class VocabularyGeneratorTests extends AsyncFunSuite with Utils with PlatformSecrets {

  test("It should generate a AML vocabulary for each functional area in a conceptual model") {
    AMF.init() map { _ =>
      val jsonld = loadExample("concepts")
      val conceptualModel = new ConceptualModel(jsonld)
      val entityGroup = conceptualModel.entityGroups.head
      val vocabulary = new VocabularyGenerator(conceptualModel, entityGroup).generate()

      val txt = new Aml10Renderer("application/yaml").generateString(new Vocabulary(vocabulary)).get().trim
      val cs = platform.fs.syncFile("src/test/resources/concepts_example/concepts.yaml").read()
      val target = cs.subSequence(0, cs.length()).toString
      assert(txt == target)
    }
  }
}
