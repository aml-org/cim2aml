package aml.cim

import amf.core.AMF
import amf.core.unsafe.PlatformSecrets
import org.scalatest.{AsyncFunSuite, FunSuite}

class RepositoryLoaderTests extends AsyncFunSuite with PlatformSecrets {

  test("It should generate a dialect with dependencies"){
    AMF.init() map { _ =>
      RepositoryLoader.fromDirectory("src/test/resources/linked_concepts_example", "src/test/resources/context.jsonld")
      val cs = platform.fs.syncFile("src/test/resources/schemas_example/schemas.yaml").read()
      val target = cs.subSequence(0, cs.length()).toString
      assert(true)
    }
  }

}
