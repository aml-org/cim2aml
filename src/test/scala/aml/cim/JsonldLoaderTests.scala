package aml.cim

import org.scalatest.FunSuite


class JsonldLoaderTests extends FunSuite {

  test("It should load a jsonld document"){
    val result = JsonldLoader.fromFile("src/test/resources/jsonld_example/simple.jsonld")
    assert(result.size() == 3)
    val results = result.listObjectsOfProperty(result.createProperty("http://www.w3.org/2002/12/cal/ical#dtstart"))
    assert(results.hasNext)
    assert(results.next().asLiteral().getDatatypeURI == "http://www.w3.org/2001/XMLSchema#dateTime")
  }

  test("It should load a jsonld document with an specific context"){
    val result = JsonldLoader.fromFile("src/test/resources/jsonld_example/simple.jsonld", "src/test/resources/jsonld_example/context.jsonld")
    assert(result.size() == 3)
    val results = result.listObjectsOfProperty(result.createProperty("http://www.w3.org/2002/12/cal/ical#dtstart"))
    assert(results.hasNext)
    assert(results.next().asLiteral().getDatatypeURI == "http://www.w3.org/2001/XMLSchema#time")
  }
}
