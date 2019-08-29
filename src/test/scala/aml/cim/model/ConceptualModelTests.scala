package aml.cim.model

import aml.cim.{CIM, Utils}
import org.scalatest.FunSuite

class ConceptualModelTests extends FunSuite with Utils {

  test("it should load rdfs classes into the conceptual model") {
    val jsonld = loadExample("concepts")
    val conceptualModel = new ConceptualModel(jsonld)
    val concepts = conceptualModel.rdfsClasses
    assert(concepts.nonEmpty)
    assert(concepts.size == 4)

    for {
      place <- concepts.find(_.name == "Place")
      location <- concepts.find(_.name == "Location")
    } yield  {
      assert(place.description.nonEmpty)
      assert(place.displayName.nonEmpty)
      assert(location.superClasses == Seq(place.id))
    }
  }

  test("it should load rdf properties into the conceptual model") {
    val jsonld = loadExample("concepts")
    val conceptualModel = new ConceptualModel(jsonld)
    val concepts = conceptualModel.rdfProperties

    assert(concepts.nonEmpty)
    assert(concepts.size == 2)

    val icaoCode = concepts.find(_.name == "icaoCode");
    assert(icaoCode.isDefined)
    assert(icaoCode.get.ranges == Seq(CIM.DATATYPE))
  }

  test("it should load a cim functional area into the conceptual model") {
    val jsonld = loadExample("concepts")
    val conceptualModel = new ConceptualModel(jsonld)
    val concepts = conceptualModel.entityGroups

    assert(concepts.nonEmpty)
    assert(concepts.size == 1)

    val fa = concepts.head
    assert(fa.name == "PlaceFA")
    assert(fa.classes.size == 4)
    assert(fa.properties.size == 2)
  }
}
