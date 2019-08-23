package aml.cim.model

import amf.core.vocabulary.Namespace
import aml.cim.{CIM, Utils}
import org.scalatest.FunSuite

class SchemasModelTests extends FunSuite with Utils  {
  test("it should load shacl shapes into the schemas model") {
    val jsonld = loadExample("schemas")
    val conceptualModel = new SchemasModel(jsonld)
    val schemas = conceptualModel.shapes
    assert(schemas.nonEmpty)
    assert(schemas.size == 2)

    val placeShape = schemas.find(_.name == "Place")
    assert(placeShape.nonEmpty)
    assert(placeShape.get.properties.size == 1)
    val geoCodeProp = placeShape.get.properties.head
    assert(geoCodeProp.path == CIM.cim("geoCode"))
    assert(geoCodeProp.objectRange.get == CIM.cim("GeoCode"))
    assert(geoCodeProp.scalarRange.isEmpty)
    assert(geoCodeProp.mandatory)

    var geoCodeShape = schemas.find(_.name == "GeoCode")
    assert(geoCodeShape.nonEmpty)
    assert(geoCodeShape.get.properties.size == 1)
    val icaoCodeProp = geoCodeShape.get.properties.head
    assert(icaoCodeProp.path == CIM.cim("icaoCode"))
    assert(icaoCodeProp.objectRange.isEmpty)
    assert(icaoCodeProp.scalarRange.get == (Namespace.Xsd + "string").iri())
    assert(!icaoCodeProp.mandatory)
  }

}
