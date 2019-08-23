package aml.cim.model

import amf.core.vocabulary.Namespace
import org.apache.jena.rdf.model.{Literal, Model, RDFNode, ResIterator, Resource}
import org.apache.jena.util.iterator.ExtendedIterator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait ModelHelper {

  val RDF_TYPE: String = Namespace.Rdf.base + "type"

  val jsonld: Model

  def findInstancesOf(classId: String): ArrayBuffer[Resource] = {
    val instances: ExtendedIterator[Resource] = jsonld.listSubjectsWithProperty(
      jsonld.createProperty(RDF_TYPE),
      jsonld.createResource(classId)
    )

    iterateResources(instances, (u: Resource) => u)
  }

  def findProperty(id: String, property: String): Option[Literal] = {
    val it = jsonld.listObjectsOfProperty(
      jsonld.createResource(id),
      jsonld.createProperty(property)
    )

    if (it.hasNext) {
      Some(it.next().asLiteral())
    } else {
      None
    }
  }

  def findRelatedResources(id: String, property: String): mutable.ArrayBuffer[Resource] = {
    val it: ExtendedIterator[RDFNode] = jsonld.listObjectsOfProperty(
      jsonld.createResource(id),
      jsonld.createProperty(property)
    )

    iterateResources(it,(n: RDFNode) => n.asResource())
  }

  def iterateResources[T,U](it: ExtendedIterator[U], c: (U) => T): mutable.ArrayBuffer[T] = {
    var acc: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()
    while(it.hasNext) {
      acc += c(it.next())
    }
    acc
  }

}
