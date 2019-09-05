package aml.cim.model

import amf.core.vocabulary.Namespace
import org.apache.jena.rdf.model.{AnonId, Literal, Model, RDFNode, ResIterator, Resource}
import org.apache.jena.util.iterator.ExtendedIterator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait ModelHelper {

  val RDF_TYPE: String = Namespace.Rdf.base + "type"

  protected val RDFS_CLASS: String = (Namespace.Rdfs + "Class").iri()
  protected  val RDFS_LABEL: String = (Namespace.Rdfs + "label").iri()
  protected val RDFS_COMMENT: String = (Namespace.Rdfs + "comment").iri()
  protected  val RDFS_SUBCLASS_OF: String = (Namespace.Rdfs + "subClassOf").iri()

  protected val RDF_PROPERTY: String = (Namespace.Rdf + "Property").iri()
  protected  val RDFS_DOMAIN: String = (Namespace.Rdfs + "domain").iri()
  protected val RDFS_RANGE: String = (Namespace.Rdfs + "range").iri()

  protected val SH_SHAPE: String = (Namespace.Shacl + "Shape").iri()
  protected val SH_PROPERTY: String = (Namespace.Shacl + "property").iri()
  protected val SH_PATH: String = (Namespace.Shacl + "path").iri()
  protected val SH_MIN_COUNT: String = (Namespace.Shacl + "minCount").iri()
  protected val SH_MAX_COUNT: String = (Namespace.Shacl + "maxCount").iri()
  protected val SH_DATATYPE: String = (Namespace.Shacl + "datatype").iri()
  protected val SH_NODE: String = (Namespace.Shacl + "node").iri()
  protected val SH_IN: String = (Namespace.Shacl + "in").iri()



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
      subject(id),
      jsonld.createProperty(property)
    )

    if (it.hasNext) {
      Some(it.next().asLiteral())
    } else {
      None
    }
  }

  def findRelatedProperties(id: String, property: String): mutable.ArrayBuffer[Literal] = {
    val it: ExtendedIterator[RDFNode] = jsonld.listObjectsOfProperty(
      subject(id),
      jsonld.createProperty(property)
    )

    iterateResources(it,(n: RDFNode) => n.asLiteral())
  }

  def findRelatedResources(id: String, property: String): mutable.ArrayBuffer[Resource] = {
    val it: ExtendedIterator[RDFNode] = jsonld.listObjectsOfProperty(
      subject(id),
      jsonld.createProperty(property)
    )

    iterateResources(it,(n: RDFNode) => n.asResource())
  }

  def findRelatedResource(id: String, property: String): Option[Resource] =
    findRelatedResources(id, property).headOption

  def iterateResources[T,U](it: ExtendedIterator[U], c: (U) => T): mutable.ArrayBuffer[T] = {
    var acc: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()
    while(it.hasNext) {
      acc += c(it.next())
    }
    acc
  }

  def findSubjectOf(property: String, id: String): ArrayBuffer[Resource] = {
    val it: ResIterator = jsonld.listSubjectsWithProperty(
      jsonld.createProperty(property),
      subject(id)
    )

    iterateResources(it,(n: RDFNode) => n.asResource())
  }

  def subject(id: String): Resource = {
    if (id.startsWith("http")) {
      jsonld.createResource(id)
    } else {
      jsonld.createResource(new AnonId(id))
    }
  }

}

class ModelOps(override val jsonld: Model) extends ModelHelper

object ModelOps {
  def apply(jsonld: Model) = new ModelOps(jsonld)
}