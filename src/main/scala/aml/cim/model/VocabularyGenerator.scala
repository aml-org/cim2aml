package aml.cim.model

import amf.core.model.DataType
import amf.plugins.document.vocabularies.model.document.Vocabulary
import amf.plugins.document.vocabularies.model.domain.{ClassTerm, DatatypePropertyTerm, ObjectPropertyTerm, PropertyTerm}
import aml.cim.CIM

class VocabularyGenerator(conceptualModel: ConceptualModel, functionalArea: FunctionalArea) {

  def generate(): Vocabulary = {
    val vocabulary = Vocabulary().withId(functionalArea.id).withName(functionalArea.name).withBase(CIM.NS.base)
    functionalArea.description.foreach(vocabulary.withUsage)
    vocabulary.withDeclares(classTerms ++ propertyTerms)
  }

  lazy protected val classTerms: Seq[ClassTerm] = {
    functionalArea.classes map { classId =>
      val rdfsClass = conceptualModel.findClassById(classId).getOrElse(throw new Exception(s"Cannot find class '$classId' for functional area '${functionalArea.id}'"))
      val classTerm = ClassTerm().withId(classId).withName(rdfsClass.name)
      rdfsClass.displayName.foreach(classTerm.withDisplayName)
      rdfsClass.description.foreach(classTerm.withDescription)
      if (rdfsClass.superClasses.nonEmpty) {
        classTerm.withSubClassOf(rdfsClass.superClasses)
      }
      classTerm
    }
  }

  lazy protected val propertyTerms: Seq[PropertyTerm] = {
    functionalArea.properties.map { propertyId =>
      val rdfProperty = conceptualModel.findPropertyById(propertyId).getOrElse(throw new Exception(s"Cannot find property '$propertyId' for functional area '${functionalArea.id}"))
      val propertyTerm: PropertyTerm = if (rdfProperty.isDataProperty) {
        DatatypePropertyTerm().withRange(DataType.Any)
      } else {
        ObjectPropertyTerm().withRange(rdfProperty.ranges.head)
      }
      propertyTerm.withId(propertyId).withName(rdfProperty.name)
      rdfProperty.displayName.foreach(propertyTerm.withDisplayName)
      rdfProperty.description.foreach(propertyTerm.withDescription)

      rdfProperty.domains.foreach { domainId =>
        classTerms.find(_.id == domainId) map { classTermDomain =>
          val fs = classTermDomain.properties.map(_.value())
          classTermDomain.withProperties(fs ++ Seq(propertyId))
        }
      }

      propertyTerm
    }
  }
}
