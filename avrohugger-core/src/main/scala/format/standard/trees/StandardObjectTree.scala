package avrohugger
package format
package standard
package trees

import generators.ScalaDocGenerator
import stores.ClassStore

import treehugger.forest._
import definitions._
import treehuggerDSL._

import org.apache.avro.Schema

import scala.collection.JavaConverters._
import treehugger.Names

object StandardObjectTree {
  
  def toCaseCompanionDef(schema: Schema, maybeFlags: Option[List[Long]]) = {
    val ParserClass = RootClass.newClass("org.apache.avro.Schema.Parser")
    val typeName = schema.getName
    val objectDef = maybeFlags match {
      case Some(flags) => OBJECTDEF(typeName).withFlags(flags:_*)
      case None => OBJECTDEF(typeName)
    }
    val avroFields = schema.getFields().asScala.toList
    val fields = avroFields.map { field =>
      REF("field") APPLY(LIT(field.name()), WILDCARD DOT FieldRenamer.rename(field.name()))
    }
    val codec = RootClass.newClass("Codec")//FIXME is this correct? what about import?
    // companion object definition
    objectDef := BLOCK(
      VAL("codec", codec TYPE_OF typeName).withFlags(Flags.IMPLICIT) := {
        (REF("Codec") DOT "record") APPLY (REF("name") := LIT(typeName), REF("namespace") := LIT(schema.getNamespace())) APPLY LAMBDA(PARAM("field")) ==> 
          BLOCK(TUPLE(fields) DOT("mapN") APPLY(REF(typeName) APPLY (List.fill(fields.length)(WILDCARD))))
      }
    )
  }

  def toScalaEnumDef(
    classStore: ClassStore, 
    schema: Schema,
    maybeBaseTrait: Option[String],
    maybeFlags: Option[List[Long]]
  ) = {
      
    val objectDef = (maybeBaseTrait, maybeFlags) match {
      case (Some(baseTrait), Some(flags)) => 
        OBJECTDEF(schema.getName)
          .withFlags(flags:_*)
          .withParents("Enumeration")
          .withParents(baseTrait) 
      case (Some(baseTrait), None) =>
        OBJECTDEF(schema.getName)
          .withParents("Enumeration")
          .withParents(baseTrait)
      case (None, Some(flags)) => 
        OBJECTDEF(schema.getName)
          .withFlags(flags:_*)
          .withParents("Enumeration")
      case (None, None) =>
        OBJECTDEF(schema.getName)
          .withParents("Enumeration")
    }
    
    val objectTree = objectDef := BLOCK(
      TYPEVAR(schema.getName) := REF("Value"),
      VAL(schema.getEnumSymbols.asScala.mkString(", ")) := REF("Value")
    )

    val treeWithScalaDoc = ScalaDocGenerator.docToScalaDoc(
      Left(schema),
      objectTree)
      
    treeWithScalaDoc
  }
  
  
  
}
