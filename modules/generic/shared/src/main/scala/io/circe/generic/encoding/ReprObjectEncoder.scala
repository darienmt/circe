package io.circe.generic.encoding

import io.circe.{ Encoder, Json, JsonObject, ObjectEncoder }
import io.circe.generic.DerivationMacros
import scala.language.experimental.macros
import shapeless.{ Coproduct, HList }

/**
 * An encoder for a generic representation of a case class or ADT.
 *
 * Note that users typically will not work with instances of this class.
 */
abstract class ReprObjectEncoder[A] extends ObjectEncoder[A] {
  def configuredEncodeObject(a: A)(transformKeys: String => String, discriminator: Option[String]): JsonObject

  final protected[this] def addDiscriminator[B](
    encode: Encoder[B],
    value: B,
    name: String,
    discriminator: Option[String]
  ): JsonObject = discriminator match {
    case None => JsonObject.singleton(name, encode(value))
    case Some(disc) => encode match {
      case oe: ObjectEncoder[B] @unchecked => oe.encodeObject(value).add(disc, Json.fromString(name))
      case _ => JsonObject.singleton(name, encode(value))
    }
  }

  final def encodeObject(a: A): JsonObject = configuredEncodeObject(a)(identity, None)
}

final object ReprObjectEncoder {
  implicit def encodeHList[R <: HList]: ReprObjectEncoder[R] = macro DerivationMacros.encodeHList[R]
  implicit def encodeCoproduct[R <: Coproduct]: ReprObjectEncoder[R] = macro DerivationMacros.encodeCoproduct[R]
}
