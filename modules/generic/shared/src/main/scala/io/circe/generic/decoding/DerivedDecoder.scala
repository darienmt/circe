package io.circe.generic.decoding

import cats.data.Validated
import io.circe.{ AccumulatingDecoder, Decoder, HCursor }
import io.circe.generic.DerivationMacros
import scala.language.experimental.macros
import shapeless.{ :+:, Coproduct, HList, Inl, Inr, LabelledGeneric, Lazy }
import shapeless.labelled.{ field, FieldType }

/**
 * A decoder for a generic representation of a case class or ADT.
 *
 * Note that users typically will not work with instances of this class, which
 * contains unsafe methods (specifically the two `configuredDecode` methods,
 * which allow passing in an untyped map of default field values).
 */
abstract class ReprDecoder[A] extends Decoder[A] {
    def configuredDecode(c: HCursor)(
    transformKeys: String => String,
    defaults: Map[String, Any],
    discriminator: Option[String]
  ): Decoder.Result[A]

  def configuredDecodeAccumulating(c: HCursor)(
    transformKeys: String => String,
    defaults: Map[String, Any],
    discriminator: Option[String]
  ): AccumulatingDecoder.Result[A]

  final protected[this] def orDefault[B](
    result: Decoder.Result[B],
    name: String,
    defaults: Map[String, Any]
  ): Decoder.Result[B] = result match {
    case r @ Right(_) => r
    case l @ Left(_) => defaults.get(name) match {
      case Some(d: B @unchecked) => Right(d)
      case _ => l
    }
  }

  final protected[this] def orDefaultAccumulating[B](
    result: AccumulatingDecoder.Result[B],
    name: String,
    defaults: Map[String, Any]
  ): AccumulatingDecoder.Result[B] = result match {
    case r @ Validated.Valid(_) => r
    case l @ Validated.Invalid(_) => defaults.get(name) match {
      case Some(d: B @unchecked) => Validated.valid(d)
      case _ => l
    }
  }

  final protected[this] def withDiscriminator[K, V, R <: Coproduct](
    decode: Decoder[V],
    c: HCursor,
    resultR: Decoder.Result[R],
    name: String,
    discriminator: Option[String]
  ): Decoder.Result[FieldType[K, V] :+: R] = discriminator match {
    case None =>
      val result = c.downField(name)

      if (result.succeeded) {
        decode.tryDecode(result) match {
          case Right(a) => Right(Inl(field[K](a)))
          case l @ Left(_) => l.asInstanceOf[Decoder.Result[FieldType[K, V] :+: R]]
        }
      } else {
        resultR match {
          case Right(last) => Right(Inr[FieldType[K, V], R](last))
          case l @ Left(_) => l.asInstanceOf[Decoder.Result[FieldType[K, V] :+: R]]
        }
      }
    case Some(disc) =>
      c.get[String](disc) match {
        case Right(leafType) if leafType == name => decode(c) match {
          case Right(a) => Right(Inl(field[K](a)))
          case l @ Left(_) => l.asInstanceOf[Decoder.Result[FieldType[K, V] :+: R]]
        }
        case Right(_) => resultR match {
          case Right(last) => Right(Inr[FieldType[K, V], R](last))
          case l @ Left(_) => l.asInstanceOf[Decoder.Result[FieldType[K, V] :+: R]]
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[FieldType[K, V] :+: R]]
    }
  }

  final protected[this] def withDiscriminatorAccumulating[K, V, R <: Coproduct](
    decode: Decoder[V],
    c: HCursor,
    resultR: AccumulatingDecoder.Result[R],
    name: String,
    discriminator: Option[String]
  ): AccumulatingDecoder.Result[FieldType[K, V] :+: R] = discriminator match {
    case None =>
      val result = c.downField(name)

      if (result.succeeded) {
        decode.tryDecodeAccumulating(result).map(a => Inl(field[K](a)))
      } else {
        resultR.map(last => Inr[FieldType[K, V], R](last))
      }
    case Some(disc) =>
      c.get[String](disc) match {
        case Right(leafType) if leafType == name =>
          decode.tryDecodeAccumulating(c.acursor).map(a => Inl(field[K](a)))
        case Right(_) => resultR.map(last => Inr[FieldType[K, V], R](last))
        case Left(err) => Validated.invalidNel(err)
    }
  }

  final def apply(c: HCursor): Decoder.Result[A] = configuredDecode(c)(identity, Map.empty, None)
  final override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
    configuredDecodeAccumulating(c)(identity, Map.empty, None)
}

object ReprDecoder {
  implicit def decodeHList[R <: HList]: ReprDecoder[R] = macro DerivationMacros.decodeHList[R]
  implicit def decodeCoproduct[R <: Coproduct]: ReprDecoder[R] = macro DerivationMacros.decodeCoproduct[R]
}

abstract class DerivedDecoder[A] extends Decoder[A]

final object DerivedDecoder extends IncompleteDerivedDecoders {
  implicit def decodeCaseClass[A, R <: HList](implicit
    gen: LabelledGeneric.Aux[A, R],
    decode: Lazy[ReprDecoder[R]]
  ): DerivedDecoder[A] = new DerivedDecoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] = decode.value(c) match {
      case Right(r) => Right(gen.from(r))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[A]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      decode.value.decodeAccumulating(c).map(gen.from)
  }

  implicit def decodeAdt[A, R <: Coproduct](implicit
    gen: LabelledGeneric.Aux[A, R],
    decode: Lazy[ReprDecoder[R]]
  ): DerivedDecoder[A] = new DerivedDecoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] = decode.value(c) match {
      case Right(r) => Right(gen.from(r))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[A]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      decode.value.decodeAccumulating(c).map(gen.from)
  }
}
