package io.circe.generic.extras

import io.circe.{ AccumulatingDecoder, Decoder, HCursor, ObjectEncoder }
import io.circe.export.Exported
import io.circe.generic.ExportMacros
import io.circe.generic.decoding.{ DerivedDecoder, ReprDecoder }
import io.circe.generic.encoding.DerivedObjectEncoder
import scala.language.experimental.macros
import shapeless.{ Coproduct, Default, HList, LabelledGeneric, Lazy, Witness }

abstract class ConfiguredDecoder[A] extends DerivedDecoder[A]

final object ConfiguredDecoder {
  implicit def decodeCaseClass[A, R <: HList, D <: HList](implicit
    gen: LabelledGeneric.Aux[A, R],
    decode: Lazy[ReprDecoder[R]],
    defaults: Default.AsRecord.Aux[A, D],
    defaultMapper: RecordToMap[D],
    config: Configuration
  ): ConfiguredDecoder[A] = new ConfiguredDecoder[A] {
    private[this] val defaultMap: Map[String, Any] = if (config.useDefaults) defaultMapper(defaults()) else Map.empty

    final def apply(c: HCursor): Decoder.Result[A] = decode.value.configuredDecode(c)(
      config.transformKeys,
      defaultMap,
      None
    ) match {
      case Right(r) => Right(gen.from(r))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[A]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      decode.value.configuredDecodeAccumulating(c)(
      config.transformKeys,
      defaultMap,
      None
    ).map(gen.from)
  }

  implicit def decodeAdt[A, R <: Coproduct](implicit
    gen: LabelledGeneric.Aux[A, R],
    decode: Lazy[ReprDecoder[R]],
    config: Configuration
  ): ConfiguredDecoder[A] = new ConfiguredDecoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] = decode.value.configuredDecode(c)(
      identity,
      Map.empty,
      config.discriminator
    ) match {
      case Right(r) => Right(gen.from(r))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[A]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      decode.value.configuredDecodeAccumulating(c)(
        identity,
        Map.empty,
        config.discriminator
      ).map(gen.from)
  }
}
