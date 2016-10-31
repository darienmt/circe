package io.circe.generic.extras

import io.circe.{ Decoder, ObjectEncoder }
import io.circe.export.Exported
import io.circe.generic.ExportMacros
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import scala.language.experimental.macros

/**
 * Fully automatic configurable codec derivation.
 *
 * Importing the contents of this object provides [[io.circe.Decoder]] and [[io.circe.Encoder]]
 * instances for case classes (if all members have instances), "incomplete" case classes, sealed
 * trait hierarchies, etc.
 */
final object auto {
  implicit def exportDecoder[A]: Exported[Decoder[A]] = macro ExportMacros.exportDecoderImpl[ConfiguredDecoder, A]
  //implicit def exportEncoder[A]: Exported[ObjectEncoder[A]] = macro ExportMacros.exportEncoderImpl[ConfiguredObjectEncoder, A]
}
