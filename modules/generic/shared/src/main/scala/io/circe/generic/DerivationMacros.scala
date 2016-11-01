package io.circe.generic

import io.circe.{ Decoder, Encoder }
import io.circe.generic.decoding.{ DerivedDecoder, ReprDecoder }
import io.circe.generic.encoding.{ DerivedObjectEncoder, ReprObjectEncoder }
import macrocompat.bundle
import scala.reflect.macros.whitebox
import shapeless.{ CNil, Coproduct, HList, HNil, Lazy }
import shapeless.labelled.KeyTag

@bundle
class DerivationMacros(val c: whitebox.Context) {
  import c.universe._

  /**
   * Crash the attempted derivation because of a failure related to the
   * specified type.
   *
   * Note that these failures are not generally visible to users.
   */
  def fail(tpe: Type): Nothing = c.abort(c.enclosingPosition, s"Cannot generically derive instance: $tpe")

  /**
   * Represents an element at the head of a `shapeless.HList` or
   * `shapeless.Coproduct`.
   */
  case class Member(label: String, keyType: Type, valueType: Type, acc: Type, accTail: Type)

  /**
   * Represents an `shapeless.HList` or `shapeless.Coproduct` type in a way
   * that's more convenient to work with.
   */
  class Members(val underlying: List[Member]) {
    /**
     * Fold over the elements of this (co-)product while accumulating instances
     * of some type class for each.
     */
    def fold[Z](resolver: Type => Tree)(init: Z)(f: (Member, TermName, Z) => Z): (List[Tree], Z) = {
      val (instanceMap, result) = underlying.foldRight((Map.empty[Type, (TermName, Tree)], init)) {
        case (member @ Member(_, _, valueType, _, _), (instanceMap, acc)) =>
          val (instanceName, instance) = instanceMap.getOrElse(valueType, (TermName(c.freshName), resolver(valueType)))
          val newInstances = instanceMap.updated(valueType, (instanceName, instance))

          (newInstances, f(member, instanceName, acc))
      }

      val instanceDefs = instanceMap.values.map {
        case (instanceName, instance) => q"private[this] val $instanceName = $instance"
      }

      (instanceDefs.toList, result)
    }
  }

  object Members {
    private[this] val ShapelessSym = typeOf[HList].typeSymbol.owner
    private[this] val HNilSym = typeOf[HNil].typeSymbol
    private[this] val HConsSym = typeOf[shapeless.::[_, _]].typeSymbol
    private[this] val CNilSym = typeOf[CNil].typeSymbol
    private[this] val CConsSym = typeOf[shapeless.:+:[_, _]].typeSymbol
    private[this] val ShapelessLabelledType = typeOf[shapeless.labelled.type]
    private[this] val KeyTagSym = typeOf[KeyTag[_, _]].typeSymbol
    private[this] val ShapelessTagType = typeOf[shapeless.tag.type]
    private[this] val ScalaSymbolType = typeOf[scala.Symbol]
    private[this] val HListType = typeOf[HList]
    private[this] val CoproductType = typeOf[Coproduct]

    case class Entry(label: String, keyType: Type, valueType: Type)

    object Entry {
      def unapply(tpe: Type): Option[(String, Type, Type)] = tpe.dealias match {
        case RefinedType(List(fieldType, TypeRef(lt, KeyTagSym, List(tagType, taggedFieldType))), _)
          if lt =:= ShapelessLabelledType && fieldType =:= taggedFieldType =>
            tagType.dealias match {
              case RefinedType(List(st, TypeRef(tt, ts, ConstantType(Constant(fieldKey: String)) :: Nil)), _)
                if st =:= ScalaSymbolType && tt =:= ShapelessTagType =>
                  Some((fieldKey, tagType, fieldType))
              case _ => None
            }
        case _ => None
      }
    }

    def fromType(tpe: Type): Option[Members] = tpe.dealias match {
      case TypeRef(ThisType(ShapelessSym), HNilSym | CNilSym, Nil) => Some(new Members(Nil))
      case acc @ TypeRef(ThisType(ShapelessSym), HConsSym | CConsSym, List(fieldType, tailType)) =>
        fieldType match {
          case Entry(label, keyType, valueType) => fromType(tailType).map(members =>
            new Members(Member(label, keyType, valueType, acc, tailType) :: members.underlying)
          )
          case _ => None
        }
      case _ => None
    }

    def fromHListType(tpe: Type): Option[Members] = if (tpe <:< HListType) fromType(tpe) else None
    def fromCoproductType(tpe: Type): Option[Members] = if (tpe <:< CoproductType) fromType(tpe) else None
  }

  def resolveInstance(tpe: Type, tcs: (Type, Boolean)*): Tree = tcs match {
    case (tc, lazily) +: rest =>
      val applied = appliedType(tc.typeConstructor, tpe)
      val target = if (lazily) appliedType(typeOf[Lazy[_]].typeConstructor, applied) else applied
      val inferred = c.inferImplicitValue(target, silent = true)

      inferred match {
        case EmptyTree => resolveInstance(tpe, rest: _*)
        case instance if lazily => q"$instance.value"
        case instance => instance
      }
    case _ => fail(tpe)
  }

  def decodeHList[R <: HList](implicit R: c.WeakTypeTag[R]): c.Expr[ReprDecoder[R]] =
    Members.fromHListType(R.tpe).fold(fail(R.tpe)) { members =>
      val (instanceDefs, (result, accumulatingResult)) = members.fold(
        tpe => resolveInstance(tpe, (typeOf[Decoder[_]], false))
      )(
        (
          q"_root_.scala.util.Right(_root_.shapeless.HNil: _root_.shapeless.HNil)",
          q"_root_.cats.data.Validated.valid(_root_.shapeless.HNil: _root_.shapeless.HNil)"
        )
      ) {
        case (Member(name, nameTpe, tpe, _, accTail), instanceName, (acc, accumulatingAcc)) => (
          q"""
            _root_.io.circe.Decoder.resultInstance.map2[
              $tpe,
              $accTail,
              _root_.shapeless.::[_root_.shapeless.labelled.FieldType[$nameTpe, $tpe], $accTail]
            ](
              orDefault[$tpe](
                $instanceName.tryDecode(c.downField(transformKeys($name))),
                $name,
                defaults
              ),
              $acc
            )((h, t) => _root_.shapeless.::(_root_.shapeless.labelled.field[$nameTpe].apply[$tpe](h), t))
          """,
          q"""
            _root_.io.circe.AccumulatingDecoder.resultInstance.map2[
              $tpe,
              $accTail,
              _root_.shapeless.::[_root_.shapeless.labelled.FieldType[$nameTpe, $tpe], $accTail]
            ](
              orDefaultAccumulating[$tpe](
                $instanceName.tryDecodeAccumulating(c.downField(transformKeys($name))),
                $name,
                defaults
              ),
              $accumulatingAcc
            )((h, t) => _root_.shapeless.::(_root_.shapeless.labelled.field[$nameTpe].apply[$tpe](h), t))
          """
        )
      }

      c.Expr[ReprDecoder[R]](
        q"""
          {
            new _root_.io.circe.generic.decoding.ReprDecoder[$R] {
              ..$instanceDefs

              final def configuredDecode(c: _root_.io.circe.HCursor)(
                transformKeys: _root_.java.lang.String => _root_.java.lang.String,
                defaults: _root_.scala.collection.immutable.Map[_root_.java.lang.String, _root_.scala.Any],
                discriminator: _root_.scala.Option[_root_.java.lang.String]
              ): _root_.io.circe.Decoder.Result[$R] = $result

              final def configuredDecodeAccumulating(
                c: _root_.io.circe.HCursor
              )(
                transformKeys: _root_.java.lang.String => _root_.java.lang.String,
                defaults: _root_.scala.collection.immutable.Map[_root_.java.lang.String, _root_.scala.Any],
                discriminator: _root_.scala.Option[_root_.java.lang.String]
              ): _root_.io.circe.AccumulatingDecoder.Result[$R] = $accumulatingResult
            }: _root_.io.circe.generic.decoding.ReprDecoder[$R]
          }
        """
      )
    }

  private[this] val cnilEitherFailure: Tree = q"""
    _root_.scala.util.Left[_root_.io.circe.DecodingFailure, _root_.shapeless.CNil](
      _root_.io.circe.DecodingFailure("CNil", c.history)
    ): _root_.scala.util.Either[_root_.io.circe.DecodingFailure, _root_.shapeless.CNil]
  """

  private[this] val cnilValidatedNelFailure: Tree = q"""
    _root_.cats.data.Validated.invalidNel[_root_.io.circe.DecodingFailure, _root_.shapeless.CNil](
      _root_.io.circe.DecodingFailure("CNil", c.history)
    )
  """

  def decodeCoproduct[R <: Coproduct](implicit R: c.WeakTypeTag[R]): c.Expr[ReprDecoder[R]] =
    Members.fromCoproductType(R.tpe).fold(fail(R.tpe)) { members =>
      val (instanceDefs, (result, accumulatingResult)) = members.fold(
        tpe => resolveInstance(tpe, (typeOf[Decoder[_]], false), (typeOf[DerivedDecoder[_]], true))
      )(
        (cnilEitherFailure, cnilValidatedNelFailure)
      ) {
        case (Member(name, nameTpe, tpe, current, accTail), instanceName, (acc, accumulatingAcc)) => (
          q"withDiscriminator[$nameTpe, $tpe, $accTail]($instanceName, c, $acc, $name, discriminator)",
          q"""
            withDiscriminatorAccumulating[$nameTpe, $tpe, $accTail](
              $instanceName,
              c,
              $accumulatingAcc,
              $name,
              discriminator
            )
          """
        )
      }

      c.Expr[ReprDecoder[R]](
        q"""
          {
            new _root_.io.circe.generic.decoding.ReprDecoder[$R] {
              ..$instanceDefs
              final def configuredDecode(c: _root_.io.circe.HCursor)(
                transformKeys: _root_.java.lang.String => _root_.java.lang.String,
                defaults: _root_.scala.collection.immutable.Map[_root_.java.lang.String, _root_.scala.Any],
                discriminator: _root_.scala.Option[_root_.java.lang.String]
              ): _root_.io.circe.Decoder.Result[$R] = $result

              final def configuredDecodeAccumulating(
                c: _root_.io.circe.HCursor
              )(
                transformKeys: _root_.java.lang.String => _root_.java.lang.String,
                defaults: _root_.scala.collection.immutable.Map[_root_.java.lang.String, _root_.scala.Any],
                discriminator: _root_.scala.Option[_root_.java.lang.String]
              ): _root_.io.circe.AccumulatingDecoder.Result[$R] = $accumulatingResult
            }: _root_.io.circe.generic.decoding.ReprDecoder[$R]
          }
        """
      )
    }

  def encodeHList[R <: HList](implicit R: c.WeakTypeTag[R]): c.Expr[ReprObjectEncoder[R]] =
    Members.fromHListType(R.tpe).fold(fail(R.tpe)) { members =>
      val (instanceDefs, (pattern, fields)) = members.fold(
        tpe => resolveInstance(tpe, (typeOf[Encoder[_]], false))
      )(
        (pq"_root_.shapeless.HNil": Tree, List.empty[Tree])
      ) {
        case (Member(name, _, tpe, _, _), instanceName, (patternAcc, fieldsAcc)) =>
        val currentName = TermName(c.freshName)

        (
          pq"_root_.shapeless.::($currentName, $patternAcc)",
          q"(transformKeys($name), $instanceName.apply($currentName))" :: fieldsAcc
        )
      }

      c.Expr[ReprObjectEncoder[R]](
        q"""
          {
            new _root_.io.circe.generic.encoding.ReprObjectEncoder[$R] {
              ..$instanceDefs
              final def configuredEncodeObject(a: $R)(
                transformKeys: _root_.java.lang.String => _root_.java.lang.String,
                discriminator: _root_.scala.Option[_root_.java.lang.String]
              ): _root_.io.circe.JsonObject = a match {
                case $pattern =>
                  _root_.io.circe.JsonObject.fromIterable(_root_.scala.collection.immutable.Vector(..$fields))
              }
            }: _root_.io.circe.generic.encoding.ReprObjectEncoder[$R]
          }
        """
      )
    }

  def encodeCoproduct[R <: Coproduct](implicit R: c.WeakTypeTag[R]): c.Expr[ReprObjectEncoder[R]] =
    Members.fromCoproductType(R.tpe).fold(fail(R.tpe)) { members =>
      val (instanceDefs, patternAndCase) = members.fold(
        tpe => resolveInstance(tpe, (typeOf[Encoder[_]], false), (typeOf[DerivedObjectEncoder[_]], true))
      )(
        cq"""_root_.shapeless.Inr(_) => _root_.scala.sys.error("Cannot encode CNil")"""
      ) {
        case (Member(name, _, tpe, _, _), instanceName, acc) =>
        val tailName = TermName(c.freshName)
        val currentName = TermName(c.freshName)

        cq"""
          _root_.shapeless.Inr($tailName) => $tailName match {
            case _root_.shapeless.Inl($currentName) =>
              addDiscriminator[$tpe]($instanceName, $currentName, $name, discriminator)
            case $acc
          }
        """
      }

      c.Expr[ReprObjectEncoder[R]](
        q"""
          {
            new _root_.io.circe.generic.encoding.ReprObjectEncoder[$R] {
              ..$instanceDefs
              final def configuredEncodeObject(a: $R)(
                transformKeys: _root_.java.lang.String => _root_.java.lang.String,
                discriminator: _root_.scala.Option[_root_.java.lang.String]
              ): _root_.io.circe.JsonObject = _root_.shapeless.Inr(a) match {
                case $patternAndCase
              }
            }: _root_.io.circe.generic.encoding.ReprObjectEncoder[$R]
          }
        """
      )
    }
}
