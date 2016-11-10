package unu

import unu.number.Nat
import unu.number._
import spire.algebra._
import spire.math._

import scala.annotation.tailrec
import scala.language.experimental.macros

private object Macro {

  import scala.reflect.macros.blackbox

  private case class Fraction private (num: Long, denom: Long) {
    def *(that: Fraction): Fraction = {
      val num = this.num * that.num
      val denom = this.denom * that.denom

      val gcd = spire.math.gcd(num, denom)

      new Fraction(num / gcd, denom / gcd)
    }

    def zero: Boolean = num == 0
    def nonzero: Boolean = !zero

    def `unary_-`: Fraction = new Fraction(-num, denom)

    def +(that: Fraction): Fraction = {
      val num = this.num * that.denom + that.num * this.denom
      val denom = this.denom * that.denom

      val gcd = spire.math.gcd(num, denom)

      new Fraction(num / gcd, denom / gcd)
    }

    def -(that: Fraction) = this + (-that)

    def positive: Boolean = num != 0 && num.signum == denom.signum

    def >(that: Fraction): Boolean = {
      val a = this.num * this.denom.signum * that.denom.abs
      val b = that.num * that.denom.signum * this.denom.abs

      a > b
    }

    def >(that: Long): Boolean = this > new Fraction(that, 1)

    def abs: Fraction = new Fraction(num.abs, denom.abs)
  }

  private object Fraction {
    def fromLong(num: Long): Fraction = new Fraction(num, 1)

    def of(num: Long, denom: Long): Fraction = {
      val gcd = spire.math.gcd(num, denom)

      new Fraction(num / gcd, denom / gcd)
    }
  }

  private def natToInt(c: blackbox.Context)(tpe: c.universe.Type): Int = {
    import c.universe._
    @tailrec
    def toInt(tpe: Type, acc: Int): Int = {
      //      c.info(c.enclosingPosition, s"Converting $tpe to an int", force = false)

      if (tpe =:= c.typeTag[Nat.One.type].tpe) {
        acc
      } else if (tpe.typeConstructor =:= c.typeTag[Nat.Succ[_]].tpe.typeConstructor) {
        toInt(tpe.typeArgs.head, acc + 1)
      } else {
        tpe match {
          case TypeRef(pre, sym, args) =>
            //            c.info(c.enclosingPosition, s"$tpe is a TypeRef($pre, $sym, $args)", force = false)

            toInt(c.universe.appliedType(sym, args).dealias, acc)

          case _ =>
            c.abort(c.enclosingPosition, s"I don't know what $tpe (${tpe.getClass}) is (trying to convert a Nat to an int)")
        }
      }
    }
    toInt(tpe, 1)
  }

  def materializeNatValue[N <: Nat](c: blackbox.Context)(implicit n: c.WeakTypeTag[N]): c.Expr[NatValue[N]] = {
    import c.universe._

    val int = natToInt(c)(n.tpe)

    c.Expr[NatValue[N]](q"new NatValue[${n.tpe}]($int)")
  }

  private def rationalToFraction(c: blackbox.Context)(tpe: c.universe.Type): Fraction = {
    import c.universe._

    val rational = c.typeTag[unu.number.Rational]

    case class RationalTypes(num: c.universe.Type, denom: c.universe.Type)
    def normalize(tpe: Type): RationalTypes = {
      tpe match {
        case TypeRef(pre, sym, args) =>
          val next = c.universe.appliedType(sym, args).dealias
          if (tpe == next) {
            c.abort(c.enclosingPosition, s"I don't know what to do next $tpe $pre $sym $args")
          } else {
            normalize(next)
          }

        case RefinedType(types, scope) if types.size == 1 && types.head =:= rational.tpe =>
          val num = scope.find(_.asType.name == TypeName("Num")).map(sym => c.universe.appliedType(sym).dealias).getOrElse(c.abort(c.enclosingPosition, s"No Num type in $tpe $types $scope"))
          val denom = scope.find(_.asType.name == TypeName("Denom")).map(sym => c.universe.appliedType(sym).dealias).getOrElse(c.abort(c.enclosingPosition, s"No Denom type in $tpe $types $scope"))
          RationalTypes(num, denom)

        case NullaryMethodType(t) =>
          normalize(t)

        case _ =>
          c.abort(c.enclosingPosition, s"I don't know what $tpe (${tpe.getClass}) is (trying to convert a Rational to a fraction)")
      }
    }

    val RationalTypes(num, denom) = normalize(tpe)

    Fraction.of(natToInt(c)(num), natToInt(c)(denom))
  }

  def materializeRationalValue[R <: unu.number.Rational](c: blackbox.Context)(implicit r: c.WeakTypeTag[R]): c.Expr[RationalValue[R]] = {
    import c.universe._

    val Fraction(num, denom) = rationalToFraction(c)(r.tpe)

    c.Expr[RationalValue[R]](q"new RationalValue[${r.tpe}]($num.toInt, $denom.toInt)")
  }

  private def convert[A <: Term, B <: Term](c: blackbox.Context)(a: c.WeakTypeTag[A], b: c.WeakTypeTag[B]): List[(c.universe.SingleType, Fraction)] = {
    import c.universe._

    val derived = c.typeTag[unu.Term.DerivedUnit[_]]
    val base = c.typeTag[unu.Term.BaseUnit]
    val dimensionless = c.typeTag[unu.Term.Dimensionless]
    val mult = c.typeTag[unu.Term.Mult[_, _]]
    val div = c.typeTag[unu.Term.Div[_, _]]
    val exp = c.typeTag[unu.Term.Exp[_, _]]

    case class UnitsAndConversions(units: List[(Type, Fraction)], conversions: List[(SingleType, Fraction)])

    def normalize(tpe: Type): UnitsAndConversions = {
//      c.info(c.enclosingPosition, s"Normalizing $tpe", force = false)
      tpe match {
        case _ if tpe =:= dimensionless.tpe =>
          //        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Dimensionless", force = false)
          UnitsAndConversions(Nil, Nil)

        case t@SingleType(pre, sym) if tpe.typeConstructor =:= derived.tpe.typeConstructor =>
          //        c.info(c.enclosingPosition, s"$tpe is a unu.Term.DerivedUnit", force = false)

          val derivedUnit = t

          @tailrec
          def convertSingleType(t: c.Type): UnitsAndConversions = t match {
            case NullaryMethodType(t) =>
              convertSingleType(t)

            case TypeRef(pre, sym, args) =>
              val args@List(a) = t.typeArgs
              val result = normalize(a)
              val (c, rest) = result.conversions.partition{ case (c, _) => c =:= derivedUnit }
              assert(c.size <= 1, s"Found more than one conversion that =:= $derivedUnit: $c")
              val conversions =
                if (c.isEmpty) (derivedUnit -> Fraction.fromLong(1)) :: rest
                else (c.head._1 -> (c.head._2 + Fraction.fromLong(1))) :: rest
              UnitsAndConversions(result.units, conversions)
          }
          convertSingleType(sym.info)

        case t@SingleType(pre, sym) if tpe.typeConstructor =:= base.tpe.typeConstructor =>
          //        c.info(c.enclosingPosition, s"$tpe is a unu.Term.BaseUnit", force = false)
          UnitsAndConversions(List(t -> Fraction.fromLong(1)), Nil)

        case _ if tpe.typeConstructor =:= div.tpe.typeConstructor =>
          val args@List(a, b) = tpe.typeArgs
          //        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Div with args $args", force = false)

          val an = normalize(a)
          val bn = normalize(b)
          val units = bn.units.foldLeft(an.units) { case (result, (typ, exp)) =>
            val (u, rest) = result.partition{ case (t, _) => t =:= typ }
            assert(u.size <= 1, s"Found more than one unit that =:= $typ: $u")
            if (u.isEmpty) (typ -> -exp) :: rest
            else (u.head._1 -> (u.head._2 - exp)) :: rest
          }
          val conversions = bn.conversions.foldLeft(an.conversions) { case (result, (typ, exp)) =>
            val (c, rest) = result.partition{ case (t, _) => t =:= typ }
            assert(c.size <= 1, s"Found more than one conversion that =:= $typ: $c")
            if (c.isEmpty) (typ -> -exp) :: rest
            else (c.head._1 -> (c.head._2 - exp)) :: rest
          }
          UnitsAndConversions(units.filterNot{ case (_, e) => e.zero }, conversions.filterNot{ case (_, e) => e.zero })

        case _ if tpe.typeConstructor =:= mult.tpe.typeConstructor =>
          val args@List(a, b) = tpe.typeArgs
          //        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Mult with args $args", force = false)

          val an = normalize(a)
          val bn = normalize(b)
          val units = bn.units.foldLeft(an.units) { case (result, (typ, exp)) =>
            val (u, rest) = result.partition{ case (t, _) => t =:= typ }
            assert(u.size <= 1, s"Found more than one unit that =:= $typ: $u")
            if (u.isEmpty) (typ -> exp) :: rest
            else (u.head._1 -> (u.head._2 + exp)) :: rest
          }
          val conversions = bn.conversions.foldLeft(an.conversions) { case (result, (typ, exp)) =>
            val (c, rest) = result.partition{ case (t, _) => t =:= typ }
            assert(c.size <= 1, s"Found more than one conversion that =:= $typ: $c")
            if (c.isEmpty) (typ -> exp) :: rest
            else (c.head._1 -> (c.head._2 + exp)) :: rest
          }
          UnitsAndConversions(units.filterNot{ case (_, e) => e.zero }, conversions.filterNot{ case (_, e) => e.zero })

        case _ if tpe.typeConstructor =:= exp.tpe.typeConstructor =>
          val args@List(a, b) = tpe.typeArgs
          //        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Exp with args $args", force = false)

          val e = rationalToFraction(c)(b)

          val an = normalize(a)
          val units = an.units.map{ case (u, exp) => u -> (exp * e) }
          val conversions = an.conversions.map{ case (c, exp) => c -> (exp * e) }
          UnitsAndConversions(units.filterNot{ case (_, e) => e.zero }, conversions.filterNot{ case (_, e) => e.zero })

        case TypeRef(pre, sym, args) =>
          val next = c.universe.appliedType(sym, args).dealias
          if (tpe == next) {
            UnitsAndConversions(List((tpe, Fraction.fromLong(1))), Nil)
          } else {
            normalize(next)
          }

        case NullaryMethodType(t) =>
          normalize(t)

        case _ =>
          c.abort(c.enclosingPosition, s"I don't know what $tpe (${tpe.getClass}) is")
      }
    }

    val an = normalize(a.tpe)
    //    c.info(c.enclosingPosition, s"Normalized ${a.tpe} as $an", force = false)

    val bn = normalize(b.tpe)//.sortBy{ case (t, _) => t.typeSymbol.fullName }
    //    c.info(c.enclosingPosition, s"Normalized ${b.tpe} as $bn", force = false)

    val au = an.units.sortBy{ case (t, _) => t.typeSymbol.fullName }
    val bu = bn.units.sortBy{ case (t, _) => t.typeSymbol.fullName }

    val ac = an.conversions.sortBy{ case (t, _) => t.typeSymbol.fullName }
    val bc = bn.conversions.sortBy{ case (t, _) => t.typeSymbol.fullName }

    def difference(au: List[(Type, Fraction)], bu: List[(Type, Fraction)]): List[(Type, Fraction)] = {
      bu.foldLeft(au) { case (result, (bt, be)) =>
        result.collect {
          case (at, ae) if !(at =:= bt) =>
            (at, ae)

          case (at, ae) if ae > be =>
            (at, ae)
        }
      }
    }

    val unitsDiff = difference(au, bu) ++ difference(bu, au)

    if (unitsDiff.nonEmpty) {
      c.abort(c.enclosingPosition, s"${a.tpe} and ${b.tpe} have different dimensions: $au, $bu")
    } else {
//      c.info(c.enclosingPosition, s"${a.tpe} -> ${b.tpe} conversion factors: $ac, $bc", force = false)
      val simplifiedConversions = bc.foldLeft(ac.map{ case (t, e) => t -> -e }) { case (result, (bt, be)) =>
        val (existing, rest) = result.partition{ case (t, _) => bt =:= t }
        assert(existing.size <= 1, s"Found more than one conversion that =:= $bt: $existing")
        if (existing.isEmpty) (bt, be) :: rest
        else if (existing.head._2 == -be) rest
        else (existing.head._1, be + existing.head._2) :: rest
      }
//      c.info(c.enclosingPosition, s"${a.tpe} -> ${b.tpe} simplified conversion factors: $simplifiedConversions", force = false)
      simplifiedConversions
    }
  }

  def materializeConvert[U, A <: Term, B <: Term](c: blackbox.Context)(implicit a: c.WeakTypeTag[A], b: c.WeakTypeTag[B], u: c.WeakTypeTag[U]): c.Expr[Convert[U, A, B]] = {
    import c.universe._
    val conversions = convert(c)(a, b)
    val mmonoid = c.inferImplicitValue(c.universe.appliedType(c.typeTag[MultiplicativeMonoid[_]].tpe.typeConstructor.typeSymbol, u.tpe))
    lazy val mgroup = c.inferImplicitValue(c.universe.appliedType(c.typeTag[MultiplicativeGroup[_]].tpe.typeConstructor.typeSymbol, u.tpe))
    lazy val nroot = c.inferImplicitValue(c.universe.appliedType(c.typeTag[NRoot[_]].tpe.typeConstructor.typeSymbol, u.tpe))
    lazy val fractional = c.inferImplicitValue(c.universe.appliedType(c.typeTag[Fractional[_]].tpe.typeConstructor.typeSymbol, u.tpe))
//    c.info(c.enclosingPosition, conversions.toString, force = false)
    val factor = conversions.foldLeft[c.Tree](q"$mmonoid.one"){ case (tree, (tpe, exp)) =>
      assert(exp.nonzero, s"Somehow got a factor with power 0: $tpe")

      val expabs = exp.abs

      val power = if (expabs.denom == 1 && expabs.num.isValidInt) {
        q"$mmonoid.prodn(${tpe.sym}.ratio[${u.tpe}], ${expabs.num}.toInt)"
      } else {
        q"$nroot.fpow(${tpe.sym}.ratio[${u.tpe}], $fractional.fromRational(spire.math.Rational(${expabs.num}, ${expabs.denom})))"
      }

      if (exp.positive) {
        q"$mmonoid.times(($tree), ($power))"
      } else {
        q"$mgroup.div(($tree), ($power))"
      }
    }
//    c.info(c.enclosingPosition, s"Got conversion factor from ${a.tpe} to ${b.tpe}: $conversions: $factor", force = false)
    val expr = c.Expr[Convert[U, A, B]](q"new Convert[${u.tpe}, ${a.tpe}, ${b.tpe}]($factor)")

//    c.info(c.enclosingPosition, c.universe.showCode(expr.tree), force = false)

    expr
  }
}
