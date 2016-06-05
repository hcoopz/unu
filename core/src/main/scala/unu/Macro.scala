package unu

import scala.annotation.tailrec
import scala.language.experimental.macros

private object Macro {

  import scala.reflect.macros.blackbox

  private def natToInt(c: blackbox.Context)(tpe: c.universe.Type): Int = {
    import c.universe._
    @tailrec
    def toInt(tpe: Type, acc: Int): Int = {
      //      c.info(c.enclosingPosition, s"Converting $tpe to an int", force = false)

      if (tpe =:= c.typeTag[unu.Nat.One.type].tpe) {
        acc
      } else if (tpe.typeConstructor =:= c.typeTag[unu.Nat.Succ[_]].tpe.typeConstructor) {
        toInt(tpe.typeArgs.head, acc + 1)
      } else {
        tpe match {

          case TypeRef(pre, sym, args) =>
            //            c.info(c.enclosingPosition, s"$tpe is a TypeRef($pre, $sym, $args)", force = false)

            toInt(c.universe.appliedType(sym, args).dealias, acc)

          case _ =>
            c.abort(c.enclosingPosition, s"I don't know what $tpe (${tpe.getClass}) is")
        }
      }
    }
    toInt(tpe, 1)
  }

  private def convert[A <: Term, B <: Term](c: blackbox.Context)(a: c.WeakTypeTag[A], b: c.WeakTypeTag[B]): List[(c.universe.SingleType, Int)] = {
    import c.universe._

    val derived = c.typeTag[unu.Term.DerivedUnit[_]]
    val base = c.typeTag[unu.Term.BaseUnit]
    val dimensionless = c.typeTag[unu.Term.Dimensionless]
    val mult = c.typeTag[unu.Term.Mult[_, _]]
    val div = c.typeTag[unu.Term.Div[_, _]]
    val exp = c.typeTag[unu.Term.Exp[_, _]]

    case class UnitsAndConversions(units: List[(Type, Int)], conversions: List[(SingleType, Int)])

    def normalize(tpe: Type, derivedUnit: Option[SingleType]): UnitsAndConversions = {

//      c.info(c.enclosingPosition, s"Normalizing $tpe with derived unit $derivedUnit", force = false)

      if (tpe =:= dimensionless.tpe) {
//        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Dimensionless", force = false)
        UnitsAndConversions(Nil, Nil)
      } else if (tpe.typeConstructor =:= derived.tpe.typeConstructor) {
//        c.info(c.enclosingPosition, s"$tpe is a unu.Term.DerivedUnit", force = false)
        tpe match {
          case t@SingleType(pre, sym) =>
            //            c.info(c.enclosingPosition, s"$tpe is a SingleType($pre, $sym)", force = false)
            normalize(sym.info, Some(t))

          case TypeRef(pre, sym, args) =>
            val args@List(a) = tpe.typeArgs
            //            c.info(c.enclosingPosition, s"$tpe is a TypeRef($pre, $sym, $args)", force = false)
            derivedUnit match {
              case Some(t) =>
                val result = normalize(a, None)
                val (c, rest) = result.conversions.partition{ case (c, _) => c =:= t }
                assert(c.size <= 1, s"Found more than one conversion that =:= $t: $c")
                val conversions =
                  if (c.isEmpty) (t -> 1) :: rest
                  else (c.head._1 -> (c.head._2 + 1)) :: rest
                UnitsAndConversions(result.units, conversions)

              case None =>
                c.abort(c.enclosingPosition, s"No derived unit single type was found (are you using DerivedUnit directly?)")
            }

          case _ =>
            c.abort(c.enclosingPosition, s"$tpe (${tpe.getClass}) is not a SingleType")
        }

      } else if (tpe.typeConstructor =:= base.tpe.typeConstructor) {
//        c.info(c.enclosingPosition, s"$tpe is a unu.Term.BaseUnit", force = false)
        tpe match {
          case t@SingleType(pre, sym) =>
//            c.info(c.enclosingPosition, s"$tpe is a SingleType($pre, $sym)", force = false)
            UnitsAndConversions(List(t -> 1), Nil)

          case _ =>
            c.abort(c.enclosingPosition, s"$tpe (${tpe.getClass}) is not a SingleType (are you using BaseUnit directly?)")
        }
      } else if (tpe.typeConstructor =:= div.tpe.typeConstructor) {
        val args@List(a, b) = tpe.typeArgs
//        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Div with args $args", force = false)

        val an = normalize(a, derivedUnit)
        val bn = normalize(b, derivedUnit)
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
        UnitsAndConversions(units.filterNot{ case (_, e) => e == 0 }, conversions.filterNot{ case (_, e) => e == 0 })

      } else if (tpe.typeConstructor =:= mult.tpe.typeConstructor) {
        val args@List(a, b) = tpe.typeArgs
//        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Mult with args $args", force = false)

        val an = normalize(a, derivedUnit)
        val bn = normalize(b, derivedUnit)
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
        UnitsAndConversions(units.filterNot{ case (_, e) => e == 0 }, conversions.filterNot{ case (_, e) => e == 0 })

      } else if (tpe.typeConstructor =:= exp.tpe.typeConstructor) {
        val args@List(a, b) = tpe.typeArgs
//        c.info(c.enclosingPosition, s"$tpe is a unu.Term.Exp with args $args", force = false)

        val e = natToInt(c)(b)
        //        c.info(c.enclosingPosition, s"exp = $e", force = false)

        val an = normalize(a, derivedUnit)
        val units = an.units.map{ case (u, exp) => u -> (exp * e) }
        val conversions = an.conversions.map{ case (c, exp) => c -> (exp * e) }
        UnitsAndConversions(units.filterNot{ case (_, e) => e == 0 }, conversions.filterNot{ case (_, e) => e == 0 })

      } else {
        tpe match {
          //          case SingleType(pre, sym) =>
          //            c.info(c.enclosingPosition, s"$tpe is a SingleType($pre, $sym)", force = false)
          //            ???

          case TypeRef(pre, sym, args) =>
//            c.info(c.enclosingPosition, s"$tpe is a TypeRef($pre, $sym, $args)", force = false)

            val next = c.universe.appliedType(sym, args).dealias
            if (tpe == next) {
              c.abort(c.enclosingPosition, s"Got stuck trying to normalize $tpe")
            } else {
              normalize(next, derivedUnit)
            }

          case NullaryMethodType(t) =>
//            c.info(c.enclosingPosition, s"$tpe is a NullaryTypeMethod($t)", force = false)
            normalize(t, derivedUnit)

          case _ =>
            c.abort(c.enclosingPosition, s"I don't know what $tpe (${tpe.getClass}) is")
        }
      }
    }

    val an = normalize(a.tpe, None)
    //    c.info(c.enclosingPosition, s"Normalized ${a.tpe} as $an", force = false)

    val bn = normalize(b.tpe, None)//.sortBy{ case (t, _) => t.typeSymbol.fullName }
    //    c.info(c.enclosingPosition, s"Normalized ${b.tpe} as $bn", force = false)

    val au = an.units.sortBy{ case (t, _) => t.typeSymbol.fullName }
    val bu = bn.units.sortBy{ case (t, _) => t.typeSymbol.fullName }

    val ac = an.conversions.sortBy{ case (t, _) => t.typeSymbol.fullName }
    val bc = bn.conversions.sortBy{ case (t, _) => t.typeSymbol.fullName }

    def difference(au: List[(Type, Int)], bu: List[(Type, Int)]): List[(Type, Int)] = {
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
        val (c, rest) = result.partition{ case (t, _) => bt =:= t }
        assert(c.size <= 1, s"Found more than one conversion that =:= $bt: $c")
        if (c.isEmpty) (bt, be) :: rest
        else if (c.head._2 == -be) rest
        else (c.head._1, be + c.head._2) :: rest
      }
//      c.info(c.enclosingPosition, s"${a.tpe} -> ${b.tpe} simplified conversion factors: $simplifiedConversions", force = false)
      simplifiedConversions
    }
  }

  def materializeConvert[U, A <: Term, B <: Term](c: blackbox.Context)(implicit a: c.WeakTypeTag[A], b: c.WeakTypeTag[B], u: c.WeakTypeTag[U]): c.Expr[Convert[U, A, B]] = {
    import c.universe._
    val conversions = convert(c)(a, b)
    val numeric = c.inferImplicitValue(c.universe.appliedType(c.typeTag[Numeric[_]].tpe.typeConstructor.typeSymbol, u.tpe))
    def fractional = c.inferImplicitValue(c.universe.appliedType(c.typeTag[Fractional[_]].tpe.typeConstructor.typeSymbol, u.tpe))
    val factor = conversions.foldLeft[c.Tree](q"$numeric.one"){ case (tree, (tpe, exp)) =>
      assert(exp != 0, s"Somehow got a factor with power 0: $tpe")
      val power = (1 to (exp.abs - 1)).toList.foldLeft[c.Tree](q"${tpe.sym}.ratio[${u.tpe}]")((tree, _) => q"${tpe.sym}.ratio[${u.tpe}] * $tree")

      if (exp > 0) {
        q"$numeric.times(($tree), ($power))"
      } else {
        q"$fractional.div(($tree), ($power))"
      }
    }
//    c.info(c.enclosingPosition, s"Got conversion factor from ${a.tpe} to ${b.tpe}: $conversions: $factor", force = false)
    c.Expr[Convert[U, A, B]](q"new Convert[${u.tpe}, ${a.tpe}, ${b.tpe}]($factor)")
  }
}
