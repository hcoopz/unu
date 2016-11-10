# unu: unit correctness using types

**unu** is a proof of concept library for ensuring unit correctness using
Scala's type system.

It's easy to lose track of units when performing numeric computations.
Consider the following contrived example:

```scala
scala> val firstPartInInches = 14d
firstPartInInches: Double = 14.0

scala> val secondPartInMeters = 2d
secondPartInMeters: Double = 2.0

scala> val someQuantityInKilograms = 100d
someQuantityInKilograms: Double = 100.0

scala> val totalLength = firstPartInInches + secondPartInMeters
totalLength: Double = 16.0

scala> val totalWeight = someQuantityInKilograms + firstPartInInches
totalWeight: Double = 114.0
```

The above code compiles, but the values it computes are nonsense! However, we
can use Scala's type system to prevent this sort of mistake. With unu, you
can use wrapper classes to specify units for numeric values:

```scala
scala> import unu._; import unu.unit._
import unu._
import unu.unit._

scala> val firstPartInInches: (Double ~ length.in) = Value(14d)
firstPartInInches: unu.~[Double,unu.unit.length.in] = 14.0

scala> val secondPartInMeters: (Double ~ length.m) = Value(2d)
secondPartInMeters: unu.~[Double,unu.unit.length.m] = 2.0

scala> val someQuantityInKilograms: (Double ~ mass.kg) = Value(100d)
someQuantityInKilograms: unu.~[Double,unu.unit.mass.kg] = 100.0

scala> /* one of the lengths will be converted to be in the same units as the other */ 
     | val totalLength = firstPartInInches + secondPartInMeters;
totalLength: unu.Value[Double,unu.unit.length.in] = 92.74015748031496

scala> /* the below line will not compile! */
     | val totalWeight = someQuantityInKilograms + firstPartInInches
<console>:19: error: unu.unit.length.in and unu.unit.mass.kg have different dimensions: List((unu.unit.length.m.type,1)), List((unu.unit.mass.kg.type,1))
       val totalWeight = someQuantityInKilograms + firstPartInInches
```

Addition, subtraction, multiplication, and division are supported.

You can convert a value to other units:
 
```scala
scala> val totalLengthInCentimeters = totalLength.in[length.cm]
totalLengthInCentimeters: unu.Value[Double,unu.unit.length.cm] = 235.56
```

You can express compound units:

```scala
scala> val speed: (Double ~ (length.m / time.s)) = Value(10d)
speed: unu.~[Double,unu./[unu.unit.length.m,unu.unit.time.s]] = 10.0

scala> val mph = speed.in[length.mi / time.hr]
mph: unu.Value[Double,unu./[unu.unit.length.mi,unu.unit.time.hr]] = 22.36936292054402
```

You can define your own units in terms of others:

```scala
scala> val km = Term.DerivedUnit[length.m](1, 1000); type km = km.type
km: unu.Term.DerivedUnit[unu.unit.length.m] = DerivedUnit(1,1000)
defined type alias km

scala> (Value(10000d): (Double ~ length.m)).in[km]
res0: unu.Value[Double,km] = 10.0

scala> val furlong = Term.DerivedUnit[length.mi](8, 1); type furlong = furlong.type
furlong: unu.Term.DerivedUnit[unu.unit.length.mi] = DerivedUnit(8,1)
defined type alias furlong

scala> import unu.number._; val atm = Term.DerivedUnit[mass.kg / (length.m ** (time.s ^ `2`))](1, 101325); type atm = atm.type
import unu.number._
atm: unu.Term.DerivedUnit[unu./[unu.unit.mass.kg,unu.**[unu.unit.length.m,unu.^[unu.unit.time.s,unu.number.2]]]] = DerivedUnit(1,101325)
defined type alias atm
```

Don't expect good performance from it! The bytecode generated isn't great.
