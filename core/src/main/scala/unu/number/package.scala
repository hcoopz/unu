package unu

import number.Nat._

package object number {
  type `1` = One.type
  type `2` = Succ[`1`]
  type `3` = Succ[`2`]
  type `4` = Succ[`3`]
}
