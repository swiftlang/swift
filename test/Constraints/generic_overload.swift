// RUN: %target-parse-verify-swift

protocol P1 { typealias Assoc }
protocol P2 : P1 { typealias Assoc }
protocol P3 { }

struct X1 : P1 { typealias Assoc = X3 }
struct X1b : P1 { typealias Assoc = Int }
struct X2 : P2 { typealias Assoc = X2 }
struct X3 : P3 { }

// Convenience variables
var i = 10
var d = 3.14159
var x1 = X1()
var x1b = X1b()
var x2 = X2()

// Overloading based on requirements
func f0<T: P1>(t: T) -> Int { return 0 }
func f0<T: P2>(t: T) -> Double { return 0 }

var f0_x1 = f0(x1)
i = f0_x1
var f0_x2 = f0(x2)
d = f0_x2

// Overloading based on the requirements of associated types
func f1<T : P1>(t: T) -> Int { return 0 }
func f1<T : P1 where T.Assoc : P3>(t: T) -> Double { return 0 }

var f1_x1 = f1(x1)
d = f1_x1

// Overloading based on same-type constraints.
func f2<T : P1, U : P1>(t: T, u: U) -> Int { return 0 }
func f2<T : P1, U : P1 where T.Assoc == U.Assoc>(t : T, u : U) -> Double { return 0 }

var f2_x1_x1 = f2(x1, x1)
d = f2_x1_x1
var f2_x1b_x1b = f2(x1b, x1b)
d = f2_x1b_x1b
var f2_x1_x1b = f2(x1, x1b)
i = f2_x1_x1b

// Overloading of struct methods
struct StructOverload<U>  {
  func f0<T : P1>(u: U, t: T) -> Int { return 0 }
  func f0<T : P2>(u: U, t: T) -> Double { return 0 }

  static func f1<T : P1>(u : U, t: T) -> Int { return 0 }
  static func f1<T : P2>(u : U, t: T) -> Double { return 0 }
}

var so = StructOverload<Int>()
var so_f0_x1 = so.f0(5, t: x1)
i = so_f0_x1
var so_f0_x2 = so.f0(5, t: x2)
d = so_f0_x2

typealias SO = StructOverload<Int>
var so_f1_x1 = SO.f1(5, t: x1)
i = so_f1_x1
var so_f1_x2 = SO.f1(5, t: x2)
d = so_f1_x2

// Overloading of class methods
class ClassOverloadA<U>  {
  func f0<T : P1>(u: U, t: T) -> Int { return 0 }
  func f1<T : P2>(u: U, t: T) -> Double { return 0 }
}

class ClassOverloadB<U> : ClassOverloadA<U?> {
  func f0<T : P2>(u: U?, t: T) -> Double { return 0 }
  func f1<T : P1>(u: U?, t: T) -> Int { return 0 }
}

var co = ClassOverloadB<Int>()
var co_f0_int_x1 = co.f0(5, t: x1)
i = co_f0_int_x1
var co_f0_int_x2 = co.f0(5, t: x2)
d = co_f0_int_x2

