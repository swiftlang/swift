// RUN: %swift -parse %s

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
def f0<T: P1>(t: T) -> Int { return 0 }
def f0<T: P2>(t: T) -> Double { return 0 }

var f0_x1 = f0(x1)
i = f0_x1
var f0_x2 = f0(x2)
d = f0_x2

// Overloading based on the requirements of associated types
def f1<T: P1>(t: T) -> Int { return 0 }
def f1<T: P1 where T.Assoc: P3>(t: T) -> Double { return 0 }

var f1_x1 = f1(x1)
d = f1_x1

// Overloading based on same-type constraints.
def f2<T: P1, U: P1>(t : T, u : U) -> Int { return 0 }
def f2<T: P1, U: P1 where T.Assoc == U.Assoc>(t : T, u : U) -> Double { return 0 }

var f2_x1_x1 = f2(x1, x1)
d = f2_x1_x1
var f2_x1b_x1b = f2(x1b, x1b)
d = f2_x1b_x1b
var f2_x1_x1b = f2(x1, x1b)
i = f2_x1_x1b
