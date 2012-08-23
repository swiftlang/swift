// RUN: %swift -repl < %s 2>&1 | FileCheck %s

protocol Fooable { func foo() }
protocol Barable { func bar() } 

extension Int : Fooable, Barable {
  func foo() {}
  func bar() {}
}

extension Float : Barable {
  func bar() {}
}

func f0(_ : Barable) {}
func f1(_ : protocol<Fooable, Barable>) {}
func f2(_ : Float) {}

func g(_ : (protocol<Barable, Fooable>) -> ()) {}

var i : Int
var f : Float
var b : Barable

//===--------------------------------------------------------------------===//
// Conversion to and among existential types
//===--------------------------------------------------------------------===//

// CHECK: Constraints:
// CHECK:   Barable -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Int << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as Barable
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(i)

// CHECK: Constraints:
// CHECK:   Barable -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Float << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as Barable
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(f)

// CHECK: Constraints:
// CHECK:   Barable -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Barable << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as Barable
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(b)

// CHECK: Constraints:
// CHECK:   protocol<Fooable, Barable> -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Int << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as protocol<Barable, Fooable>
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f1(i)

// CHECK: Constraints:
// CHECK:   protocol<Fooable, Barable> -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Float << $T0
// CHECK: UNSOLVED
// CHECK: Type Variables:
// CHECK:   $T0 as protocol<Barable, Fooable>
// CHECK:   $T1 as ()
// CHECK: UNSOLVED
// CHECK: No solution found.
:dump_constraints f1(f)

// CHECK: Constraints:
// CHECK:   protocol<Fooable, Barable> -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Barable << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as protocol<Barable, Fooable>
// CHECK:   $T1 as ()
// CHECK: UNSOLVED
// CHECK: No solution found.
:dump_constraints f1(b)

//===--------------------------------------------------------------------===//
// Subtyping
//===--------------------------------------------------------------------===//

// CHECK: Constraints:
// CHECK:   (protocol<Barable, Fooable>) -> () -> () == $T0 -> $T1
// CHECK:   Barable -> () << $T0
// CHECK: UNSOLVED
// CHECK: Type Variables:
// CHECK:   $T0 as protocol<Barable, Fooable> -> ()
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints g(f0)

// CHECK: Constraints:
// CHECK:   (protocol<Barable, Fooable>) -> () -> () == $T0 -> $T1
// CHECK:   protocol<Fooable, Barable> -> () << $T0
// CHECK: UNSOLVED
// CHECK: Type Variables:
// CHECK:   $T0 as protocol<Barable, Fooable> -> ()
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints g(f1)

// CHECK: Constraints:
// CHECK:   (protocol<Barable, Fooable>) -> () -> () == $T0 -> $T1
// CHECK:   Float -> () << $T0
// CHECK: UNSOLVED
// CHECK: Type Variables:
// CHECK:   $T0 as protocol<Barable, Fooable> -> ()
// CHECK:   $T1 as ()
// CHECK: Constraints:
// CHECK: UNSOLVED
// CHECK: No solution found.
:dump_constraints g(f2)
