// RUN: %target-swift-frontend -O -emit-sil -enforce-exclusivity=unchecked -Xllvm -sil-print-generic-specialization-loops -Xllvm -sil-print-generic-specialization-info %s 2>&1 | %FileCheck --check-prefix=CHECK %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// Check that the generic specializer does not hang a compiler by
// creating and infinite loop of generic specializations.

// This is a complete set of expected detected generic specialization loops:
// CHECK-DAG: generic specialization loop{{.*}}testFoo7
// CHECK-DAG: generic specialization loop{{.*}}testFoo6
// CHECK-DAG: generic specialization loop{{.*}}foo3
// CHECK-DAG: generic specialization loop{{.*}}foo4
// CHECK-DAG: generic specialization loop{{.*}}bar4
// CHECK-DAG: generic specialization loop{{.*}}Something{{.*}}compoundValue

// CHECK-LABEL: sil_stage canonical

// Check that a specialization information for a specialized function was produced.
// CHECK-LABEL: // Generic specialization information for function $s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lFSi_SdTg5
// CHECK-NEXT:  // Caller: $s044generic_specialization_loops_detection_with_C011testFooBar4yyF
// CHECK-NEXT:  // Parent: $s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lF
// CHECK-NEXT:  // Substitutions: <Int, Double>

// Check that the compiler has produced a specialization information for a call-site that
// was inlined from a specialized generic function.
//
// Currently, bar4<Int, Double> is inlined into foo4<Int, Double>.
// CHECK-LABEL: sil shared [noinline] @$s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lFSi_SdTg5 : $@convention(thin) (Int, Double) -> () {
// CHECK:       // Generic specialization information for call-site $s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lFSaySays5UInt8VGG_SaySaySiGGTg5:
// CHECK-NEXT:  // Caller: $s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lFSi_SdTg5
// CHECK-NEXT:  // Parent: $s044generic_specialization_loops_detection_with_C04bar4yyx_q_tr0_lF
// CHECK-NEXT:  // Substitutions: <Array<UInt8>, Array<Int>>
// CHECK-NEXT:  //
// CHECK-NEXT:  // Caller: $s044generic_specialization_loops_detection_with_C011testFooBar4yyF
// CHECK-NEXT:  // Parent: $s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lF
// CHECK-NEXT:  // Substitutions: <Int, Double>
// CHECK-NEXT:  //
// CHECK-NEXT: apply %{{.*}}Array<Array<UInt8>>
// CHECK-LABEL: } // end sil function '$s044generic_specialization_loops_detection_with_C04foo4yyx_q_tr0_lFSi_SdTg5'

// Check specializations of mutually recursive functions which
// may result in an infinite specialization loop.
public struct MyStruct<A, B> {
}

func foo3<T, S>(_ t: T, _ s: S) {
  bar3(s, t)
}

func bar3<T, S>(_ t: T, _ s: S) {
  foo3(t, MyStruct<T, S>())
}

public func testFooBar3() {
  foo3(1, 2.0)
}

// Check specializations of mutually recursive functions which
// may result in an infinite specialization loop.
public var g = 0

// Don't inline foo4 just so we can reliably check for specialization
// information both at the function and call-site level.
// bar4 is still inlined so we can test for inlined specialization info.
@inline(never)
func foo4<T, S>(_ t: T, _ s: S) {
  // Here we have multiple call-sites of the same generic
  // functions inside the same caller.
  // Some of these call-sites use different generic type parameters.
  bar4([UInt8(1)], [t])
  if g > 0 {
    bar4(t, t)
  } else {
    bar4(t, s)
  }
}

func bar4<T, S>(_ t: T, _ s: S) {
  foo4([t], [s])
}

public func testFooBar4() {
  foo4(1, 2.0)
}

// This is an example of a deeply nested generics which
// may result in an infinite specialization loop.
class Something<T> {
   var somethingArray: Something<Array<T>>? = nil
   var somethingOptional: Something<Optional<T>>? = nil
   var value: T? = nil

   init() {
   }

   init(plainValue: T) {
      value = plainValue
   }

   init(compoundValue: T) {
      value = compoundValue
      somethingArray = Something<Array<T>>(compoundValue: [compoundValue])
      somethingOptional = Something<Optional<T>>(plainValue: compoundValue as T?)
   }

   func map<U>(_ f: (T) -> (U)) -> Something<U> {
      let somethingArrayU = somethingArray?.map { $0.map { f($0) } }
      let somethingOptionalU = somethingOptional?.map { $0.map { f($0) } }
      let valueU = value.map { f($0) }
      let s = Something<U>()
      s.value = valueU
      s.somethingArray = somethingArrayU
      s.somethingOptional = somethingOptionalU
      return s
   }
}

print(Something<Int8>(compoundValue: 0))
print(Something<Int8>(compoundValue: 0).map { Double($0) })

// Test more complex cases, where types of substitutions are partially
// contained in each other.
protocol P {
  associatedtype X: P
}

struct Start {}
struct Step<Param> {}

struct Outer<Param>: P {
  typealias X = Outer<Step<Param>>
}

func testFoo6<T: P>(_: T.Type) {
  testFoo6(T.X.self)
}

func testFoo7<T: P>(_: T.Type) {
  testFoo7(T.X.self)
}

struct Outer1<Param>: P {
  typealias X = Outer2<Param>
}

struct Outer2<Param>: P {
  typealias X = Outer3<Param>
}

struct Outer3<Param>: P {
  typealias X = Outer4<Param>
}

struct Outer4<Param>: P {
  typealias X = Outer5<Param>
}

struct Outer5<Param>: P {
  typealias X = Outer1<Step<Param>>
}

// T will look like:
// Outer<Start>
// Outer<Step<Start>>
// Outer<Step<Step<Start>>>
// ...
// As it can be seen, the substitution type is growing, but a type
// on each specialization iteration would not completely contain a type from
// the previous iteration. Instead, it partially contains it. That is,
// if all common structural prefixes are dropped, then it looks like:
//  Start
//  Step<Start>
//  Step<Step<Start>>
//  ...
//  And it can be easily seen that the type used by the new iteration contains
//  a type from the previous one.
testFoo6(Outer<Start>.self)
// Check a more complex, but similar idea.
testFoo7(Outer1<Start>.self)
