// RUN: %target-swift-frontend -O -emit-sil -Xllvm -sil-print-generic-specialization-loops -Xllvm -sil-print-generic-specialization-info %s 2>&1 | %FileCheck --check-prefix=CHECK %s

// Check that the generic specializer does not hang a compiler by
// creating and infinite loop of generic specializations.

// This is a complete set of expected detected generic specialization loops:
// CHECK-DAG: generic specialization loop{{.*}}foo3
// CHECK-DAG: generic specialization loop{{.*}}foo4
// CHECK-DAG: generic specialization loop{{.*}}bar4
// CHECK-DAG: generic specialization loop{{.*}}Something{{.*}}map

// CHECK-LABEL: sil_stage canonical

// Check that a specialization information for a specialized function was produced.
// CHECK-LABEL: // Generic specialization information for function _T0044generic_specialization_loops_detection_with_C04foo4yx_q_tr0_lFSi_SdTg5
// CHECK-NEXT:  // Caller: _T0044generic_specialization_loops_detection_with_C011testFooBar4yyF
// CHECK-NEXT:  // Parent: _T0044generic_specialization_loops_detection_with_C04foo4yx_q_tr0_lF
// CHECK-NEXT:  // Substitutions: <Int, Double>

// Check that the compiler has produced a specialization information for a call-site that
// was inlined from a specialized generic function.
// CHECK-LABEL: // Generic specialization information for call-site _T0044generic_specialization_loops_detection_with_C04foo4yx_q_tr0_lF <Array<Int>, Array<Double>>
// CHECK-NEXT:  // Caller: _T0044generic_specialization_loops_detection_with_C04foo4yx_q_tr0_lFSi_SdTg5
// CHECK-NEXT:  // Parent: _T0044generic_specialization_loops_detection_with_C04bar4yx_q_tr0_lF
// CHECK-NEXT:  // Substitutions: <Int, Double>
// CHECK-NEXT:  //
// CHECK-NEXT:  // Caller: _T0044generic_specialization_loops_detection_with_C011testFooBar4yyF
// CHECK-NEXT:  // Parent: _T0044generic_specialization_loops_detection_with_C04foo4yx_q_tr0_lF
// CHECK-NEXT:  // Substitutions: <Int, Double>
// CHECK-NEXT:  //
// CHECK-NEXT: apply %{{[0-9]+}}<Array<Int>, Array<Double>>

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
      somethingArray = Something<Array<T>>(plainValue: [compoundValue])
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

print(Something<Int8>(compoundValue: 0).map { Double($0) })

