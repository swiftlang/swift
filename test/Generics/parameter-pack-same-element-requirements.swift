// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -enable-experimental-feature SameElementRequirements -disable-availability-checking 2>&1 | %FileCheck %s -dump-input=fail

// REQUIRES: swift_feature_SameElementRequirements

protocol P {
  associatedtype A

  func f(_ self: Self) -> Self
}

//////
///
/// Same-element requirements.
///
//////

// FIXME: Implement concrete same-type requirements.
//func sameElementConcrete<each T>(
//  _: repeat each T
//) where repeat each T == Int {}

// CHECK-LABEL: sameElementGeneric
// CHECK-NEXT: Generic signature: <each T, U where repeat each T == U>
func sameElementGeneric<each T, U>(
  _: repeat each T
) where repeat each T == U {}

// FIXME: Implement concrete same-element requirements.
//func dependentSameElementConcrete<each C: Collection>(
//  _: repeat each C
//) where repeat (each C).Element == Int {}

// CHECK-LABEL: dependentSameElementGeneric
// CHECK-NEXT: Generic signature: <each C, Element where repeat each C : Collection, repeat Element == (each C).[Sequence]Element>
func dependentSameElementGeneric<each C: Collection, Element>(
  _: repeat each C
) where repeat (each C).Element == Element {}

// FIXME: Either 'repeat each T: P' or 'U: P' should be redundant.
// CHECK-LABEL: sameElementRedundantConformance
// CHECK-NEXT: Generic signature: <each T, U where repeat each T : P, repeat each T == U, U : P>
func sameElementRedundantConformance<each T, U>(
  t: repeat each T,
  u: U
) where repeat each T: P,
        repeat each T == U {
  let _ = (repeat (each t).f(u))
}

// CHECK-LABEL: forEachEach
// CHECK-NEXT: Generic signature: <each C, U where repeat each C : Collection, repeat U == (each C).[Sequence]Element>
func forEachEach<each C, U>(
  c: repeat each C,
  function: (U) -> Void
) where repeat each C: Collection,
        repeat (each C).Element == U {
  repeat (each c).forEach(function)
}

