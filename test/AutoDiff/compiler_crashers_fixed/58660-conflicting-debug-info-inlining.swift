// RUN: %target-build-swift %s
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -O -g %s | %FileCheck %s

// REQUIRES: swift_in_compiler
// UNSUPPORTED: OS=linux-gnu

// Issue #58660: Specifically-shaped differentiable functions yield "conflicting debug info for argument" assertion failure
// Ensure that proper location is preserved after sil-mem2reg location-less stores (created during inlining)

import _Differentiation

// May be a `struct` or `class`.
class MyState: Differentiable {
  // All of these must be stored instance properties. There must be at least 7
  // differentiable properties of any type.
  var property1: Float = 0
  var property2: Float = 0
  var property3: Float = 0
  var property4: Float = 0
  var property5: Float = 0
  var property6: Float = 0
  var property7: Float = 0
}

struct MyModel: Differentiable {
  // May be `var` or `let`, but must not be `@noDerivative`. Must be a stored
  // instance property.
  let property1 = MyState()

  // Must be an instance property, either stored or computed.
  var property2: Float {
    // `get` must exist, and may add `mutating` attribute.
    get { 0 }
    // Cannot add `nonmutating` attribute to `set`.
    set { }
  }

  // Must be an instance member. May be a function or computed property, but not
  // a stored property.
  var member3: Float {
    // May not add `mutating` attribute.
    get { 0 }
  }

  @differentiable(reverse)
  mutating func member4() {
// CHECK-LABEL: // pullback of MyModel.member4()
// CHECK-NOT: debug_value %{{.*}} : $MyModel.TangentVector, var, name %{{.*}}, argno 1, scope
// CHECK: bb0(%{{.*}} : $_AD__$s4main7MyModelV7member4yyF_bb3__Pred__src_0_wrt_0):
// CHECK: debug_value %{{.*}} : $MyModel.TangentVector, var, (name "derivative of 'self' in scope at {{.*}} (scope #1)"{{.*}}), scope
    // Must be a differentiable type.
    var localVar: Float = 0

    // Must be assigned from the value of `localVar`, not the value of anything else.
    property2 = localVar

    // `false` may instead be any expression that returns a `Bool`.
    // TODO: cannot use literal `false` because it crashes
    if 1 == 0 {
      localVar = member3
    }
  }
}
