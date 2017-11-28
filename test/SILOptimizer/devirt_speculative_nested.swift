// RUN: %target-swift-frontend %s -parse-as-library -O -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -parse-as-library -Osize -emit-sil
//
// Test speculative devirtualization.

public class Outer<T> {
  final class Inner : Base {
    @inline(never) override func update() {
    }
  }
}

public class Base {
  @inline(never) func update() { }
}

// FIXME: We don't speculate to the override Outer<T>.Inner.update() here,
// because we cannot express the cast -- the cast "returns" a new archetype
// T, much like an opened existential.
//
// But at least, we shouldn't crash.

// CHECK-LABEL: sil [thunk] [always_inline] @_T025devirt_speculative_nested3fooyAA4BaseC1x_tF : $@convention(thin) (@owned Base) -> ()
// CHECK: checked_cast_br [exact] %0 : $Base to $Base
// CHECK: function_ref @_T025devirt_speculative_nested4BaseC6updateyyF
// CHECK: class_method %0 : $Base, #Base.update!1
// CHECK: return

public func foo(x: Base) {
  x.update()
}
