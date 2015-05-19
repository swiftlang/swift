// RUN: %target-swift-frontend -O -emit-sil -disable-func-sig-opts -primary-file %s | FileCheck %s
//
// This is a .swift test because the SIL parser does not support Self.

class C {
  required init() {}
}

class SubC : C {}

var g: AnyObject = SubC()

@inline(never)
func gen<R>() -> R {
  return g as! R
}

extension C {
  @inline(__always)
  class func factory(z: Int) -> Self {
    return gen()
  }
}

// Do not inline C.factory into main. Doing so would lose the ability
// to materialize local Self metadata.
//
// CHECK-LABEL: sil @main : $@convention(c)
// CHECK: function_ref static inline_self.C.factory (inline_self.C.Type)(Swift.Int) -> Self
// CHECK: [[F:%[0-9]+]] = function_ref @_TZFC11inline_self1C7factoryfMS0_FSiDS0_ : $@convention(thin) (Int, @thick C.Type) -> @owned C
// CHECK: apply [[F]](%{{.+}}, %{{.+}}) : $@convention(thin) (Int, @thick C.Type) -> @owned C

// Call the function so it can be inlined.
var x = C()
var x2 = C.factory(1)
