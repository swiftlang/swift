// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -O -emit-sil  -primary-file %s | FileCheck %s
//
// This is a .swift test because the SIL parser does not support Self.

// Do not inline C.factory into main. Doing so would lose the ability
// to materialize local Self metadata.
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
  class func factory(_ z: Int) -> Self {
    return gen()
  }
}

// Call the function so it can be inlined.
var x = C()
var x2 = C.factory(1)

@inline(never)
func callIt(fn: () -> ()) {
  fn()
}

// Do not inline C.capturesSelf() into main either.
class Z {
  func capturesSelf() -> Self {
    let fn = { [weak self] in _ = self }
    callIt(fn: fn)
    return self
  }
}

_ = Z().capturesSelf()

// CHECK-LABEL: sil @main : $@convention(c)
// CHECK: function_ref static inline_self.C.factory (Swift.Int) -> Self
// CHECK: [[F:%[0-9]+]] = function_ref @_TZFC11inline_self1C7factory{{.*}} : $@convention(method) (Int, @thick C.Type) -> @owned C
// CHECK: apply [[F]](%{{.+}}, %{{.+}}) : $@convention(method) (Int, @thick C.Type) -> @owned C

// CHECK: [[Z:%.*]] = alloc_ref $Z
// CHECK: function_ref inline_self.Z.capturesSelf () -> Self
// CHECK: [[F:%[0-9]+]] = function_ref @_TFC11inline_self1Z12capturesSelffT_DS0_ : $@convention(method) (@guaranteed Z) -> @owned Z
// CHECK: apply [[F]]([[Z]]) : $@convention(method) (@guaranteed Z) -> @owned Z
