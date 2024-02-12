// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -O -emit-sil  -primary-file %s | %FileCheck %s
//
// This is a .swift test because the SIL parser does not support Self.

// Do not inline C.factory into main. Doing so would lose the ability
// to materialize local Self metadata.

// REQUIRES: swift_in_compiler

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

protocol Use {
  func use<T>(_ t: T)
}

var user: Use? = nil

class BaseZ {
  final func baseCapturesSelf() -> Self {
    let fn = { [weak self] in _ = self }
    callIt(fn: fn)
    return self
  }
}

// Do not inline C.capturesSelf() into main either. Doing so would lose the ability
// to materialize local Self metadata.
class Z : BaseZ {
  @inline(__always)
  final func capturesSelf() -> Self {
    let fn = { [weak self] in _ = self }
    callIt(fn: fn)
    user?.use(self)
    return self
  }

  // Inline captureSelf into callCaptureSelf,
  // because their respective Self types refer to the same type.
  final func callCapturesSelf() -> Self {
    return capturesSelf()
  }

  final func callBaseCapturesSelf() -> Self {
    return baseCapturesSelf()
  }
}

_ = Z().capturesSelf()

// CHECK-LABEL: sil {{.*}}@main : $@convention(c)
// CHECK: function_ref static inline_self.C.factory(Swift.Int) -> Self
// CHECK: [[F:%[0-9]+]] = function_ref @$s11inline_self1CC7factory{{[_0-9a-zA-Z]*}}FZ : $@convention(method) (Int, @thick C.Type) -> @owned C
// CHECK: apply [[F]](%{{.+}}, %{{.+}}) : $@convention(method) (Int, @thick C.Type) -> @owned C

// CHECK: [[Z:%.*]] = alloc_ref $Z
// CHECK: [[C:%.*]] = upcast [[Z]]
// CHECK: [[EI:%.*]] = end_init_let_ref [[C]]
// CHECK: [[DC:%.*]] = unchecked_ref_cast [[EI]]
// CHECK: function_ref inline_self.Z.capturesSelf() -> Self
// CHECK: [[F:%[0-9]+]] = function_ref @$s11inline_self1ZC12capturesSelfACXDyF : $@convention(method) (@guaranteed Z) -> @owned Z
// CHECK: apply [[F]]([[DC]]) : $@convention(method) (@guaranteed Z) -> @owned
// CHECK: return

// CHECK-LABEL: sil hidden @$s11inline_self1ZC16callCapturesSelfACXDyF : $@convention(method)
// CHECK-NOT: function_ref @$s11inline_self1ZC12capturesSelfACXDyF :
// CHECK: }

// CHECK-LABEL: sil hidden {{.*}}@$s11inline_self1ZC20callBaseCapturesSelfACXDyF
// CHECK-NOT: function_ref @$s11inline_self5BaseZC16baseCapturesSelfACXDyF :
// CHECK: }
