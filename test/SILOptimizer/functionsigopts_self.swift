// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -O -sil-inline-threshold 0 -emit-sil -primary-file %s | %FileCheck %s
//
// This is a .swift test because the SIL parser does not support Self.

class C {
  required init() {}
}

class SubC : C {}

var g: AnyObject = SubC()

func gen<R>() -> R {
  return g as! R
}

extension C {
  class func factory(_ z: Int) -> Self {
    return gen()
  }
}
// The integer argument is truly dead, but the C.Type metadata argument may not be removed.
// function signature specialization <Arg[0] = Dead> of static functionsigopts_self.C.factory (functionsigopts_self.C.Type)(Swift.Int) -> Self
// CHECK-LABEL: sil hidden @_T020functionsigopts_self1CC7factory{{[_0-9a-zA-Z]*}}FZTf4dn_n : $@convention(method) (@thick C.Type) -> @owned C
// CHECK: bb0(%0 : $@thick C.Type):
// CHECK: function_ref functionsigopts_self.gen <A> () -> A
// CHECK: apply %{{[0-9]+}}<@dynamic_self C>

// Call the function so the specialization is not dead.
var x = C()
var x2 = C.factory(1)
