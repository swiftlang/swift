// RUN: %target-swift-frontend -O -sil-inline-threshold 0 -emit-sil -primary-file %s | FileCheck %s
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
  class func factory(z: Int) -> Self {
    return gen()
  }
}
// The integer argument is truly dead, but the C.Type metadata argument may not be removed.
// function signature specialization <Arg[0] = Dead> of static functionsigopts_self.C.factory (functionsigopts_self.C.Type)(Swift.Int) -> Self
// CHECK-LABEL: sil hidden @_TTSf4d_n___TZFC20functionsigopts_self1C7factoryfMS0_FSiDS0_ : $@convention(thin) (@thick C.Type) -> @owned C
// CHECK-NEXT: bb0(%0 : $@thick C.Type):
// CHECK: function_ref functionsigopts_self.gen <A> () -> A
// CHECK: apply %{{[0-9]+}}<Self>

// Call the function so the specialization is not dead.
var x = C()
var x2 = C.factory(1)
