// RUN: %empty-directory(%t)
// RUN: %swift -c -disable-large-loadable-types-reg2mem -primary-file %s -Xllvm -sil-print-types -Xllvm -sil-print-after=loadable-address -sil-verify-all -o %t/big_types_generic.o 2>&1 | %FileCheck %s

struct Big<T> {
  var a0 : T
  var a1 : T
  var a2 : T
  var a3 : T
  var a4 : T
  var a5 : T
  var a6 : T
  var a7 : T
  var a8 : T
  init(_ t: T) {
    a0 = t
    a1 = t
    a2 = t
    a3 = t
    a4 = t
    a5 = t
    a6 = t
    a7 = t
    a8 = t
  }
}

// CHECK-LABEL: sil hidden @$s17big_types_generic10nonGenericAA3BigVys5Int32VG_AGt_AGyctSgyF : $@convention(thin) () -> @out Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>
// CHECK: bb0(%0 : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>):
// CHECK: [[ENUMCONSTRUCT:%.*]] = enum $Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>, #Optional.none!enumelt
// CHECK: store [[ENUMCONSTRUCT]] to %0 : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s17big_types_generic10nonGenericAA3BigVys5Int32VG_AGt_AGyctSgyF'
func nonGeneric() -> ((Big<Int32>, Big<Int32>), () -> Big<Int32>)? {
  return nil
}

// CHECK-LABEL: sil hidden @$s17big_types_generic11nonGeneric2AA3BigVys5Int32VG_AGt_AGyctyF : $@convention(thin) () -> (Big<Int32>, Big<Int32>, @owned @callee_guaranteed () -> @out Big<Int32>)
// CHECK: bb0:
// CHECK: [[TUPLECONSTRUCT:%.*]] = tuple (%{{.*}} : $Big<Int32>, %{{.*}} : $Big<Int32>, %{{.*}} : $@callee_guaranteed () -> @out Big<Int32>)
// CHECK: return [[TUPLECONSTRUCT]]
// CHECK-LABEL: } // end sil function '$s17big_types_generic11nonGeneric2AA3BigVys5Int32VG_AGt_AGyctyF'
func nonGeneric2() -> ((Big<Int32>, Big<Int32>), () -> Big<Int32>) {
  return ((Big(1), Big(2)), { return Big(1) })
}

func generic<T>(_ t: T) -> ((Big<T>, Big<T>), () -> Big<T>)? {
  return nil
}

func generic2<T>(_ t: T) -> ((Big<T>, Big<T>), () -> Big<T>) {
  return ((Big(t), Big(t)), { return Big(t) })
}

// CHECK-LABEL: sil hidden @$s17big_types_generic8useStuffyyF : $@convention(thin) () -> ()
// CHECK: switch_enum_addr %{{.*}} : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>, case #Optional.some!enumelt
// CHECK: switch_enum_addr %{{.*}} : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>, case #Optional.some!enumelt
// CHECK: switch_enum %{{.*}} : $Optional<((Big<Int>, Big<Int>), @callee_guaranteed () -> @out Big<Int>)>, case #Optional.some!enumelt
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s17big_types_generic8useStuffyyF'
func useStuff() {
  print(nonGeneric()!.0)
  print(nonGeneric()!.1)
  print(nonGeneric2().0)
  print(nonGeneric2().1)
  print(generic(1)!.0.0)
  print(generic(1)!.1)
  print(generic2(1).0)
  print(generic2(1).1)
}


public struct BigThing<T> {
  var x: (Int64, Int64, Int64, Int64) = (0, 0, 0, 0)
  var y: (Int64, Int64, Int64, Int64) = (0, 0, 0, 0)
  var z: (Int64, Int64, Int64, Int64) = (0, 0, 0, 0)
}

public protocol P {}

public protocol Assoc {
 associatedtype A
 func foo() -> A
}

extension Int : P {}

public struct DefineSome : Assoc {
   public func foo() -> some P {
     return 5
   }
}

public func abiIndirect() -> BigThing<DefineSome.A> {
  return BigThing<DefineSome.A>()
}
