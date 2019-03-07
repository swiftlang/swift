// RUN: %empty-directory(%t)
// RUN: %swift -c -primary-file %s -enable-large-loadable-types -Xllvm -sil-print-after=loadable-address -sil-verify-all -o %t/big_types_constructors_source.o 2>&1 | %FileCheck %s

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

// CHECK-LABEL: sil hidden @$s29big_types_constructors_source10nonGenericAA3BigVys5Int32VG_AGt_AGyctSgyF : $@convention(thin) () -> @out Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>
// CHECK: bb0(%0 : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>):
// CHECK: [[ENUMCONSTRUCT:%.*]] = enum $Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>, #Optional.none!enumelt
// CHECK: store [[ENUMCONSTRUCT]] to %0 : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s29big_types_constructors_source10nonGenericAA3BigVys5Int32VG_AGt_AGyctSgyF'
func nonGeneric() -> ((Big<Int32>, Big<Int32>), () -> Big<Int32>)? {
  return nil
}

// CHECK-LABEL: sil hidden @$s29big_types_constructors_source11nonGeneric2AA3BigVys5Int32VG_AGt_AGyctyF : $@convention(thin) () -> @out (Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>)
// CHECK: bb0(%0 : $*(Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>)):
// CHECK: [[ALLOCTUPLE:%.*]] = alloc_stack $(Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>)
// CHECK: [[TELEM0:%.*]] = tuple_element_addr [[ALLOCTUPLE]] : $*(Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>), 0
// CHECK: store %{{.*}} to [[TELEM0]] : $*Big<Int32>
// CHECK: [[TELEM1:%.*]] =  tuple_element_addr %1 : $*(Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>), 1
// CHECK: store %{{.*}} to [[TELEM1]] : $*Big<Int32>
// CHECK: [[TELEM2:%.*]] = tuple_element_addr %1 : $*(Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>), 2
// CHECK: store %{{.*}} to [[TELEM2]] : $*@callee_guaranteed () -> @out Big<Int32>
// CHECK: copy_addr [take] [[ALLOCTUPLE]] to [initialization] %0 : $*(Big<Int32>, Big<Int32>, @callee_guaranteed () -> @out Big<Int32>)
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s29big_types_constructors_source11nonGeneric2AA3BigVys5Int32VG_AGt_AGyctyF'
func nonGeneric2() -> ((Big<Int32>, Big<Int32>), () -> Big<Int32>) {
  return ((Big(1), Big(2)), { return Big(1) })
}

func generic<T>(_ t: T) -> ((Big<T>, Big<T>), () -> Big<T>)? {
  return nil
}

func generic2<T>(_ t: T) -> ((Big<T>, Big<T>), () -> Big<T>) {
  return ((Big(t), Big(t)), { return Big(t) })
}

// CHECK-LABEL: sil hidden @$s29big_types_constructors_source8useStuffyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK: switch_enum_addr %{{.*}} : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>, case #Optional.some!enumelt.1: bb7, case #Optional.none!enumelt: bb1
// CHECK: bb7:
// CHECK: switch_enum_addr %{{.*}} : $*Optional<((Big<Int32>, Big<Int32>), @callee_guaranteed () -> @out Big<Int32>)>, case #Optional.some!enumelt.1: bb14, case #Optional.none!enumelt: bb8
// CHECK: bb14:
// CHECK: bb17(%222 : $Optional<((Big<Int>, Big<Int>), @callee_guaranteed () -> @out Big<Int>)>):
// CHECK: switch_enum_addr %{{.*}} : $*Optional<((Big<Int>, Big<Int>), @callee_guaranteed () -> @out Big<Int>)>, case #Optional.some!enumelt.1: bb24, case #Optional.none!enumelt: bb18
// CHECK: bb24:
// CHECK: return %{{.*}} : $()
// CHECK-LABEL: } // end sil function '$s29big_types_constructors_source8useStuffyyF'
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
