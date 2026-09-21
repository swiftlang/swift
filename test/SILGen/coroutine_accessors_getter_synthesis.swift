// Verifies getter synthesis for coroutine-accessor storage: a concrete copyable
// 'yielding borrow'/'_read' property synthesizes an owned getter (matching the
// getter that a copyable '_read' has shipped since before the feature), the
// getter delegates to the coroutine (running its full body), and NO getter is
// synthesized where one would be invalid (a noncopyable value) or where it must
// stay absent (a protocol requirement, which stays symmetric with '@_borrowed').

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=GETTER

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=GETTER

// The getter-delegation and absence checks each get their own FileCheck
// invocation so a lone CHECK-LABEL / CHECK-NOT scans the whole input.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=DELEGATES

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-GETTER

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefixes=NO-GETTER,NO-GETTER-RESILIENT

// REQUIRES: swift_feature_CoroutineAccessors

// A concrete copyable 'yielding borrow' property synthesizes an owned getter, in
// addition to its coroutine, in both resilience modes.
public struct StructYB {
  var _s = 0
  public var i: Int {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}
// GETTER-DAG: sil{{.*}} @$s1m8StructYBV1iSivg :
// GETTER-DAG: sil{{.*}} @$s1m8StructYBV1iSivy : $@yield_once_2
// GETTER-DAG: sil{{.*}} @$s1m8StructYBV1iSivx : $@yield_once_2

open class ClassYB {
  var _s = 0
  open var i: Int {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}
// GETTER-DAG: sil{{.*}} @$s1m7ClassYBC1iSivg :
// GETTER-DAG: sil{{.*}} @$s1m7ClassYBC1iSivy : $@yield_once_2
// GETTER-DAG: sil{{.*}} @$s1m7ClassYBC1iSivx : $@yield_once_2

var _g = 0
public var globalYB: Int {
  yielding borrow { yield _g }
  yielding mutate { yield &_g }
}
// GETTER-DAG: sil{{.*}} @$s1m8globalYBSivg :
// GETTER-DAG: sil{{.*}} @$s1m8globalYBSivy : $@yield_once_2
// GETTER-DAG: sil{{.*}} @$s1m8globalYBSivx : $@yield_once_2

// Spelling invariance: the '_read'/'_modify' spelling produces the same getter
// (the feature remaps it to 'yielding borrow'/'yielding mutate').
public struct StructRM {
  var _s = 0
  public var i: Int {
    _read { yield _s }
    _modify { yield &_s }
  }
}
// GETTER-DAG: sil{{.*}} @$s1m8StructRMV1iSivg :
// GETTER-DAG: sil{{.*}} @$s1m8StructRMV1iSivy : $@yield_once_2
// GETTER-DAG: sil{{.*}} @$s1m8StructRMV1iSivx : $@yield_once_2

// The synthesized getter delegates to the coroutine, running its full body: the
// front half up to the yield (begin_apply) and the back half (end_apply).
// DELEGATES-LABEL: sil{{.*}} @$s1m8StructYBV1iSivg :
// DELEGATES:         [[CORO:%[^ ]+]] = function_ref @$s1m8StructYBV1iSivy
// DELEGATES:         ({{.*}}) = begin_apply [[CORO]](
// DELEGATES:         end_apply
// DELEGATES-LABEL: } // end sil function '$s1m8StructYBV1iSivg'

// A 'yielding borrow' of a noncopyable value cannot produce an owned copy, so NO
// getter is synthesized -- only the coroutine(s).
public struct NCVal: ~Copyable {}
public struct NCHolder: ~Copyable {
  var _n = NCVal()
  public var n: NCVal {
    yielding borrow { yield _n }
  }
}
// NO-GETTER-NOT: @$s1m8NCHolderV1nAA5NCValVvg
// NO-GETTER-DAG: sil{{.*}} @$s1m8NCHolderV1nAA5NCValVvy : $@yield_once_2

// A protocol requirement stays a borrowing opaque read (no getter requirement),
// so the '@_borrowed' and 'yielding borrow' spellings keep the same witness
// layout.  Neither gets a getter requirement.
public protocol ProtoYB {
  var i: Int { yielding borrow set }
}
public protocol ProtoB {
  @_borrowed var i: Int { get set }
}
// NO-GETTER-NOT: #ProtoYB.i!getter
// NO-GETTER-NOT: #ProtoB.i!getter
// NO-GETTER-RESILIENT-DAG: sil{{.*}} @$s1m7ProtoYBP1iSivy : $@yield_once_2
// NO-GETTER-RESILIENT-DAG: sil{{.*}} @$s1m6ProtoBP1iSivy : $@yield_once_2
