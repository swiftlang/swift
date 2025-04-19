// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-silgen                           \
// RUN:     %t/Library.swift                                \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -I %t                                           \
// RUN: | %FileCheck %t/Library.swift --check-prefixes=CHECK

// RUN: %target-swift-frontend                              \
// RUN:     %t/Library.swift                                \
// RUN:     -emit-module                                    \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-emit-silgen                           \
// RUN:     %t/Downstream.swift                             \
// RUN:     -target %target-swift-5.9-abi-triple            \
// RUN:     -module-name main                               \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -lLibrary                                       \
// RUN:     -I %t                                           \
// RUN: | %FileCheck %t/Downstream.swift --check-prefixes=CHECK,CHECK-OLD

// RUN: %target-swift-emit-silgen                           \
// RUN:     %t/Downstream.swift                             \
// TODO: CoroutineAccessors: Change to %target-swift-x.y-abi-triple
// RUN:     -target %target-future-triple                   \
// RUN:     -module-name main                               \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -lLibrary                                       \
// RUN:     -I %t                                           \
// RUN: | %FileCheck %t/Downstream.swift --check-prefixes=CHECK,CHECK-NEW

// REQUIRES: swift_stable_abi
// REQUIRES: swift_feature_CoroutineAccessors

//--- Library.swift

extension Int {
  public mutating func increment() {
    self = self + 1
  }
}

// TODO: CoroutineAccessors: Change to X.Y throughout file.
@available(SwiftStdlib 9999, *)
public struct StructNew {
  var _i: Int
  public var i: Int {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }
}

@available(SwiftStdlib 9999, *)
public func getNew() -> StructNew {
  return StructNew(_i: 3)
}

@inlinable
@available(SwiftStdlib 9999, *)
@_silgen_name("readNewInlinableNew")
public func readNewInlinableNew(_ n: StructNew) -> Int {
// CHECK-LABEL: sil {{.*}}@readNewInlinableNew : {{.*}} {
                  // function_ref StructNew.i.read2
// CHECK:         function_ref @$s7Library9StructNewV1iSivy
// CHECK-LABEL: } // end sil function 'readNewInlinableNew'
  return n.i
}

@available(SwiftStdlib 9999, *)
@_silgen_name("readNewNoninlinableNew")
public func readNewNoninlinableNew(_ n: StructNew) -> Int {
// CHECK-LABEL: sil {{.*}}@readNewNoninlinableNew : {{.*}} {
                  // function_ref StructNew.i.read2
// CHECK:         function_ref @$s7Library9StructNewV1iSivy
// CHECK-LABEL: } // end sil function 'readNewNoninlinableNew'
  return n.i
}

@inlinable
@available(SwiftStdlib 9999, *)
@_silgen_name("modifyNewInlinableNew")
public func modifyNewInlinableNew(_ n: inout StructNew) {
// CHECK-LABEL: sil {{.*}}@modifyNewInlinableNew : {{.*}} {
                  // function_ref StructNew.i.modify2
// CHECK:         function_ref @$s7Library9StructNewV1iSivx
// CHECK-LABEL: } // end sil function 'modifyNewInlinableNew'
  n.i.increment()
}

@available(SwiftStdlib 9999, *)
@_silgen_name("modifyNewNoninlinableNew")
public func modifyNewNoninlinableNew(_ n: inout StructNew) {
// CHECK-LABEL: sil {{.*}}@modifyNewNoninlinableNew : {{.*}} {
                  // function_ref StructNew.i.modify2
// CHECK:         function_ref @$s7Library9StructNewV1iSivx
// CHECK-LABEL: } // end sil function 'modifyNewNoninlinableNew'
  n.i.increment()
}

public struct StructOld {
  var _i: Int
  public var i: Int {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }
}

public func getOld() -> StructOld {
  return StructOld(_i: 3)
}

@inlinable
@_silgen_name("readOldInlinableOld")
public func readOldInlinableOld(_ n: StructOld) -> Int {
// CHECK-LABEL: sil {{.*}}@readOldInlinableOld : {{.*}} {
// Could be inlined into code that's running before CoroutineAccessors is
// available--must use read.
                  // function_ref StructOld.i.read
// CHECK:         function_ref @$s7Library9StructOldV1iSivr
// CHECK-LABEL: } // end sil function 'readOldInlinableOld'
  return n.i
}

@_silgen_name("readOldNoninlinableOld")
public func readOldNoninlinableOld(_ n: StructOld) -> Int {
// CHECK-LABEL: sil {{.*}}@readOldNoninlinableOld : {{.*}} {
// Opaque symbol never inlined.  Even though it was available before
// CoroutineAccessors was, the implementation in this version of the module--can
// use read2.
                  // function_ref StructOld.i.read2
// CHECK:         function_ref @$s7Library9StructOldV1iSivy
// CHECK-LABEL: } // end sil function 'readOldNoninlinableOld'
  return n.i
}

@inlinable
@available(SwiftStdlib 9999, *)
@_silgen_name("readOldInlinableNew")
public func readOldInlinableNew(_ n: StructOld) -> Int {
// CHECK-LABEL: sil {{.*}}@readOldInlinableNew : {{.*}} {
// Could be inlined, but only into code that's running at or after
// CoroutineAccessors is available--can use read2.
                  // function_ref StructOld.i.read2
// CHECK:         function_ref @$s7Library9StructOldV1iSivy
// CHECK-LABEL: } // end sil function 'readOldInlinableNew'
  return n.i
}

@available(SwiftStdlib 9999, *)
@_silgen_name("readOldNoninlinableNew")
public func readOldNoninlinableNew(_ n: StructOld) -> Int {
// CHECK-LABEL: sil {{.*}}@readOldNoninlinableNew : {{.*}} {
// Neither inlinable nor available before CoroutineAccessors--can use read2.
                  // function_ref StructOld.i.read2
// CHECK:         function_ref @$s7Library9StructOldV1iSivy
// CHECK-LABEL: } // end sil function 'readOldNoninlinableNew'
  return n.i
}

@inlinable
@_silgen_name("modifyOldInlinableOld")
public func modifyOldInlinableOld(_ n: inout StructOld) {
// CHECK-LABEL: sil {{.*}}@modifyOldInlinableOld : {{.*}} {
// Could be inlined into code that's running before CoroutineAccessors is
// available--must use modify.
                  // function_ref StructOld.i.modify
// CHECK:         function_ref @$s7Library9StructOldV1iSivM
// CHECK-LABEL: } // end sil function 'modifyOldInlinableOld'
  n.i.increment()
}

@_silgen_name("modifyOldNoninlinableOld")
public func modifyOldNoninlinableOld(_ n: inout StructOld) {
// CHECK-LABEL: sil {{.*}}@modifyOldNoninlinableOld : {{.*}} {
// Opaque symbol never inlined.  Even though it was available before
// CoroutineAccessors was, the implementation in this version of the module--can
// use modify2.
                  // function_ref StructOld.i.modify2
// CHECK:         function_ref @$s7Library9StructOldV1iSivx
// CHECK-LABEL: } // end sil function 'modifyOldNoninlinableOld'
  n.i.increment()
}

@inlinable
@available(SwiftStdlib 9999, *)
@_silgen_name("modifyOldInlinableNew")
public func modifyOldInlinableNew(_ n: inout StructOld) {
// CHECK-LABEL: sil {{.*}}@modifyOldInlinableNew : {{.*}} {
// Could be inlined, but only into code that's running at or after
// CoroutineAccessors is available--can use modify2.
                  // function_ref StructOld.i.modify2
// CHECK:         function_ref @$s7Library9StructOldV1iSivx
// CHECK-LABEL: } // end sil function 'modifyOldInlinableNew'
  n.i.increment()
}

@available(SwiftStdlib 9999, *)
@_silgen_name("modifyOldNoninlinableNew")
public func modifyOldNoninlinableNew(_ n: inout StructOld) {
// CHECK-LABEL: sil {{.*}}@modifyOldNoninlinableNew : {{.*}} {
// Neither inlinable nor available before CoroutineAccessors--can use modify2.
                  // function_ref StructOld.i.modify2
// CHECK:         function_ref @$s7Library9StructOldV1iSivx
// CHECK-LABEL: } // end sil function 'modifyOldNoninlinableNew'
  n.i.increment()
}

public func takeInt(_ i: Int) {
}

open class BaseClassOld {
  public init(_ i : Int) {
    self._i = i
  }
  var _i: Int
  open var i: Int {
    read {
      yield _i
    }
    modify {
      yield &_i
    }
  }
}

//--- Downstream.swift

import Library

@_silgen_name("readNewOld")
func readNewOld() {
// CHECK-LABEL: sil {{.*}}@readNewOld : {{.*}} {
// Neither inlinable nor available before CoroutineAccessors--can use read2.
                  // function_ref StructNew.i.read2
// CHECK:         function_ref @$s7Library9StructNewV1iSivy
// CHECK-LABEL: } // end sil function 'readNewOld'
  if #available(SwiftStdlib 9999, *) {
    let n = getNew()

    let i = n.i
  }
}

@available(SwiftStdlib 9999, *)
@_silgen_name("readNewNew")
func readNewNew() {
// CHECK-LABEL: sil {{.*}}@readNewNew : {{.*}} {
// Neither inlinable nor available before CoroutineAccessors--can use read2.
                  // function_ref StructNew.i.read2
// CHECK:         function_ref @$s7Library9StructNewV1iSivy
// CHECK-LABEL: } // end sil function 'readNewNew'
  let n = getNew()

  let i = n.i
}

@available(SwiftStdlib 9999, *)
@_silgen_name("readOldNew")
func readOldNew() {
// CHECK-LABEL: sil {{.*}}@readOldNew : {{.*}} {
// Although this module could be back-deployed to before CoroutineAccessors were
// available, it can only run in environments where the feature is available--
// can use read2.
                  // function_ref StructOld.i.read2
// CHECK:         function_ref @$s7Library9StructOldV1iSivy
// CHECK-LABEL: } // end sil function 'readOldNew'
  let n = getOld()

  let i = n.i
}

@_silgen_name("readOldOld")
func readOldOld() {
// CHECK-LABEL: sil {{.*}}@readOldOld : {{.*}} {
// This module could be back-deployed to before CoroutineAccessors were
// available, and nothing ensures the function is available only after the
// feature is available--must use read.
                  // function_ref StructOld.i.read
// CHECK-OLD:     function_ref @$s7Library9StructOldV1iSivr
// This module cannot be back-deployed (the deployment target is
// available, and nothing ensures the function is available only after the
// feature is available--must use read.
                  // function_ref StructOld.i.read2
// CHECK-NEW:     function_ref @$s7Library9StructOldV1iSivy
// CHECK-LABEL: } // end sil function 'readOldOld'
  let n = getOld()

  let i = n.i
}

@_silgen_name("modifyNewOld")
func modifyNewOld() {
// CHECK-LABEL: sil {{.*}}@modifyNewOld : {{.*}} {
// Neither inlinable nor available before CoroutineAccessors--can use modify2.
                  // function_ref StructNew.i.modify2
// CHECK:         function_ref @$s7Library9StructNewV1iSivx
// CHECK-LABEL: } // end sil function 'modifyNewOld'
  if #available(SwiftStdlib 9999, *) {
    var n = getNew()

    n.i.increment()
  }
}

@available(SwiftStdlib 9999, *)
@_silgen_name("modifyNewNew")
func modifyNewNew() {
// CHECK-LABEL: sil {{.*}}@modifyNewNew : {{.*}} {
// Neither inlinable nor available before CoroutineAccessors--can use modify2.
                  // function_ref StructNew.i.modify2
// CHECK:         function_ref @$s7Library9StructNewV1iSivx
// CHECK-LABEL: } // end sil function 'modifyNewNew'
  var n = getNew()

  n.i.increment()
}

@available(SwiftStdlib 9999, *)
@_silgen_name("modifyOldNew")
func modifyOldNew() {
// CHECK-LABEL: sil {{.*}}@modifyOldNew : {{.*}} {
// Although this module could be back-deployed to before CoroutineAccessors were
// available, it can only run in environments where the feature is available--
// can use modify2.
                  // function_ref StructOld.i.modify2
// CHECK:         function_ref @$s7Library9StructOldV1iSivx
// CHECK-LABEL: } // end sil function 'modifyOldNew'
  var n = getOld()

  n.i.increment()
}

@_silgen_name("modifyOldOld")
func modifyOldOld() {
// CHECK-LABEL: sil {{.*}}@modifyOldOld : {{.*}} {
// This module could be back-deployed to before CoroutineAccessors were
// available, and nothing ensures the function is available only after the
// feature is available--must use modify.
                  // function_ref StructOld.i.modify
// CHECK-OLD:     function_ref @$s7Library9StructOldV1iSivM
// This module cannot be back-deployed (the deployment target is
// available, and nothing ensures the function is available only after the
// feature is available--must use modify.
                  // function_ref StructOld.i.modify2
// CHECK-NEW:     function_ref @$s7Library9StructOldV1iSivx
// CHECK-LABEL: } // end sil function 'modifyOldOld'
  var n = getOld()

  n.i.increment()
}

public class DerivedOldFromBaseClassOld : BaseClassOld {
  public init(_ i : Int, _ j : Int) {
    self._j = j
    super.init(i)
  }
  var _j: Int
  override public var i: Int {
    read {
      yield _j
    }
    modify {
      yield &_j
    }
  }
}

// CHECK-LABEL: sil_vtable [serialized] DerivedOldFromBaseClassOld {
// CHECK-NEXT:    #BaseClassOld.init!allocator
// CHECK-NEXT:    #BaseClassOld.i!read
// CHECK-NEXT:    #BaseClassOld.i!read2
// CHECK-NEXT:    #BaseClassOld.i!setter
// CHECK-NEXT:    #BaseClassOld.i!modify
// CHECK-NEXT:    #BaseClassOld.i!modify2
// CHECK:       }
