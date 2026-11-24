// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -enable-library-evolution -parse-as-library -module-name Resilient -emit-module-path %t/Resilient.swiftmodule %t/Resilient.swift
// RUN: %target-swift-frontend -emit-module -O -wmo -parse-as-library -module-name Lib -I %t -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -emit-sil -O -wmo -module-name main -I %t -sil-verify-all %t/Main.swift | %FileCheck %s

// Check that the DeinitDevirtualizer does not decompose destroys of non-copyable types into
// destroys of fields which are not ABI accessible in the client module.
// rdar://188400107

//--- Resilient.swift

public struct R {
  var s: String
  public init() { s = "" }
}

//--- Lib.swift

import Resilient

// Internal and containing a resilient field: not ABI accessible in the client.
struct Inner: ~Copyable {
  var r: R
  init() { r = R() }
}

public struct Manager: ~Copyable {
  var inner: Inner
  public init() { inner = Inner() }
}

//--- Main.swift

import Lib

final class Holder {
  var m: Manager
  init() { m = Manager() }
}

// CHECK-LABEL: sil hidden @$s4main6HolderCfD :
// CHECK:         [[A:%.*]] = ref_element_addr %0, #Holder.m
// CHECK-NOT:     struct_element_addr
// CHECK:         destroy_addr [[A]]
// CHECK:       } // end sil function '$s4main6HolderCfD'

// CHECK-LABEL: sil [noinline] @$s4main14destroyManageryy3Lib0C0VnF :
// CHECK-NOT:     struct_element_addr
// CHECK:         destroy_addr %0
// CHECK:       } // end sil function '$s4main14destroyManageryy3Lib0C0VnF'
@inline(never)
public func destroyManager(_ m: consuming Manager) {
}

_ = Holder()
