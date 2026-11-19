// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -O -wmo -parse-as-library -module-name Lib -emit-module-path %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -emit-sil -O -wmo -module-name main -I %t -sil-verify-all %t/Main.swift | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -O -wmo -module-name main -I %t %t/Main.swift | %FileCheck %s --check-prefix=IR --implicit-check-not='$s3Lib5InnerV'

// Check that the optimizer does not split the destroy of a non-copyable value into destroys
// of its fields when a field's type has a deinit and metadata the client cannot access.

//--- Lib.swift

struct Inner: ~Copyable {
  let p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit { p.deallocate() }
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
// CHECK:         ref_element_addr %0, #Holder.m
// CHECK-NOT:     struct_element_addr
// CHECK:         release_value
// CHECK:       } // end sil function '$s4main6HolderCfD'

// CHECK-LABEL: sil [noinline] @$s4main14destroyManageryy3Lib0C0VnF :
// CHECK-NOT:     struct_extract
// CHECK:         release_value %0
// CHECK:       } // end sil function '$s4main14destroyManageryy3Lib0C0VnF'

// IR-LABEL: define {{.*}} @"$s4main14destroyManageryy3Lib0C0VnF"(
// IR:         $s3Lib7ManagerVN
// IR:       }
@inline(never)
public func destroyManager(_ m: consuming Manager) {
}

_ = Holder()
