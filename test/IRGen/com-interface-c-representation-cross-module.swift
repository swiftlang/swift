// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D LIBRARY -module-name Library -parse-as-library -emit-module -emit-module-path %t/Library.swiftmodule %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D CLIENT -module-name Client -parse-as-library -emit-ir %s -o - | %FileCheck %s

#if LIBRARY

@com(interface: "46000000-0000-0000-0000-000000000001")
public protocol IItem {
}

@com(interface: "46000000-0000-0000-0000-000000000002")
public protocol IProvider {
  func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt
}

@com(implementation: "46000000-0000-0000-0000-000000000003")
open class CObject: IProvider {
  public init() {
  }

  open func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt {
    0
  }
}

#elseif CLIENT

import Library

@com
public final class DerivedObject: CObject {
  public override func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt {
    1
  }
}

public func makeDerivedObject() -> DerivedObject {
  DerivedObject()
}

// The client constructs its private vtable with the inherited native entry.
// That entry dynamically dispatches to the client's override.

// CHECK-DAG: @"$s{{.*}}13DerivedObjectCMn.com.vtable.$s7Library9IProviderMp" = private constant {{.*}} ptr [[GET_ITEM:@"\$s.*7CObjectC.*GetItem.*TWV"]]
// CHECK: {{define|declare}} {{.*}}i32 [[GET_ITEM]](ptr{{.*}}, ptr{{.*}})

#endif
