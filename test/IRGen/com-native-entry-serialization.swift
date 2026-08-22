// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D LIBRARY -module-name Library -parse-as-library -emit-module -emit-module-path %t/Library.swiftmodule %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D CLIENT -module-name Client -parse-as-library -emit-ir %s -o - | %FileCheck %s

#if LIBRARY

@com(interface: "27000000-0000-0000-0000-000000000001")
public protocol IWidget {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
open class BaseWidget: IWidget {
  public init() {
  }

  open func value(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 42
    return 0
  }
}

#elseif CLIENT

import Library

@com
public final class DerivedWidget: BaseWidget {
}

// Demand the class metadata, which references the private COM prefix and
// interface vtables emitted by Client.
public func makeDerivedWidget() -> DerivedWidget {
  DerivedWidget()
}

// The client vtables recover their native entries from the inherited witness
// tables serialized by Library. This covers both the user interface and the
// default protocol-extension witnesses for the Swift identity interface.

// CHECK-DAG: @"$s{{.*}}13DerivedWidgetCMn.com.vtable.$s7Library7IWidgetMp" = private constant {{.*}} ptr [[VALUE:@"\$s.*10BaseWidgetC.*IWidget.*5value.*TWV"]]
// CHECK-DAG: @"$s{{.*}}13DerivedWidgetCMn.com.vtable.$s3COM12ISwiftObjectMp" = private constant {{.*}} ptr [[OBJECT:@"\$s.*10BaseWidgetC.*ISwiftObject.*6object.*TWV"]], ptr [[METADATA:@"\$s.*10BaseWidgetC.*ISwiftObject.*8metadata.*TWV"]]
// CHECK-DAG: declare {{.*}}i32 [[VALUE]](ptr, ptr)
// CHECK-DAG: declare {{.*}}ptr [[OBJECT]](ptr)
// CHECK-DAG: declare {{.*}}ptr [[METADATA]](ptr)

#endif
