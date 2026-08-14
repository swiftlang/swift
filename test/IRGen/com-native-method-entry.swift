// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -O -emit-ir %s | %FileCheck %s -check-prefix CHECK-OPT

@com(interface: "20000000-0000-0000-0000-000000000001")
public protocol IWidget {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32
}

extension IWidget {
  public func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32 {
    result?.pointee = 17
    return 0
  }
}

@com(interface: "20000000-0000-0000-0000-000000000002")
protocol IReferenceWidget: AnyObject {
  func reference(_ result: UnsafeMutablePointer<Int32>?) -> UInt32
}

// A COM interface need not spell an AnyObject refinement. Its implementation
// is nevertheless a class, so the native entry binds the recovered object
// directly.

@com
final class Widget: IWidget {
  @inline(never)
  func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32 {
    result?.pointee = 42
    return result == nil ? 1 : 0
  }
}

// CHECK-LABEL: define {{.*}}i32 {{.*}}6WidgetC{{.*}}TW.com.entry
// CHECK-SAME:  (ptr [[PTHIS:%.*]], ptr [[RESULT:%.*]])
// CHECK-NOT:     alloca ptr
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[PTHIS]]
// CHECK:         [[ADJUSTMENT_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i32 -1
// CHECK:         [[ADJUSTMENT:%.*]] = load i64, ptr [[ADJUSTMENT_SLOT]]
// CHECK:         [[OBJECT:%.*]] = getelementptr inbounds i8, ptr [[PTHIS]], i64 [[ADJUSTMENT]]
// CHECK-NOT:     store ptr
// CHECK:         [[CALL_RESULT:%.*]] = call swiftcc i32 {{.*}}WidgetC5value
// CHECK:         ret i32 [[CALL_RESULT]]

// CHECK-OPT-LABEL: define {{.*}}i32 {{.*}}6WidgetC{{.*}}TW.com.entry
// CHECK-OPT:         [[OPT_RESULT:%.*]] = tail call swiftcc i32
// CHECK-OPT-NEXT:    ret i32 [[OPT_RESULT]]

@com
public class DefaultWidget: IWidget {
}

// A class-bound archetype Self is recovered from the interface pointer. The
// native entry neither receives a separate metadata argument nor materializes
// address storage for the object before calling the extension witness.

// CHECK-LABEL: define {{.*}}i32 {{.*}}13DefaultWidgetC{{.*}}TW.com.entry
// CHECK-SAME:  (ptr [[DEFAULT_PTHIS:%.*]], ptr [[DEFAULT_RESULT:%.*]])
// CHECK-NOT:     alloca ptr
// CHECK:         [[DEFAULT_VTABLE:%.*]] = load ptr, ptr [[DEFAULT_PTHIS]]
// CHECK:         [[DEFAULT_ADJUSTMENT_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[DEFAULT_VTABLE]], i32 -1
// CHECK:         [[DEFAULT_ADJUSTMENT:%.*]] = load i64, ptr [[DEFAULT_ADJUSTMENT_SLOT]]
// CHECK:         [[DEFAULT_OBJECT:%.*]] = getelementptr inbounds i8, ptr [[DEFAULT_PTHIS]], i64 [[DEFAULT_ADJUSTMENT]]
// CHECK-NOT:     store ptr [[DEFAULT_OBJECT]]
// CHECK:         [[DEFAULT_CALL_RESULT:%.*]] = call swiftcc i32 {{.*}}IWidgetPAAE5value
// CHECK:         ret i32 [[DEFAULT_CALL_RESULT]]

// A class-bound interface passes the witness Self as a loadable reference. The
// recovered object is therefore bound directly without address storage.

@com
final class ReferenceWidget: IReferenceWidget {
  @inline(never)
  func reference(_ result: UnsafeMutablePointer<Int32>?) -> UInt32 {
    result?.pointee = 43
    return result == nil ? 2 : 1
  }
}

// CHECK-LABEL: define {{.*}}i32 {{.*}}15ReferenceWidgetC{{.*}}TW.com.entry
// CHECK-SAME:  (ptr [[REFERENCE_PTHIS:%.*]], ptr [[REFERENCE_RESULT:%.*]])
// CHECK-NOT:     alloca ptr
// CHECK:         [[REFERENCE_VTABLE:%.*]] = load ptr, ptr [[REFERENCE_PTHIS]]
// CHECK:         [[REFERENCE_ADJUSTMENT_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[REFERENCE_VTABLE]], i32 -1
// CHECK:         [[REFERENCE_ADJUSTMENT:%.*]] = load i64, ptr [[REFERENCE_ADJUSTMENT_SLOT]]
// CHECK:         [[REFERENCE_OBJECT:%.*]] = getelementptr inbounds i8, ptr [[REFERENCE_PTHIS]], i64 [[REFERENCE_ADJUSTMENT]]
// CHECK-NOT:     store ptr [[REFERENCE_OBJECT]]
// CHECK:         [[CALL_RESULT:%.*]] = call swiftcc i32 {{.*}}ReferenceWidgetC9referencey
// CHECK:         ret i32 [[CALL_RESULT]]

// CHECK-OPT-LABEL: define {{.*}}i32 {{.*}}15ReferenceWidgetC{{.*}}TW.com.entry
// CHECK-OPT:         [[OPT_REFERENCE_RESULT:%.*]] = tail call swiftcc i32
// CHECK-OPT-NEXT:    ret i32 [[OPT_REFERENCE_RESULT]]
