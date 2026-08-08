// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -parse-as-library -module-name M -emit-ir -enable-experimental-com-interop -I %t %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget { }

// CHECK: @"$s{{.*}}16kExistentialSizeSivp" = {{.*}}constant %TSi <{ i64 8 }>
public let kExistentialSize = MemoryLayout<any IWidget>.size

// CHECK: @"$s{{.*}}24kOptionalExistentialSizeSivp" = {{.*}}constant %TSi <{ i64 8 }>
public let kOptionalExistentialSize = MemoryLayout<(any IWidget)?>.size

// The protocol descriptor marks the interface as the COM special protocol so
// dynamically-created existential metadata selects the same representation.
// CHECK-LABEL: @"$s{{.*}}7IWidgetMp" = {{.*}}constant
// --       0x0009_0043: special protocol 02, non-class, unique, protocol context
// CHECK-SAME: i32 589891,

// Both the existential and Optional existential are one pointer.
// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}4copyyAA7IWidget_pAaC_pF"
// CHECK-SAME: (ptr [[INTERFACE:%.*]])
// CHECK:         icmp ne ptr [[INTERFACE]], null
// CHECK:         load ptr, ptr [[INTERFACE]]
// CHECK:         getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 1
// CHECK:         load ptr, ptr {{%.*}}
// CHECK:         call i32 {{%.*}}(ptr [[INTERFACE]])
// CHECK-NOT:     call swift_retain
// CHECK-NOT:     call swift_unknownObjectRetain
// CHECK:         ret ptr [[INTERFACE]]
public func copy(_ value: borrowing any IWidget) -> any IWidget {
  copy value
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}7consumeyyAA7IWidget_pnF"(ptr
// CHECK:         [[INTERFACE:%.*]] = load ptr, ptr
// CHECK:         icmp ne ptr [[INTERFACE]], null
// CHECK:         load ptr, ptr [[INTERFACE]]
// CHECK:         getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 2
// CHECK:         load ptr, ptr {{%.*}}
// CHECK:         call i32 {{%.*}}(ptr [[INTERFACE]])
// CHECK-NOT:     call swift_release
// CHECK-NOT:     call swift_unknownObjectRelease
// CHECK:         ret void
public func consume(_ value: consuming any IWidget) { }

public struct Holder {
  public var value: any IWidget

  // CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}6HolderVyAcA7IWidget_pcfC"(ptr
  // CHECK:         ret ptr %0
  public init(_ value: consuming any IWidget) {
    self.value = value
  }
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}4copy8optionalAA7IWidget_pSgA{{.*}}F"
// CHECK-SAME: (ptr [[OPTIONAL:%.*]])
// CHECK: icmp ne ptr [[OPTIONAL]], null
// CHECK: getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 1
// CHECK: call i32 {{%.*}}(ptr [[OPTIONAL]])
// CHECK: ret ptr [[OPTIONAL]]
public func copy(optional value: borrowing (any IWidget)?) -> (any IWidget)? {
  copy value
}
