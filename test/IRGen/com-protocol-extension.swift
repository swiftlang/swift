// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -disable-llvm-optzns -I %t -emit-ir %s | %FileCheck %s

public typealias DWORD = UInt32
public typealias HRESULT = Int32

public let S_OK: HRESULT = HRESULT(bitPattern: 0)

public struct E: Error {
  public init() {
  }
}

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IInfo {
  func GetValue(_ pdwValue: UnsafeMutablePointer<DWORD>) -> HRESULT
}

extension IInfo {
  internal var value: DWORD {
    // CHECK-LABEL: define{{.*}} @"$s4main5IInfoPAAE5value
    // CHECK:         [[OBJECT:%.*]] = load ptr, ptr [[SELF:%.*]]
    // CHECK:         [[INTERFACE:%.*]] = getelementptr inbounds i8, ptr [[OBJECT]], i{{32|64}} [[ADJUSTMENT:%.*]]
    // CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
    // CHECK:         [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
    // CHECK:         [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
    // CHECK:         [[CALL_OBJECT:%.*]] = load ptr, ptr [[SELF]]
    // CHECK:         [[CALL_SELF:%.*]] = getelementptr inbounds i8, ptr [[CALL_OBJECT]], i{{32|64}} [[ADJUSTMENT]]
    // CHECK:         call i32 [[METHOD]](ptr [[CALL_SELF]], ptr
    get throws {
      var value: DWORD = .max
      guard GetValue(&value) == S_OK else {
        throw E()
      }
      return value
    }
  }
}
