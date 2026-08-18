// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s

public typealias HRESULT = UInt32
private var S_OK: HRESULT {
  0
}

@com(interface: "20000000-0000-0000-0000-000000000001")
public protocol IBase {
  func method(_ result: UnsafeMutablePointer<CInt>?) -> HRESULT
}

@com(interface: "20000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func refined(_ result: UnsafeMutablePointer<CInt>?) -> HRESULT
}

@com(interface: "20000000-0000-0000-0000-000000000003")
public protocol IUnrelated {
  func function(_ result: UnsafeMutablePointer<CInt>?) -> HRESULT
}

@com
internal final class CCOMObject: IDerived, IUnrelated {
  internal init() {
  }

  public func method(_ result: UnsafeMutablePointer<CInt>?) -> HRESULT {
    result?.pointee = 1
    return S_OK
  }

  public func refined(_ result: UnsafeMutablePointer<CInt>?) -> HRESULT {
    result?.pointee = 2
    return S_OK
  }

  public func function(_ result: UnsafeMutablePointer<CInt>?) -> HRESULT {
    result?.pointee = 3
    return S_OK
  }
}

// ISwiftObject stores its two default property witnesses after the common COM
// entries. The IDerived projection then stores IBase.method followed by
// IDerived.refined, while IUnrelated starts its own method sequence at slot
// three.

// CHECK-DAG: @"$s{{.*}}10CCOMObjectCMn.com.vtable.$s3COM12ISwiftObjectMp" = private constant { ptr, i64, ptr, ptr, ptr, ptr, ptr } { ptr @"$s{{.*}}10CCOMObjectCMn.com.interface_map", i64  8, ptr @QueryInterface, ptr @AddRef, ptr @Release, ptr @"$s{{.*}}10CCOMObjectC{{.*}}ISwiftObject{{.*}}6object{{.*}}TW.com.entry", ptr @"$s{{.*}}10CCOMObjectC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TW.com.entry"
// CHECK-DAG: @"$s{{.*}}10CCOMObjectCMn.com.vtable.$s{{.*}}8IDerivedMp"    = private constant { ptr, i64, ptr, ptr, ptr, ptr, ptr } { ptr @"$s{{.*}}10CCOMObjectCMn.com.interface_map", i64 16, ptr @QueryInterface, ptr @AddRef, ptr @Release, ptr @"$s{{.*}}10CCOMObjectCAA5IBaseA2aDP6methodys6UInt32VSpys5Int32VGSgFTW.com.entry", ptr @"$s{{.*}}10CCOMObjectCAA8IDerivedA2aDP7refinedys6UInt32VSpys5Int32VGSgFTW.com.entry"
// CHECK-DAG: @"$s{{.*}}10CCOMObjectCMn.com.vtable.$s{{.*}}10IUnrelatedMp" = private constant { ptr, i64, ptr, ptr, ptr, ptr }      { ptr @"$s{{.*}}10CCOMObjectCMn.com.interface_map", i64 24, ptr @QueryInterface, ptr @AddRef, ptr @Release, ptr @"$s{{.*}}10CCOMObjectCAA10IUnrelatedA2aDP8functionys6UInt32VSpys5Int32VGSgFTW.com.entry"
