// Please keep this file in alphabetical order!

// REQUIRES: objc_interop
// REQUIRES: swift_feature_CImplementation

// Temporarily disable on arm64e (rdar://127675057)
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module -target %target-stable-abi-triple
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift -target %target-stable-abi-triple
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift -target %target-stable-abi-triple
// FIXME: END -enable-source-import hackaround


// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -enable-experimental-feature CImplementation -I %S/Inputs/custom-modules -import-underlying-module -o %t %s -disable-objc-attr-requires-foundation-module -target %target-stable-abi-triple
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/objc_implementation.swiftmodule -typecheck -verify -emit-objc-header-path %t/objc_implementation-Swift.h -enable-experimental-feature CImplementation -I %S/Inputs/custom-modules -import-underlying-module -disable-objc-attr-requires-foundation-module -target %target-stable-abi-triple
// RUN: %FileCheck %s --input-file %t/objc_implementation-Swift.h
// RUN: %FileCheck --check-prefix=NEGATIVE %s --input-file %t/objc_implementation-Swift.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ %t/objc_implementation-Swift.h
// RUN: %check-in-clang -I %S/Inputs/custom-modules/ -fno-modules -Qunused-arguments %t/objc_implementation-Swift.h

import Foundation

extension ObjCClass {
  // CHECK: - (id _Nullable)pureSwiftMethod SWIFT_WARN_UNUSED_RESULT;
  @objc public func pureSwiftMethod() -> Any? { nil }
}

@_objcImplementation extension ObjCClass {
  // NEGATIVE-NOT: )init{{ }}
  // Implicit `override init()` to override superclass

  // NEGATIVE-NOT: )swiftMethod{{ }}
  @objc public func swiftMethod() -> Any? { nil }

  // NEGATIVE-NOT: )privateMethod{{ }}
  @objc private func privateMethod() -> Any? { nil }
}

// Has no contents that need to be printed
// NEGATIVE-NOT: ObjCClass2
@_objcImplementation extension ObjCClass2 {
  // NEGATIVE-NOT: )init{{ }}
  public override init() { }
}

@_cdecl("CImplFunc") @_objcImplementation func CImplFunc() {}
// NEGATIVE-NOT: CImplFunc(
