// Please keep this file in alphabetical order!

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround


// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -I %S/Inputs/custom-modules -import-underlying-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -parse-as-library %t/objc_implementation.swiftmodule -typecheck -I %S/Inputs/custom-modules -emit-objc-header-path %t/objc_implementation-Swift.h -import-underlying-module -disable-objc-attr-requires-foundation-module
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
  @objc func swiftMethod() -> Any? { nil }

  // NEGATIVE-NOT: )privateMethod{{ }}
  @objc private func privateMethod() -> Any? { nil }
}

@_cdecl("CImplFunc") @_objcImplementation func CImplFunc() {}
// NEGATIVE-NOT: CImplFunc(
