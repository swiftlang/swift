// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-emit-module-interface(%t/assoc_type_inference_tvos.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/assoc_type_inference_tvos
// RUN: %FileCheck --input-file %t/assoc_type_inference_tvos.swiftinterface %s
// RUN: %target-swift-typecheck-module-from-interface(%t/assoc_type_inference_tvos.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/assoc_type_inference_tvos

// REQUIRES: objc_interop

import CAssoc

extension TheColor {
  // This checks for the absence of an @available(tvOS) on an associated type
  // that is inferred to have tvOS availability based on its enclosing type
  // having iOS availability that predates the introduction of tvOS.

  // CHECK: public init?(rawValue: Swift.String)
  // CHECK-NOT: @available(tvOS)
  // CHECK: public typealias RawValue = Swift.String
  public enum StaticNamedColor: String {
    case black
  }
}
