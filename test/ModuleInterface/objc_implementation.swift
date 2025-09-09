// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-emit-module-interface(%t/objc_implementation.swiftinterface) %s -import-underlying-module %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/objc_implementation -target %target-stable-abi-triple
// RUN: %FileCheck --input-file %t/objc_implementation.swiftinterface %s
// RUN: %FileCheck --input-file %t/objc_implementation.swiftinterface --check-prefix NEGATIVE %s
// RUN: %target-swift-typecheck-module-from-interface(%t/objc_implementation.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/objc_implementation -target %target-stable-abi-triple

// REQUIRES: objc_interop

import Foundation

// We should never see @_objcImplementation in the header
// NEGATIVE-NOT: @_objcImplementation
// NEGATIVE-NOT: @implementation

// @objc should be omitted on extensions
// NEGATIVE-NOT: @objc{{.*}} extension

// Stored properties in objcImpl extensions shouldn't have @_hasStorage
// NEGATIVE-NOT: @_hasStorage

//
// @_objcImplementation class
//

// CHECK-LABEL: extension objc_implementation.ImplClass {
@_objcImplementation extension ImplClass {
  // CHECK-NOT: init()
  @objc public override init() {
    implProperty = 42
    implProperty2 = NSObject()
    super.init()
  }

  // CHECK-NOT: var implProperty:
  @objc public var implProperty: Int32 {
    didSet { print(implProperty) }
  }

  // CHECK-NOT: var letProperty1:
  @objc public let letProperty1: Int32

  // CHECK-DAG: @nonobjc public var letProperty2: Swift.Int32 { get }
  @nonobjc public let letProperty2: Int32

  // CHECK-DAG: final public var implProperty2: ObjectiveC.NSObject? { get set }
  public final var implProperty2: NSObject?

  // CHECK-DAG: final public var implProperty3: ObjectiveC.NSObject? {
  public final var implProperty3: NSObject? {
    didSet { }
  }

  // CHECK-NOT: func mainMethod
  @objc public func mainMethod(_: Int32) { print(implProperty) }

  // CHECK-NOT: deinit
}
// CHECK: }

//
// @_objcImplementation category
//

// Empty category should be omitted, so there's only one `extension ImplClass`.
// CHECK-NOT: extension objc_implementation.ImplClass {
@_objcImplementation(Category1) extension ImplClass {
  // NEGATIVE-NOT: func category1Method
  @objc public func category1Method(_: Int32) {
    print("category1Method")
  }
}

//
// Second @_objcImplementation class, inherited initializer
//

// NEGATIVE-NOT: extension objc_implementation.NoInitImplClass
@_objcImplementation extension NoInitImplClass {
  // NEGATIVE-NOT: var s1:
  @objc public let s1 = "s1v"
  // NEGATIVE-NOT: var s2:
  @objc public var s2 = "s2v"
  // NEGATIVE-NOT: var s3:
  @objc(s3) public let s3 = "s3v"
  // NEGATIVE-NOT: var s4:
  @objc(s4) public var s4 = "s4v"
}

//
// @objc subclass of @_objcImplementation class
//

// CHECK-LABEL: @objc @_inheritsConvenienceInitializers open class SwiftSubclass : objc_implementation.ImplClass {
open class SwiftSubclass: ImplClass {
  // CHECK-DAG: @objc override dynamic open func mainMethod
  override open func mainMethod(_: Int32) {
    print("subclass mainMethod")
  }

  // CHECK-DAG: @objc override dynamic public init()
  // CHECK-DAG: @objc deinit
}
// CHECK: }

//
// Epilogue
//
