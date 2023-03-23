// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -I %S/Inputs/objc_implementation -F %clang-importer-sdk-path/frameworks %s -import-underlying-module -swift-version 5 -enable-library-evolution -emit-module-interface-path %t.swiftinterface
// RUN: %FileCheck --input-file %t.swiftinterface %s
// RUN: %FileCheck --input-file %t.swiftinterface --check-prefix NEGATIVE %s
// REQUIRES: objc_interop

// We should never see @_objcImplementation in the header
// NEGATIVE-NOT: @_objcImplementation

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

  // CHECK-DAG: final public var implProperty2: ObjectiveC.NSObject? { get set }
  public final var implProperty2: NSObject?

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
