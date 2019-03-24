// RUN: %empty-directory(%t)

// Directly printing the type-checked AST
// RUN: %target-swift-ide-test -print-ast-typechecked -print-interface -source-filename %s | %FileCheck %s -check-prefix=CHECK-TYPECHECKED

// Directly printing the non-type-checked AST
// RUN: %target-swift-ide-test -print-ast-not-typechecked -print-interface -source-filename %s | %FileCheck %s -check-prefix=CHECK-NOTTYPECHECKED

// Creating a module, then printing from the module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s
// RUN: %target-swift-ide-test -print-module -module-to-print=print_property_delegates -print-interface -I %t -source-filename %s | %FileCheck %s -check-prefix=CHECK-MODULE

@propertyDelegate
public struct Wrapper<T> {
  public var value: T
}

@propertyDelegate
public struct WrapperWithInitialValue<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }

  public init(alternate value: T) {
    self.value = value
  }
}

// CHECK-TYPECHECKED: public struct HasDelegates {
public struct HasDelegates {
  // CHECK-TYPECHECKED: public var x: Int by public Wrapper
  // CHECK-NOTTYPECHECKED: public var x: Int by public Wrapper
  // CHECK-MODULE: public var x: Int by public print_property_delegates.Wrapper
  public var x: Int by public Wrapper

  // CHECK-TYPECHECKED: public private(set) var y: Int by public WrapperWithInitialValue
  // CHECK-NOTTYPECHECKED: public private(set) var y: Int by public WrapperWithInitialValue
  // CHECK-MODULE: public private(set) var y: Int by public print_property_delegates.WrapperWithInitialValue
  public private(set) var y: Int by public WrapperWithInitialValue = 17

  // CHECK-TYPECHECKED: public var z: Bool { get set }
  // CHECK-NOTTYPECHECKED: public var z
  // CHECK-MODULE: public var z: Bool
  public var z by WrapperWithInitialValue(alternate: false)
}
