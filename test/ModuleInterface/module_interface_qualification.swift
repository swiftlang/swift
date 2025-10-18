// RUN: %empty-directory(%t)

// Test with -module-interface-qualification none
// RUN: %empty-directory(%t/none)
// RUN: %target-swift-emit-module-interface(%t/none/module_interface_qualification.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple -module-interface-qualification none
// RUN: %FileCheck --input-file %t/none/module_interface_qualification.swiftinterface %s --check-prefixes UNALIASED,UNQUALIFIED
// RUN: %target-swift-typecheck-module-from-interface(%t/none/module_interface_qualification.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple

// Test with -module-interface-qualification alias
// RUN: %empty-directory(%t/alias)
// RUN: %target-swift-emit-module-interface(%t/alias/module_interface_qualification.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple -module-interface-qualification alias
// RUN: %FileCheck --input-file %t/alias/module_interface_qualification.swiftinterface %s --check-prefixes ALIASED
// RUN: %target-swift-typecheck-module-from-interface(%t/alias/module_interface_qualification.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple

// Test with -module-interface-qualification selector
// RUN: %empty-directory(%t/selector)
// RUN: %target-swift-emit-module-interface(%t/selector/module_interface_qualification.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple -module-interface-qualification selector
// RUN: %FileCheck --input-file %t/selector/module_interface_qualification.swiftinterface %s --check-prefixes UNALIASED,QUALIFIED
// RUN: %target-swift-typecheck-module-from-interface(%t/selector/module_interface_qualification.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple

// Test with -module-interface-qualification conditional
// RUN: %empty-directory(%t/conditional)
// RUN: %target-swift-emit-module-interface(%t/conditional/module_interface_qualification.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple -module-interface-qualification conditional
// RUN: %FileCheck --input-file %t/conditional/module_interface_qualification.swiftinterface %s --check-prefixes UNALIASED,QUALIFIED,UNQUALIFIED,CONDITIONS
// RUN: %target-swift-typecheck-module-from-interface(%t/conditional/module_interface_qualification.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_interface_qualification -target %target-stable-abi-triple

import enums_using_attributes

public struct Struct<T: Hashable>: Hashable {
  public let integer: Int = 42
  public let enumeration: CFEnumWithAttr = CFEnumWithAttr.first
  public let t: T? = nil
}

extension Struct {
  public enum Nested {
    case integer(Int)
    case enumeration(CFEnumWithAttr)
    case t(T)
    case `struct`(Struct)
  }
}

public func fn<T: Hashable>(_: Struct<T>, _: Struct<T>.Nested) {}

@available(SwiftStdlib 5.1, *)
public func fn2<T: Identifiable>(_: T) where T.ID == Struct<Int> {}

// UNALIASED: import enums_using_attributes
// ALIASED: import Module___enums_using_attributes

// CONDITIONS: #if swift(>=6.2) && $ModuleSelector

// QUALIFIED: public struct Struct<T> : Swift::Hashable where T : Swift::Hashable {
// QUALIFIED: public let integer: Swift::Int
// QUALIFIED: public let enumeration: enums_using_attributes::CFEnumWithAttr
// QUALIFIED: public let t: T?
// QUALIFIED: public static func == (a: module_interface_qualification::Struct<T>, b: module_interface_qualification::Struct<T>) -> Swift::Bool
// QUALIFIED: public func hash(into hasher: inout Swift::Hasher)
// QUALIFIED: public var hashValue: Swift::Int {
// QUALIFIED: }

// QUALIFIED: extension module_interface_qualification::Struct {
// QUALIFIED: public enum Nested {
// QUALIFIED: case integer(Swift::Int)
// QUALIFIED: case enumeration(enums_using_attributes::CFEnumWithAttr)
// QUALIFIED: case t(T)
// QUALIFIED: case `struct`(module_interface_qualification::Struct<T>)
// QUALIFIED: }

// QUALIFIED: public func fn<T>(_: module_interface_qualification::Struct<T>, _: module_interface_qualification::Struct<T>.module_interface_qualification::Nested)
// QUALIFIED: public func fn2<T>(_: T) where T : Swift::Identifiable, T.ID == module_interface_qualification::Struct<Swift::Int>

// CONDITIONS: #else

// UNQUALIFIED: public struct Struct<T> : Swift.Hashable where T : Swift.Hashable {
// UNQUALIFIED: public let integer: Swift.Int
// UNQUALIFIED: public let enumeration: enums_using_attributes.CFEnumWithAttr
// UNQUALIFIED: public let t: T?
// UNQUALIFIED: public static func == (a: module_interface_qualification.Struct<T>, b: module_interface_qualification.Struct<T>) -> Swift.Bool
// UNQUALIFIED: public func hash(into hasher: inout Swift.Hasher)
// UNQUALIFIED: public var hashValue: Swift.Int {
// UNQUALIFIED: }

// UNQUALIFIED: extension module_interface_qualification.Struct {
// UNQUALIFIED: public enum Nested {
// UNQUALIFIED: case integer(Swift.Int)
// UNQUALIFIED: case enumeration(enums_using_attributes.CFEnumWithAttr)
// UNQUALIFIED: case t(T)
// UNQUALIFIED: case `struct`(module_interface_qualification.Struct<T>)
// UNQUALIFIED: }

// UNQUALIFIED: public func fn<T>(_: module_interface_qualification.Struct<T>, _:  module_interface_qualification.Struct<T>.Nested)
// UNQUALIFIED: public func fn2<T>(_: T) where T : Swift.Identifiable, T.ID == module_interface_qualification.Struct<Swift.Int>

// CONDITIONS: #endif

// ALIASED: public struct Struct<T> : Swift.Hashable where T : Swift.Hashable {
// ALIASED: public let integer: Swift.Int
// ALIASED: public let enumeration: Module___enums_using_attributes.CFEnumWithAttr
// ALIASED: public let t: T?
// ALIASED: public static func == (a: Module___module_interface_qualification.Struct<T>, b: Module___module_interface_qualification.Struct<T>) -> Swift.Bool
// ALIASED: public func hash(into hasher: inout Swift.Hasher)
// ALIASED: public var hashValue: Swift.Int {
// ALIASED: }

// ALIASED: extension Module___module_interface_qualification.Struct {
// ALIASED: public enum Nested {
// ALIASED: case integer(Swift.Int)
// ALIASED: case enumeration(Module___enums_using_attributes.CFEnumWithAttr)
// ALIASED: case t(T)
// ALIASED: case `struct`(Module___module_interface_qualification.Struct<T>)
// ALIASED: }

// ALIASED: public func fn<T>(_: Module___module_interface_qualification.Struct<T>, _:  Module___module_interface_qualification.Struct<T>.Nested)
// ALIASED: public func fn2<T>(_: T) where T : Swift.Identifiable, T.ID == Module___module_interface_qualification.Struct<Swift.Int>
