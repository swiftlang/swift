// RUN: %empty-directory(%t)

// Test with -enable-module-selectors-in-module-interface
// RUN: %empty-directory(%t/enabled)
// RUN: %target-swift-emit-module-interface(%t/enabled/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -enable-module-selectors-in-module-interface
// RUN: %FileCheck --input-file %t/enabled/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-ENABLED
// RUN: %target-swift-typecheck-module-from-interface(%t/enabled/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -disable-module-selectors-in-module-interface
// RUN: %empty-directory(%t/disabled)
// RUN: %target-swift-emit-module-interface(%t/disabled/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -disable-module-selectors-in-module-interface
// RUN: %FileCheck --input-file %t/disabled/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-DISABLED
// RUN: %target-swift-typecheck-module-from-interface(%t/disabled/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test default behavior
// RUN: %empty-directory(%t/default)
// RUN: %target-swift-emit-module-interface(%t/default/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase
// RUN: %FileCheck --input-file %t/default/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-DISABLED
// RUN: %target-swift-typecheck-module-from-interface(%t/default/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -enable-module-selectors-in-module-interface and blocklist
// RUN: %empty-directory(%t/blocked)
// RUN: %target-swift-emit-module-interface(%t/blocked/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -enable-module-selectors-in-module-interface -blocklist-file %S/Inputs/module_selector/blocklist.yml
// RUN: %FileCheck --input-file %t/blocked/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-DISABLED
// RUN: %target-swift-typecheck-module-from-interface(%t/blocked/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// CHECK: import enums_using_attributes
import enums_using_attributes

// CHECK-LABEL: public struct Struct<T> :
// CHECK-ENABLED-SAME: Swift::Hashable where T : Swift::Hashable {
// CHECK-DISABLED-SAME: Swift.Hashable where T : Swift.Hashable {
public struct Struct<T: Hashable>: Hashable {
  // CHECK: public let integer:
  // CHECK-ENABLED-SAME: Swift::Int
  // CHECK-DISABLED-SAME: Swift.Int
  public let integer: Int = 42

  // CHECK: public let enumeration:
  // CHECK-ENABLED-SAME: enums_using_attributes::CFEnumWithAttr
  // CHECK-DISABLED-SAME: enums_using_attributes.CFEnumWithAttr
  public let enumeration: CFEnumWithAttr = CFEnumWithAttr.first

  // CHECK: public let t: T?
  public let t: T? = nil

  // CHECK: public static func ==
  // CHECK-ENABLED-SAME: (a: TestCase::Struct<T>, b: TestCase::Struct<T>) -> Swift::Bool
  // CHECK-DISABLED-SAME: (a: TestCase.Struct<T>, b: TestCase.Struct<T>) -> Swift.Bool

  // CHECK: public func hash
  // CHECK-ENABLED-SAME: (into hasher: inout Swift::Hasher)
  // CHECK-DISABLED-SAME: (into hasher: inout Swift.Hasher)

  // CHECK: public var hashValue:
  // CHECK-ENABLED-SAME: Swift::Int {
  // CHECK-DISABLED-SAME: Swift.Int {

  // CHECK: }
}

// CHECK-ENABLED: extension TestCase::Struct {
// CHECK-DISABLED: extension TestCase.Struct {
extension Struct {
  // CHECK-LABEL: public enum Nested {
  public enum Nested {
    // CHECK: case integer
    // CHECK-ENABLED-SAME: (Swift::Int)
    // CHECK-DISABLED-SAME: (Swift.Int)
    case integer(Int)

    // CHECK: case enumeration
    // CHECK-ENABLED-SAME: (enums_using_attributes::CFEnumWithAttr)
    // CHECK-DISABLED-SAME: (enums_using_attributes.CFEnumWithAttr)
    case enumeration(CFEnumWithAttr)

    // CHECK: case t(T)
    case t(T)

    // CHECK: case `struct`
    // CHECK-ENABLED-SAME: (TestCase::Struct<T>)
    // CHECK-DISABLED-SAME: (TestCase.Struct<T>)
    case `struct`(Struct)

    // CHECK: }
  }

  // CHECK: }
}

// CHECK-ENABLED: extension Swift::Int {
// CHECK-DISABLED: extension Swift.Int {
extension Swift::Int {
  // CHECK-LABEL: public enum RetroactiveNested {
  public enum RetroactiveNested {}
}

// CHECK-ENABLED: extension Swift::Int.TestCase::RetroactiveNested {
// CHECK-DISABLED: extension Swift.Int.RetroactiveNested {
extension Int.RetroactiveNested {
  public func anchor() {}
}

// CHECK-LABEL: public func fn<T>
// CHECK-ENABLED-SAME: (_: TestCase::Struct<T>, _: TestCase::Struct<T>.TestCase::Nested)
// CHECK-DISABLED-SAME: (_: TestCase.Struct<T>, _:  TestCase.Struct<T>.Nested)
public func fn<T: Hashable>(_: Struct<T>, _: Struct<T>.Nested) {}

// CHECK-LABEL: public func fn2<T>
// CHECK-ENABLED-SAME: (_: T) where T : Swift::Identifiable, T.ID == TestCase::Struct<Swift::Int>
// CHECK-DISABLED-SAME: (_: T) where T : Swift.Identifiable, T.ID == TestCase.Struct<Swift.Int>
@available(SwiftStdlib 5.1, *)
public func fn2<T: Identifiable>(_: T) where T.ID == Struct<Int> {}

// CHECK-LABEL: public protocol Proto {
@available(SwiftStdlib 5.1, *)
public protocol Proto {
  // CHECK: associatedtype AssocType :
  // CHECK-ENABLED-SAME: Swift::Identifiable
  // CHECK-DISABLED-SAME: Swift.Identifiable
  associatedtype AssocType: Identifiable

  // CHECK: typealias TypeAlias =
  // CHECK-ENABLED-SAME: TestCase::Struct<Swift::Int>
  // CHECK-DISABLED-SAME: TestCase.Struct<Swift.Int>
  typealias TypeAlias = Struct<Int>

  // CHECK: func requirement() -> Self.AssocType.ID
  func requirement() -> AssocType.ID

  // CHECK: func requirement2() ->
  // CHECK-ENABLED: Self.TypeAlias.TestCase::Nested
  // CHECK-DISABLED: Self.TypeAlias.Nested
  func requirement2() -> TypeAlias.Nested

  // CHECK: }
}
