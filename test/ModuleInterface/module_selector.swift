// RUN: %empty-directory(%t)

// Test with -enable-module-selectors-in-module-interface
// RUN: %empty-directory(%t/enabled)
// RUN: %target-swift-emit-module-interface(%t/enabled/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -enable-module-selectors-in-module-interface 2>%t/enabled/stderr.txt
// RUN: %FileCheck --input-file %t/enabled/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-ENABLED,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/enabled/stderr.txt %s --allow-empty --check-prefix DIAG-PRESERVE-NOT-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/enabled/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -disable-module-selectors-in-module-interface
// RUN: %empty-directory(%t/disabled)
// RUN: %target-swift-emit-module-interface(%t/disabled/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -disable-module-selectors-in-module-interface 2>%t/disabled/stderr.txt
// RUN: %FileCheck --input-file %t/disabled/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-DISABLED,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/disabled/stderr.txt %s --allow-empty --check-prefix DIAG-PRESERVE-NOT-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/disabled/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test default behavior
// RUN: %empty-directory(%t/default)
// RUN: %target-swift-emit-module-interface(%t/default/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase 2>%t/default/stderr.txt
// RUN: %FileCheck --input-file %t/default/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-DISABLED,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/default/stderr.txt %s --allow-empty --check-prefix DIAG-PRESERVE-NOT-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/default/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -enable-module-selectors-in-module-interface and blocklist
// RUN: %empty-directory(%t/blocked)
// RUN: %target-swift-emit-module-interface(%t/blocked/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -enable-module-selectors-in-module-interface -blocklist-file %S/Inputs/module_selector/blocklist.yml 2>%t/blocked/stderr.txt
// RUN: %FileCheck --input-file %t/blocked/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-DISABLED,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/blocked/stderr.txt %s --allow-empty --check-prefix DIAG-PRESERVE-NOT-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/blocked/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -module-interface-preserve-types-as-written and -disable-module-selectors-in-module-interface
// RUN: %empty-directory(%t/disabled-preserve)
// RUN: %target-swift-emit-module-interface(%t/disabled-preserve/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -module-interface-preserve-types-as-written -disable-module-selectors-in-module-interface 2>%t/disabled-preserve/stderr.txt
// RUN: %FileCheck --input-file %t/disabled-preserve/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-PRESERVE,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/disabled-preserve/stderr.txt %s --allow-empty --check-prefix DIAG-PRESERVE-NOT-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/disabled-preserve/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -module-interface-preserve-types-as-written and -enable-module-selectors-in-module-interface
// RUN: %empty-directory(%t/enabled-preserve)
// RUN: %target-swift-emit-module-interface(%t/enabled-preserve/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -module-interface-preserve-types-as-written -enable-module-selectors-in-module-interface 2>%t/enabled-preserve/stderr.txt
// RUN: %FileCheck --input-file %t/enabled-preserve/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-ENABLED,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/enabled-preserve/stderr.txt %s --check-prefix DIAG-PRESERVE-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/enabled-preserve/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -module-interface-preserve-types-as-written and -alias-module-names-in-module-interface
// RUN: %empty-directory(%t/disabled-alias)
// RUN: %target-swift-emit-module-interface(%t/disabled-alias/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -alias-module-names-in-module-interface -disable-module-selectors-in-module-interface 2>%t/disabled-alias/stderr.txt
// RUN: %FileCheck --input-file %t/disabled-alias/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-ALIAS
// RUN: %FileCheck --input-file %t/disabled-alias/stderr.txt %s --allow-empty --check-prefix DIAG-ALIAS-NOT-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/disabled-alias/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// Test with -module-interface-preserve-types-as-written and -alias-module-names-in-module-interface
// RUN: %empty-directory(%t/enabled-alias)
// RUN: %target-swift-emit-module-interface(%t/enabled-alias/TestCase.swiftinterface) %s %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase -alias-module-names-in-module-interface -enable-module-selectors-in-module-interface 2>%t/enabled-alias/stderr.txt
// RUN: %FileCheck --input-file %t/enabled-alias/TestCase.swiftinterface %s --check-prefixes CHECK,CHECK-ENABLED,CHECK-NOT-ALIAS
// RUN: %FileCheck --input-file %t/enabled-alias/stderr.txt %s --check-prefix DIAG-ALIAS-OVERRIDDEN
// RUN: %target-swift-typecheck-module-from-interface(%t/enabled-alias/TestCase.swiftinterface) %clang-importer-sdk -F %clang-importer-sdk-path/frameworks -I %S/Inputs/module_selector -target %target-stable-abi-triple -module-name TestCase

// CHECK-NOT-ALIAS: import enums_using_attributes
// CHECK-ALIAS: import Module___enums_using_attributes
import enums_using_attributes

// CHECK-LABEL: public struct Struct<T> :
// CHECK-ENABLED-SAME: Swift::Hashable where T : Swift::Hashable {
// CHECK-DISABLED-SAME: Swift.Hashable where T : Swift.Hashable {
// CHECK-PRESERVE-SAME: Hashable where T : Swift.Hashable {
// Preserve quirk: Hashable qualified in constraint?
// CHECK-ALIAS-SAME: Swift.Hashable where T : Swift.Hashable {
public struct Struct<T: Hashable>: Hashable {
  // CHECK: public let integer:
  // CHECK-ENABLED-SAME: Swift::Int
  // CHECK-DISABLED-SAME: Swift.Int
  // CHECK-PRESERVE-SAME: Int
  // CHECK-ALIAS-SAME: Swift.Int
  public let integer: Int = 42

  // CHECK: public let enumeration:
  // CHECK-ENABLED-SAME: enums_using_attributes::CFEnumWithAttr
  // CHECK-DISABLED-SAME: enums_using_attributes.CFEnumWithAttr
  // CHECK-PRESERVE-SAME: CFEnumWithAttr
  // CHECK-ALIAS-SAME: Module___enums_using_attributes.CFEnumWithAttr
  public let enumeration: CFEnumWithAttr = CFEnumWithAttr.first

  // CHECK: public let t: T?
  public let t: T? = nil

  // CHECK: public static func ==
  // CHECK-ENABLED-SAME: (a: TestCase::Struct<T>, b: TestCase::Struct<T>) -> Swift::Bool
  // CHECK-DISABLED-SAME: (a: TestCase.Struct<T>, b: TestCase.Struct<T>) -> Swift.Bool
  // CHECK-PRESERVE-SAME: (a: TestCase.Struct<T>, b: TestCase.Struct<T>) -> Swift.Bool
  // CHECK-ALIAS-SAME: (a: Module___TestCase.Struct<T>, b: Module___TestCase.Struct<T>) -> Swift.Bool

  // CHECK: public func hash
  // CHECK-ENABLED-SAME: (into hasher: inout Swift::Hasher)
  // CHECK-DISABLED-SAME: (into hasher: inout Swift.Hasher)
  // CHECK-PRESERVE-SAME: (into hasher: inout Swift.Hasher)
  // CHECK-ALIAS-SAME: (into hasher: inout Swift.Hasher)

  // CHECK: public var hashValue:
  // CHECK-ENABLED-SAME: Swift::Int {
  // CHECK-DISABLED-SAME: Swift.Int {
  // CHECK-PRESERVE-SAME: Swift.Int {
  // CHECK-ALIAS-SAME: Swift.Int {

  // CHECK: }
}

// CHECK-ENABLED: extension TestCase::Struct {
// CHECK-DISABLED: extension TestCase.Struct {
// CHECK-PRESERVE: extension Struct {
// CHECK-ALIAS: extension Module___TestCase.Struct {
extension Struct {
  // CHECK-LABEL: public enum Nested {
  public enum Nested {
    // CHECK: case integer
    // CHECK-ENABLED-SAME: (Swift::Int)
    // CHECK-DISABLED-SAME: (Swift.Int)
    // CHECK-PRESERVE-SAME: (Int)
    // CHECK-ALIAS-SAME: (Swift.Int)
    case integer(Int)

    // CHECK: case enumeration
    // CHECK-ENABLED-SAME: (enums_using_attributes::CFEnumWithAttr)
    // CHECK-DISABLED-SAME: (enums_using_attributes.CFEnumWithAttr)
    // CHECK-PRESERVE-SAME: (CFEnumWithAttr)
    // CHECK-ALIAS-SAME: (Module___enums_using_attributes.CFEnumWithAttr)
    case enumeration(CFEnumWithAttr)

    // CHECK: case t(T)
    case t(T)

    // CHECK: case `struct`
    // CHECK-ENABLED-SAME: (TestCase::Struct<T>)
    // CHECK-DISABLED-SAME: (TestCase.Struct<T>)
    // CHECK-PRESERVE-SAME: (Struct)
    // CHECK-ALIAS-SAME: (Module___TestCase.Struct<T>)
    case `struct`(Struct)

    // CHECK: }
  }

  // CHECK: }
}

// CHECK-ENABLED: extension Swift::Int {
// CHECK-DISABLED: extension Swift.Int {
// CHECK-PRESERVE: extension Int {
// Preserve quirk: Int has no module selector?
// CHECK-ALIAS: extension Swift.Int {
extension Swift::Int {
  // CHECK-LABEL: public enum RetroactiveNested {
  public enum RetroactiveNested {}
}

// CHECK-ENABLED: extension Swift::Int.TestCase::RetroactiveNested {
// CHECK-DISABLED: extension Swift.Int.RetroactiveNested {
// CHECK-PRESERVE: extension Int.RetroactiveNested {
// CHECK-ALIAS: extension Swift.Int.RetroactiveNested {
extension Int.RetroactiveNested {
  public func anchor() {}
}

// CHECK-LABEL: public func fn<T>
// CHECK-ENABLED-SAME: (_: TestCase::Struct<T>, _: TestCase::Struct<T>.TestCase::Nested)
// CHECK-DISABLED-SAME: (_: TestCase.Struct<T>, _:  TestCase.Struct<T>.Nested)
// CHECK-PRESERVE-SAME: (_: Struct<T>, _:  Struct<T>.Nested)
// CHECK-ALIAS-SAME: (_: Module___TestCase.Struct<T>, _:  Module___TestCase.Struct<T>.Nested)
public func fn<T: Hashable>(_: Struct<T>, _: Struct<T>.Nested) {}

// CHECK-LABEL: public func fn2<T>
// CHECK-ENABLED-SAME: (_: T) where T : Swift::Identifiable, T.ID == TestCase::Struct<Swift::Int>
// CHECK-DISABLED-SAME: (_: T) where T : Swift.Identifiable, T.ID == TestCase.Struct<Swift.Int>
// CHECK-PRESERVE-SAME: (_: T) where T : Swift.Identifiable, T.ID == TestCase.Struct<Swift.Int>
// Preserve quirk: Still qualified in constraints?
// CHECK-ALIAS-SAME: (_: T) where T : Swift.Identifiable, T.ID == Module___TestCase.Struct<Swift.Int>
@available(SwiftStdlib 5.1, *)
public func fn2<T: Identifiable>(_: T) where T.ID == Struct<Int> {}

// CHECK-LABEL: public protocol Proto {
@available(SwiftStdlib 5.1, *)
public protocol Proto {
  // CHECK: associatedtype AssocType :
  // CHECK-ENABLED-SAME: Swift::Identifiable
  // CHECK-DISABLED-SAME: Swift.Identifiable
  // CHECK-PRESERVE-SAME: Identifiable
  // CHECK-ALIAS-SAME: Swift.Identifiable
  associatedtype AssocType: Identifiable

  // CHECK: typealias TypeAlias =
  // CHECK-ENABLED-SAME: TestCase::Struct<Swift::Int>
  // CHECK-DISABLED-SAME: TestCase.Struct<Swift.Int>
  // CHECK-PRESERVE-SAME: Struct<Int>
  // CHECK-ALIAS-SAME: Module___TestCase.Struct<Swift.Int>
  typealias TypeAlias = Struct<Int>

  // CHECK: typealias ID =
  // CHECK-ENABLED-SAME: Self.AssocType.ID
  // CHECK-DISABLED-SAME: Self.AssocType.ID
  // CHECK-PRESERVE-SAME: AssocType.ID
  // CHECK-ALIAS-SAME: Self.AssocType.ID
  typealias ID = AssocType.ID

  // CHECK: func requirement() ->
  // CHECK-ENABLED-SAME: Self.AssocType.ID
  // CHECK-DISABLED-SAME: Self.AssocType.ID
  // CHECK-PRESERVE-SAME: AssocType.ID
  // CHECK-ALIAS-SAME: Self.AssocType.ID
  func requirement() -> AssocType.ID

  // CHECK: func requirement2() ->
  // CHECK-ENABLED-SAME: Self.TypeAlias.Nested
  // CHECK-DISABLED-SAME: Self.TypeAlias.Nested
  // CHECK-PRESERVE-SAME: TypeAlias.Nested
  // CHECK-ALIAS-SAME: Self.TypeAlias.Nested
  func requirement2() -> TypeAlias.Nested

  // CHECK: }
}

@available(SwiftStdlib 5.1, *)
extension Proto {
  public typealias Text = String
}

// Test cases from rdar://166180424

// CHECK-LABEL: func fn3<T>(_ t: T) ->
// CHECK-SAME: T.TypeAlias.Nested
@available(SwiftStdlib 5.1, *)
public func fn3<T: Proto>(_ t: T) -> T.TypeAlias.Nested { fatalError() }

// CHECK-LABEL: struct ProtoSet
@available(SwiftStdlib 5.1, *)
public struct ProtoSet<T: Proto> {
  // CHECK: let ids:
  // CHECK-ENABLED-SAME: Swift::Set<T.ID>
  // CHECK-DISABLED-SAME: Swift.Set<T.ID>
  // CHECK-PRESERVE-SAME: Set<T.ID>
  // CHECK-ALIAS-SAME: Swift.Set<T.ID>
  public let ids: Set<T.ID>

  // CHECK: let texts:
  // CHECK-ENABLED-SAME: Swift::Set<T.Text>
  // CHECK-DISABLED-SAME: Swift.Set<T.Text>
  // CHECK-PRESERVE-SAME: Set<T.Text>
  // CHECK-ALIAS-SAME: Swift.Set<T.Text>
  public let texts: Set<T.Text>

  // CHECK: let singleText:
  // CHECK-SAME: T.Text
  public let singleText: T.Text
}

// DIAG-PRESERVE-OVERRIDDEN: warning: ignoring -module-interface-preserve-types-as-written (overridden by -enable-module-selectors-in-module-interface)
// DIAG-PRESERVE-NOT-OVERRIDDEN-NOT: warning: ignoring -module-interface-preserve-types-as-written (overridden by -enable-module-selectors-in-module-interface)

// DIAG-ALIAS-OVERRIDDEN: warning: ignoring -alias-module-names-in-module-interface (overridden by -enable-module-selectors-in-module-interface)
// DIAG-ALIAS-NOT-OVERRIDDEN-NOT: warning: ignoring -alias-module-names-in-module-interface (overridden by -enable-module-selectors-in-module-interface)

// rdar://169720990 -- complicated protocol hierarchy with typealias in protocol

// CHECK-LABEL: class TypeAliasNestClass
public final class TypeAliasNestClass<GenericParam: TypeAliasNestProtocol>: TypeAliasNestProtocol {
  // CHECK: typealias AssociatedType4 =
  // CHECK-ENABLED-SAME: TestCase::TypeAliasNestClass<GenericParam>.AssociatedType1.AssociatedType4
  // CHECK-DISABLED-SAME: TestCase.TypeAliasNestClass<GenericParam>.AssociatedType1.AssociatedType4
  // CHECK-PRESERVE-SAME: AssociatedType1.AssociatedType4
  // CHECK-ALIAS-SAME: Module___TestCase.TypeAliasNestClass<GenericParam>.AssociatedType1.AssociatedType4
  public typealias AssociatedType4 = AssociatedType1.AssociatedType4

  public typealias AssociatedType1 = GenericParam.AssociatedType1
}

public protocol TypeAliasNestProtocol {
  associatedtype AssociatedType1: AssociatedType1Protocol
}

public protocol AssociatedType1Protocol {
  associatedtype AssociatedType2: AssociatedType2Protocol
  typealias AssociatedType4 = AssociatedType2.AssociatedType3.AssociatedType4
}

public protocol AssociatedType2Protocol {
  associatedtype AssociatedType3: AssociatedType3Protocol
}

public protocol AssociatedType3Protocol {
  associatedtype AssociatedType4
}
