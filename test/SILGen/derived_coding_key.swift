// RUN: %target-swift-frontend -emit-silgen -emit-sorted-sil %s -swift-version 4 -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) | %FileCheck -check-prefix CHECK -check-prefix CHECK-FRAGILE %s
// RUN: %target-swift-frontend -emit-silgen -emit-sorted-sil %s -swift-version 4 -enable-library-evolution -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) | %FileCheck -check-prefix CHECK -check-prefix CHECK-RESILIENT %s
// RUN: %target-swift-frontend -emit-silgen -emit-sorted-sil %s -swift-version 4 | %FileCheck -check-prefix CHECK -check-prefix CHECK-FRAGILE %s
// RUN: %target-swift-frontend -emit-silgen -emit-sorted-sil %s -swift-version 4 -enable-library-evolution | %FileCheck -check-prefix CHECK -check-prefix CHECK-RESILIENT %s

// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: enum PlainEnum {
enum PlainEnum {
  // CHECK: case a
  case a
  // CHECK: case b
  case b
  // CHECK: case c
  case c
  // CHECK-DAG: init?(stringValue: String)
  // CHECK-DAG: init?(intValue: Int)
  // CHECK-DAG: var stringValue: String { get }
  // CHECK-DAG: var intValue: Int? { get }
}
// CHECK: }

extension PlainEnum: CodingKey {}

// CHECK-LABEL: enum StringRawEnum : String {
enum StringRawEnum: String {
  // CHECK: case alpha
  case alpha = "a"
  // CHECK: case beta
  case beta = "b"
  // CHECK: case gamma
  case gamma = "c"
  // CHECK-DAG: init?(stringValue: String)
  // CHECK-DAG: init?(intValue: Int)
  // CHECK-DAG: var stringValue: String { get }
  // CHECK-DAG: var intValue: Int? { get }
}
// CHECK: }

extension StringRawEnum: CodingKey {}

// CHECK-LABEL: enum IntRawEnum : Int {
enum IntRawEnum: Int {
  // CHECK: case one
  case one = 1
  // CHECK: case two
  case two = 2
  // CHECK: case three
  case three = 3
  // CHECK-DAG: init?(stringValue: String)
  // CHECK-DAG: init?(intValue: Int)
  // CHECK-DAG: var stringValue: String { get }
  // CHECK-DAG: var intValue: Int? { get }
}
// CHECK: }

extension IntRawEnum: CodingKey {}

// CHECK-LABEL: enum EmptyEnum {
enum EmptyEnum {
  // CHECK-DAG: init?(stringValue: String)
  // CHECK-DAG: init?(intValue: Int)
  // CHECK-DAG: var stringValue: String { get }
  // CHECK-DAG: var intValue: Int? { get }
}
// CHECK: }

extension EmptyEnum: CodingKey {}

// CHECK-LABEL: enum ManyEnum {
enum ManyEnum {
  // CHECK: case first
  case first
  // CHECK: case second
  case second
  // CHECK: case third
  case third
  // CHECK: case fourth
  case fourth
  // CHECK: case fifth
  case fifth
  // CHECK-DAG: init?(stringValue: String)
  // CHECK-DAG: init?(intValue: Int)
  // CHECK-DAG: var stringValue: String { get }
  // CHECK-DAG: var intValue: Int? { get }
}
// CHECK: }

extension ManyEnum: CodingKey {}

// MARK: - Enum with unavailable cases

@available(macOS 10.15, *)
// CHECK-LABEL: enum AvailableEnum {
enum AvailableEnum {
  // CHECK: case old
  case old
  @available(macOS 11.0, *)
  // CHECK: case new
  case new
  // CHECK-DAG: init?(stringValue: String)
  // CHECK-DAG: init?(intValue: Int)
  // CHECK-DAG: var stringValue: String { get }
  // CHECK-DAG: var intValue: Int? { get }
}
// CHECK: }

extension AvailableEnum: CodingKey {}

// IntRawEnum

// CHECK-LABEL: // IntRawEnum.init(stringValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}10IntRawEnumO11stringValueACSgSS_tcfC

// CHECK-LABEL: // IntRawEnum.stringValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}10IntRawEnumO11stringValueSSvg

// CHECK-LABEL: // IntRawEnum.init(intValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}10IntRawEnumO8intValueACSgSi_tcfC

// CHECK-LABEL: // IntRawEnum.intValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}10IntRawEnumO8intValueSiSgvg

// AvailableEnum

// CHECK-LABEL: // AvailableEnum.init(stringValue:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden{{.*}}[ossa] @$s{{.*}}13AvailableEnumO11stringValueACSgSS_tcfC

// CHECK-LABEL: // AvailableEnum.stringValue.getter
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden{{.*}}[ossa] @$s{{.*}}13AvailableEnumO11stringValueSSvg

// StringRawEnum

// CHECK-LABEL: // StringRawEnum.init(stringValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}13StringRawEnumO11stringValueACSgSS_tcfC

// CHECK-LABEL: // StringRawEnum.stringValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}13StringRawEnumO11stringValueSSvg

// CHECK-LABEL: // StringRawEnum.init(intValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}13StringRawEnumO8intValueACSgSi_tcfC

// CHECK-LABEL: // StringRawEnum.intValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}13StringRawEnumO8intValueSiSgvg

// EmptyEnum

// CHECK-LABEL: // EmptyEnum.init(stringValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}9EmptyEnumO11stringValueACSgSS_tcfC

// CHECK-LABEL: // EmptyEnum.stringValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}9EmptyEnumO11stringValueSSvg

// CHECK-LABEL: // EmptyEnum.init(intValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}9EmptyEnumO8intValueACSgSi_tcfC

// CHECK-LABEL: // EmptyEnum.intValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}9EmptyEnumO8intValueSiSgvg

// PlainEnum

// CHECK-LABEL: // PlainEnum.init(stringValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}9PlainEnumO11stringValueACSgSS_tcfC

// CHECK-LABEL: // PlainEnum.stringValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}9PlainEnumO11stringValueSSvg

// CHECK-LABEL: // PlainEnum.init(intValue:)
// CHECK: sil hidden [ossa] @$s{{.*}}9PlainEnumO8intValueACSgSi_tcfC

// CHECK-LABEL: // PlainEnum.intValue.getter
// CHECK: sil hidden [ossa] @$s{{.*}}9PlainEnumO8intValueSiSgvg

// Witness tables

// CHECK-LABEL: sil_witness_table hidden IntRawEnum: CodingKey module {{.*}} {
// CHECK: method #CodingKey.stringValue!getter
// CHECK: method #CodingKey.init!allocator
// CHECK: method #CodingKey.intValue!getter
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden StringRawEnum: CodingKey module {{.*}} {
// CHECK: method #CodingKey.stringValue!getter
// CHECK: method #CodingKey.init!allocator
// CHECK: method #CodingKey.intValue!getter
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden EmptyEnum: CodingKey module {{.*}} {
// CHECK: method #CodingKey.stringValue!getter
// CHECK: method #CodingKey.init!allocator
// CHECK: method #CodingKey.intValue!getter
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden PlainEnum: CodingKey module {{.*}} {
// CHECK: method #CodingKey.stringValue!getter
// CHECK: method #CodingKey.init!allocator
// CHECK: method #CodingKey.intValue!getter
// CHECK: }
