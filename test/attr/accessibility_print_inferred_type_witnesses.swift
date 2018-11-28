// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -print-access -source-filename=%s -swift-version 4| %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s
// RUN: %target-swift-frontend -emit-module-path %t/accessibility_print.swiftmodule -module-name accessibility_print %s -swift-version 4
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -print-access -module-to-print=accessibility_print -I %t -source-filename=%s -swift-version 4 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s

// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -print-access -source-filename=%s -swift-version 5 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s
// RUN: %target-swift-frontend -emit-module-path %t/accessibility_print.swiftmodule -module-name accessibility_print %s -swift-version 5
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -print-access -module-to-print=accessibility_print -I %t -source-filename=%s -swift-version 5 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s

// Enabling resilience means opting into the new behavior.
// RUN: %target-swift-frontend -emit-module-path %t/accessibility_print.swiftmodule -module-name accessibility_print %s -swift-version 4 -enable-resilience
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -print-access -module-to-print=accessibility_print -I %t -source-filename=%s -swift-version 4 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s

internal struct InternalStruct {}

public protocol PublicAssocTypeProto {
  associatedtype PublicValue
  var publicValue: PublicValue { get }
}
fileprivate protocol FilePrivateAssocTypeProto {
  associatedtype FilePrivateValue
  var filePrivateValue: FilePrivateValue { get }
}

// CHECK-LABEL: private{{(\*/)?}} class PrivateImpl : PublicAssocTypeProto, FilePrivateAssocTypeProto {
private class PrivateImpl: PublicAssocTypeProto, FilePrivateAssocTypeProto {
  fileprivate var publicValue: InternalStruct?
  fileprivate var filePrivateValue: Int?
  // CHECK-DAG: {{^}} fileprivate typealias PublicValue
  // CHECK-DAG: {{^}} fileprivate typealias FilePrivateValue
} // CHECK: {{^[}]}}

// CHECK-LABEL: public{{(\*/)?}} class PublicImpl : PublicAssocTypeProto, FilePrivateAssocTypeProto {
public class PublicImpl: PublicAssocTypeProto, FilePrivateAssocTypeProto {
  public var publicValue: Int?
  fileprivate var filePrivateValue: InternalStruct?
  // CHECK-DAG: {{^}} public typealias PublicValue
  // CHECK-4-DAG: {{^}} internal typealias FilePrivateValue
  // CHECK-5-DAG: {{^}} fileprivate typealias FilePrivateValue
} // CHECK: {{^[}]}}
