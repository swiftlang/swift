// RUN: %target-swift-frontend -typecheck -dump-ast %s | %FileCheck %s

public struct Data {
  init(_ bytes: [UInt8]) { }
}

internal struct Item {
  public let data: Data

  public init(tag: UInt8) {
    self.data = Data([tag << 2])
  }

  // CHECK-LABEL: constructor_decl{{.*}}"init(tag:value:)"
  public init(tag: UInt8, value: UInt) {
    // CHECK: assign_expr
    // CHECK: member_ref_expr type="@lvalue Data"
    // CHECK-NEXT: declref_expr type="@lvalue Item"
    // CHECK-NEXT: member_ref_expr type="Data"
    self.data = Self(tag: tag).data
  }
}
