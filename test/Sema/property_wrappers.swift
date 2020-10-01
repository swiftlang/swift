// RUN: %target-swift-frontend -typecheck -disable-availability-checking -dump-ast %s | %FileCheck %s

struct Transaction {
  var state: Int?
}

@propertyWrapper
struct WrapperWithClosureArg<Value> {
  var wrappedValue: Value

  init(wrappedValue: Value,
       reset: @escaping (Value, inout Transaction) -> Void) {
    self.wrappedValue = wrappedValue
  }
}

// rdar://problem/59685601
// CHECK-LABEL: R_59685601
struct R_59685601 {
  // CHECK: tuple_expr implicit type='(wrappedValue: String, reset: (String, inout Transaction) -> Void)'
  // CHECK-NEXT: property_wrapper_value_placeholder_expr implicit type='String'
  // CHECK-NEXT: opaque_value_expr implicit type='String'
  // CHECK-NEXT: string_literal_expr type='String'
  @WrapperWithClosureArg(reset: { value, transaction in
    transaction.state = 10
  })
  private var value = "hello"
}

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

// CHECK-LABEL: struct_decl{{.*}}TestInitSubscript
struct TestInitSubscript {
  enum Color: CaseIterable { case pink }

  // CHECK: tuple_expr type='(wrappedValue: TestInitSubscript.Color)'
  // CHECK: subscript_expr type='TestInitSubscript.Color'
  // CHECK: paren_expr type='(Int)'
  // CHECK-NOT: property_wrapper_value_placeholder_expr implicit type='Int'
  // CHECK: integer_literal_expr type='Int'
  @Wrapper(wrappedValue: Color.allCases[0])
  var color: Color
}
