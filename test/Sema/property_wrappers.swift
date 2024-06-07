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
  // CHECK:      argument_list implicit labels="wrappedValue:reset:"
  // CHECK-NEXT:   argument label="wrappedValue"
  // CHECK-NEXT:     property_wrapper_value_placeholder_expr implicit type="String"
  // CHECK-NEXT:     opaque_value_expr implicit type="String"
  // CHECK-NEXT:     string_literal_expr type="String"
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

  // CHECK:      argument_list labels="wrappedValue:"
  // CHECK-NEXT:   argument label="wrappedValue"
  // CHECK-NEXT:     subscript_expr type="TestInitSubscript.Color"
  // CHECK:            argument_list implicit
  // CHECK-NEXT:         argument
  // CHECK-NOT:            property_wrapper_value_placeholder_expr implicit type="Int"
  // CHECK:                integer_literal_expr type="Int"
  @Wrapper(wrappedValue: Color.allCases[0])
  var color: Color
}

// https://github.com/apple/swift/issues/58201

@propertyWrapper
public class W_58201<Value> {
  private var _value: Value

  public var wrappedValue: Value {
    get { _value }
    set {
      _value = newValue
    }
  }

  public init(wrappedValue value: @autoclosure @escaping () -> Value) {
    self._value = value()
  }
}

// CHECK-LABEL: struct_decl{{.*}}S1_58201
struct S1_58201 {
  // CHECK:      argument_list implicit labels="wrappedValue:"
  // CHECK-NEXT:   argument label="wrappedValue"
  // CHECK-NEXT:     autoclosure_expr implicit type="() -> Bool?" discriminator=0 nonisolated captures=(<opaque_value> ) escaping
  // CHECK:            autoclosure_expr implicit type="() -> Bool?" discriminator=1 nonisolated escaping
  @W_58201 var a: Bool?
}

// CHECK-LABEL: struct_decl{{.*}}S2_58201
struct S2_58201 {
  // CHECK:      argument_list implicit labels="wrappedValue:"
  // CHECK-NEXT:   argument label="wrappedValue"
  // CHECK-NEXT:     autoclosure_expr implicit type="() -> Bool" location={{.*}}.swift:[[@LINE+2]]:26 range=[{{.+}}] discriminator=0 nonisolated captures=(<opaque_value> ) escaping
  // CHECK:            autoclosure_expr implicit type="() -> Bool" location={{.*}}.swift:[[@LINE+1]]:26 range=[{{.+}}] discriminator=1 nonisolated escaping
  @W_58201 var b: Bool = false
}

// https://github.com/apple/swift/issues/61570
// rdar://problem/101813792 - Check that captured variables are re-parented to backing var initializer
do {
  @propertyWrapper
  struct Wrapper {
    init(_: String) {}

    var wrappedValue: Int { return 0 }
  }

  struct TestExplicitCapture {
    @Wrapper("\([1,2,3].map({[x = 42] in "\($0 + x)" }).reduce("", +))")
    var wrapped: Int // Ok, becomes var _wrapped: Wrapper<Int> = Wrapper(wrappedValue: "\([1,2,3].map({[x = 42] in "\($0 + x)" }).reduce("", +)))
  }

  @propertyWrapper
  struct Option {
    let help: String
    var value: Double = 0.0

    var wrappedValue: Double {
      get { value }
      set { value = newValue }
    }
  }

  enum TestEnum: String, CaseIterable {
    case hello = "hello"
    case world = "world"
  }

  struct TestImplicitCapture {
    @Option(help: "Values: \(TestEnum.allCases.map(\.rawValue))")
    var value // Ok - $kp$ of key path is captured implicitly by backing variable init
  }
}
