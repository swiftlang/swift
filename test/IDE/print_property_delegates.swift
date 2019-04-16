// RUN: %empty-directory(%t)

// Directly printing the type-checked AST
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s | %FileCheck %s

@propertyDelegate
struct Delegate<Value> {
  var value: Value

  init(initialValue: Value) {
    self.value = initialValue
  }

  init(closure: () -> Value) {
    self.value = closure()
  }
}

func foo() -> Int { return 17 }

// CHECK: struct HasDelegates {
struct HasDelegates {
  // CHECK: @Delegate var x: Int {
  // CHECK-NEXT:  get
  // CHECK: var $x: Delegate<Int>
  @Delegate(closure: foo)
  var x: Int

  @Delegate
  var y = true

  // Memberwise initializer.
  // CHECK: init(x: Delegate<Int> = Delegate(closure: foo), y: Bool = true)
}

func trigger() {
  _ = HasDelegates(y: false)
}
