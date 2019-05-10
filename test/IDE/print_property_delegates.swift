// RUN: %empty-directory(%t)

// Directly printing the type-checked AST
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s | %FileCheck %s

@_propertyDelegate
struct Delegate<Value> {
  var _stored: Value?

  var value: Value {
    get {
      return _stored!
    }

    set {
      _stored = newValue
    }
  }

  init() {
    self._stored = nil
  }

  init(initialValue: Value) {
    self._stored = initialValue
  }

  init(closure: () -> Value) {
    self._stored = closure()
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

  @Delegate
  var z: String

  // Memberwise initializer.
  // CHECK: init(x: Delegate<Int> = Delegate(closure: foo), y: Bool = true, z: String = Delegate())
}

func trigger() {
  _ = HasDelegates(y: false)
}
