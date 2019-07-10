// RUN: %empty-directory(%t)

// Directly printing the type-checked AST
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s | %FileCheck %s

@propertyWrapper
struct Wrapper<Value> {
  var _stored: Value?

  var wrappedValue: Value {
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

  init(wrappedValue initialValue: Value) {
    self._stored = initialValue
  }

  init(closure: () -> Value) {
    self._stored = closure()
  }
}

func foo() -> Int { return 17 }

// CHECK: struct HasWrappers {
struct HasWrappers {
  // CHECK: @Wrapper var x: Int {
  // CHECK-NEXT:  get
  // CHECK: var _x: Wrapper<Int>
  @Wrapper(closure: foo)
  var x: Int

  @Wrapper
  var y = true

  @Wrapper
  var z: String

  // Memberwise initializer.
  // CHECK: init(x: Wrapper<Int> = Wrapper(closure: foo), y: Bool = true, z: String = Wrapper())
}

func trigger() {
  _ = HasWrappers(y: false, z: "hello")
}
