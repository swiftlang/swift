// RUN: %target-swift-frontend -emit-sil -swift-version 5 -primary-file %s -o /dev/null -verify

// ---------------------------------------------------------------------------
// Enclosing instance diagnostics
// ---------------------------------------------------------------------------
@propertyWrapper
struct Observable<Value> {
  private var stored: Value

  init(wrappedValue: Value) {
    self.stored = wrappedValue
  }

  @available(*, unavailable, message: "must be in a class")
  var wrappedValue: Value {
    get { fatalError("called wrappedValue getter") }
    set { fatalError("called wrappedValue setter") }
  }

  static subscript<EnclosingSelf>(
      _enclosingInstance observed: EnclosingSelf,
      wrapped wrappedKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Value>,
      storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Self>
    ) -> Value {
    get {
      observed[keyPath: storageKeyPath].stored
    }
    set {
      observed[keyPath: storageKeyPath].stored = newValue
    }
  }
}

class Superclass {
  var x: Int = 0
}

class SubclassWithEnclosingSelfInInitializer: Superclass {

  init(test1: String) {
    // Verify the init operand is accepted.
    self.y = test1

    // Verify the set operand is not accepted.
    self.z = test1 // expected-error{{'self' captured by a closure before all members were initialized}}
  }

  init(test2: String) {
    // Verify the init operand is accepted.
    self.y = test2

    super.init()

    // Verify the set operand is accepted.
    self.z = test2
  }

  init(test3: String) {
    super.init() // expected-error{{property 'self.y' not initialized at super.init call}}

    // Verify the init operand is not accepted.
    self.y = test3 

    // Verify the set operand is accepted.
    self.z = test3
  }

  @Observable
  var y: String

  @Observable
  var z: String = ""
}
