protocol Observed: AnyObject {
}

struct Other<Value: Equatable> {
  var value: Value

  func hello() -> String {
    return "Hello from \(value)"
  }
}

@propertyWrapper
struct Observable<Value: Equatable> {
  private var stored: Value

  
  init(wrappedValue: Value) {
    self.stored = wrappedValue
  }

  var wrappedValue: Value {
    get { fatalError("called wrappedValue getter") }
    set { fatalError("called wrappedValue setter") }
  }

  var projectedValue: Other<Value> {
    get { fatalError("called projectedValue getter") }
    set { fatalError("called projectedValue setter") }
  }
  
  static subscript<EnclosingSelf: Observed, FinalValue>(
      _enclosingInstance observed: EnclosingSelf,
      wrapped wrappedKeyPath: ReferenceWritableKeyPath<EnclosingSelf, FinalValue>,
      storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Self>
    ) -> Value {
    get {
      fatalError("blah")
    }
    set {
    }
  }

  static subscript<EnclosingSelf: Observed>(
      _enclosingInstance observed: EnclosingSelf,
      projected wrappedKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Other<Value>>,
      storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Self>
    ) -> Other<Value> {
    get {
      fatalError("blah")
    }
    set {
    }
  }
}
