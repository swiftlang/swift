@typeWrapper
public struct Wrapper<S> {
  var underlying: S

  public init(memberwise: S) {
    print("Wrapper.init(\(memberwise))")
    self.underlying = memberwise
  }

  public subscript<V>(storageKeyPath path: WritableKeyPath<S, V>) -> V {
    get {
      print("in getter")
      return underlying[keyPath: path]
    }
    set {
      print("in setter => \(newValue)")
      underlying[keyPath: path] = newValue
    }
  }
}

@propertyWrapper
public struct PropWrapper<Value> {
  public var value: Value

  public init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  public var projectedValue: Self { return self }

  public var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

@propertyWrapper
public struct PropWrapperWithoutInit<Value> {
  public var value: Value

  public init(value: Value) {
    self.value = value
  }

  public var projectedValue: Self { return self }

  public var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

@propertyWrapper
public struct PropWrapperWithoutProjection<Value> {
  public var value: Value

  public init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  public var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

@Wrapper
public class Person<T> {
  public var name: String
  public var projects: [T]
}

@Wrapper
public struct PersonWithDefaults {
  public var name: String = "<no name>"
  public var age: Int = 99
}

@Wrapper
public struct PropWrapperTest {
  @PropWrapper public var test: Int
}

@Wrapper
public struct DefaultedPropWrapperTest {
  @PropWrapper public var test: Int = 0
}

@Wrapper
public struct DefaultedPropWrapperWithArgTest {
  @PropWrapper(wrappedValue: 3) public var test: Int
}

@Wrapper
public struct PropWrapperNoInitTest {
  @PropWrapperWithoutInit public var a: Int
  @PropWrapperWithoutInit(value: "b") public var b: String
}

@Wrapper
public struct PropWrapperNoProjectionTest {
  @PropWrapperWithoutProjection public var a: Int = 0
  @PropWrapperWithoutProjection(wrappedValue: PropWrapper(wrappedValue: "b")) @PropWrapper public var b: String
}

@Wrapper
public struct ComplexPropWrapperTest {
  @PropWrapper public var a: [String] = ["a"]
  @PropWrapperWithoutInit(value: PropWrapper(wrappedValue: [1, 2, 3])) @PropWrapper public var b: [Int]
}
