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
