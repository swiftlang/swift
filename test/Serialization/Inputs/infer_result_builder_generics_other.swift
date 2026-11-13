@resultBuilder
public enum ArrayBuilder<Element> {
  public static func buildBlock(_ elements: Element...) -> [Element] {
    elements
  }
}

@resultBuilder
public enum DictionaryBuilder<Key: Hashable, Value> {
  public static func buildBlock(_ elements: (Key, Value)...) -> [Key: Value] {
    var dictionary = [Key: Value]()
    for (key, value) in elements {
      dictionary[key] = value
    }
    return dictionary
  }
}

extension Array {
  @resultBuilder
  public enum Builder {
    public static func buildBlock(_ elements: Element...) -> [Element] {
      elements
    }
  }
}

public func makeArray(@ArrayBuilder _ build: () -> [String]) -> [String] {
  build()
}

public func makeDictionary(
  @DictionaryBuilder<String, _> _ build: () -> [String: Int]
) -> [String: Int] {
  build()
}

public func makeNestedArray(@Array<_>.Builder _ build: () -> [Int]) -> [Int] {
  build()
}

public struct Invocation {
  @ArrayBuilder public var arguments: () -> [String]

  public init(@ArrayBuilder arguments: @escaping () -> [String]) {
    self.arguments = arguments
  }
}

// The generic arguments are inferred from the requirements of an opaque
// result type.
@ArrayBuilder
public func makeOpaqueArray() -> some Collection<String> {
  "format"
  "--in-place"
}

@DictionaryBuilder<String, _>
public func makeOpaqueDictionary() -> some Collection<(key: String, value: Int)> {
  ("format", 1)
}
