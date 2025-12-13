// RUN: %target-typecheck-verify-swift

enum CustomError: Error {
}

struct ValidationError: Error {
}

func parseOption(_: String) throws -> Int {
  42
}

@propertyWrapper
public struct Option<Value> {
  public var wrappedValue: Value

  public init(
    wrappedValue: Value,
    transform: @Sendable @escaping (String) throws -> Value
  ) {
    self.wrappedValue = wrappedValue
  }
}

struct Test {
  @Option(transform: {
    do {
      return try parseOption($0)
    } catch let error as CustomError {
      throw ValidationError()
    }
  })
  var prop: Int = 0
}
