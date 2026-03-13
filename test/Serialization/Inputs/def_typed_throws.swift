public enum MyError: Error {
  case fail
}

public func throwsMyError() throws(MyError) { }

public struct SomeStruct {
  public init() throws(MyError) { }

  public var value: Int {
    get throws(MyError) {
      return 17
    }
  }
}
