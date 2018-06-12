// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/Lib.swiftmodule %s -module-name Lib
// RUN: echo 'import Lib; _ = MyClass(with: "test", and: 341)' | %target-build-swift -I %t -typecheck -

public protocol MyProtocol {
  var string: String { get }
  var int: Int { get }

  init(with string: String, and int: Int)

  func doSomething<T, U>(with this: T, for string: String, expecting: U.Type) throws -> U
  func doSomething<T>(with this: T, for string: String) throws
}

public class MyClass: MyProtocol {
  public var string: String
  public var int: Int

  public required init(with string: String, and int: Int) {
    self.string = string
    self.int = int
  }

  public func doSomething<T>(with this: T, for string: String) throws {}
  public func doSomething<T, U>(with this: T, for string: String, expecting: U.Type) throws -> U {
    fatalError()
  }
}
