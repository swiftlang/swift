// RUN: %target-typecheck-verify-swift

@propertyWrapper public struct Wrapper<ValueType> {
  public var wrappedValue: ValueType
  public init(wrappedValue: ValueType) {}
}

public protocol Proto {
  typealias TA = Wrapper
}

public class SubToProto : Proto {
  @TA // Ok
  var a = 12

  @SubToProto.TA // Ok - typealias is declared in the protocol `Proto`
  var b = 12

  @Proto.TA // Ok
  var c = 12

  func foo(a: SubToProto.TA<Int>, b: Proto.TA<Int>) {} // Ok
}

public class BaseClass {
  typealias TA = Wrapper
}

public class SubClass : BaseClass {
  @SubClass.TA // Ok
  var b = 12

  @BaseClass.TA // Ok
  var c = 12
}
