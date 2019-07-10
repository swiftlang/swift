public class SomeClass {}

public protocol SomeProtocol {}

@frozen
public struct ConcreteFragileStruct {
  var field: Int32

  public struct NestedResilientStruct {}
}

@frozen
public struct NonDependentFragileStruct<T : AnyObject> {
  var field: T
}

@frozen
public struct DependentFragileStruct<T> {
  var field: T
}

public struct ConcreteResilientStruct {
  var field: Int64
}

public struct NonDependentResilientStruct<T : AnyObject> {
  var field: T
}

public struct DependentResilientStruct<T> {
  var field: T

  public struct NestedNonDependentResilientStruct {
    var field: UnsafePointer<T>
  }
}

public enum ConcreteResilientEnum {
  case left(Int64)
  case right(Int64)
}

public enum NonDependentResilientEnum<T : AnyObject> {
  case some(UnsafePointer<T>)
  case none
}

public enum DependentResilientEnum<T> {
  case some(T)
  case none
}

extension DependentResilientEnum {
  public enum NestedNonDependentResilientEnum {
    case some(UnsafePointer<T>)
    case none
  }
}

extension Int {
  public struct NestedInExtension {
    var field: Int32
  }
}
