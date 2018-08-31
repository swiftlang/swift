public enum GenericResilientEnum<T> {
#if BEFORE
  case FirstCase
  case SecondCase
  case ThirdCase(SomeObject)
  case ForthCase(T)
#else
  case SecondCase
  case ForthCase(T)
  case FirstCase
  case ThirdCase(SomeObject)
#endif
}

public class SomeObject {
  public func someNumber() -> Int{
    return 7
  }
}
public func createGenericResilientEnum1() -> GenericResilientEnum<Int> {
  return GenericResilientEnum.FirstCase
}

public func createGenericResilientEnum2() -> GenericResilientEnum<Int> {
  return GenericResilientEnum.SecondCase
}
public func createGenericResilientEnum3() -> GenericResilientEnum<Int> {
  return GenericResilientEnum.ThirdCase(SomeObject())
}
public func createGenericResilientEnum4<T>(_ t: T) -> GenericResilientEnum<T> {
  return GenericResilientEnum.ForthCase(t)
}

public func getCase<T>(_ e: GenericResilientEnum<T>) -> Int {
  var whichCase = 0
  switch e {
    case .FirstCase:
      whichCase = 1
    case .SecondCase:
      whichCase = 2
    case .ThirdCase(_):
      whichCase = 3
    case .ForthCase(_):
      whichCase = 4
  }
  return whichCase
}
