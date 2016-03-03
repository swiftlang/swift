
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public protocol ElementProtocol : Equatable {
  func increment() -> Self
}

public protocol AddRequirementsProtocol {
  associatedtype Element : ElementProtocol

  func importantOperation() -> Element
  func unimportantOperation() -> Element

#if AFTER
  func uselessOperation() -> Element
#endif
}

extension AddRequirementsProtocol {
  public func unimportantOperation() -> Element {
    return importantOperation().increment()
  }

#if AFTER
  public func uselessOperation() -> Element {
    return unimportantOperation().increment()
  }
#endif
}

public func doSomething<T : AddRequirementsProtocol>(t: T) -> [T.Element] {
#if BEFORE
  return [
      t.importantOperation(),
      t.unimportantOperation(),
      t.unimportantOperation().increment(),
  ]
#else
  return [
      t.importantOperation(),
      t.unimportantOperation(),
      t.uselessOperation(),
  ]
#endif
}
