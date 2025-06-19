public protocol P {}
public protocol Q {}

public func wrap<T: P & Q>(_ t: T) -> some P & Q {
  return t
}


