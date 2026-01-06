// RUN: not %target-swift-frontend -emit-ir %s

public protocol Ungulate {
  func eat()
}

public class Horse<T> : Ungulate {
  public var saddle: AnyObject? = nil

  public func eat() {}
}

public struct Hay {}

// Here, ClassDecl::getSuperclassDecl() will find the 'Horse' class, but
// ClassDecl::getSuperclass() will return ErrorType because the generic
// argument 'DoesNotExist' cannot be resolved.
public class Pony : Horse<DoesNotExist> {
  public override var saddle: AnyObject? {
    didSet {}
  }

  public func eat(_: Hay) {
    eat()
  }
}
