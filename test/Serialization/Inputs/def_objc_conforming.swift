import Foundation

@objc public protocol Proto {
  @objc optional func badness()
}

public class Foo: Proto {
  public var badness: Int = 0 // unrelated
}
