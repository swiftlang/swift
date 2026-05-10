public protocol P {
  func f()
}

public protocol PDerived: P {}

public protocol MyObject: AnyObject {}
public protocol MyCommand: AnyObject {}
public protocol ClientBoundCommand: AnyObject {}
public protocol ServerBoundCommand: AnyObject {}
public protocol BaseCommand: AnyObject {}
