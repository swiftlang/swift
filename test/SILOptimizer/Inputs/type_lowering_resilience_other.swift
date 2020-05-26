@frozen public struct Holder<T> {
  var ref: AnyObject?
  var inner: (T, T)

  public init(_ t: T) { ref = nil; inner = (t, t) }
}

public struct Inner {
  var x: Int

  public init() { x = 0 }
}
