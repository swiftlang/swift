public class Base<T> {
  var t: T
  init(_ a: T) {
    t = a
  }
}

public class Sub<T> : Base<T> {
}

public func requestTypeThrough<T>(closure: ((Sub<T>, Int)) -> (), arg: T) {
  closure((Sub(arg), 0))
}
