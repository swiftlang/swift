
public func makeSomeClosures<T, U>(t: T, x: Int, y: C1<U>)
    -> (() -> (), () -> (), () -> ()) {
  return ({ _ = t }, { _ = x }, { _ = y })
}
