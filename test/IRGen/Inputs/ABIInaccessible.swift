private struct Private<T> {
  init(_ t: T) {
    p = t
  }
  var p : T
}

public struct Public<T> {
  init(_ t: T) {
    p = Private<T>(t)
  }
  private var p: Private<T>
}
