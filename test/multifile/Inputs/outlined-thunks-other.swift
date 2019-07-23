private class OtherPrivate<T> { }

struct OtherInternal<T> {
  fileprivate var myPrivate: OtherPrivate<T>? = nil
}
