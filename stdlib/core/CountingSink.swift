struct CountingSink<T> : Sink {
  func put(_: T) {
    ++count
  }
  var count: Int
}
