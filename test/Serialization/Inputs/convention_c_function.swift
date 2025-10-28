public func foo(fn: @convention(c) () -> ()) -> () {
  fn()
}
