open class Base {
  internal func method() {}
}

open class Middle : Base {
  open override func method() {}
}
