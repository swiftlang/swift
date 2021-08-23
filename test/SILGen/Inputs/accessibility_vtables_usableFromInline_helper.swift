open class Base {
  internal func internalMethod() {}
  @usableFromInline internal func usableFromInlineMethod() {}
}

open class Middle : Base {
  open override func internalMethod() {}
  open override func usableFromInlineMethod() {}
}
