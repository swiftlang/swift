// RUN: %batch-code-completion

// rdar://133460404 - Make sure we complete for the rebound local variable.
func bar() {
  var foo: Int?
  if let foo = foo {
    let foo = ""
    #^COMPLETE^#
  }
}
// COMPLETE: Decl[LocalVar]/Local: foo[#String#]; name=foo
