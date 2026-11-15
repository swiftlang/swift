// RUN: not --crash %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s
_ = {
  struct S {
    var v: Int = { let x = 0; return x }()
  }
  undefined
}
