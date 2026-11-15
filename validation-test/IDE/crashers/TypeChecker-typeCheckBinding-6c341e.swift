// RUN: not --crash %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s
_ = {
  func f() {
    _ = { let x = 0 }
  }
  undefined
}
