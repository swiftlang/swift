// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s
extension Undefined {
  func f() {
    _ = { let x = 0 }
  }
}
