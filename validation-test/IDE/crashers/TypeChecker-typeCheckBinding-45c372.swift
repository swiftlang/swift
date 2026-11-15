// RUN: not --crash %target-swift-ide-test -print-indexed-symbols -include-locals -enable-library-evolution -experimental-lazy-typecheck -source-filename %s
func f() {
  _ = { let x = 0 }
}
