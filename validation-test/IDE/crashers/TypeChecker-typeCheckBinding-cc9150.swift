// RUN: not --crash %target-swift-ide-test -print-indexed-symbols -include-locals -enable-library-evolution -experimental-lazy-typecheck -source-filename %s
struct S {
  var v: Int = { let x = 0; return x }()
}
