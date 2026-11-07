// {"kind":"typecheck","original":"09e735b9","signature":"unknown"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a
  enum b<c: a {
    case (b<d<c>>)
    e{switch self{}}
  }
  struct d<c>: a
