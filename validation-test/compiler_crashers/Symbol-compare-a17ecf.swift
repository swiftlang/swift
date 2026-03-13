// {"kind":"typecheck","original":"f1107ad8","signature":"swift::rewriting::Symbol::compare(swift::rewriting::Symbol, swift::rewriting::RewriteContext&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b> {
  extension InlineArray<b, UInt8> {
  }
}
