// {"signature":"swift::ide::printTypeUSR(swift::Type, llvm::raw_ostream&)"}
// RUN: not %target-swift-frontend -typecheck %s
class a {
  class b < c class e : a<> {
    d = b
