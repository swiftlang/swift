// {"kind":"typecheck","signature":"(anonymous namespace)::LookupResultBuilder::add(swift::ValueDecl*, swift::DeclContext*, swift::ValueDecl*, swift::Type, bool)::'lambda'(swift::ValueDecl*)::operator()(swift::ValueDecl*) const","signatureAssert":"Assertion failed: (IndexOfFirstOuterResult > 0 && \"found outer results without an inner one\"), function add"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{b} protocol c : a protocol d : c extension d {
  struct e : d {
    f {
      b
