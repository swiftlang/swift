// {"signature":"(anonymous namespace)::LookupResultBuilder::add(swift::ValueDecl*, swift::DeclContext*, swift::ValueDecl*, swift::Type, bool)::'lambda'(swift::ValueDecl*)::operator()(swift::ValueDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{b} protocol c : a protocol d : c extension d {
  struct e : d {
    f {
      b
