// {"signature":"swift::Parser::parseNewDeclAttribute(swift::DeclAttributes&, swift::SourceLoc, swift::DeclAttrKind, bool)::$_4::operator()() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  class override b
