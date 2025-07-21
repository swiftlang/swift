// {"kind":"typecheck","signature":"swift::Mangle::ASTMangler::appendProtocolName(swift::ProtocolDecl const*, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension Collection where Self : a {
  struct Index protocol a
