// {"kind":"typecheck","original":"0004c325","signature":"swift::rewriting::RewriteContext::getRequirementMachine(swift::ProtocolDecl const*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: a
  associatedtype c: a
  where
    d == Self,
    c.c.b.c.c.b == Self, b.e == Self
}
