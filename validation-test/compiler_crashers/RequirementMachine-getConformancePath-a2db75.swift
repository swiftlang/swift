// {"kind":"emit-ir","signature":"swift::rewriting::RequirementMachine::getConformancePath(swift::Type, swift::ProtocolDecl*)","signatureNext":"Mangle::ASTMangler::appendAnyProtocolConformance"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
protocol a {
}
protocol b {
}
class c: a {
}
class k: c, b {
}
func d<e: c>(f: e) -> (b, some a) {
  (k(), f)
}
func g(h: k.Type) -> (a.Type, b.Type) {
  let i = h
  let j = h
  return (i, j)
}
