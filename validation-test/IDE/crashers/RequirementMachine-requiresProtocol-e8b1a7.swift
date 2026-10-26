// {"kind":"complete","original":"6730aacc","signature":"swift::rewriting::RequirementMachine::requiresProtocol(swift::Type, swift::ProtocolDecl const*) const","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  associatedtype c
  associatedtype d: a
}
protocol e {
  associatedtype b where b == Self
  associatedtype f: a where f.b == b
  associatedtype g: a where g.b == b
}
protocol h {
  associatedtype i where i.d.c == j
  associatedtype k: e where k.g == i
  associatedtype l: e where l.f == i, l.g == m
  struct n<o>: h where o: h {
    typealias i = #^^#
    typealias k = o.l
  }
}
