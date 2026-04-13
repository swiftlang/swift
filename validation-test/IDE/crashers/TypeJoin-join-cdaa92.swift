// {"kind":"complete","original":"24a549e7","signature":"(anonymous namespace)::TypeJoin::join(swift::CanType, swift::CanType)","signatureAssert":"Assertion failed: (first->getWithoutSpecifierType()->isEqual(first) && \"Expected simple type!\"), function join","signatureNext":"Type::join"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
[
  {
    $0
    0
  },
  {
    $0 *= 2
  },
  { a in
    #^^#
  },
  {
    $0
  },
]
