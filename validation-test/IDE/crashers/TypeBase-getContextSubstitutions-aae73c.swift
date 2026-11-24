// {"kind":"complete","original":"4ad616e2","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (0 && \"Bad base type\"), function getContextSubstitutions","signatureNext":"TypeBase::getMemberSubstitutions"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension Optional {
  a { switch self { case .
  #^^#}
  }
  enum b {
    case c
  }
}
