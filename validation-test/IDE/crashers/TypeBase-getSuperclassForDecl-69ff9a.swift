// {"kind":"complete","original":"139d8a94","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"TypeBase::getContextSubstitutions"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a
  class b<c {
    g -> d
  }
  class e<f>: b
    extension a where Self: e<Int> {
      #^^#
