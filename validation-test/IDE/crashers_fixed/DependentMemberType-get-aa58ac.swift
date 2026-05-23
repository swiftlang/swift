// {"kind":"complete","original":"9e6fdf3a","signature":"swift::DependentMemberType::get(swift::Type, swift::AssociatedTypeDecl*)","signatureAssert":"Assertion failed: (assocType && \"Missing associated type\"), function get","signatureNext":"GenericSignatureImpl::getUpperBound"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b
  protocol c
    func d {
      #^^#
      func e -> [some a: c]
