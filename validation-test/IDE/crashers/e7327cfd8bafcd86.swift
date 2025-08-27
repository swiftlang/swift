// {"kind":"complete","original":"bc7ab9d8","signature":"swift::constraints::ConstraintSystem::getTypeOfReference(swift::ValueDecl*, swift::FunctionRefInfo, swift::constraints::ConstraintLocatorBuilder, swift::DeclContext*, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (func->isOperator() && \"Lookup should only find operators\"), function getTypeOfReference"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  func a(query: String) {
    {
      "\(query)"
      break #^^#
    }
  }
}
