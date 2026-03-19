// {"kind":"complete","languageMode":6,"original":"ef8f1d69","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (func->isOperator() && \"Lookup should only find operators\"), function getTypeOfReferencePre","signatureNext":"ConstraintSystem::prepareOverloadImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
class a {
  var b = {
    func c() {
      switch <#expression#> {
      case "":
        Bool.random
      case #^^#:
      }
    }
  }
}
