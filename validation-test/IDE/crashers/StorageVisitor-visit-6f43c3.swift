// {"kind":"complete","original":"81d68486","signature":"swift::StorageVisitor::visit(swift::NominalTypeDecl*, swift::DeclContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"checkBitwiseCopyableInstanceStorage"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b
  extension a: c  [ {
    enum d: Error {
      case (b)
      #^^#
