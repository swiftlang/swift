// {"kind":"complete","original":"60f1254a","signature":"swift::StorageVisitor::visit(swift::NominalTypeDecl*, swift::DeclContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a<b>() =
  {
    enum c {
      #^^#      case (b)
    }
  }
