// {"kind":"complete","original":"65b0d821","signature":"swift::ModuleDecl::lookupMember(llvm::SmallVectorImpl<swift::ValueDecl*>&, swift::DeclContext*, swift::DeclName, swift::Identifier) const","signatureAssert":"Assertion failed: (privateDiscriminator.empty() && \"unnecessary private discriminator\"), function lookupMember","signatureNext":"Demangle::ASTBuilder::findTypeDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
package struct a: Codable {
  var b: #^^#
}
