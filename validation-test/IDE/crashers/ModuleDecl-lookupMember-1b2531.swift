// {"kind":"complete","original":"521e07c8","signature":"swift::ModuleDecl::lookupMember(llvm::SmallVectorImpl<swift::ValueDecl*>&, swift::DeclContext*, swift::DeclName, swift::Identifier) const","signatureAssert":"Assertion failed: (privateDiscriminator.empty() && \"unnecessary private discriminator\"), function lookupMember","signatureNext":"Demangle::ASTBuilder::findDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
package actor a {
  private
  b { #^^#
