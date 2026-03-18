// {"kind":"complete","original":"94a984af","signature":"swift::ide::CodeCompletionStringBuilder::addCallArgumentPatterns(llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::ParamDecl const*>, swift::DeclContext const*, swift::GenericSignature, swift::ide::DefaultArgumentOutputMode, bool)","signatureAssert":"Assertion failed: (declParams.empty() || typeParams.size() == declParams.size()), function addCallArgumentPatterns","signatureNext":"CompletionLookup::addEnumElementRef"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<each b> {
  case (repeat each b)
}
a<
>#^^#
