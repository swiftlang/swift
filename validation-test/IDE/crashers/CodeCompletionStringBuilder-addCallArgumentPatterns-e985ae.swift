// {"kind":"complete","original":"78722387","signature":"swift::ide::CodeCompletionStringBuilder::addCallArgumentPatterns(llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::ParamDecl const*>, swift::DeclContext const*, swift::GenericSignature, swift::ide::DefaultArgumentOutputMode, bool)","signatureAssert":"Assertion failed: (declParams.empty() || typeParams.size() == declParams.size()), function addCallArgumentPatterns","signatureNext":"CompletionLookup::addSubscriptCallPattern"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a
  struct b {
    subscript<each c>(repeat each c)  a
    func d(e: b) {
      (e[#^^#
      0
