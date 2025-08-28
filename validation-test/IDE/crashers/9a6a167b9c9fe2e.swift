// {"kind":"complete","signature":"swift::ide::CompletionLookup::addCallArgumentPatterns(swift::ide::CodeCompletionResultBuilder&, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::ParamDecl const*>, swift::GenericSignature, bool)","signatureAssert":"Assertion failed: (declParams.empty() || typeParams.size() == declParams.size()), function addCallArgumentPatterns"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<each b>( repeat each b)c == a(
#^COMPLETE^# d
