// {"kind":"complete","original":"7fd0164b","signature":"swift::ide::ArgumentTypeCheckCompletionCallback::addPossibleParams(swift::ide::ArgumentTypeCheckCompletionCallback::Result const&, llvm::SmallVectorImpl<swift::ide::PossibleParamInfo>&, llvm::SmallVectorImpl<swift::Type>&)","signatureAssert":"Assertion failed: (start <= end && \"Invalid integral range\"), function range","signatureNext":"ArgumentTypeCheckCompletionCallback::collectResults"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<each b>(c: repeat each b
repeat each b
) a(1
#^^#
