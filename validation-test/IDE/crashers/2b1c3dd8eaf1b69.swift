// {"kind":"complete","signature":"swift::ide::IDEInspectionInstance::performCachedOperationIfPossible(llvm::hash_code, llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>, swift::SearchPathOptions const&, llvm::MemoryBuffer*, unsigned int, swift::DiagnosticConsumer*, std::__1::shared_ptr<std::__1::atomic<bool>>, llvm::function_ref<void (swift::ide::CancellableResult<swift::ide::IDEInspectionInstanceResult>)>)","signatureAssert":"Assertion failed: (reparsedState->hasIDEInspectionDelayedDeclState() && \"Didn't find IDE inspection point?\"), function performCachedOperationIfPossible","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
#^^#
#if a)#^COMPLETE3^#
var
