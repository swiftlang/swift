// {"kind":"complete","signature":"void std::__1::__introsort<std::__1::_ClassicAlgPolicy, printCodeCompletionLookedupTypeNames(llvm::ArrayRef<swift::NullTerminatedStringRef>, llvm::raw_ostream&)::$_0&, swift::NullTerminatedStringRef*, false>(swift::NullTerminatedStringRef*, swift::NullTerminatedStringRef*, printCodeCompletionLookedupTypeNames(llvm::ArrayRef<swift::NullTerminatedStringRef>, llvm::raw_ostream&)::$_0&, std::__1::iterator_traits<swift::NullTerminatedStringRef*>::difference_type, bool)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
$1 == [$1 ]#^^#
