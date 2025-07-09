// {"kind":"complete","signature":"void std::__1::__introsort<std::__1::_ClassicAlgPolicy, printCodeCompletionLookedupTypeNames(llvm::ArrayRef<swift::NullTerminatedStringRef>, llvm::raw_ostream&)::$_0&, swift::NullTerminatedStringRef*, false>(swift::NullTerminatedStringRef*, swift::NullTerminatedStringRef*, printCodeCompletionLookedupTypeNames(llvm::ArrayRef<swift::NullTerminatedStringRef>, llvm::raw_ostream&)::$_0&, std::__1::iterator_traits<swift::NullTerminatedStringRef*>::difference_type, bool)","useGuardMalloc":true}
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
// REQUIRES: no_asan
// REQUIRES: target-same-as-host
struct a<each b: Collection
{
c: (repeat each b.Index
func == {
repeat each d == (each c)#^^#
