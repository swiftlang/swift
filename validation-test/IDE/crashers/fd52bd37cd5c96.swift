// {"kind":"complete","original":"ea806f48","signature":"swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const"}
// The issue here is that the solver attempts to recursively apply the same
// dynamic member lookup until eventually it overflows the stack. Make sure
// we either timeout or crash.
// RUN: not %{python} %S/../../../test/Inputs/timeout.py 60 \
// RUN:   %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s || \
// RUN:   not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@dynamicMemberLookup
struct a<b{
c: () -> b^subscript<d>(dynamicMember e: WritableKeyPath<b, d>) a<d> }
let binding = a
{ buffer #^^#??
binding.0
