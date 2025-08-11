// {"kind":"complete","original":"ea806f48","signature":"swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@dynamicMemberLookup
struct a<b{
c: () -> b^subscript<d>(dynamicMember e: WritableKeyPath<b, d>) a<d> }
let binding = a
{ buffer #^^#??
binding.0
