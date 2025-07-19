// {"kind":"typecheck","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)"}
// The issue here is that the solver attempts to recursively apply the same
// dynamic member lookup until eventually it overflows the stack. Make sure
// we either timeout or crash.
// RUN: not %{python} %S/../../test/Inputs/timeout.py 60 %target-swift-frontend -typecheck %s || \
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct S<T> {
  init(_: () -> T) {}
  subscript<U>(dynamicMember d: WritableKeyPath<T, U>) -> S<U> {}
}
let x = S { x.e }
