// {"kind":"typecheck","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)"}
// RUN: not %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct S<T> {
  init(_: () -> T) {}
  subscript<U>(dynamicMember d: WritableKeyPath<T, U>) -> S<U> {}
}
let x = S { x.e }
