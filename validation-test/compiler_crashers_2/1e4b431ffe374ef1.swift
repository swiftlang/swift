// {"kind":"typecheck","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<b {
  init(() -> b, subscript
   <c>(dynamicMember d: WritableKeyPath<b, c>)
  a<c>
}
let binding = a { ?? buffer ?? binding.e
