// {"kind":"emit-ir","signature":"std::__1::optional<swift::Type> llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*, swift::TypePosition)>::callback_fn<swift::rewriting::PropertyMap::getTypeFromSubstitutionSchema(swift::Type, llvm::ArrayRef<swift::rewriting::Term>, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::MutableTerm const&) const::$_0>(long, swift::TypeBase*, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
class a<b> {
}
protocol c {
  associatedtype f: a<d>
  associatedtype d
}
extension c where Self == f, f == d {
  func e() {
  }
}
