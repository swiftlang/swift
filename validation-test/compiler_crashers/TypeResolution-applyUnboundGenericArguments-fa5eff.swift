// {"kind":"typecheck","signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>) const::$_0 const>(long, swift::SubstitutableType*)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b> {
  typealias c<d> = Int
}
func e(): a.c<Int>
