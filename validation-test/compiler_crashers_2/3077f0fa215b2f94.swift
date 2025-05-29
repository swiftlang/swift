// {"signature":"swift::Type llvm::function_ref<swift::Type (swift::SubstitutableType*)>::callback_fn<swift::TypeResolution::applyUnboundGenericArguments(swift::GenericTypeDecl*, swift::Type, swift::SourceLoc, llvm::ArrayRef<swift::Type>) const::$_0 const>(long, swift::SubstitutableType*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a extension a {
  class b {
    struct c < f struct d func e->c<d>
