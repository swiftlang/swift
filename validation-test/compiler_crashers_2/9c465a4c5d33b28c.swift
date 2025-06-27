// {"signature":"void llvm::function_ref<void (llvm::raw_ostream&)>::callback_fn<getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)::$_0::operator()(swift::GenericTypeParamType*) const::'lambda'(auto&)>(long, llvm::raw_ostream&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b { extension a!
