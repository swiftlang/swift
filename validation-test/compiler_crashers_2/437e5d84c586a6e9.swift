// {"signature":"std::__1::optional<swift::Type> llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>::callback_fn<swift::constraints::FailureDiagnostic::resolveType(swift::Type, bool, bool) const::$_0>(long, swift::TypeBase*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
Array(repeat a
