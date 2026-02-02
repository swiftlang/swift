// {"signature":"swift::ProtocolConformanceRef::forEachMissingConformance(llvm::function_ref<bool (swift::BuiltinProtocolConformance*)>) const"}
// RUN: not %target-swift-frontend -typecheck %s
.a == .!   == b /  c
