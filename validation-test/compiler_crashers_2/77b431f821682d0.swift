// {"signature":"swift::ProtocolConformanceRef::forEachMissingConformance(llvm::function_ref<bool (swift::BuiltinProtocolConformance*)>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: rdar152763265
.a == .!   == b /  c
