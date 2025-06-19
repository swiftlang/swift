// {"signature":"std::__1::__function::__func<resolveTypeWitnessViaLookup(swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*)::$_3, std::__1::allocator<resolveTypeWitnessViaLookup(swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*)::$_3>, void (swift::NormalProtocolConformance*)>::~__func()"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b let b c a = b protocol a : a
