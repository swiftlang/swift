// RUN: %target-swift-frontend -typecheck -enable-experimental-feature VariadicGenerics -enable-experimental-feature UnknownFeature %s

// REQUIRES: asserts

// Make sure definition is defined for variadic generics
#if $VariadicGenerics
// okay
#else
let x = BOOM
#endif

// Use variadic generics
func debugPrint<@_typeSequence T>(_ items: T...)
  where T: CustomDebugStringConvertible
{
}
