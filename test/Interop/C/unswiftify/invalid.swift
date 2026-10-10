// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_CxxImplementation

// RUN: %target-verify-unswiftify %s -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature CxxImplementation -cxx-interoperability-mode=default

@implementation
// expected-error@-1 {{'@implementation' used without specifying the language being implemented}}
func no_lang(_ p: Span<CInt>) {}

@_cdecl("under_c") @implementation
// expected-error@-1 {{'@implementation' of global function 'under_c' with a safe-interop parameter or result type requires a non-underscored '@c' attribute}}
func under_c(_ p: Span<CInt>) {}

@cxx @implementation
// expected-error@-1 {{'@implementation' of global function 'cxx' with a safe-interop parameter or result type requires a non-underscored '@c' attribute}}
func cxx(_ p: Span<CInt>) {}
