// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %target-typecheck-verify-swift -target %target-stable-abi-triple \
// RUN:   -plugin-path %swift-plugin-dir \
// RUN:   -import-bridging-header %S/Inputs/safe_c_implementation.h \
// RUN:   -enable-experimental-feature SafeInteropImplementations \
// RUN:   -enable-experimental-feature SafeInteropWrappers \
// RUN:   -disable-objc-interop

// `@implementation(safe)` without any language attribute hits the existing
// "must specify the language" diagnostic before the safe-mode check runs.
@implementation(safe)
// expected-error@-1 {{'@implementation' used without specifying the language being implemented}}
func no_lang(_ x: CInt) {}

// `@implementation(safe)` rejects `@_cdecl` (the underscored variant), since
// `@_cdecl` produces both a Swift and a C entry point.
@_cdecl("under_c") @implementation(safe)
// expected-error@-1 {{'@implementation(safe)' requires a non-underscored '@c' attribute on the same global function}}
func under_c(_ x: CInt) {}

// `@implementation(safe)` on a function whose matching C header declaration
// has no bounds/lifetime annotations should produce a warning explaining the
// no-op.
@c @implementation(safe)
// expected-warning@-1 {{has no effect}}
func plain_func(_ x: CInt) -> CInt { return x }

// A function carrying `@implementation(safe)` for a C function with
// `__counted_by` accepts a matching `Span` parameter without triggering the
// usual `@c`-cannot-represent error, gets an `@_Unswiftify` macro attached,
// and the macro expands into the `@c @implementation` peer wrapper.
@c @implementation(safe)
public func buffered_func(_ p: Span<CInt>) {}

// `@implementation(safe)` on a function with no matching C declaration in the
// header should diagnose like `@implementation` without `(safe)`.
@c @implementation(safe)
func nonexistent_func() {}
// expected-error@-2 {{could not find imported function 'nonexistent_func' matching global function 'nonexistent_func()'; make sure you import the module or header that declares it}}
