// RUN: %target-typecheck-verify-swift \
// RUN:   -parse-stdlib -module-name Swift

@_marker protocol Copyable {}

// This is how Sendable was defined prior to NoncopyableGenerics.
// Notice that there is no ~Copyable or ~Escapable on it. That would normally
// imply that it requires Copyable & Escapable. We don't want that.
//
// In order to allow bootstrapping with hostlibs and various mix-and-matched
// compilers and stdlibs this test ensures that we still treat this legacy
// definition of Sendable as if it does _not_ require Copyable and Escapable.
@_marker protocol Sendable {}

enum E: ~Copyable, Sendable {}
struct S: ~Copyable, Sendable {}

// expected-note@+2 3{{add}}
// expected-error@+1 {{parameter of noncopyable type 'T' must specify ownership}}
func checkGeneric<T>(_ t: T) where T: ~Copyable, T: Sendable {}
