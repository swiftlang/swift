// RUN: %target-swift-emit-ir %s -parse-as-library -enable-experimental-feature Embedded -target arm64-apple-macos14 -wmo -module-name main | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: SWIFT_STDLIB_ARCH=arm64
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// A single-payload enum is non-`Copyable` when its payload is, even if the
// enum itself is only conditionally Copyable, as Optional is. Embedded Swift
// emits Optional's value witness table statically, and it used to describe
// Optional<NC> as copyable (no IsNonCopyable flag, real copy witnesses).

public struct NC: ~Copyable { var x = 1 }

@inline(never) public func use(_ o: consuming NC?) { _ = consume o }
@inline(never) public func wrap<T: ~Copyable>(_ t: consuming T) -> any ~Copyable { t }
public func go() { _ = wrap(Optional<NC>.some(NC())) }

// The copy witnesses refuse to copy, and the flags (0xa00007) include
// IsNonCopyable (0x800000).
// CHECK: @"$e4main2NCVSgWV" = {{.*}} { ptr @__swift_cannot_copy_noncopyable_type, ptr @__swift_noop_void_return, ptr @__swift_cannot_copy_noncopyable_type, ptr @__swift_cannot_copy_noncopyable_type, {{.*}}, i32 10485767,
