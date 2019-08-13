// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

@differentiable
func foo(_ x: Float) -> Float { x }

// CHECK-LABEL: // VJP wrt 0 source 0 for foo(_:)
// CHECK-NEXT: sil hidden @$s8mangling3fooyS2fFTZp0r0

// CHECK-LABEL: // pullback wrt 0 source 0 for foo(_:)
// CHECK-NEXT: sil hidden @$s8mangling3fooyS2fFTUp0r0

// CHECK-LABEL: // JVP wrt 0 source 0 for foo(_:)
// CHECK-NEXT: sil hidden @$s8mangling3fooyS2fFTzp0r0

// CHECK-LABEL: // differential wrt 0 source 0 for foo(_:)
// CHECK-NEXT: sil hidden @$s8mangling3fooyS2fFTup0r0

// Test case where original function name cannot be demangled.
// (i.e. test custom original function name via `@_silgen_name`.)
@differentiable
@_silgen_name("bar")
func bar(_ x: Float) -> Float { x }

// CHECK-LABEL: // VJP wrt 0 source 0 for bar
// CHECK-NEXT: sil hidden @$s3barTZp0r0

// CHECK-LABEL: // pullback wrt 0 source 0 for bar
// CHECK-NEXT: sil hidden @$s3barTUp0r0

// CHECK-LABEL: // JVP wrt 0 source 0 for bar
// CHECK-NEXT: sil hidden @$s3barTzp0r0

// CHECK-LABEL: // differential wrt 0 source 0 for bar
// CHECK-NEXT: sil hidden @$s3barTup0r0
