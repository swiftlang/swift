// RUN: %target-swift-frontend %s -Onone -emit-sil -emit-sorted-sil                       | %FileCheck %s -check-prefix=REGULAR
// RUN: %target-swift-frontend %s -Onone -emit-sil -emit-sorted-sil -ignore-always-inline | %FileCheck %s -check-prefix=IGNORED

@inline(__always)
func foo() {
}

// REGULAR: {{^}}@inline(__always) func foo()
// REGULAR: sil hidden [always_inline] @$s4main3fooyyF

// IGNORED: {{^}}@inline(__always) func foo()
// IGNORED: sil hidden @$s4main3fooyyF
