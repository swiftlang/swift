// Tests for location marker error handling in the Swift frontend's `-verify` mode.

// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s

// Duplicate marker definition.
// Marker scan errors are emitted before expected-diagnostic parsing errors.
func dup1() {} // #dup
// CHECK: [[@LINE+1]]:19: error: location marker '#dup' already defined
func dup2() {} // #dup

// Use of undefined marker.
// CHECK: [[@LINE+1]]:19: error: use of undefined location marker '#undefined'
// expected-error@#undefined {{some error}}

// Empty marker name after '#'.
// CHECK: [[@LINE+1]]:19: error: expected marker name after '#'
// expected-error@# {{some error}}

// Comment with extra text after #name is NOT a marker definition.
// #notAMarker this has extra text
// CHECK: [[@LINE+1]]:19: error: use of undefined location marker '#notAMarker'
// expected-error@#notAMarker {{some error}}
