public func retRegex() -> Regex<Substring> {
  /foo/
}

// REQUIRES: swift_swift_parser

// RUN: %sourcekitd-test -req=sema %s -- %s -Xfrontend -enable-bare-slash-regex -Xfrontend -disable-availability-checking | %FileCheck %s

// CHECK: [
// CHECK:   {
// CHECK:     key.kind: source.lang.swift.ref.struct
// CHECK:     key.offset: 26
// CHECK:     key.length: 5
// CHECK:     key.is_system: 1
// CHECK:   },
// CHECK:   {
// CHECK:     key.kind: source.lang.swift.ref.struct
// CHECK:     key.offset: 32
// CHECK:     key.length: 9
// CHECK:     key.is_system: 1
// CHECK:   }
// CHECK: ]

// Ensure there's no diagnostics
// CHECK-NOT: key.severity:
