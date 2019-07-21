// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -primary-file %s -module-cache-path %t/mcp -emit-remap-file-path %t/edits.remap -swift-version 4
// RUN: %FileCheck %s -input-file=%t/edits.remap

enum SomeStringEnum : String {
  case val = ""
}

#if swift(>=4.2)
func foo() {
  let e : SomeStringEnum = "aa"
}
#endif

// CHECK:[
// CHECK:  {
// CHECK:    "file": "{{.*}}rdar31892850.swift",
// CHECK:    "offset": 325,
// CHECK:    "text": "SomeStringEnum(rawValue: "
// CHECK:  },
// CHECK:  {
// CHECK:    "file": "{{.*}}rdar31892850.swift",
// CHECK:    "offset": 329,
// CHECK:    "text": ")"
// CHECK:  }
// CHECK:]
