// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -primary-file %s -module-cache-path %t/mcp -emit-remap-file-path %t/edits.remap -swift-version 4 %api_diff_data_dir
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
// CHECK-NEXT:    "offset": 344,
// CHECK-NEXT:    "text": "SomeStringEnum(rawValue: "
// CHECK:  },
// CHECK:  {
// CHECK:    "file": "{{.*}}rdar31892850.swift",
// CHECK-NEXT:    "offset": 348,
// CHECK-NEXT:    "text": ") ?? <#default value#>"
// CHECK:  }
// CHECK:]
