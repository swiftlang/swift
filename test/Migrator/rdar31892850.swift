// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -typecheck -primary-file %s -module-cache-path %t/mcp -emit-remap-file-path %t/edits.remap
// RUN: diff -u %t/edits.remap %S/rdar31892850.remap

// REQUIRES: rdar_31897099
enum SomeStringEnum : String {
  case val = ""
}

#if swift(>=4)
func foo() {
  let e : SomeStringEnum = "aa"
}
#endif
