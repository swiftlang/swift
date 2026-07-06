// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: OS=macosx

// RUN: %swift -emit-ir -sanitize=memtag-stack -parse-as-library -target arm64-apple-macosx10.9 %s | %FileCheck %s

// Check that functions have the sanitize_memtag attribute when -sanitize=memtag-stack is enabled

// CHECK-LABEL: define {{.*}} @"${{.*}}testFunction{{.*}}"
// CHECK-SAME: [[ATTRS:#[0-9]+]]
// CHECK: attributes [[ATTRS]] = {{.*}} sanitize_memtag

func testFunction() -> Int {
  return 42
}
