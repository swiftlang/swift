// RUN: %target-swift-frontend -emit-ir -module-name main -sanitize=memtag-stack \
// RUN:   -target arm64-apple-macos15 \
// RUN:   %s | %FileCheck %s

// REQUIRES: CODEGENERATOR=AArch64

// CHECK: define {{.*}}@"$s4main7withTagSiyF"() [[TAG:#[0-9]+]]
public func withTag() -> Int { 0 }

// CHECK: define {{.*}}@"$s4main5noTagSiyF"() [[NO_TAG:#[0-9]+]]
@instrumentation(disable: memtagStack)
public func noTag() -> Int { 1 }

// CHECK: attributes [[TAG]] = {{{.*}}sanitize_memtag
// CHECK-NOT: attributes [[NO_TAG]] = {{{.*}}sanitize_memtag
