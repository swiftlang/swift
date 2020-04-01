// RUN: %swift -typecheck -target arm64e-apple-ios12.0 -parse-stdlib -parse-as-library %s -module-name TBDTester -emit-tbd -emit-tbd-path - | %FileCheck %s

public func testSwiftFunc() {}

// CHECK: --- !tapi-tbd-v3
// CHECK: archs: [ arm64e ]
// CHECK: symbols: [ '_$s{{.*}}testSwiftFunc{{.*}}' ]
