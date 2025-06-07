// RUN: %swift -typecheck -target arm64e-apple-ios12.0 -parse-stdlib -parse-as-library %s -module-name TBDTester -emit-tbd -emit-tbd-path %t/arm64e.tbd  -tbd-install_name arm64e 
// RUN: %llvm-nm -arch arm64e %t/arm64e.tbd | %FileCheck %s

public func testSwiftFunc() {}


// CHECK: _$s{{.*}}testSwiftFunc{{.*}}
