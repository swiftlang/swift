// RUN: %target-swiftc_driver  -enable-regex-literals -disallow-use-new-driver -driver-print-jobs  %s 2>^1 | %FileCheck %s
// The new driver has its own test for this

// REQUIRES: cplusplus_driver

// CHECK: {{.*}}swift{{c|-frontend}}{{(.exe)?"?}} -frontend{{.*}}-enable-regex-literals

public func foo() -> Int {
    return 42
}
