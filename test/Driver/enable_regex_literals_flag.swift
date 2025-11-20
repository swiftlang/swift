// RUN: %target-swiftc_driver  -enable-bare-slash-regex -disallow-use-new-driver -driver-print-jobs  %s 2>^1 | %FileCheck %s
// The new driver has its own test for this

// REQUIRES: cplusplus_driver

// CHECK: {{.*}}swift{{c|c-legacy-driver|-frontend}}{{(.exe)?"?}} -frontend{{.*}}-enable-bare-slash-regex

public func foo() -> Int {
    return 42
}
