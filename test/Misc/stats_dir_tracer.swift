// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swiftc_driver -o %t/main -module-name main -stats-output-dir %t %s -trace-stats-events
// RUN: %FileCheck -input-file %t/*.csv %s

// CHECK: {{"Sema.NumTypesDeserialized"}}

public func foo() {
    print("hello")
}
