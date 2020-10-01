// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// This is an end-to-end test for SR-9627.

@inline(never)
func save(value: Double?) {
   var params: [[String : Any]] = [["a": 0]]
   params = [[
     "b": 0,
     "c": value.map({ String.init($0) }) as Any
   ]]
}

save(value: 0)

// CHECK: ok
print("ok")
