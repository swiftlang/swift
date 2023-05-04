// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct Context {
    unowned(unsafe) var x: AnyObject? = nil
}

@inline(never)
func test2(x: Context) -> Context? {
  return .some(x)
}

// CHECK: works
if (test2(x: Context()) == nil) {
    print("bug")
} else {
    print("works")
}
