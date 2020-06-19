// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | grep 'check-prefix' > %t/prefix-option
// RUN: %target-run %t/a.out 2>&1 >/dev/null | %FileCheck `cat %t/prefix-option` %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// This test doesn't use StdlibUnittest because it's primarily concerned with
// checking the presence and absence of output.

// A tricky way to make the FileCheck tests conditional on the OS version.
if #available(macOS 9999, *) {
  print("-check-prefix=CHECK")
} else {
  print("-check-prefix=DONT-CHECK")
  // Need at least one check, otherwise FileCheck will complain.
  // DONT-CHECK: {{.}}
}

if #available(macOS 9999, *) {
  _setMetaDataDebugging(logLevel: 2)
}

struct S<T> {
}

class K<T> {
}

@_optimize(none)
func test_tuple<T>(_ t: T) -> Any {
    return (t, t)
}

// CHECK: ### test.S<Swift.Float>
// CHECK: {{^[0-9]+ +a.out +}}
public let s: Any = S<Float>()

// CHECK: ### (Swift.Int, Swift.Int)
// CHECK: {{^[0-9]+ +a.out +}}
public let t: Any = test_tuple(27)

// CHECK: ### test.K<Swift.Int>
// CHECK: {{^[0-9]+ +a.out +}}
public let k: Any = K<Int>()

