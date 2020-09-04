// RUN: %target-build-swift %s -sanitize=address -g -target %sanitizers-target-triple -o %t
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0                           not %target-run %t 2>&1 | %FileCheck %s -check-prefix=OOP
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0,external_symbolizer_path= not %target-run %t 2>&1 | %FileCheck %s -check-prefix=IP
// REQUIRES: executable_test
// REQUIRES: asan_runtime
// REQUIRES: VENDOR=apple

// REQUIRES: rdar68353068

// Check that Sanitizer reports are properly symbolicated on Apple platforms,
// both out-of-process (via `atos`) and when falling back to in-process
// symbolication.  Note that `atos` also demangles Swift symbol names.

@inline(never)
func foo() {
  let x = UnsafeMutablePointer<Int>.allocate(capacity: 1)
  x.deallocate()
  print(x.pointee)
}

@inline(never)
func bar() {
  foo()
  print("Prevent tail call optimization")
}

bar()

// Out-of-process
// OOP:      #0 0x{{[0-9a-f]+}} in foo() symbolication.swift:[[@LINE-12]]
// OOP-NEXT: #1 0x{{[0-9a-f]+}} in bar() symbolication.swift:[[@LINE-8]]
// OOP-NEXT: #2 0x{{[0-9a-f]+}} in main symbolication.swift:[[@LINE-5]]

// In-process
// IP:      #0 0x{{[0-9a-f]+}} in main.foo() -> ()+0x
// IP-NEXT: #1 0x{{[0-9a-f]+}} in main.bar() -> ()+0x
// IP-NEXT: #2 0x{{[0-9a-f]+}} in main+0x
