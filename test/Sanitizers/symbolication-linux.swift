// RUN: %target-build-swift %s -sanitize=address -g -o %t
// RUN: env %env-ASAN_OPTIONS=abort_on_error=0                           not %target-run %t 2>&1 | %swift-demangle | %FileCheck %s -check-prefix=OOP
// In-process symbolication doesn't work on Linux (yet)
// XXX: env %env-ASAN_OPTIONS=abort_on_error=0,external_symbolizer_path= not %target-run %t 2>&1 | %swift-demangle | %FileCheck %s -check-prefix=IP
// REQUIRES: executable_test
// REQUIRES: asan_runtime
// REQUIRES: OS=linux-gnu

// Check that Sanitizer reports are properly symbolicated on Linux, both
// out-of-process (via `llvm-symbolizer`) and when falling back to in-process
// symbolication.  Note that `llvm-symbolizer` does not demangle Swift symbol
// names, so we use `swift demangle`.

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
// FIXME: There is no instruction with the location of the failing `.` operator and it's used inside implicit setup code, thus the crash is associated with the previous line.
// OOP:      #0 0x{{[0-9a-f]+}} in main.foo() -> () {{.*}}
// FIXME:                                                  symbolication-linux.swift:[[@LINE-14]]
// OOP-NEXT: #1 0x{{[0-9a-f]+}} in main.bar() -> () {{.*}}symbolication-linux.swift:[[@LINE-10]]
// OOP-NEXT: #2 0x{{[0-9a-f]+}} in main {{.*}}symbolication-linux.swift:[[@LINE-7]]

// In-process
// IP:      #0 0x{{[0-9a-f]+}} in main.foo() -> ()+0x
// IP-NEXT: #1 0x{{[0-9a-f]+}} in main.bar() -> ()+0x
// IP-NEXT: #2 0x{{[0-9a-f]+}} in main+0x
