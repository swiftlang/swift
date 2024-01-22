// RUN: split-file --leading-lines %s %t

//--- input.swift

func foo() -> Int { 1 }
func foo() -> String { "" }
func test() {
// RUN: %sourcekitd-test \
// RUN:   -req=open %t/memory-only.swift -text-input %t/input.swift -print-raw-response -- %t/memory-only.swift == \
// RUN:   -req=cursor -pos=%(line + 1):7 %t/memory-only.swift -text-input %t/input.swift -- %t/memory-only.swift | %FileCheck %s
  _ = foo()
}

// CHECK: () -> Int
// CHECK-LABEL: SECONDARY SYMBOLS BEGIN
// CHECK: () -> String
