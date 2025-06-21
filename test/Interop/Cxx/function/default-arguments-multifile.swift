// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/main.swift %t/b.swift -I %S/Inputs -o %t/out -cxx-interoperability-mode=upcoming-swift

//--- main.swift
import DefaultArguments
func foo() {
  let _ = isZero()
}
foo()

//--- b.swift
import DefaultArguments
func bar() {
  let _ = isZero()
}
