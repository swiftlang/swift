// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/main.swift %t/b.swift -I %S/Inputs -o %t/out -cxx-interoperability-mode=upcoming-swift
// RUN: %target-build-swift %t/main.swift %t/b.swift %t/c.swift -I %S/Inputs -o %t/out -cxx-interoperability-mode=upcoming-swift

//--- main.swift
import DefaultArguments
public func foo() {
  let _ = isZero()
}
foo()

//--- b.swift
import DefaultArguments
public func bar() {
  let _ = isZero()
}

//--- c.swift
import DefaultArguments
public func baz() {
  let _ = isZero(123)
}
