// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/artifacts

// Multiple usages in the same module.
// RUN: %target-build-swift %t/main.swift %t/b.swift %t/c.swift -cxx-interoperability-mode=upcoming-swift -I %S/Inputs -o %t/artifacts/out
// RUN: %empty-directory(%t/artifacts)

// Multiple usages across different modules.
// RUN: %target-build-swift -emit-library -module-name BarLibrary -emit-module -emit-module-path %t/artifacts/BarLibrary.swiftmodule %t/b.swift %t/c.swift -cxx-interoperability-mode=upcoming-swift -I %S/Inputs -o %t/artifacts/%target-library-name(BarLibrary)
// RUN: %target-build-swift %t/uses-library.swift -cxx-interoperability-mode=upcoming-swift -I %S/Inputs -I %t/artifacts -L %t/artifacts -lBarLibrary -o %t/artifacts/uses-library

//--- main.swift
import DefaultArguments
public func foo() {
  let _ = isZero()
}
foo()
bar()
baz()

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

//--- uses-library.swift
import DefaultArguments
import BarLibrary

let _ = isZero()
bar()
baz()