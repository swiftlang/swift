// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -target armv7em-none-none-eabi -parse-as-library -module-name main -O -c -num-threads 2 %t/a.swift %t/b.swift -o %t/a.o -o %t/b.o -enable-experimental-feature Embedded
// RUN: llvm-objdump -r %t/b.o | %FileCheck -check-prefix B-OBJDUMP %s

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

//--- a.swift

public struct X {
}

public protocol P {
  func f()
}

extension X: P {
  public func f() { }
}


//--- b.swift
// B-OBJDUMP: R_ARM_REL32 $e4main1XVMf
// B-OBJDUMP: R_ARM_REL32 $e4main1XVAA1PAAWP
public func getP() -> any P {
  return X()
}

