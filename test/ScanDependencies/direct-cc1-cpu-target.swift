// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-frontend-plain -target arm64-apple-ios18 -clang-target arm64-apple-ios26 -scan-dependencies -o %t/deps.json -I %t \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-builtin-module \
// RUN:   %t/test.swift -module-name Test -swift-version 5 -experimental-clang-importer-direct-cc1-scan

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json A | %FileCheck %s
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json Test | %FileCheck %s
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:B | %FileCheck %s

/// ios26 is default to a12 CPU and ios18 is a10 CPU. -Xcc is selecting frontend parsing target options so it should be a12.
// CHECK: target-cpu
// CHECK-NEXT: -Xcc
// CHECK-NEXT: apple-a12

// RUN: %{python} %S/../../utils/swift-build-modules.py %swift_frontend_plain %t/deps.json -o %t/Test.cmd

// RUN: %swift-frontend-plain -target arm64-apple-ios18 -clang-target arm64-apple-ios26 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-builtin-module \
// RUN:   %t/test.swift -module-name Test -swift-version 5 -O @%t/Test.cmd -S -o %t/test.s

// RUN: %FileCheck %s --check-prefix A10-CODEGEN < %t/test.s

//--- A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

//--- module.modulemap
module B {
  header "b.h"
  export *
}

//--- b.h
#ifndef __ARM_FEATURE_ATOMICS
#error should use -clang-target, not -target
#endif

//--- test.swift
import A
import B

public func cmpxchg_test(_ ptr: Builtin.RawPointer, a: Builtin.Int8) {
  // A10-CODEGEN-LABEL: _$s4Test12cmpxchg_test_1ayBp_Bi8_tF
  // A10-CODEGEN: ldaxrb
  // A10-CODEGEN: stlxrb
  // A10-CODEGEN-NOT: swpalb
  Builtin.atomicrmw_xchg_seqcst_Int8(ptr, a)
}
