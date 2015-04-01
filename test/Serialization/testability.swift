// RUN: rm -rf %t && mkdir %t

// This test has two purposes. This first block just tests that we serialize
// the -enable-testing flag correctly...

// RUN: %target-swift-frontend -emit-module -DBASE -o %t %s
// RUN: llvm-bcanalyzer -dump %t/testability.swiftmodule > %t/testability.dump.txt
// RUN: FileCheck -check-prefix=CHECK -check-prefix=NO-TESTING %s < %t/testability.dump.txt

// RUN: %target-swift-frontend -emit-module -DBASE -o %t -enable-testing %s
// RUN: llvm-bcanalyzer -dump %t/testability.swiftmodule > %t/testability2.dump.txt
// RUN: FileCheck -check-prefix=CHECK -check-prefix=TESTING %s < %t/testability2.dump.txt
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/testability2.dump.txt

// ...but the second block checks that a module that /depends/ on a testable
// module can still be loaded. This is what happens when debugging a unit test.

// RUN: %target-swift-frontend -emit-module -DSUB -o %t -enable-testing %s -module-name testability_client -I %t
// RUN: %target-swift-frontend -emit-sil -DMAIN %s -module-name main -I %t > /dev/null
// RUN: %target-swift-frontend -emit-sil -DMAIN -DTESTABLE %s -module-name main -I %t > /dev/null
// RUN: %target-swift-frontend -emit-sil -DMAIN -DDEBUG -disable-access-control %s -module-name main -I %t > /dev/null

// CHECK: <MODULE_BLOCK {{.*}}>
// TESTING: <IS_TESTABLE abbrevid={{[0-9]+}}/>
// NO-TESTING-NOT: IS_TESTABLE
// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode

#if BASE

  internal class Base {}

#elseif SUB

  @testable import testability

  class Sub : Base {}
  
  public func unrelated() {}

#elseif MAIN

  #if TESTABLE
    @testable import testability_client
  #else 
    import testability_client
  #endif

  #if TESTABLE || DEBUG
    let x: Sub? = nil
  #endif

  unrelated()

#else

  let _: INVALID_TESTING_MODE

#endif
