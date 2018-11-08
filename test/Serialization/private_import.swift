// RUN: %empty-directory(%t)

// This test has two purposes. This first block just tests that we serialize
// the -enable-private-imports flag correctly...

// RUN: %target-swift-frontend -emit-module -DBASE -o %t %s
// RUN: llvm-bcanalyzer -dump %t/private_import.swiftmodule > %t/private_import.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=NO-PRIVATE-IMPORT %s < %t/private_import.dump.txt

// RUN: %target-swift-frontend -emit-module -DBASE -o %t -enable-private-imports %s
// RUN: llvm-bcanalyzer -dump %t/private_import.swiftmodule > %t/private_import.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=PRIVATE-IMPORT %s < %t/private_import.dump.txt
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/private_import.dump.txt

// RUN: %target-swift-frontend -emit-module -DCLIENT -o %t -enable-private-imports %s -module-name client -I %t
// RUN: %target-swift-frontend -emit-sil -DMAIN %s -module-name main -I %t > /dev/null

// CHECK: <MODULE_BLOCK {{.*}}>
// PRIVATE-IMPORT: <ARE_PRIVATE_IMPORTS_ENABLED abbrevid={{[0-9]+}}/>
// NO-PRIVATE-IMPORT-NOT: ARE_PRIVATE_IMPORTS_ENABLED
// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode

#if BASE

  private struct Base {
  }

#elseif  CLIENT

  @_private(sourceFile: "private_import.swift") import private_import

  extension Base {
    private func foo() {}
  }
  public func unreleated() {}

#elseif MAIN

  @_private(sourceFile: "private_import.swift") import private_import
  @_private(sourceFile: "private_import.swift") import client

  Base().foo()

  unreleated()
#endif
