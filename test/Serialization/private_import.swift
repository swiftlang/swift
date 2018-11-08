// RUN: %empty-directory(%t)

// This test has two purposes. This first block just tests that we serialize
// the -enable-private-imports flag correctly...

// RUN: %target-swift-frontend -emit-module -DBASE -o %t %s
// RUN: llvm-bcanalyzer -dump %t/private_import.swiftmodule > %t/private_import.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=NO-PRIVATE-IMPORT %s < %t/private_import.dump.txt

// RUN: %target-build-swift -module-name private_import -emit-module -o %t -enable-private-imports %S/Inputs/private_import_other.swift %S/Inputs/private_import_other_2.swift
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
#elseif  CLIENT

  @_private(sourceFile: "private_import_other.swift") import private_import

  extension Base {
    private func foo() {}
  }

  extension Base {
    // This should not cause a failure.
    private func shouldNotBeVisible() {}
  }

  public func unreleated() {}

	// This should not conflict with Other from private_import_other_2.swift.
  struct Other {}
#elseif MAIN

  @_private(sourceFile: "private_import_other.swift") import private_import
  @_private(sourceFile: "private_import.swift") import client

  Base().foo()

  unreleated()
#endif
