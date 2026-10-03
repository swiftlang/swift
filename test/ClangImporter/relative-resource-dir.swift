// The system overlay exists only in ClangImporter's in-memory filesystem. Its
// relative path must remain readable after that filesystem is added to the
// overlay stack and inherits the base filesystem's working directory.
// Exercise both an explicit Clang working directory and the process directory.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/resources
// RUN: touch %t/sdk/usr/include/inttypes.h %t/sdk/usr/include/stdint.h
// RUN: %swift_frontend_plain -typecheck -parse-stdlib -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir resources -Xcc -working-directory -Xcc %t -module-cache-path %t/explicit-module-cache -dump-clang-diagnostics %t/main.swift 2>&1 | tee /dev/stderr | %FileCheck %s
// RUN: cd %t && %swift_frontend_plain -typecheck -parse-stdlib -target x86_64-unknown-linux-gnu -sdk %t/sdk -resource-dir resources -module-cache-path %t/module-cache -dump-clang-diagnostics main.swift 2>&1 | tee /dev/stderr | %FileCheck %s

// CHECK: clang importer driver args:
// CHECK-SAME: '-ivfsoverlay' 'resources{{/|\\}}<clang-system-vfs-overlay>'

//--- sdk/usr/include/unistd.h

//--- sdk/usr/lib/swift/linux/x86_64/glibc.modulemap
module SwiftGlibc [system] {
  header "SwiftGlibc.h"
  export *
}

//--- sdk/usr/lib/swift/linux/x86_64/SwiftGlibc.h
void relative_resource_dir(void);

//--- main.swift
import SwiftGlibc
