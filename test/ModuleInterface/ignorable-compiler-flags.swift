// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/module-cache)

// RUN: echo "// swift-interface-format-version: 1.0" > %t/textual/Foo.swiftinterface
// RUN: echo "// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Foo" >> %t/textual/Foo.swiftinterface

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/textual -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=USER-MODULE-PRINT-NOT

// RUN: echo "// swift-module-flags-ignorable: -enable-library-evolution -user-module-version 13.13 -future-flag1 3 -future-flag2 abc -future-flag3 /tmp/t.swift /tmp/u.swift -tbd-install_name=aaa" >> %t/textual/Foo.swiftinterface

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/textual -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// USER-MODULE-PRINT-NOT-NOT: user module version: 13.13.0.0
// USER-MODULE-PRINT: user module version: 13.13.0.0
