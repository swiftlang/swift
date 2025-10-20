// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: split-file %s %t

// Build module from interface
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/Foo.swiftmodule/Foo.swiftmodule -module-name Foo %t/Foo.swiftmodule/Foo.swiftinterface -explicit-interface-module-build

// Add to the textual interface
// RUN: echo "struct GoodBye {}" >> %t/Foo.swiftmodule/Foo.swiftinterface

// Build module from interface
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/Foo.swiftmodule/Foo.swiftmodule -module-name Foo %t/Foo.swiftmodule/Foo.swiftinterface -explicit-interface-module-build

// Get the list of strings and ensure the binary module reflects the new addition
// RUN: %llvm-strings %t/Foo.swiftmodule/Foo.swiftmodule > %t/module_strings.txt

// RUN: cat %t/module_strings.txt | %FileCheck %s
// CHECK-DAG: Hello
// CHECK-DAG: GoodBye

//--- Foo.swiftmodule/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Foo
struct Hello {}
