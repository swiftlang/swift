
// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -module-name Bar %t/Bar.swift -emit-module -emit-module-path %t/Bar.swiftmodule
// RUN: test -f %t/Bar.swiftmodule

// Accessing a package decl should fail without @package import
// RUN: not %target-swift-frontend -typecheck %t/Foo1.swift -I %t 2> %t/Foo1.output
// RUN: %FileCheck %s -input-file %t/Foo1.output -check-prefix CHECK-ERROR
// CHECK-ERROR: error: cannot find 'world' in scope

// Should pass with -package-modules to import Bar as a package module
// RUN: %target-swift-frontend -typecheck %t/Foo1.swift -I %t -package-modules Bar -c -o %t/Foo1.o
// RUN: %llvm-nm %t/Foo1.o | %FileCheck %s --check-prefix=CHECK-NM
// CHECK-NM: s3Bar5helloyyF
// CHECK-NM: s3Bar5worldyyF
// CHECK-NM: s4Foo14showyyF

// Should pass with @package import Bar
// RUN: %target-swift-frontend -typecheck %t/Foo2.swift -I %t -c -o %t/Foo2.o
// RUN: %llvm-nm %t/Foo2.o | %FileCheck %s --check-prefix=CHECK-NM
// CHECK-NM: s3Bar5helloyyF
// CHECK-NM: s3Bar5worldyyF
// CHECK-NM: s4Foo24showyyF

// BEGIN Foo1.swift
import Bar
public func show() {
    hello()
    // pass if -package-modules is passed, else
    world()
}

// BEGIN Foo2.swift
@package import Bar
public func show() {
    hello()
    world()
}

// BEGIN Bar.swift
public func hello() {}

@package
public func world() {}


