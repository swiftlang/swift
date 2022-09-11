// RUN: %empty-directory(%t)

// Ensure the attribute is printed in swiftinterface files
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// Ensure the attribute is in .swiftmodule files
// RUN: %target-swift-ide-test -print-module -module-to-print Foo -I %t -source-filename %s -fully-qualified-types -print-access > %t/printed-module.txt
// RUN: %FileCheck %s < %t/printed-module.txt

@_alwaysEmitConformanceMetadata
public protocol TestP {
    static var foo: Int { get }
}


// CHECK: @_alwaysEmitConformanceMetadata public protocol TestP
