// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/sources)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/alternative-inputs)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/outputs)

// RUN: echo "public func foo() {}" > %t/sources/Foo.swift
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/inputs/Foo.swiftinterface %t/sources/Foo.swift -module-name Foo -disable-implicit-concurrency-module-import -enable-library-evolution -module-cache-path %t/module-cache -I %t/inputs -swift-version 5
// RUN: cp %t/inputs/Foo.swiftinterface %t/alternative-inputs/Foo.swiftinterface
// RUN: echo "mess_mess_mess" >> %t/inputs/Foo.swiftinterface
// RUN: not %target-swift-frontend -compile-module-from-interface -module-name Foo %t/inputs/Foo.swiftinterface -o %t/outputs/Foo.swiftmodule

// RUN: %target-swift-frontend -compile-module-from-interface -module-name Foo %t/inputs/Foo.swiftinterface -o %t/outputs/Foo.swiftmodule -backup-module-interface-path %t/alternative-inputs
