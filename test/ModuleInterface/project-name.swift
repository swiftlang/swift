// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %s -I %t \
// RUN:   -module-name Library -project-name ProjectName \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Library.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Library.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Library.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.private.swiftinterface) -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.package.swiftinterface) -module-name Library

// RUN: %FileCheck %s < %t/Library.swiftinterface --check-prefix CHECK-PUBLIC
// RUN: %FileCheck %s < %t/Library.private.swiftinterface --check-prefix CHECK-NONPUBLIC
// RUN: %FileCheck %s < %t/Library.package.swiftinterface --check-prefix CHECK-NONPUBLIC

// CHECK-PUBLIC-NOT: -project-name

// CHECK-NONPUBLIC: swift-module-flags-ignorable:
// CHECK-NONPUBLIC-SAME: -project-name ProjectName

public func foo() {}
