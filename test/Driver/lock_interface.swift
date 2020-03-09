// REQUIRES: rdar59977439
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache-lock)
// RUN: %empty-directory(%t/module-cache-no-lock)
// RUN: echo 'public func foo() {}' > %t/Foo.swift
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/Foo.swiftinterface %t/Foo.swift -enable-library-evolution
// RUN: touch %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'import Foo' > %t/file-01.swift
// RUN: echo 'import Foo' > %t/file-02.swift
// RUN: echo 'import Foo' > %t/file-03.swift
// RUN: %target-swiftc_driver -j20 %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift -I %t -Xfrontend -Rmodule-interface-rebuild -module-cache-path %t/module-cache &> %t/result.txt
// RUN: %FileCheck %s  -check-prefix=CHECK-REBUILD < %t/result.txt

// RUN: %target-swiftc_driver -j20 %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift -I %t -Xfrontend -Rmodule-interface-rebuild -Xfrontend -disable-interface-lock -module-cache-path %t/module-cache-no-lock &> %t/result.txt

// RUN: %FileCheck %s  -check-prefix=CHECK-REBUILD-NO-LOCK < %t/result.txt

// Ensure we only build Foo module once from the interface
// CHECK-REBUILD: rebuilding module 'Foo' from interface
// CHECK-REBUILD-NOT: rebuilding module 'Foo' from interface

// CHECK-REBUILD-NO-LOCK: rebuilding module 'Foo' from interface
// CHECK-REBUILD-NO-LOCK: rebuilding module 'Foo' from interface
// CHECK-REBUILD-NO-LOCK: rebuilding module 'Foo' from interface
