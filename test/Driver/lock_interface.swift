// REQUIRES: radar82261445
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/sdk)
// RUN: %empty-directory(%t/module-cache-lock)
// RUN: %empty-directory(%t/module-cache-no-lock)
// RUN: echo 'public func foo() {}' > %t/sdk/Foo.swift
// RUN: %target-swift-frontend-typecheck -emit-module-interface-path %t/sdk/Foo.swiftinterface %t/sdk/Foo.swift -enable-library-evolution
// RUN: chmod a-w %t/sdk

// RUN: touch %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift
// RUN: echo 'import Foo' > %t/file-01.swift
// RUN: echo 'import Foo' > %t/file-02.swift
// RUN: echo 'import Foo' > %t/file-03.swift
// RUN: %target-swiftc_driver -j20 %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift -I %t/sdk -Xfrontend -Rmodule-interface-rebuild -module-cache-path %t/module-cache &> %t/result-with-lock.txt

/// Checking that the compiler rebuilds the same module more than once can
/// fail depending on how fast the jobs are executed. Just make sure the
/// compiler accepts the -disable-interface-lock flag.
// RUN: %target-swiftc_driver -j20 %t/main.swift %t/file-01.swift %t/file-02.swift %t/file-03.swift -I %t/sdk -Xfrontend -Rmodule-interface-rebuild -Xfrontend -disable-interface-lock -module-cache-path %t/module-cache-no-lock &> %t/result-no-lock.txt

/// Reset the permissions before running the tests likely to fail.
// RUN: chmod a+w %t/sdk

// Ensure we only build Foo module once from the interface
// RUN: %FileCheck %s  -check-prefix=CHECK-REBUILD < %t/result-with-lock.txt
// CHECK-REBUILD: rebuilding module 'Foo' from interface
// CHECK-REBUILD-NOT: rebuilding module 'Foo' from interface
// CHECK-REBUILD-NOT: building module interface without lock file

// RUN: %FileCheck %s  -check-prefix=CHECK-REBUILD-NO-LOCK < %t/result-no-lock.txt
// CHECK-REBUILD-NO-LOCK: rebuilding module 'Foo' from interface
