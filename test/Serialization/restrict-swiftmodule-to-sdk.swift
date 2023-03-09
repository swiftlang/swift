// RUN: %empty-directory(%t/cache)
// RUN: %empty-directory(%t/build)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Build Lib against SDK A.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -target-sdk-name A -o %t/build -parse-stdlib -module-cache-path %t/cache

/// Building Client against SDK A should work fine as expected.
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_PER_SDK=true \
// RUN:   %target-swift-frontend -typecheck %t/Client.swift -swift-version 5 -target-sdk-name A -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Build Client against SDK B, this should fail at loading Lib against a different SDK than A.
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_PER_SDK=true \
// RUN:   not %target-swift-frontend -typecheck %t/Client.swift -swift-version 5 -target-sdk-name B -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s -check-prefix=CHECK-AvsB
// CHECK-AvsB: cannot load module 'Lib' built with SDK 'A' when using SDK 'B': {{.*}}Lib.swiftmodule

/// Build Client against SDK A.Secret, this should accept the SDK as being a super set of A.
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_PER_SDK=true \
// RUN:   %target-swift-frontend -typecheck %t/Client.swift -swift-version 5 -target-sdk-name A.Secret -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Build Lib against SDK C.Secret and Client against SDK C, this should be rejected.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -target-sdk-name C.Secret -o %t/build -parse-stdlib -module-cache-path %t/cache
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_PER_SDK=true \
// RUN:   not %target-swift-frontend -typecheck %t/Client.swift -swift-version 5 -target-sdk-name C -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s -check-prefix=CHECK-C
// CHECK-C: cannot load module 'Lib' built with SDK 'C.Secret' when using SDK 'C': {{.*}}Lib.swiftmodule

/// Build a resilient Lib against SDK A, and a client against SDK B.
/// This should succeed after rebuilding from the swiftinterface.
// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -target-sdk-name A -o %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -enable-library-evolution -emit-module-interface-path %t/build/Lib.swiftinterface
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_PER_SDK=true \
// RUN:   %target-swift-frontend -typecheck %t/Client.swift -swift-version 5 -target-sdk-name B -I %t/build -parse-stdlib -module-cache-path %t/cache \
// RUN:   -Rmodule-interface-rebuild 2>&1 | %FileCheck %s -check-prefix=CHECK-AvsB-REBUILD
// CHECK-AvsB-REBUILD: remark: rebuilding module 'Lib' from interface

/// Modules loaded from the resource dir are not usually rebuilt from
/// the swiftinterface as it would indicate a configuration problem.
/// Lift that behavior for SDK mismatch and still rebuild them.
// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -target-sdk-name A -parse-stdlib -module-cache-path %t/cache \
// RUN:   -o %t/build -emit-module-interface-path %t/build/Lib.swiftinterface
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_PER_SDK=true \
// RUN:   %target-swift-frontend -typecheck %t/Client.swift \
// RUN:   -target-sdk-name B -resource-dir %t/build -I %t/build \
// RUN:   -parse-stdlib -module-cache-path %t/cache \
// RUN:   -Rmodule-interface-rebuild 2>&1 | %FileCheck %s -check-prefix=CHECK-AvsB-REBUILD

// BEGIN Lib.swift
public func foo() {}

// BEGIN Client.swift
import Lib
foo()
