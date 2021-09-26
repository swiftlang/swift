// RUN: %empty-directory(%t/cache)
// RUN: %empty-directory(%t/build)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Unsupported on Windows for the use of env vars.
// UNSUPPORTED: OS=windows-msvc

/// 1. Compile Lib.swift as non-resilient untagged, resilient untagged, and resilient tagged
// BEGIN Lib.swift
public func foo() {}

/// Build Lib as a resilient and non-resilient swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -o %t/build -parse-stdlib -module-cache-path %t/cache -module-name ResilientLib -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -o %t/build -parse-stdlib -module-cache-path %t/cache -module-name NonresilientLib
// RUN: SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=my-revision \
// RUN:   %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -o %t/build -parse-stdlib -module-cache-path %t/cache -module-name TaggedLib -enable-library-evolution


/// 2. Test importing the non-resilient untagged library
// BEGIN NonresilientClient.swift
import NonresilientLib
foo()

/// Building a NonresilientLib client should always succeed
// RUN: %target-swift-frontend -typecheck %t/NonresilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache
// RUN: SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=my-revision \
// RUN:   %target-swift-frontend -typecheck %t/NonresilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache


/// 3. Test importing the resilient untagged library
// BEGIN ResilientClient.swift
import ResilientLib
foo()

/// Building a ResilientLib client should succeed in non-revision/dev mode
// RUN: %target-swift-frontend -typecheck %t/ResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Building a ResilientLib client should reject the import for a tagged compiler
// RUN: SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=not-a-revision \
// RUN:   not %target-swift-frontend -typecheck %t/ResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s
// CHECK: compiled module was created by a different version of the compiler; rebuild 'ResilientLib' and try again: {{.*}}ResilientLib.swiftmodule

/// Building a ResilientLib client should succeed for a tagged compiler with SWIFT_DEBUG_IGNORE_SWIFTMODULE_REVISION
// RUN: SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=not-a-revision SWIFT_DEBUG_IGNORE_SWIFTMODULE_REVISION=true \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache


/// 4. Test importing the resilient tagged library
// BEGIN TaggedClient.swift
import TaggedLib
foo()

/// Importing TaggedLib should success with the same tag or a dev compiler
// RUN: SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=my-revision \
// RUN:   %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache
// RUN: %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Building a TaggedLib client should reject the import for a different tagged compiler
// RUN: SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=not-a-revision \
// RUN:   not %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s --check-prefix=CHECK-TAGGED
// CHECK-TAGGED: compiled module was created by a different version of the compiler; rebuild 'TaggedLib' and try again: {{.*}}TaggedLib.swiftmodule
