// RUN: %empty-directory(%t/cache)
// RUN: %empty-directory(%t/build)
// RUN: %{python} %utils/split_file.py -o %t %s

/// 1. Compile Lib.swift as non-resilient untagged, resilient untagged, and resilient tagged
// BEGIN Lib.swift
public func foo() {}

/// Build Lib as a resilient and non-resilient swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -o %t/build -parse-stdlib -module-cache-path %t/cache -module-name ResilientLib -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -o %t/build -parse-stdlib -module-cache-path %t/cache -module-name NonResilientLib
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.0.1 \
// RUN:   %target-swift-frontend -emit-module %t/Lib.swift -swift-version 5 -o %t/build -parse-stdlib -module-cache-path %t/cache -module-name TaggedLib -enable-library-evolution


/// 2. Test importing the non-resilient untagged library
// BEGIN NonResilientClient.swift
import NonResilientLib
foo()

/// Building a NonResilientLib client should reject the import for a tagged compiler
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.0.1 \
// RUN:   not %target-swift-frontend -typecheck %t/NonResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck -check-prefix=CHECK-NON-RESILIENT %s
// CHECK-NON-RESILIENT: compiled module was created by a different version of the compiler ''; rebuild 'NonResilientLib' and try again: {{.*}}NonResilientLib.swiftmodule


/// 3. Test importing the resilient untagged library
// BEGIN ResilientClient.swift
import ResilientLib
foo()

/// Building a ResilientLib client should succeed in non-revision/dev mode
// RUN: %target-swift-frontend -typecheck %t/ResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Building a ResilientLib client should reject the import for a tagged compiler
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.0.1 \
// RUN:   not %target-swift-frontend -typecheck %t/ResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s
// CHECK: compiled module was created by a different version of the compiler ''; rebuild 'ResilientLib' and try again: {{.*}}ResilientLib.swiftmodule

/// Building a ResilientLib client should succeed for a tagged compiler with SWIFT_IGNORE_SWIFTMODULE_REVISION
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.0.1 SWIFT_IGNORE_SWIFTMODULE_REVISION=true \
// RUN:   %target-swift-frontend -typecheck %t/ResilientClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache


/// 4. Test importing the resilient tagged library
// BEGIN TaggedClient.swift
import TaggedLib
foo()

/// Importing TaggedLib should succeed with the same tag.
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.0.1 \
// RUN:   %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Importing TaggedLib should succeed with a dev compiler
// RUN: %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache

/// Importing TaggedLib should succeed but remark on a last digit difference.
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.0.2 \
// RUN:   %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s --check-prefix=CHECK-LAST-DIGIT
// CHECK-LAST-DIGIT: remark: compiled module was created by a different version of the compiler '1.0.0.0.1': {{.*}}TaggedLib.swiftmodule

/// Building a TaggedLib client should reject the import for a different tagged compiler
// RUN: env SWIFT_DEBUG_FORCE_SWIFTMODULE_REVISION=1.0.0.1.1 \
// RUN:   not %target-swift-frontend -typecheck %t/TaggedClient.swift -swift-version 5 -I %t/build -parse-stdlib -module-cache-path %t/cache 2>&1 | %FileCheck %s --check-prefix=CHECK-TAGGED
// CHECK-TAGGED: error: compiled module was created by a different version of the compiler '1.0.0.0.1'; rebuild 'TaggedLib' and try again: {{.*}}TaggedLib.swiftmodule
