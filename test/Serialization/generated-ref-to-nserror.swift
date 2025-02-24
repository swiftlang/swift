// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines
// REQUIRES: objc_interop

/// Build a minimal version of Foundation defining only NSError
// RUN: %target-swift-frontend -emit-module %t/Foundation.swift \
// RUN:   -o %t/Foundation.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/FoundationExporter.swift -I%t \
// RUN:   -o %t/FoundationExporter.swiftmodule

/// Enabled existential to NSError optimization
// CHECK-OPTIMIZED: $NSError

/// Optimize Foundation itself
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   %t/Foundation.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s

/// Public import or non-resilient modules
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   %t/inlinable-public.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   %t/inlinable-public.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   %t/inlinable-internal.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s

/// Foundation is imported from a different file
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t -module-name main \
// RUN:   %t/inlinable-not-imported-fileA.swift \
// RUN:   %t/inlinable-not-imported-fileB.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t -module-name main \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   %t/inlinable-not-imported-fileA.swift \
// RUN:   %t/inlinable-not-imported-fileB.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s

/// Foundation is imported via a transitive dependency
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t -module-name main \
// RUN:   %t/inlinable-imported-transitive.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s

/// Any non-inlinable uses
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   %t/non-inlinable-public.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   %t/non-inlinable-ioi.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   %t/non-inlinable-internal.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t -module-name main \
// RUN:   %t/non-inlinable-not-imported-fileA.swift \
// RUN:   %t/non-inlinable-not-imported-fileB.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t -module-name main \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   %t/non-inlinable-not-imported-fileA.swift \
// RUN:   %t/non-inlinable-not-imported-fileB.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-OPTIMIZED --input-file %t/main.sil %s

/// Disabled existential to NSError optimization
// CHECK-NOT-OPTIMIZED-NOT: $NSError

/// Implementation-only import
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   %t/inlinable-ioi.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-NOT-OPTIMIZED --input-file %t/main.sil %s
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   %t/inlinable-ioi.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-NOT-OPTIMIZED --input-file %t/main.sil %s

/// Internal import from resilient module
// RUN: %target-swift-frontend -emit-module -emit-sil -I%t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   %t/inlinable-internal.swift > %t/main.sil
// RUN: %FileCheck --check-prefix CHECK-NOT-OPTIMIZED --input-file %t/main.sil %s

//--- Foundation.swift

class NSError {}

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- FoundationExporter.swift

@_exported import Foundation

//--- inlinable-public.swift
public import Foundation

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- inlinable-ioi.swift
@_implementationOnly import Foundation

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- inlinable-internal.swift
internal import Foundation

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- inlinable-not-imported-fileA.swift
@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}
//--- inlinable-not-imported-fileB.swift
import Foundation

//--- inlinable-imported-transitive.swift
public import FoundationExporter

@inlinable public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- non-inlinable-public.swift
public import Foundation

public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- non-inlinable-ioi.swift
@_implementationOnly import Foundation

public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- non-inlinable-internal.swift
internal import Foundation

public func foo<E: Error>(e: E) -> Error {
    return e
}

//--- non-inlinable-not-imported-fileA.swift
public func foo<E: Error>(e: E) -> Error {
    return e
}
//--- non-inlinable-not-imported-fileB.swift
import Foundation
