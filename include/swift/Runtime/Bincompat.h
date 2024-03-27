//===--- Bincompat.h - Binary compatibility checks. -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checks for enabling binary compatibility workarounds.
//
//===----------------------------------------------------------------------===//

namespace swift {

namespace runtime {

namespace bincompat {

/// Whether protocol conformance iteration should be reversed, to prefer
/// conformances from images that are later in the list over earlier ones.
/// Default is false starting with Swift 5.4.
bool useLegacyProtocolConformanceReverseIteration();

/// Whether we should crash when we encounter a non-nullable Obj-C
/// reference with a null value as the source of a cast.
/// Default is true starting with Swift 5.4.
bool useLegacyPermissiveObjCNullSemanticsInCasting();

/// Whether we should use the legacy semantics for casting nil optionals
/// to nested optionals
bool useLegacyOptionalNilInjectionInCasting();

/// Whether to use legacy semantics when boxing Swift values for
/// Obj-C interop
bool useLegacyObjCBoxingInCasting();

/// Whether to use legacy semantics when unboxing __SwiftValue
bool useLegacySwiftValueUnboxingInCasting();

/// Legacy semantics use trivial implementations for -hashValue/-isEqual:
/// requests from ObjC to Swift values.
/// New semantics attempt to dispatch to Swift Hashable/Equatable conformances
/// if present.
bool useLegacySwiftObjCHashing();

/// Legacy semantics allowed for the `swift_task_reportUnexpectedExecutor` to
/// only log a warning. This changes in future releases and this function
/// will fatal error always.

/// Old behavior:
/// - logging a warning on concurrency violation is allowed
/// New behavior:
/// - always fatal error in `swift_task_reportUnexpectedExecutor`
SWIFT_RUNTIME_STDLIB_SPI
bool swift_bincompat_useLegacyWarningModeReportUnexpectedExecutor();

} // namespace bincompat

} // namespace runtime

} // namespace swift
