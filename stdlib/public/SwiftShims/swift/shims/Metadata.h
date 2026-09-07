//===--- Metadata.h - Swift metadata ABI structures -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// C-compatible declarations for Swift metadata ABI structures consumed by
// Swift code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_METADATA_H
#define SWIFT_STDLIB_SHIMS_METADATA_H

#include "SwiftStdint.h"

#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

/// The fixed portion of a Swift protocol descriptor.
///
/// Protocol-specific trailing objects immediately follow this header.
typedef struct _SwiftProtocolDescriptorHeader {
  /// Context descriptor flags.
  __swift_uint32_t Flags;

  /// Relative reference to the parent context descriptor.
  __swift_int32_t Parent;

  /// Relative reference to the protocol name.
  __swift_int32_t Name;

  /// Number of requirements in the protocol's requirement signature.
  __swift_uint32_t NumRequirementsInSignature;

  /// Number of protocol requirements.
  __swift_uint32_t NumRequirements;

  /// Nullable relative reference to the associated-type names.
  __swift_int32_t AssociatedTypeNames;
} _SwiftProtocolDescriptorHeader;

#ifdef __cplusplus
} // extern "C"
} // namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_METADATA_H
