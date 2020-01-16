//===--- Overrides.cpp - Compat overrides for Swift 5.0 runtime ----s------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides compatibility override hooks for Swift 5.0 runtimes.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Runtime/Metadata.h"

namespace swift {

using ConformsToProtocol_t =
  const WitnessTable *(const Metadata *, const ProtocolDescriptor *);

const WitnessTable *
swift50override_conformsToProtocol(const Metadata * const type,
  const ProtocolDescriptor *protocol,
  ConformsToProtocol_t *original);

}
