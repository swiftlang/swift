//===--- Serialization.h - Swiftmodule emission -----------------*- C++ -*-===//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_H
#define SWIFT_SERIALIZATION_H

#include "swift/Subsystems.h"

namespace swift {

class SILModule;

namespace serialization {

/// Serialize a module to the given stream.
void writeToStream(
    raw_ostream &os, ModuleOrSourceFile DC, const SILModule *M,
    const SerializationOptions &options,
    const fine_grained_dependencies::SourceFileDepGraph *DepGraph);

/// Serialize module documentation to the given stream.
void writeDocToStream(raw_ostream &os, ModuleOrSourceFile DC,
                      StringRef GroupInfoPath);

/// Serialize module source info to the given stream.
void writeSourceInfoToStream(raw_ostream &os, ModuleOrSourceFile DC);

} // end namespace serialization
} // end namespace swift

#endif // SWIFT_SERIALIZATION_H
