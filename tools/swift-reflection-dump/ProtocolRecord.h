//===--- ProtocolRecord.h -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SWIFTREFLECTIONDUMP_PROTOCOLRECORD_H
#define SWIFT_SWIFTREFLECTIONDUMP_PROTOCOLRECORD_H

#include "swift/Basic/LLVM.h"
#include "swift/Reflection/ProtocolConformance.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace reflection {
namespace yaml {

struct ProtocolRecord {
  StringRef name;
  unsigned numRequirementsInSignature;
  unsigned numRequirements;
  StringRef associatedTypeNames;

  ProtocolRecord() = default;

  template <typename Runtime>
  ProtocolRecord(const TargetProtocolDescriptor<Runtime> &descriptor) {
    name = descriptor.Name.get();
    numRequirementsInSignature = descriptor.NumRequirementsInSignature;
    numRequirements = descriptor.NumRequirements;
    // TODO: Split these up.
    if (const char *ptr = descriptor.AssociatedTypeNames.get()) {
      associatedTypeNames = StringRef(ptr);
    }
  }
};

} // namespace yaml
} // namespace reflection
} // namespace swift

namespace llvm {
namespace yaml {

template <> struct MappingTraits<swift::reflection::yaml::ProtocolRecord> {
  static void mapping(IO &io, swift::reflection::yaml::ProtocolRecord &info) {
    io.mapRequired("name", info.name);
    io.mapRequired("numRequirementsInSignature",
                   info.numRequirementsInSignature);
    io.mapRequired("numRequirements", info.numRequirements);
    io.mapRequired("associatedTypeNames", info.associatedTypeNames);
  }
};

} // namespace yaml
} // namespace llvm

#endif
