//===--- AbstractLayout.h - Abstract type layout information ----*- C++ -*-===//
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
// This file defines data structures for abstract type layout information,
// used to encode the layout of hidden C types in .swiftmodule files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ABSTRACTLAYOUT_H
#define SWIFT_AST_ABSTRACTLAYOUT_H

#include <memory>
#include <optional>

#include "swift/AST/ReferenceCounting.h"
#include "swift/SIL/SILTypeProperties.h"

namespace swift {

class SerializableHiddenTypeInfoRepresentation;
class ModuleFile;

/// Identifies a type record in the serialized module from which an abstract
/// layout was recovered. Serialization can replay this record graph without
/// first reconstructing a complete AST type.
struct SerializedTypeReference {
  ModuleFile *moduleFile = nullptr;
  unsigned typeID = 0;

  explicit operator bool() const { return moduleFile && typeID; }
};

struct AbstractTypeLayout {
  SILTypeProperties typeProperties;
  std::optional<ReferenceCounting> referenceCountingSystem;
  std::shared_ptr<SerializableHiddenTypeInfoRepresentation>
      typeInfoRepresentation;
  SerializedTypeReference serializedType;
};

} // namespace swift

#endif // SWIFT_AST_ABSTRACTLAYOUT_H
