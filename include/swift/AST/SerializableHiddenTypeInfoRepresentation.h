//===--- SerializableHiddenTypeInfoRepresentation.h ------------*- C++ -*-===//
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
// This file defines serializable representations of IRGen TypeInfo objects
// used to describe hidden types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SERIALIZABLEHIDDENTYPEINFOREPRESENTATION_H
#define SWIFT_AST_SERIALIZABLEHIDDENTYPEINFOREPRESENTATION_H

#include "swift/AST/TypeInfoStorage.h"
#include "llvm/IR/Type.h"
#include <cstdint>
#include <memory>
#include <vector>

namespace swift {

// This is used to provide a serializable representation of
// the llvm::Type used to represent type layout in TypeInfo objects.
// In producing this structure from an llvm::Type, we also transform
// any llvm::StructTypes in identified form into literal form. The identity
// of a member field type is not important for hidden types, only the layout.
struct SerializableLLVMTypeRepresentation {
  explicit SerializableLLVMTypeRepresentation(llvm::Type::TypeID kind)
      : kind(kind) {}

  llvm::Type::TypeID kind;

  // llvm::Type objects form a graph. Each node has a type, and some have children
  // and additional attributes. Payload is a re-useable field to represent such attributes.
  // For example, integers have a bit width attribute, arrays and vectors have a size attribute.
  uint64_t payload = 0;
  // Packged is used to describe if a struct is packed or not.
  bool packed = false;
  std::vector<std::unique_ptr<SerializableLLVMTypeRepresentation>>
      children;
};

class SerializableHiddenTypeInfoRepresentation {
public:
  std::unique_ptr<SerializableLLVMTypeRepresentation> storageType;
  irgen::TypeInfoBitfields bits = {0};

  virtual ~SerializableHiddenTypeInfoRepresentation() = default;
};

class SerializableFixedTypeInfoRepresentation
    : public SerializableHiddenTypeInfoRepresentation {
public:
  uint32_t size = 0;
  irgen::SpareBitVector spareBits;
};

class SerializableLoadableTypeInfoRepresentation
    : public SerializableFixedTypeInfoRepresentation {};

struct SerializableRecordFieldRepresentation {
  std::unique_ptr<SerializableHiddenTypeInfoRepresentation> typeInfo;
  irgen::ElementLayoutStorage layout;
  irgen::RecordFieldStorage storage;
};

class SerializableLoadableRecordTypeInfoRepresentation
    : public SerializableLoadableTypeInfoRepresentation {
public:
  std::vector<SerializableRecordFieldRepresentation> fields;
  bool fieldsAreABIAccessible = false;
  uint32_t explosionSize = 0;
};

class SerializableLoadableStructTypeInfoRepresentation
    : public SerializableLoadableRecordTypeInfoRepresentation {};

struct SerializableAggLoweringInputRepresentation {
  uint64_t begin = 0;
  uint64_t end = 0;
  std::unique_ptr<SerializableLLVMTypeRepresentation> type;
};

class SerializableLoadableClangRecordTypeInfoRepresentation final
    : public SerializableLoadableStructTypeInfoRepresentation {
public:
  bool hasReferenceField = false;
  std::vector<SerializableAggLoweringInputRepresentation> aggLoweringInputs;
};

} // namespace swift

#endif // SWIFT_AST_SERIALIZABLEHIDDENTYPEINFOREPRESENTATION_H
