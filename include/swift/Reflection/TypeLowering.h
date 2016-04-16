//===--- TypeLowering.h - Swift Type Lowering for Reflection ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements logic for computing in-memory layouts from TypeRefs loaded from
// reflection metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TYPELOWERING_H
#define SWIFT_REFLECTION_TYPELOWERING_H

#include "llvm/ADT/DenseMap.h"

#include <iostream>
#include <memory>

namespace swift {
namespace reflection {

class TypeRef;
class TypeRefBuilder;

enum class TypeInfoKind : unsigned {
  Builtin,
  Struct,
  Tuple,
};

class TypeInfo {
  TypeInfoKind Kind;
  unsigned Size, Alignment, Stride, NumExtraInhabitants;

public:
  TypeInfo(TypeInfoKind Kind,
           unsigned Size, unsigned Alignment,
           unsigned Stride, unsigned NumExtraInhabitants)
    : Kind(Kind), Size(Size), Alignment(Alignment), Stride(Stride),
      NumExtraInhabitants(NumExtraInhabitants) {}

  TypeInfoKind getKind() const { return Kind; }

  unsigned getSize() const { return Size; }
  unsigned getAlignment() const { return Alignment; }
  unsigned getStride() const { return Stride; }
  unsigned getNumExtraInhabitants() const { return NumExtraInhabitants; }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;
};

struct FieldInfo {
  std::string Name;
  unsigned Offset;
  const TypeInfo &TI;
};

/// Class instances, structs, tuples
class RecordTypeInfo : public TypeInfo {
  std::vector<FieldInfo> Fields;

public:
  RecordTypeInfo(TypeInfoKind Kind,
                 unsigned Size, unsigned Alignment,
                 unsigned Stride, unsigned NumExtraInhabitants,
                 const std::vector<FieldInfo> &Fields)
    : TypeInfo(Kind, Size, Alignment, Stride, NumExtraInhabitants),
      Fields(Fields) {}

  unsigned getNumFields() const { return Fields.size(); }
  const std::vector<FieldInfo> &getFields() const { return Fields; }
};

/// This class owns the memory for all TypeInfo instances that it vends.
class TypeConverter {
  TypeRefBuilder &Builder;
  std::vector<std::unique_ptr<const TypeInfo>> Pool;
  llvm::DenseMap<const TypeRef *, const TypeInfo *> Cache;

public:
  explicit TypeConverter(TypeRefBuilder &Builder) : Builder(Builder) {}

  TypeRefBuilder &getBuilder() { return Builder; }

  const TypeInfo *getTypeInfo(const TypeRef *TR);
};

}
}

#endif
