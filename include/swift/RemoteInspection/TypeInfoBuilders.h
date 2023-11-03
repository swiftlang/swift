//===--- TypeInfoBuilders.h - Swift Type Reference Builder --------*- C++ -*-===//
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
// Implements utilities for building TypeInfos.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TYPEINFOBUILDERS_H
#define SWIFT_REFLECTION_TYPEINFOBUILDERS_H

#include "swift/ABI/Enum.h"
#include "swift/RemoteInspection/TypeLowering.h"
#include "swift/RemoteInspection/TypeRef.h"
#include "swift/RemoteInspection/TypeRefBuilder.h"

#ifdef DEBUG_TYPE_LOWERING
#define DEBUG_LOG(expr) expr;
#else
#define DEBUG_LOG(expr)
#endif

namespace swift {
namespace reflection {

/// Utility class for performing universal layout for types such as
/// tuples, structs, thick functions, etc.

class RecordTypeInfoBuilder {
  TypeConverter &TC;
  unsigned Size, Alignment, NumExtraInhabitants;
  bool BitwiseTakable;
  RecordKind Kind;
  std::vector<FieldInfo> Fields;
  bool Empty;
  bool Invalid;

public:
  RecordTypeInfoBuilder(TypeConverter &TC, RecordKind Kind)
      : TC(TC), Size(0), Alignment(1), NumExtraInhabitants(0),
        BitwiseTakable(true), Kind(Kind), Empty(true), Invalid(false) {}

  bool isInvalid() const;

  unsigned addField(unsigned fieldSize, unsigned fieldAlignment,
                    unsigned numExtraInhabitants, bool bitwiseTakable);

  // Add a field of a record type, such as a struct.
  void addField(const std::string &Name, const TypeRef *TR,
                remote::TypeInfoProvider *ExternalTypeInfo);

  const RecordTypeInfo *build();

  unsigned getNumFields() const;

  unsigned getFieldOffset(unsigned Index) const;
};

class EnumTypeInfoBuilder {
  TypeConverter &TC;
  unsigned Size, Alignment, NumExtraInhabitants;
  bool BitwiseTakable;
  std::vector<FieldInfo> Cases;
  bool Invalid;

  const TypeRef *getCaseTypeRef(FieldTypeInfo Case);

  void addCase(const std::string &Name);

  void addCase(const std::string &Name, const TypeRef *TR, const TypeInfo *TI);

public:
  EnumTypeInfoBuilder(TypeConverter &TC)
      : TC(TC), Size(0), Alignment(1), NumExtraInhabitants(0),
        BitwiseTakable(true), Invalid(false) {}

  const TypeInfo *build(const TypeRef *TR, RemoteRef<FieldDescriptor> FD,
                        remote::TypeInfoProvider *ExternalTypeInfo);
};

/// Utility class for building values that contain witness tables.
class ExistentialTypeInfoBuilder {
  TypeConverter &TC;
  std::vector<const TypeRef *> Protocols;
  const TypeRef *Superclass = nullptr;
  ExistentialTypeRepresentation Representation;
  ReferenceCounting Refcounting;
  bool ObjC;
  unsigned WitnessTableCount;
  bool Invalid;

  bool isSingleError() const;

  void examineProtocols();

public:
  ExistentialTypeInfoBuilder(TypeConverter &TC)
      : TC(TC), Representation(ExistentialTypeRepresentation::Opaque),
        Refcounting(ReferenceCounting::Unknown), ObjC(false),
        WitnessTableCount(0), Invalid(false) {}

  void addProtocol(const TypeRef *P);

  void addProtocolComposition(const ProtocolCompositionTypeRef *PC);

  void addAnyObject();

  void markInvalid();

  const TypeInfo *build(remote::TypeInfoProvider *ExternalTypeInfo);

  const TypeInfo *buildMetatype(remote::TypeInfoProvider *ExternalTypeInfo);
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEINFOBUILDERS_H
