//===-- LocalTypeDataKind.h - Kinds of locally-cached type data -*- C++ -*-===//
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
//  This file defines the LocalTypeDataKind class, which opaquely
//  represents a particular kind of local type data that we might
//  want to cache during emission.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LOCALTYPEDATAKIND_H
#define SWIFT_IRGEN_LOCALTYPEDATAKIND_H

#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Type.h"
#include <stdint.h>
#include "llvm/ADT/DenseMapInfo.h"

namespace swift {
  class NormalProtocolConformance;
  class ProtocolDecl;

namespace irgen {
  enum class ValueWitness : unsigned;

/// The kind of local type data we might want to store for a type.
class LocalTypeDataKind {
public:
  using RawType = uintptr_t;
private:
  RawType Value;
  
  explicit LocalTypeDataKind(RawType Value) : Value(Value) {}
  
  /// Magic values for special kinds of type metadata.  These should be
  /// small so that they should never conflict with a valid pointer.
  ///
  /// Since this representation is opaque, we don't worry about being able
  /// to distinguish different kinds of pointer; we just assume that e.g. a
  /// ProtocolConformance will never have the same address as a Decl.
  enum : RawType {
    TypeMetadata,
    ValueWitnessTable,
    // <- add more special cases here

    // The first enumerator for an individual value witness.
    ValueWitnessBase,

    FirstPayloadValue = 2048,
    Kind_Decl = 0,
    Kind_Conformance = 1,
    KindMask = 0x1,
  };

public:
  LocalTypeDataKind() = default;
  
  // The magic values are all odd and so do not collide with pointer values.
  
  /// A reference to the type metadata.
  static LocalTypeDataKind forTypeMetadata() {
    return LocalTypeDataKind(TypeMetadata);
  }

  /// A reference to the value witness table.
  static LocalTypeDataKind forValueWitnessTable() {
    return LocalTypeDataKind(ValueWitnessTable);
  }

  /// A reference to a specific value witness.
  static LocalTypeDataKind forValueWitness(ValueWitness witness) {
    return LocalTypeDataKind(ValueWitnessBase + (unsigned)witness);
  }
  
  /// A reference to a protocol witness table for an archetype.
  ///
  /// This only works for non-concrete types because in principle we might
  /// have multiple concrete conformances for a concrete type used in the
  /// same function.
  static LocalTypeDataKind
  forAbstractProtocolWitnessTable(ProtocolDecl *protocol) {
    assert(protocol && "protocol reference may not be null");
    return LocalTypeDataKind(uintptr_t(protocol) | Kind_Decl);
  }

  /// A reference to a protocol witness table for an archetype.
  static LocalTypeDataKind
  forConcreteProtocolWitnessTable(ProtocolConformance *conformance) {
    assert(conformance && "conformance reference may not be null");
    return LocalTypeDataKind(uintptr_t(conformance) | Kind_Conformance);
  }

  static LocalTypeDataKind
  forProtocolWitnessTable(ProtocolConformanceRef conformance) {
    if (conformance.isConcrete()) {
      return forConcreteProtocolWitnessTable(conformance.getConcrete());
    } else {
      return forAbstractProtocolWitnessTable(conformance.getAbstract());
    }
  }

  LocalTypeDataKind getCachingKind() const;

  bool isSingletonKind() const {
    return (Value < FirstPayloadValue);
  }

  bool isConcreteProtocolConformance() const {
    return (!isSingletonKind() &&
            ((Value & KindMask) == Kind_Conformance));
  }

  ProtocolConformance *getConcreteProtocolConformance() const {
    assert(isConcreteProtocolConformance());
    return reinterpret_cast<ProtocolConformance*>(Value - Kind_Conformance);
  }

  bool isAbstractProtocolConformance() const {
    return (!isSingletonKind() &&
            ((Value & KindMask) == Kind_Decl));
  }

  ProtocolDecl *getAbstractProtocolConformance() const {
    assert(isAbstractProtocolConformance());
    return reinterpret_cast<ProtocolDecl*>(Value - Kind_Decl);
  }

  ProtocolConformanceRef getProtocolConformance() const {
    assert(!isSingletonKind());
    if ((Value & KindMask) == Kind_Decl) {
      return ProtocolConformanceRef(getAbstractProtocolConformance());
    } else {
      return ProtocolConformanceRef(getConcreteProtocolConformance());
    }
  }
  
  RawType getRawValue() const {
    return Value;
  }

  void dump() const;
  void print(llvm::raw_ostream &out) const;

  bool operator==(LocalTypeDataKind other) const {
    return Value == other.Value;
  }
  bool operator!=(LocalTypeDataKind other) const {
    return Value != other.Value;
  }
};

class LocalTypeDataKey {
public:
  CanType Type;
  LocalTypeDataKind Kind;

  LocalTypeDataKey getCachingKey() const;

  bool operator==(const LocalTypeDataKey &other) const {
    return Type == other.Type && Kind == other.Kind;
  }

  void dump() const;
  void print(llvm::raw_ostream &out) const;
};

}
}

template <> struct llvm::DenseMapInfo<swift::irgen::LocalTypeDataKey> {
  using LocalTypeDataKey = swift::irgen::LocalTypeDataKey;
  using CanTypeInfo = DenseMapInfo<swift::CanType>;
  static inline LocalTypeDataKey getEmptyKey() {
    return { CanTypeInfo::getEmptyKey(),
             swift::irgen::LocalTypeDataKind::forTypeMetadata() };
  }
  static inline LocalTypeDataKey getTombstoneKey() {
    return { CanTypeInfo::getTombstoneKey(),
             swift::irgen::LocalTypeDataKind::forTypeMetadata() };
  }
  static unsigned getHashValue(const LocalTypeDataKey &key) {
    return combineHashValue(CanTypeInfo::getHashValue(key.Type),
                            key.Kind.getRawValue());
  }
  static bool isEqual(const LocalTypeDataKey &a, const LocalTypeDataKey &b) {
    return a == b;
  }
};

#endif
