//===-- LocalTypeDataKind.h - Kinds of locally-cached type data -*- C++ -*-===//
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
//  This file defines the LocalTypeDataKind class, which opaquely
//  represents a particular kind of local type data that we might
//  want to cache during emission.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_LOCALTYPEDATAKIND_H
#define SWIFT_IRGEN_LOCALTYPEDATAKIND_H

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
  
  explicit LocalTypeDataKind(unsigned Value) : Value(Value) {}
  
  /// Magic values for special kinds of type metadata.  These should be
  /// small so that they should never conflict with a valid pointer.
  ///
  /// Since this representation is opaque, we don't worry about being able
  /// to distinguish different kinds of pointer; we just assume that e.g. a
  /// ProtocolConformance will never have the same address as a Decl.
  enum : RawType {
    Metatype,
    ValueWitnessTable,
    // <- add more special cases here

    // The first enumerator for an individual value witness.
    ValueWitnessBase,
  };
  
public:
  LocalTypeDataKind() = default;
  
  // The magic values are all odd and so do not collide with pointer values.
  
  /// A reference to the type metadata.
  static LocalTypeDataKind forMetatype() {
    return LocalTypeDataKind(Metatype);
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
  forArchetypeProtocolWitnessTable(ProtocolDecl *protocol) {
    return LocalTypeDataKind(uintptr_t(protocol));
  }

  /// A reference to a protocol witness table for an archetype.
  ///
  /// We assume that the protocol conformance is a sufficiently unique key.
  /// This implicitly assumes that we don't care about having multiple
  /// specializations of a conditional conformance for different
  /// conformances.
  static LocalTypeDataKind
  forConcreteProtocolWitnessTable(NormalProtocolConformance *conformance) {
    return LocalTypeDataKind(uintptr_t(conformance));
  }
  
  RawType getRawValue() const {
    return Value;
  }

  bool operator==(LocalTypeDataKind other) const {
    return Value == other.Value;
  }
};

struct LocalTypeDataKey {
  CanType Type;
  LocalTypeDataKind Kind;

  bool operator==(const LocalTypeDataKey &other) const {
    return Type == other.Type && Kind == other.Kind;
  }
};

}
}

template <> struct llvm::DenseMapInfo<swift::irgen::LocalTypeDataKey> {
  using LocalTypeDataKey = swift::irgen::LocalTypeDataKey;
  using CanTypeInfo = DenseMapInfo<swift::CanType>;
  static inline LocalTypeDataKey getEmptyKey() {
    return { CanTypeInfo::getEmptyKey(),
             swift::irgen::LocalTypeDataKind::forMetatype() };
  }
  static inline LocalTypeDataKey getTombstoneKey() {
    return { CanTypeInfo::getTombstoneKey(),
             swift::irgen::LocalTypeDataKind::forMetatype() };
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
