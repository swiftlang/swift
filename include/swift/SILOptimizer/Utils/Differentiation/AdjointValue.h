//===--- AdjointValue.h - Helper class for differentiation ----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// SWIFT_ENABLE_TENSORFLOW
//
// AdjointValue - a symbolic representation for adjoint values that allows
// for efficient differentiation of aggregates.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_ADJOINTVALUE_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_ADJOINTVALUE_H

#include "swift/SIL/SILValue.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {
namespace autodiff {

enum AdjointValueKind {
  /// An empty adjoint, i.e. zero. This case exists due to its special
  /// mathematical properties: `0 + x = x`. This is a guaranteed optimization
  /// when we combine a zero adjoint with another (e.g. differentiating a
  /// fanout).
  Zero,

  /// An aggregate of adjoint values.
  Aggregate,

  /// A concrete SIL value.
  Concrete,
};

class AdjointValue;

class AdjointValueBase {
  friend class AdjointValue;

  /// The kind of this adjoint value.
  AdjointValueKind kind;

  /// The type of this value as if it were materialized as a SIL value.
  SILType type;

  /// The underlying value.
  union Value {
    llvm::ArrayRef<AdjointValue> aggregate;
    SILValue concrete;
    Value(llvm::ArrayRef<AdjointValue> v) : aggregate(v) {}
    Value(SILValue v) : concrete(v) {}
    Value() {}
  } value;

  explicit AdjointValueBase(SILType type,
                            llvm::ArrayRef<AdjointValue> aggregate)
      : kind(AdjointValueKind::Aggregate), type(type), value(aggregate) {}

  explicit AdjointValueBase(SILValue v)
      : kind(AdjointValueKind::Concrete), type(v->getType()), value(v) {}

  explicit AdjointValueBase(SILType type)
      : kind(AdjointValueKind::Zero), type(type) {}
};

/// A symbolic adjoint value that is capable of representing zero value 0 and
/// 1, in addition to a materialized SILValue. This is expected to be passed
/// around by value in most cases, as it's two words long.
class AdjointValue final {

private:
  /// The kind of this adjoint value.
  AdjointValueBase *base;
  /*implicit*/ AdjointValue(AdjointValueBase *base = nullptr) : base(base) {}

public:
  AdjointValueBase *operator->() const { return base; }
  AdjointValueBase &operator*() const { return *base; }

  static AdjointValue createConcrete(llvm::BumpPtrAllocator &allocator,
                                     SILValue value) {
    return new (allocator.Allocate<AdjointValueBase>()) AdjointValueBase(value);
  }

  static AdjointValue createZero(llvm::BumpPtrAllocator &allocator,
                                 SILType type) {
    return new (allocator.Allocate<AdjointValueBase>()) AdjointValueBase(type);
  }

  static AdjointValue createAggregate(llvm::BumpPtrAllocator &allocator,
                                      SILType type,
                                      llvm::ArrayRef<AdjointValue> aggregate) {
    return new (allocator.Allocate<AdjointValueBase>())
        AdjointValueBase(type, aggregate);
  }

  AdjointValueKind getKind() const { return base->kind; }
  SILType getType() const { return base->type; }
  CanType getSwiftType() const { return getType().getASTType(); }

  NominalTypeDecl *getAnyNominal() const {
    return getSwiftType()->getAnyNominal();
  }

  bool isZero() const { return getKind() == AdjointValueKind::Zero; }
  bool isAggregate() const { return getKind() == AdjointValueKind::Aggregate; }
  bool isConcrete() const { return getKind() == AdjointValueKind::Concrete; }

  unsigned getNumAggregateElements() const {
    assert(isAggregate());
    return base->value.aggregate.size();
  }

  AdjointValue getAggregateElement(unsigned i) const {
    assert(isAggregate());
    return base->value.aggregate[i];
  }

  llvm::ArrayRef<AdjointValue> getAggregateElements() const {
    return base->value.aggregate;
  }

  SILValue getConcreteValue() const {
    assert(isConcrete());
    return base->value.concrete;
  }

  void print(llvm::raw_ostream &s) const {
    switch (getKind()) {
    case AdjointValueKind::Zero:
      s << "Zero";
      break;
    case AdjointValueKind::Aggregate:
      s << "Aggregate<";
      if (auto *decl =
              getType().getASTType()->getStructOrBoundGenericStruct()) {
        s << "Struct>(";
        interleave(
            llvm::zip(decl->getStoredProperties(), base->value.aggregate),
            [&s](std::tuple<VarDecl *, const AdjointValue &> elt) {
              s << std::get<0>(elt)->getName() << ": ";
              std::get<1>(elt).print(s);
            },
            [&s] { s << ", "; });
      } else if (auto tupleType = getType().getAs<TupleType>()) {
        s << "Tuple>(";
        interleave(
            base->value.aggregate,
            [&s](const AdjointValue &elt) { elt.print(s); },
            [&s] { s << ", "; });
      } else {
        llvm_unreachable("Invalid aggregate");
      }
      s << ')';
      break;
    case AdjointValueKind::Concrete:
      s << "Concrete(" << base->value.concrete << ')';
      break;
    }
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const AdjointValue &adjVal) {
  adjVal.print(os);
  return os;
}

} // end namespace autodiff
} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_ADJOINTVALUE_H
