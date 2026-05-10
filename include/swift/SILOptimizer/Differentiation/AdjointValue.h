//===--- AdjointValue.h - Helper class for differentiation ----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// AdjointValue - a symbolic representation for adjoint values enabling
// efficient differentiation by avoiding zero materialization.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_ADJOINTVALUE_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_ADJOINTVALUE_H

#include "swift/AST/Decl.h"
#include "swift/SIL/SILDebugVariable.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Debug.h"

#include <variant>

namespace swift {
namespace autodiff {

enum AdjointValueKind {
  /// An empty adjoint, i.e. zero. This case exists due to its special
  /// mathematical properties: `0 + x = x`. This is a guaranteed optimization
  /// when we combine a zero adjoint with another (e.g. differentiating a
  /// fanout).
  Zero,

  /// An aggregate of adjoint values: a struct or tuple.
  Aggregate,

  /// A concrete SIL value.
  Concrete,

  /// A special adjoint, made up of 2 adjoints -- an aggregate base adjoint and
  /// an element adjoint to add to one of its fields. This case exists to avoid
  /// eager materialization of a base adjoint upon addition with one of its
  /// fields.
  AddElement,
};

class AdjointValue;

struct AddElementValue;

class AdjointValueBase {
  friend class AdjointValue;

  /// The kind of this adjoint value.
  AdjointValueKind kind;

  /// The type of this value as if it were materialized as a SIL value.
  SILType type;

  using DebugInfo = std::pair<SILDebugLocation, SILDebugVariable>;

  /// The debug location and variable info associated with the original value.
  std::optional<DebugInfo> debugInfo;

  /// The underlying value.
  union Value {
    unsigned numAggregateElements;
    SILValue concrete;
    AddElementValue *addElementValue;

    Value(unsigned numAggregateElements)
        : numAggregateElements(numAggregateElements) {}
    Value(SILValue v) : concrete(v) {}
    Value(AddElementValue *addElementValue)
        : addElementValue(addElementValue) {}
    Value() {}
  } value;

  // Begins tail-allocated aggregate elements, if
  // `kind == AdjointValueKind::Aggregate`.

  explicit AdjointValueBase(SILType type,
                            llvm::ArrayRef<AdjointValue> aggregate,
                            std::optional<DebugInfo> debugInfo)
      : kind(AdjointValueKind::Aggregate), type(type), debugInfo(debugInfo),
        value(aggregate.size()) {
    MutableArrayRef<AdjointValue> tailElements(
        reinterpret_cast<AdjointValue *>(this + 1), aggregate.size());
    std::uninitialized_copy(
        aggregate.begin(), aggregate.end(), tailElements.begin());
  }

  explicit AdjointValueBase(SILValue v, std::optional<DebugInfo> debugInfo)
      : kind(AdjointValueKind::Concrete), type(v->getType()),
        debugInfo(debugInfo), value(v) {}

  explicit AdjointValueBase(SILType type, std::optional<DebugInfo> debugInfo)
      : kind(AdjointValueKind::Zero), type(type), debugInfo(debugInfo) {}

  explicit AdjointValueBase(SILType type, AddElementValue *addElementValue,
                            std::optional<DebugInfo> debugInfo)
      : kind(AdjointValueKind::AddElement), type(type), debugInfo(debugInfo),
        value(addElementValue) {}
};

/// A symbolic adjoint value that wraps a `SILValue`, a zero, or an aggregate
/// thereof.
class AdjointValue final {

private:
  /// The kind of this adjoint value.
  AdjointValueBase *base;
  /*implicit*/ AdjointValue(AdjointValueBase *base = nullptr) : base(base) {}

public:
  AdjointValueBase *operator->() const { return base; }
  AdjointValueBase &operator*() const { return *base; }

  using DebugInfo = AdjointValueBase::DebugInfo;

  static AdjointValue
  createConcrete(llvm::BumpPtrAllocator &allocator, SILValue value,
                 std::optional<DebugInfo> debugInfo = std::nullopt) {
    auto *buf = allocator.Allocate<AdjointValueBase>();
    return new (buf) AdjointValueBase(value, debugInfo);
  }

  static AdjointValue
  createZero(llvm::BumpPtrAllocator &allocator, SILType type,
             std::optional<DebugInfo> debugInfo = std::nullopt) {
    auto *buf = allocator.Allocate<AdjointValueBase>();
    return new (buf) AdjointValueBase(type, debugInfo);
  }

  static AdjointValue
  createAggregate(llvm::BumpPtrAllocator &allocator, SILType type,
                  ArrayRef<AdjointValue> elements,
                  std::optional<DebugInfo> debugInfo = std::nullopt) {
    AdjointValue *buf = reinterpret_cast<AdjointValue *>(allocator.Allocate(
        sizeof(AdjointValueBase) + elements.size() * sizeof(AdjointValue),
        alignof(AdjointValueBase)));
    return new (buf) AdjointValueBase(type, elements, debugInfo);
  }

  static AdjointValue
  createAddElement(llvm::BumpPtrAllocator &allocator, SILType type,
                   AddElementValue *addElementValue,
                   std::optional<DebugInfo> debugInfo = std::nullopt) {
    auto *buf = allocator.Allocate<AdjointValueBase>();
    return new (buf) AdjointValueBase(type, addElementValue, debugInfo);
  }

  AdjointValueKind getKind() const { return base->kind; }
  SILType getType() const { return base->type; }
  CanType getSwiftType() const { return getType().getASTType(); }
  std::optional<DebugInfo> getDebugInfo() const { return base->debugInfo; }
  void setDebugInfo(DebugInfo debugInfo) const { base->debugInfo = debugInfo; }

  NominalTypeDecl *getAnyNominal() const {
    return getSwiftType()->getAnyNominal();
  }

  bool isZero() const { return getKind() == AdjointValueKind::Zero; }
  bool isAggregate() const { return getKind() == AdjointValueKind::Aggregate; }
  bool isConcrete() const { return getKind() == AdjointValueKind::Concrete; }
  bool isAddElement() const {
    return getKind() == AdjointValueKind::AddElement;
  }

  unsigned getNumAggregateElements() const {
    assert(isAggregate());
    return base->value.numAggregateElements;
  }

  AdjointValue getAggregateElement(unsigned i) const {
    return getAggregateElements()[i];
  }

  llvm::ArrayRef<AdjointValue> getAggregateElements() const {
    assert(isAggregate());
    return {
        reinterpret_cast<const AdjointValue *>(base + 1),
        getNumAggregateElements()};
  }

  SILValue getConcreteValue() const {
    assert(isConcrete());
    return base->value.concrete;
  }

  AddElementValue *getAddElementValue() const {
    assert(isAddElement());
    return base->value.addElementValue;
  }

  void print(llvm::raw_ostream &s) const;

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); };
};

/// An abstraction that represents the field locator in
/// an `AddElement` adjoint kind. Depending on the aggregate
/// kind - tuple or struct, of the `baseAdjoint` in an
/// `AddElement` adjoint, the field locator may be an `unsigned int`
/// or a `VarDecl *`.
struct FieldLocator final {
  FieldLocator(VarDecl *field) : inner(field) {}
  FieldLocator(unsigned int index) : inner(index) {}

  friend AddElementValue;

private:
  bool isTupleFieldLocator() const {
    return std::holds_alternative<unsigned int>(inner);
  }

  const static constexpr std::true_type TUPLE_FIELD_LOCATOR_TAG =
      std::true_type{};
  const static constexpr std::false_type STRUCT_FIELD_LOCATOR_TAG =
      std::false_type{};

  unsigned int getInner(std::true_type) const {
    return std::get<unsigned int>(inner);
  }

  VarDecl *getInner(std::false_type) const {
    return std::get<VarDecl *>(inner);
  }

  std::variant<unsigned int, VarDecl *> inner;
};

/// The underlying value for an `AddElement` adjoint.
struct AddElementValue final {
  AdjointValue baseAdjoint;
  AdjointValue eltToAdd;
  FieldLocator fieldLocator;

  AddElementValue(AdjointValue baseAdjoint, AdjointValue eltToAdd,
                  FieldLocator fieldLocator)
      : baseAdjoint(baseAdjoint), eltToAdd(eltToAdd),
        fieldLocator(fieldLocator) {
    assert(baseAdjoint.getType().is<TupleType>() ||
           baseAdjoint.getType().getStructOrBoundGenericStruct() != nullptr);
  }

  bool isTupleAdjoint() const { return fieldLocator.isTupleFieldLocator(); }

  bool isStructAdjoint() const { return !isTupleAdjoint(); }

  VarDecl *getFieldDecl() const {
    assert(isStructAdjoint());
    return this->fieldLocator.getInner(FieldLocator::STRUCT_FIELD_LOCATOR_TAG);
  }

  unsigned int getFieldIndex() const {
    assert(isTupleAdjoint());
    return this->fieldLocator.getInner(FieldLocator::TUPLE_FIELD_LOCATOR_TAG);
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
