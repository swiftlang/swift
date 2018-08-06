//===--- CaptureInfo.h - Data Structure for Capture Lists -------*- C++ -*-===//
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

#ifndef SWIFT_AST_CAPTURE_INFO_H
#define SWIFT_AST_CAPTURE_INFO_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/TypeAlignments.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerIntPair.h"
#include <vector>

namespace swift {
class CapturedValue;
}

namespace llvm {
class raw_ostream;
template <> struct DenseMapInfo<swift::CapturedValue>;
}

namespace swift {
class ValueDecl;
class FuncDecl;

/// CapturedValue includes both the declaration being captured, along with flags
/// that indicate how it is captured.
class CapturedValue {
  llvm::PointerIntPair<ValueDecl*, 2, unsigned> Value;

  explicit CapturedValue(llvm::PointerIntPair<ValueDecl*, 2, unsigned> V) : Value(V) {}

public:
  friend struct llvm::DenseMapInfo<CapturedValue>;

  enum {
    /// IsDirect is set when a VarDecl with storage *and* accessors is captured
    /// by its storage address.  This happens in the accessors for the VarDecl.
    IsDirect = 1 << 0,

    /// IsNoEscape is set when a vardecl is captured by a noescape closure, and
    /// thus has its lifetime guaranteed.  It can be closed over by a fixed
    /// address if it has storage.
    IsNoEscape = 1 << 1
  };

  CapturedValue(ValueDecl *D, unsigned Flags) : Value(D, Flags) {}

  static CapturedValue getDynamicSelfMetadata() {
    return CapturedValue(nullptr, 0);
  }

  bool isDirect() const { return Value.getInt() & IsDirect; }
  bool isNoEscape() const { return Value.getInt() & IsNoEscape; }

  bool isDynamicSelfMetadata() const { return !Value.getPointer(); }

  CapturedValue mergeFlags(CapturedValue cv) {
    assert(Value.getPointer() == cv.Value.getPointer() &&
           "merging flags on two different value decls");
    return CapturedValue(Value.getPointer(), getFlags() & cv.getFlags());
  }

  ValueDecl *getDecl() const {
    assert(Value.getPointer() && "dynamic Self metadata capture does not "
           "have a value");
    return Value.getPointer();
  }

  unsigned getFlags() const { return Value.getInt(); }

  bool operator==(CapturedValue RHS) const {
    return Value == RHS.Value;
  }

  bool operator!=(CapturedValue RHS) const {
    return Value != RHS.Value;
  }

  bool operator<(CapturedValue RHS) const {
    return Value < RHS.Value;
  }
};

} // end swift namespace

namespace llvm {

template <> struct DenseMapInfo<swift::CapturedValue> {
  using CapturedValue = swift::CapturedValue;

  using PtrIntPairDenseMapInfo =
      DenseMapInfo<llvm::PointerIntPair<swift::ValueDecl *, 2, unsigned>>;

  static inline swift::CapturedValue getEmptyKey() {
    return CapturedValue{PtrIntPairDenseMapInfo::getEmptyKey()};
  }

  static inline CapturedValue getTombstoneKey() {
    return CapturedValue{PtrIntPairDenseMapInfo::getTombstoneKey()};
  }

  static unsigned getHashValue(const CapturedValue &Val) {
    return PtrIntPairDenseMapInfo::getHashValue(Val.Value);
  }

  static bool isEqual(const CapturedValue &LHS, const CapturedValue &RHS) {
    return PtrIntPairDenseMapInfo::isEqual(LHS.Value, RHS.Value);
  }
};

} // end llvm namespace

namespace swift {

class DynamicSelfType;

/// \brief Stores information about captured variables.
class CaptureInfo {
  const CapturedValue *Captures;
  DynamicSelfType *DynamicSelf;
  unsigned Count = 0;
  bool GenericParamCaptures : 1;
  bool Computed : 1;

public:
  CaptureInfo()
    : Captures(nullptr), DynamicSelf(nullptr), Count(0),
      GenericParamCaptures(0), Computed(0) { }

  bool hasBeenComputed() { return Computed; }

  bool isTrivial() {
    return Count == 0 && !GenericParamCaptures && !DynamicSelf;
  }

  ArrayRef<CapturedValue> getCaptures() const {
    return llvm::makeArrayRef(Captures, Count);
  }
  void setCaptures(ArrayRef<CapturedValue> C) {
    Captures = C.data();
    Computed = true;
    Count = C.size();
  }

  /// \brief Return a filtered list of the captures for this function,
  /// filtering out global variables.  This function returns the list that
  /// actually needs to be closed over.
  ///
  void getLocalCaptures(SmallVectorImpl<CapturedValue> &Result) const;

  /// \returns true if getLocalCaptures() will return a non-empty list.
  bool hasLocalCaptures() const;

  /// \returns true if the function captures any generic type parameters.
  bool hasGenericParamCaptures() const {
    return GenericParamCaptures;
  }

  void setGenericParamCaptures(bool genericParamCaptures) {
    GenericParamCaptures = genericParamCaptures;
  }

  /// \returns true if the function captures the dynamic Self type.
  bool hasDynamicSelfCapture() const {
    return DynamicSelf != nullptr;
  }

  /// \returns the captured dynamic Self type, if any.
  DynamicSelfType *getDynamicSelfType() const {
    return DynamicSelf;
  }

  void setDynamicSelfType(DynamicSelfType *dynamicSelf) {
    DynamicSelf = dynamicSelf;
  }

  void dump() const;
  void print(raw_ostream &OS) const;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_CAPTURE_INFO_H

