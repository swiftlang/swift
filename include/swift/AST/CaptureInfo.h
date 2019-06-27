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
#include "llvm/ADT/PointerUnion.h"
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
class OpaqueValueExpr;

/// CapturedValue includes both the declaration being captured, along with flags
/// that indicate how it is captured.
class CapturedValue {
public:
  using Storage =
      llvm::PointerIntPair<llvm::PointerUnion<ValueDecl*, OpaqueValueExpr*>, 2,
                           unsigned>;

private:
  Storage Value;

  explicit CapturedValue(Storage V) : Value(V) {}

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

  CapturedValue(llvm::PointerUnion<ValueDecl*, OpaqueValueExpr*> Ptr,
                unsigned Flags)
      : Value(Ptr, Flags) {}

  static CapturedValue getDynamicSelfMetadata() {
    return CapturedValue((ValueDecl *)nullptr, 0);
  }

  bool isDirect() const { return Value.getInt() & IsDirect; }
  bool isNoEscape() const { return Value.getInt() & IsNoEscape; }

  bool isDynamicSelfMetadata() const { return !Value.getPointer(); }
  bool isOpaqueValue() const {
    return Value.getPointer().is<OpaqueValueExpr *>();
  }

  CapturedValue mergeFlags(CapturedValue cv) {
    assert(Value.getPointer() == cv.Value.getPointer() &&
           "merging flags on two different value decls");
    return CapturedValue(Value.getPointer(), getFlags() & cv.getFlags());
  }

  ValueDecl *getDecl() const {
    assert(Value.getPointer() && "dynamic Self metadata capture does not "
           "have a value");
    return Value.getPointer().dyn_cast<ValueDecl *>();
  }

  OpaqueValueExpr *getOpaqueValue() const {
    assert(Value.getPointer() && "dynamic Self metadata capture does not "
           "have a value");
    return Value.getPointer().dyn_cast<OpaqueValueExpr *>();
  }


  unsigned getFlags() const { return Value.getInt(); }
};

} // end swift namespace

namespace swift {

class DynamicSelfType;

/// Stores information about captured variables.
class CaptureInfo {
  const CapturedValue *Captures;
  DynamicSelfType *DynamicSelf;
  OpaqueValueExpr *OpaqueValue;
  unsigned Count = 0;
  bool GenericParamCaptures : 1;
  bool Computed : 1;

public:
  CaptureInfo()
    : Captures(nullptr), DynamicSelf(nullptr), OpaqueValue(nullptr), Count(0),
      GenericParamCaptures(0), Computed(0) { }

  bool hasBeenComputed() { return Computed; }

  bool isTrivial() {
    return Count == 0 && !GenericParamCaptures && !DynamicSelf && !OpaqueValue;
  }

  ArrayRef<CapturedValue> getCaptures() const {
    return llvm::makeArrayRef(Captures, Count);
  }
  void setCaptures(ArrayRef<CapturedValue> C) {
    Captures = C.data();
    Computed = true;
    Count = C.size();
  }

  /// Return a filtered list of the captures for this function,
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

  bool hasOpaqueValueCapture() const {
    return OpaqueValue != nullptr;
  }

  OpaqueValueExpr *getOpaqueValue() const {
    return OpaqueValue;
  }

  void setOpaqueValue(OpaqueValueExpr *OVE) {
    OpaqueValue = OVE;
  }

  void dump() const;
  void print(raw_ostream &OS) const;
};

} // namespace swift

#endif // LLVM_SWIFT_AST_CAPTURE_INFO_H

