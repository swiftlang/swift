//===--- Signature.h - An IR function signature -----------------*- C++ -*-===//
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
//  This file defines the Signature type, which encapsulates all the
//  information necessary to call a function value correctly.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SIGNATURE_H
#define SWIFT_IRGEN_SIGNATURE_H

#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallingConv.h"
#include "swift/AST/Types.h"
#include "swift/Basic/ExternalUnion.h"

namespace llvm {
  class FunctionType;
}

namespace clang {
  namespace CodeGen {
    class CGFunctionInfo;    
  }
}

namespace swift {
  class Identifier;
  enum class SILFunctionTypeRepresentation : uint8_t;
  class SILType;

namespace irgen {

class IRGenModule;

/// An encapsulation of different foreign calling-convention lowering
/// information we might have.  Should be interpreted according to the
/// abstract CC of the formal function type.
class ForeignFunctionInfo {
public:
  const clang::CodeGen::CGFunctionInfo *ClangInfo = nullptr;
};

/// An encapsulation of the extra lowering information we might want to
/// store about a coroutine.
///
/// The ABI for yields breaks the yielded values down into a scalar type
/// sequence, much like argument lowering does: indirect yields (either
/// due to abstraction or size) become pointers and direct yields undergo
/// the general expansion algorithm.  We then find the longest prefix of
/// the resulting sequence for which the concatenation
///   (continuation function pointer) + prefix + (optional remainder pointer)
/// is a legal return type according to the swiftcall ABI.  The remainder
/// pointer must be included whenever the prefix is strict; it points to
/// a structure containing the remainder of the type sequence, with each
/// element naturally aligned.
///
/// For example, suppose the yields are
///   @in Any, @inout Double, @in Int64, Float, Int8, Int16, Int32
/// This is expanded to the legal type sequence:
///   Any*, double*, i64*, float, i8, i16, i32
/// To the beginning is always appended a continuation function pointer:
///   void (...)*, Any*, double*, i64*, float, i8, i16, i32
/// Suppose that the current target can support at most 4 scalar results.
/// Then the final sequence will be:
///   void (...)*, Any*, double*, { i64*, float, i8, i16, i32 }*
///
/// This final sequence becomes the result type of the coroutine's ramp
/// function and (if a yield_many coroutine) its continuation functions.
class CoroutineInfo {
public:
  /// The number of yield components that are returned directly in the
  /// coroutine return value.
  unsigned NumDirectYieldComponents = 0;
};

namespace {
  class SignatureExpansion;
}

/// A signature represents something which can actually be called.
class Signature {
  using ExtraData = SimpleExternalUnion<void,
                                        ForeignFunctionInfo,
                                        CoroutineInfo>;

  llvm::FunctionType *Type;
  llvm::AttributeList Attributes;
  llvm::CallingConv::ID CallingConv;
  ExtraData::Kind ExtraDataKind; // packed with above
  ExtraData ExtraDataStorage;
  static_assert(ExtraData::union_is_trivially_copyable,
                "not trivially copyable");

  friend class irgen::SignatureExpansion;

public:
  Signature() : Type(nullptr) {}
  Signature(llvm::FunctionType *fnType, llvm::AttributeList attrs,
            llvm::CallingConv::ID callingConv)
    : Type(fnType), Attributes(attrs), CallingConv(callingConv),
      ExtraDataKind(ExtraData::kindForMember<void>()) {}

  bool isValid() const {
    return Type != nullptr;
  }

  /// Compute the signature of the given type.
  ///
  /// This is a private detail of the implementation of
  /// IRGenModule::getSignature(CanSILFunctionType), which is what
  /// clients should generally be using.
  static Signature getUncached(IRGenModule &IGM,
                               CanSILFunctionType formalType);

  /// Compute the signature of a coroutine's continuation function.
  static Signature forCoroutineContinuation(IRGenModule &IGM,
                                            CanSILFunctionType coroType);

  llvm::FunctionType *getType() const {
    assert(isValid());
    return Type;
  }

  llvm::CallingConv::ID getCallingConv() const {
    assert(isValid());
    return CallingConv;
  }

  llvm::AttributeList getAttributes() const {
    assert(isValid());
    return Attributes;
  }

  ForeignFunctionInfo getForeignInfo() const {
    assert(isValid());
    if (auto info =
          ExtraDataStorage.dyn_cast<ForeignFunctionInfo>(ExtraDataKind))
      return *info;
    return ForeignFunctionInfo();
  }

  CoroutineInfo getCoroutineInfo() const {
    assert(isValid());
    if (auto info = ExtraDataStorage.dyn_cast<CoroutineInfo>(ExtraDataKind))
      return *info;
    return CoroutineInfo();
  }

  // The mutators below should generally only be used when building up
  // a callee.

  void setType(llvm::FunctionType *type) {
    Type = type;
  }

  llvm::AttributeList &getMutableAttributes() & {
    assert(isValid());
    return Attributes;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
