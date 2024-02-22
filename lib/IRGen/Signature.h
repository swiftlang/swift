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

#include "MetadataSource.h"
#include "swift/AST/Types.h"
#include "swift/Basic/ExternalUnion.h"
#include "swift/IRGen/GenericRequirement.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallingConv.h"

namespace llvm {
  class FunctionType;
}

namespace clang {
  class CXXConstructorDecl;
  namespace CodeGen {
    class CGFunctionInfo;    
  }
}

namespace swift {
  class Identifier;
  enum class SILFunctionTypeRepresentation : uint8_t;
  class SILType;

namespace irgen {

class FunctionPointerKind;
class IRGenModule;
class TypeInfo;

/// An encapsulation of different foreign calling-convention lowering
/// information we might have.  Should be interpreted according to the
/// abstract CC of the formal function type.
class ForeignFunctionInfo {
public:
  const clang::CodeGen::CGFunctionInfo *ClangInfo = nullptr;
  /// True if the foreign function can throw an Objective-C / C++ exception.
  bool canThrow = false;
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
  llvm::StructType *indirectResultsType = nullptr;
};

namespace {
  class SignatureExpansion;
}

class AsyncInfo {
public:
  uint32_t AsyncContextIdx = 0;
  uint32_t AsyncResumeFunctionSwiftSelfIdx = 0;
};

/// Represents the source of the corresponding type pointer computed
/// during the expansion of the polymorphic signature.
///
/// The source is either a \c GenericRequirement, or a \c MetadataSource.
class PolymorphicSignatureExpandedTypeSource {
public:
  inline PolymorphicSignatureExpandedTypeSource(
      const GenericRequirement &requirement)
      : requirement(requirement){};
  inline PolymorphicSignatureExpandedTypeSource(
      const MetadataSource &metadataSource)
      : metadataSource(metadataSource) {}

  inline void
  visit(llvm::function_ref<void(const GenericRequirement &)> requirementVisitor,
        llvm::function_ref<void(const MetadataSource &)> metadataSourceVisitor)
      const {
    if (requirement)
      return requirementVisitor(*requirement);
    return metadataSourceVisitor(*metadataSource);
  }

private:
  std::optional<GenericRequirement> requirement;
  std::optional<MetadataSource> metadataSource;
};

/// Recorded information about the specific ABI details.
class SignatureExpansionABIDetails {
public:
  /// Recorded information about the direct result type convention.
  struct DirectResult {
    std::reference_wrapper<const irgen::TypeInfo> typeInfo;
    inline DirectResult(const irgen::TypeInfo &typeInfo) : typeInfo(typeInfo) {}
  };
  /// The direct result, or \c None if direct result is void.
  std::optional<DirectResult> directResult;
  /// Recorded information about the indirect result parameters convention.
  struct IndirectResult {
    /// Does this indirect result parameter have the `sret` attribute?
    bool hasSRet;
  };
  /// The indirect results passed as parameters to the call.
  llvm::SmallVector<IndirectResult, 1> indirectResults;
  /// Recorded information about the parameter convention.
  struct Parameter {
    std::reference_wrapper<const irgen::TypeInfo> typeInfo;
    ParameterConvention convention;
    bool isSelf;

    inline Parameter(const irgen::TypeInfo &typeInfo,
                     ParameterConvention convention)
        : typeInfo(typeInfo), convention(convention), isSelf(false) {}
  };
  /// The parameters passed to the call.
  llvm::SmallVector<Parameter, 8> parameters;
  /// Type sources added to the signature during expansion.
  llvm::SmallVector<PolymorphicSignatureExpandedTypeSource, 2>
      polymorphicSignatureExpandedTypeSources;
  /// True if a trailing self parameter is passed to the call.
  bool hasTrailingSelfParam = false;
  /// True if a context parameter passed to the call.
  bool hasContextParam = false;
  /// True if an error result value indirect parameter is passed to the call.
  bool hasErrorResult = false;
  /// The number of LLVM IR parameters in the LLVM IR function signature.
  size_t numParamIRTypesInSignature = 0;
};

/// A signature represents something which can actually be called.
class Signature {
  using ExtraData =
      SimpleExternalUnion<void, ForeignFunctionInfo, CoroutineInfo, AsyncInfo>;

  llvm::FunctionType *Type;
  llvm::AttributeList Attributes;
  llvm::CallingConv::ID CallingConv;
  ExtraData::Kind ExtraDataKind; // packed with above
  ExtraData ExtraDataStorage;
  std::optional<SignatureExpansionABIDetails> ABIDetails;
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
  static Signature
  getUncached(IRGenModule &IGM, CanSILFunctionType formalType,
              FunctionPointerKind kind, bool forStaticCall = false,
              const clang::CXXConstructorDecl *cxxCtorDecl = nullptr);

  static SignatureExpansionABIDetails
  getUncachedABIDetails(IRGenModule &IGM, CanSILFunctionType formalType,
                        FunctionPointerKind kind);

  /// Compute the signature of a coroutine's continuation function.
  static Signature forCoroutineContinuation(IRGenModule &IGM,
                                            CanSILFunctionType coroType);

  static Signature forAsyncReturn(IRGenModule &IGM,
                                  CanSILFunctionType asyncType);
  static Signature forAsyncAwait(IRGenModule &IGM, CanSILFunctionType asyncType,
                                 FunctionPointerKind kind);
  static Signature forAsyncEntry(IRGenModule &IGM, CanSILFunctionType asyncType,
                                 FunctionPointerKind kind);

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

  AsyncInfo getAsyncInfo() const {
    assert(isValid());
    if (auto info = ExtraDataStorage.dyn_cast<AsyncInfo>(ExtraDataKind))
      return *info;
    return AsyncInfo();
  }

  uint32_t getAsyncContextIndex() const {
    return getAsyncInfo().AsyncContextIdx;
  }
  uint32_t getAsyncResumeFunctionSwiftSelfIndex() const {
    return getAsyncInfo().AsyncResumeFunctionSwiftSelfIdx;
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

  const SignatureExpansionABIDetails &getABIDetails() {
    assert(ABIDetails.has_value());
    return *ABIDetails;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
