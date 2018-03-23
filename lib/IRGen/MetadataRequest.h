//===--- MetadataRequest.h - Operations for accessing metadata --*- C++ -*-===//
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
// This file defines some types and operations for accessing type metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATAREQUEST_H
#define SWIFT_IRGEN_METADATAREQUEST_H

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/Types.h"

namespace llvm {
class Constant;
class Function;
class GlobalVariable;
class Value;
}

namespace swift {
enum ForDefinition_t : bool;

namespace irgen {
class ConstantReference;
class Explosion;
class IRGenFunction;
class IRGenModule;
enum class SymbolReferenceKind : unsigned char;

/// A dynamic metadata request.
class DynamicMetadataRequest {
  MetadataRequest StaticRequest;
  llvm::Value *DynamicRequest = nullptr;
public:
  DynamicMetadataRequest(MetadataRequest::BasicKind request)
    : DynamicMetadataRequest(MetadataRequest(request)) {}
  DynamicMetadataRequest(MetadataRequest request)
    : StaticRequest(request), DynamicRequest(nullptr) {}
  explicit DynamicMetadataRequest(llvm::Value *request)
    : StaticRequest(), DynamicRequest(request) {}

  bool isStatic() const { return DynamicRequest == nullptr; }
  MetadataRequest getStaticRequest() const {
    assert(isStatic());
    return StaticRequest;
  }

  llvm::Value *getDynamicRequest() const {
    assert(!isStatic());
    return DynamicRequest;
  }

  /// If a function call taking this request returns a MetadataResponse,
  /// can the status of a MetadataResponse be generally ignored?
  bool canResponseStatusBeIgnored() const {
    return isStatic() && StaticRequest.isBlocking();
  }

  /// Is this request statically known to be blocking on success?
  ///
  /// This is a useful query because the result of such a request is
  /// always statically-known complete.
  bool isStaticallyBlockingComplete() const {
    return isStatic() && StaticRequest == MetadataRequest::Complete;
  }

  llvm::Value *get(IRGenFunction &IGF) const;
};

/// See the comment for swift::MetadataResponse in Metadata.h.
class MetadataResponse {
  llvm::Value *Metadata;
  llvm::Value *State;

public:
  MetadataResponse() : Metadata(nullptr) {}

  /// A metadata response that might not be dynamically complete.
  explicit MetadataResponse(llvm::Value *metadata, llvm::Value *state)
      : Metadata(metadata), State(state) {
    assert(metadata && "must be valid");
    assert(state && "must be valid");
  }

  /// A metadata response that's known to be complete.
  static MetadataResponse forComplete(llvm::Value *metadata) {
    assert(metadata && "must be valid");
    MetadataResponse result;
    result.Metadata = metadata;
    result.State = nullptr;
    return result;
  }

  /// An undef metadata response.
  static MetadataResponse getUndef(IRGenFunction &IGF);

  bool isValid() const { return Metadata != nullptr; }
  explicit operator bool() const { return isValid(); }

  bool isStaticallyKnownComplete() const {
    assert(isValid());
    return State == nullptr;
  }

  llvm::Value *getMetadata() const {
    assert(isValid());
    return Metadata;
  }
  llvm::Value *getDynamicState(IRGenFunction &IGF) const;

  static MetadataResponse handle(IRGenFunction &IGF,
                                 DynamicMetadataRequest request,
                                 llvm::Value *responsePair);
  llvm::Value *combine(IRGenFunction &IGF) const;

  /// Return a constant value representing the fully-completed state
  /// (MetadataRequest::Complete).
  static llvm::Constant *getCompletedState(IRGenModule &IGM);
};

/// A dependency that is blocking a metadata initialization from completing.
class MetadataDependency {
  llvm::Value *RequiredMetadata;
  MetadataRequest::BasicKind RequiredState;
public:
  /// Construct the null dependency, i.e. the initialization is not blocked.
  MetadataDependency() : RequiredMetadata(nullptr) {}

  /// Construct a non-trivial dependency.
  MetadataDependency(llvm::Value *requiredMetadata,
                     MetadataRequest::BasicKind requiredState)
      : RequiredMetadata(requiredMetadata), RequiredState(requiredState) {
    assert(requiredMetadata != nullptr);
    assert(requiredState != MetadataRequest::Abstract &&
           "cannot have trivial requirement on abstract state!");
  }

  bool isTrivial() const { return RequiredMetadata == nullptr; }
  bool isNonTrivial() const { return RequiredMetadata != nullptr; }
  explicit operator bool() const { return isNonTrivial(); }

  /// Return the metadata that the initialization depends on.
  llvm::Value *getRequiredMetadata() const {
    assert(isNonTrivial());
    return RequiredMetadata;
  }

  /// Return the state that the metadata needs to reach before the
  /// initialization is unblocked.
  MetadataRequest::BasicKind getRequiredState() const {
    assert(isNonTrivial());
    return RequiredState;
  }
  llvm::Constant *getRequiredState(IRGenFunction &IGF) const;

  static llvm::Constant *getTrivialDependency(IRGenModule &IGM);

  llvm::Value *combine(IRGenFunction &IGF) const;
};

enum class MetadataAccessStrategy {
  /// There is a unique public accessor function for the given type metadata.
  PublicUniqueAccessor,

  /// There is a unique hidden accessor function for the given type metadata.
  HiddenUniqueAccessor,

  /// There is a unique private accessor function for the given type metadata.
  PrivateAccessor,

  /// There is no unique accessor function for the given type metadata, but
  /// one should be made automatically.
  NonUniqueAccessor
};

/// Is it basically trivial to access the given metadata?  If so, we don't
/// need a cache variable in its accessor.
bool isTypeMetadataAccessTrivial(IRGenModule &IGM, CanType type);

/// Determine how the given type metadata should be accessed.
MetadataAccessStrategy getTypeMetadataAccessStrategy(CanType type);

/// Return the address of a function that will return type metadata 
/// for the given non-dependent type.
llvm::Function *getOrCreateTypeMetadataAccessFunction(IRGenModule &IGM,
                                                      CanType type);

llvm::Function *getTypeMetadataAccessFunction(IRGenModule &IGM,
                                              CanType type,
                                              ForDefinition_t shouldDefine);

using MetadataAccessGenerator =
  llvm::function_ref<MetadataResponse(IRGenFunction &IGF,
                                      DynamicMetadataRequest request,
                                      llvm::Constant *cache)>;

llvm::Function *getTypeMetadataAccessFunction(IRGenModule &IGM,
                                              CanType type,
                                              ForDefinition_t shouldDefine,
                                             MetadataAccessGenerator generator);

llvm::Function *
getGenericTypeMetadataAccessFunction(IRGenModule &IGM,
                                     NominalTypeDecl *nominal,
                                     ForDefinition_t shouldDefine);

llvm::Constant *
getRequiredTypeMetadataAccessFunction(IRGenModule &IGM,
                                      NominalTypeDecl *theDecl,
                                      ForDefinition_t shouldDefine);

using InPlaceMetadataInitializer =
  llvm::function_ref<llvm::Value*(IRGenFunction &IGF, llvm::Value *metadata)>;

MetadataResponse
emitInPlaceTypeMetadataAccessFunctionBody(IRGenFunction &IGF,
                                          CanNominalType type,
                                          llvm::Constant *cacheVariable,
                                    InPlaceMetadataInitializer &&initializer);

llvm::Value *uniqueForeignTypeMetadataRef(IRGenFunction &IGF,
                                          llvm::Value *candidate);

llvm::Constant *getTypeRef(IRGenModule &IGM, CanType type);

using LazyCacheEmitter =
  llvm::function_ref<MetadataResponse(IRGenFunction &IGF, Explosion &params)>;

/// Emit the body of a lazy cache access function.
void emitLazyCacheAccessFunction(IRGenModule &IGM,
                                 llvm::Function *accessor,
                                 llvm::GlobalVariable *cache,
                                 LazyCacheEmitter getValue,
                                 bool isReadNone = true);

/// Emit a declaration reference to a metatype object.
void emitMetatypeRef(IRGenFunction &IGF, CanMetatypeType type,
                     Explosion &explosion);

/// Emit a reference to a compile-time constant piece of type metadata, or
/// return a null pointer if the type's metadata cannot be represented by a
/// constant.
ConstantReference tryEmitConstantTypeMetadataRef(IRGenModule &IGM,
                                                 CanType type,
                                                 SymbolReferenceKind refKind);

/// Get the type as it exists in Swift's runtime type system, removing any
/// erased generic parameters.
CanType getRuntimeReifiedType(IRGenModule &IGM, CanType type);

/// Emit a reference to a compile-time constant piece of heap metadata, or
/// return a null pointer if the type's heap metadata cannot be represented
/// by a constant.
llvm::Constant *tryEmitConstantHeapMetadataRef(IRGenModule &IGM,
                                               CanType type,
                                               bool allowUninitialized);

enum class MetadataValueType { ObjCClass, TypeMetadata };

/// Emit a reference to the heap metadata for a class.
///
/// \returns a value of type ObjCClassPtrTy or TypeMetadataPtrTy,
///    depending on desiredType
llvm::Value *emitClassHeapMetadataRef(IRGenFunction &IGF, CanType type,
                                      MetadataValueType desiredType,
                                      bool allowUninitialized = false);

/// Emit a reference to the (initialized) ObjC heap metadata for a class.
///
/// \returns a value of type ObjCClassPtrTy
llvm::Value *emitObjCHeapMetadataRef(IRGenFunction &IGF, ClassDecl *theClass,
                                     bool allowUninitialized = false);

/// Emit a reference to type metadata corresponding to the given
/// heap metadata.  This may be ObjCWrapper metadata if the heap metadata
/// is not a class.
llvm::Value *emitObjCMetadataRefForMetadata(IRGenFunction &IGF,
                                            llvm::Value *classPtr);

/// Given a class metadata reference, produce the appropriate heap
/// metadata reference for it.
llvm::Value *emitClassHeapMetadataRefForMetatype(IRGenFunction &IGF,
                                                 llvm::Value *metatype,
                                                 CanType type);

} // end namespace irgen
} // end namespace swift

#endif
