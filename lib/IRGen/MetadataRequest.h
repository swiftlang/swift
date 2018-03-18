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

namespace llvm {
  class Value;
}

namespace swift {
namespace irgen {
  class IRGenFunction;

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

  /// A metadata response that's known to be complete.
  explicit MetadataResponse(llvm::Value *metadata)
      : Metadata(metadata), State(nullptr) {
    assert(metadata && "must be valid");
  }

  /// A metadata response that might not be dynamically complete.
  explicit MetadataResponse(llvm::Value *metadata, llvm::Value *state)
      : Metadata(metadata), State(state) {
    assert(metadata && "must be valid");
    assert(state && "must be valid");
  }

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

  static MetadataResponse split(IRGenFunction &IGF,
                                DynamicMetadataRequest request,
                                llvm::Value *responsePair);
  static MetadataResponse split(IRGenFunction &IGF,
                                llvm::Value *responsePair);
  llvm::Value *combine(IRGenFunction &IGF) const;

  /// Return a constant value representing the fully-completed state
  /// (MetadataRequest::Complete).
  static llvm::Constant *getCompletedState(IRGenModule &IGM);
};

} // end namespace irgen
} // end namespace swift

#endif
