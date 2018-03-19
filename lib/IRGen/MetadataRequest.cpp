//===--- MetadataRequest.cpp - IR generation for metadata requests --------===//
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
//  This file implements IR generation for accessing metadata.
//
//===----------------------------------------------------------------------===//

#include "IRGenFunction.h"
#include "IRGenModule.h"

#include "MetadataRequest.h"

using namespace swift;
using namespace irgen;

llvm::Value *DynamicMetadataRequest::get(IRGenFunction &IGF) const {
  if (isStatic()) {
    return IGF.IGM.getSize(Size(StaticRequest.getOpaqueValue()));
  } else {
    return DynamicRequest;
  }
}

MetadataResponse
MetadataResponse::split(IRGenFunction &IGF, DynamicMetadataRequest request,
                        llvm::Value *response) {
  if (request.isStaticallyBlockingComplete()) {
    assert(response->getType() == IGF.IGM.TypeMetadataResponseTy);
    auto value = IGF.Builder.CreateExtractValue(response, 0);
    return MetadataResponse(value);
  } else {
    return split(IGF, response);
  }
}

MetadataResponse
MetadataResponse::split(IRGenFunction &IGF, llvm::Value *response) {
  assert(response->getType() == IGF.IGM.TypeMetadataResponseTy);
  auto value = IGF.Builder.CreateExtractValue(response, 0);
  auto state = IGF.Builder.CreateExtractValue(response, 1);
  return MetadataResponse(value, state);
}

llvm::Value *MetadataResponse::combine(IRGenFunction &IGF) const {
  assert(isValid());
  llvm::Value *pair =
    llvm::UndefValue::get(IGF.IGM.TypeMetadataResponseTy);
  pair = IGF.Builder.CreateInsertValue(pair, Metadata, 0);
  pair = IGF.Builder.CreateInsertValue(pair, getDynamicState(IGF), 1);
  return pair;
}

llvm::Value *MetadataResponse::getDynamicState(IRGenFunction &IGF) const {
  assert(isValid());
  return State ? State : getCompletedState(IGF.IGM);
}

llvm::Constant *MetadataResponse::getCompletedState(IRGenModule &IGM) {
  return IGM.getSize(Size(MetadataRequest::Complete));
}

// TODO: move most of the rest of metadata-reference emission here...
