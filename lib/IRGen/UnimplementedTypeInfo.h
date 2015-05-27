//===--- UnimplementedTypeInfo.h - Stub for implemented layout --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This TypeInfo implementation stubs out all type info operations to
// simply return 'undef'. It is intended to be used to allow recovery from
// unimplemented type layout.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_UNIMPLEMENTEDTYPEINFO_H
#define SWIFT_IRGEN_UNIMPLEMENTEDTYPEINFO_H

#include "TypeInfo.h"

namespace swift {
namespace irgen {

class UnimplementedTypeInfo : public TypeInfo {
public:
  UnimplementedTypeInfo(IRGenModule &IGM, llvm::Type *storageTy);
  
  std::pair<llvm::Value*,llvm::Value*>
  getSizeAndAlignmentMask(IRGenFunction &IGF, SILType T) const override;
  
  std::tuple<llvm::Value*,llvm::Value*,llvm::Value*>
  getSizeAndAlignmentMaskAndStride(IRGenFunction &IGF, SILType T) const override;
  
  llvm::Value *getSize(IRGenFunction &IGF, SILType T) const override;
  llvm::Value *getAlignmentMask(IRGenFunction &IGF, SILType T) const override;
  llvm::Value *getStride(IRGenFunction &IGF, SILType T) const override;
  llvm::Constant *getStaticSize(IRGenModule &IGM) const override;
  llvm::Constant *getStaticAlignmentMask(IRGenModule &IGM) const override;
  llvm::Constant *getStaticStride(IRGenModule &IGM) const override;
  void getSchema(ExplosionSchema &schema) const override;
  ContainedAddress allocateStack(IRGenFunction &IGF,
                                 SILType T,
                                 const llvm::Twine &name) const override;
  void deallocateStack(IRGenFunction &IGF, Address addr,
                       SILType T) const override;
  OwnedAddress allocateBox(IRGenFunction &IGF, SILType T,
                           const llvm::Twine &name) const override;
  void deallocateBox(IRGenFunction &IGF, llvm::Value *boxOwner,
                     SILType T) const override;
  void assignWithCopy(IRGenFunction &IGF, Address dest,
                      Address src, SILType T) const override;
  void assignWithTake(IRGenFunction &IGF, Address dest,
                      Address src, SILType T) const override;
  void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr, SILType T) const override;
  void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                          Address srcAddr, SILType T) const override;
  void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                            Address src, SILType T) const override;
  void destroy(IRGenFunction &IGF, Address address, SILType T) const override;
  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override;
  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src,
                                       SILType T) const override;
  void storeExtraInhabitant(IRGenFunction &IGF,
                            llvm::Value *index,
                            Address dest,
                            SILType T) const override;
  void initializeMetadata(IRGenFunction &IGF,
                          llvm::Value *metadata,
                          llvm::Value *vwtable,
                          SILType T) const override;
  
  llvm::Value *isDynamicallyPackedInline(IRGenFunction &IGF,
                                         SILType T) const override;
  
  static bool classof(const TypeInfo *type) {
    return type->getSpecialTypeInfoKind() <= STIK_Unimplemented;
  }
};
  
}
}

#endif

