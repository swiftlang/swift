//===--- GenStruct.h - Swift IR generation for classes ------------*- C++ -*-===//
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
//  This file provides the private interface to the class-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCLASS_H
#define SWIFT_IRGEN_GENCLASS_H

namespace llvm {
  class Constant;
  class Value;
  class Function;
}

namespace swift {
  class ClassDecl;
  class GenericMemberRefExpr;
  class MemberRefExpr;
  class CanType;
  class VarDecl;

namespace irgen {
  class HeapLayout;
  class IRGenFunction;
  class IRGenModule;
  class LValue;
  class OwnedAddress;

  OwnedAddress projectPhysicalClassMemberAddress(IRGenFunction &IGF,
                                                 llvm::Value *base,
                                                 CanType baseType,
                                                 VarDecl *field);

  LValue emitPhysicalClassMemberLValue(IRGenFunction &IGF, MemberRefExpr *E);
  LValue emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                       GenericMemberRefExpr *E);

  llvm::Constant *emitClassPrivateData(IRGenModule &IGM, ClassDecl *theClass);

  /// Emit an allocation of a class.
  llvm::Value *emitClassAllocation(IRGenFunction &IGF, CanType thisType);

  /// Emit the deallocating destructor for a class in terms of its destroying
  /// destructor.
  void emitDeallocatingDestructor(IRGenModule &IGM,
                                  ClassDecl *theClass,
                                  llvm::Function *deallocator,
                                  llvm::Function *destroyer);
} // end namespace irgen
} // end namespace swift

#endif
