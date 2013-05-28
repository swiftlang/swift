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
  class ExtensionDecl;
  class GenericMemberRefExpr;
  class MemberRefExpr;
  class CanType;
  class VarDecl;
  class SILType;
  class Type;

namespace irgen {
  class HeapLayout;
  class IRGenFunction;
  class IRGenModule;
  class OwnedAddress;

  OwnedAddress projectPhysicalClassMemberAddress(IRGenFunction &IGF,
                                                 llvm::Value *base,
                                                 SILType baseType,
                                                 VarDecl *field);

  llvm::Constant *emitClassPrivateData(IRGenModule &IGM, ClassDecl *theClass);
  llvm::Constant *emitCategoryData(IRGenModule &IGM, ExtensionDecl *ext);

  /// Emit an allocation of a class.
  llvm::Value *emitClassAllocation(IRGenFunction &IGF, SILType thisType);

  /// Emit the deallocating destructor for a class in terms of its destroying
  /// destructor.
  void emitDeallocatingDestructor(IRGenModule &IGM,
                                  ClassDecl *theClass,
                                  llvm::Function *deallocator,
                                  llvm::Function *destroyer);
  
  /// True if the value is of class type, or of a type that is bridged to class
  /// type in the ObjC world.
  bool hasObjCClassRepresentation(IRGenModule &IGM, Type t);
} // end namespace irgen
} // end namespace swift

#endif
