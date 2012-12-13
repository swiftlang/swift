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
}

namespace swift {
  class ClassDecl;
  class GenericMemberRefExpr;
  class MemberRefExpr;

namespace irgen {
  class HeapLayout;
  class IRGenFunction;
  class IRGenModule;
  class LValue;

  LValue emitPhysicalClassMemberLValue(IRGenFunction &IGF, MemberRefExpr *E);
  LValue emitPhysicalClassMemberLValue(IRGenFunction &IGF,
                                       GenericMemberRefExpr *E);

  llvm::Constant *emitClassPrivateData(IRGenModule &IGM, ClassDecl *theClass);

} // end namespace irgen
} // end namespace swift

#endif
