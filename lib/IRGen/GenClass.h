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

namespace swift {
  class MemberRefExpr;

namespace irgen {
  class IRGenFunction;
  class LValue;


  LValue emitPhysicalClassMemberLValue(IRGenFunction &IGF, MemberRefExpr *E);

} // end namespace irgen
} // end namespace swift

#endif
