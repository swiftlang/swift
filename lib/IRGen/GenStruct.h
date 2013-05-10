//===--- GenStruct.h - Swift IR generation for structs ------------*- C++ -*-===//
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
//  This file provides the private interface to the struct-emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENSTRUCT_H
#define SWIFT_IRGEN_GENSTRUCT_H

namespace swift {
  class GenericMemberRefExpr;
  class MemberRefExpr;
  class CanType;
  class SILType;

namespace irgen {
  class IRGenFunction;
  class OwnedAddress;
  class Explosion;
  
  OwnedAddress projectPhysicalStructMemberAddress(IRGenFunction &IGF,
                                                  OwnedAddress base,
                                                  SILType baseType,
                                                  unsigned fieldIndex);
  void projectPhysicalStructMemberFromExplosion(IRGenFunction &IGF,
                                                SILType baseType,
                                                Explosion &base,
                                                unsigned fieldNo,
                                                Explosion &out);

} // end namespace irgen
} // end namespace swift

#endif
