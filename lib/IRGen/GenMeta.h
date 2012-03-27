//===--- GenMeta.h - Swift IR generation for metadata ----------*- C++ -*-===//
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
//  This file provides the private interface to the metadata emission code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENMETA_H
#define SWIFT_IRGEN_GENMETA_H

namespace swift {
  class MetaTypeType;

namespace irgen {
  class Explosion;
  class IRGenFunction;

  /// Emit a requalification expression as an r-value.
  void emitMetaTypeRef(IRGenFunction &IGF, Type type, Explosion &explosion);

} // end namespace irgen
} // end namespace swift

#endif
