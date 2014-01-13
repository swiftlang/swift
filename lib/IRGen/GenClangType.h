//==--- GenClangType.h - Interface for Clang AST Type Generation -*- C++ -*-==//
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
// This file defines the private interface used for turning Swift AST
// types that are representable in C and Objective-C interfaces into
// Clang AST types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCLANGTYPE_H
#define SWIFT_IRGEN_GENCLANGTYPE_H

#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "clang/AST/CanonicalType.h"

namespace swift {
namespace irgen {

/// Given a Swift type, attempt to return an appropriate Clang
/// CanQualType for the purpose of generating correct code for the
/// ABI.
class GenClangType : public CanTypeVisitor<GenClangType, clang::CanQualType> {
public:
  /// Return the Clang struct type which was imported and resulted in
  /// this Swift struct type. We do not currently handle generating a
  /// new Clang struct type for Swift struct types that are created
  /// independently of importing a Clang module.
  clang::CanQualType visitStructType(CanStructType type);
  clang::CanQualType visitTupleType(CanTupleType type);
  clang::CanQualType visitType(CanType type);
};

} // end namespace irgen
} // end namespace swift

#endif
