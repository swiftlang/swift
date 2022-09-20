//===--- GenericRequirement.h - Generic requirement -------------*- C++ -*-===//
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

#ifndef SWIFT_AST_GENERIC_REQUIREMENT_H
#define SWIFT_AST_GENERIC_REQUIREMENT_H

#include "swift/AST/Type.h"

namespace swift {

class ProtocolDecl;

/// An abstract generic requirement.
struct GenericRequirement {
  CanType TypeParameter;
  ProtocolDecl *Protocol;
};

} // end namespace swift

namespace llvm {
template <> struct DenseMapInfo<swift::GenericRequirement> {
  using GenericRequirement = swift::GenericRequirement;
  using CanTypeInfo = llvm::DenseMapInfo<swift::CanType>;
  static GenericRequirement getEmptyKey() {
    return {CanTypeInfo::getEmptyKey(), nullptr};
  }
  static GenericRequirement getTombstoneKey() {
    return {CanTypeInfo::getTombstoneKey(), nullptr};
  }
  static llvm::hash_code getHashValue(GenericRequirement req) {
    return hash_combine(CanTypeInfo::getHashValue(req.TypeParameter),
                        hash_value(req.Protocol));
  }
  static bool isEqual(GenericRequirement lhs, GenericRequirement rhs) {
    return (lhs.TypeParameter == rhs.TypeParameter &&
            lhs.Protocol == rhs.Protocol);
  }
};
} // end namespace llvm

#endif // SWIFT_AST_GENERIC_REQUIREMENT_H
