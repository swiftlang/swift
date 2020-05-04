//===--- GenericSignatureBuilder2.h - Generic signature builder --*- C++ -*-===//
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

#ifndef SWIFT_GENERICSIGNATUREBUILDER2_H
#define SWIFT_GENERICSIGNATUREBUILDER2_H

#include <vector>
#include "llvm/ADT/DenseMap.h"

namespace swift {

class CanType;
class ASTContext;
class CanGenericSignature;
class Requirement;

class GenericSignatureBuilder2 {
  class RewriteSystem;
  ASTContext &ctx;
  std::unique_ptr<RewriteSystem> rewriteSystem;
  llvm::DenseMap<CanType, std::vector<Requirement>> pendingRequirements;

  void addRequirement(Requirement req);
  void addSameTypeRequirement(CanType lhs, CanType rhs);

public:
  GenericSignatureBuilder2(ASTContext &ctx, CanGenericSignature sig);
  ~GenericSignatureBuilder2();

  GenericSignatureBuilder2(const GenericSignatureBuilder2 &) = delete;
  GenericSignatureBuilder2 &operator=(const GenericSignatureBuilder2 &) = delete;
};

} // end namespace swift

#endif
