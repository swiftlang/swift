//===--- RequirementMachine.h - Generics with term rewriting ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REQUIREMENTMACHINE_H
#define SWIFT_REQUIREMENTMACHINE_H

namespace swift {

class ASTContext;
class AssociatedTypeDecl;
class CanGenericSignature;
class CanType;
class GenericSignature;
class ProtocolDecl;
class Requirement;

namespace rewriting {

class Term;

Term getTermForType(CanType paramType, const ProtocolDecl *proto);

} // end namespace rewriting

/// Wraps a rewrite system with higher-level operations in terms of
/// generic signatures and interface types.
class RequirementMachine final {
  friend class ASTContext;

  struct Implementation;

  ASTContext &Context;
  Implementation *Impl;

  explicit RequirementMachine(ASTContext &ctx);

  RequirementMachine(const RequirementMachine &) = delete;
  RequirementMachine(RequirementMachine &&) = delete;
  RequirementMachine &operator=(const RequirementMachine &) = delete;
  RequirementMachine &operator=(RequirementMachine &&) = delete;

  void addGenericSignature(CanGenericSignature sig);

  bool isComplete() const;
  void markComplete();

public:
  ~RequirementMachine();
};

} // end namespace swift

#endif