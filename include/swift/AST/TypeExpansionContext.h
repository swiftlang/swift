//===--- TypeExpansionContext.h - Swift Type Expansion Context --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the TypeExpansionContext class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPEEXPANSIONCONTEXT_H
#define SWIFT_TYPEEXPANSIONCONTEXT_H

#include "swift/AST/ResilienceExpansion.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class DeclContext;
  class SILFunction;

/// Describes the context in which SIL types should eventually be expanded.
/// Required for lowering resilient types and deciding whether to look through
/// opaque result types to their underlying type.
class TypeExpansionContext {
  ResilienceExpansion expansion;
  // The context (module, function, ...) we are expanding the type in.
  const DeclContext *inContext;
  // Is the context in which we are expanding in the whole module.
  bool isContextWholeModule;

  // The minimal expansion.
  TypeExpansionContext() {
    inContext = nullptr;
    expansion = ResilienceExpansion::Minimal;
    isContextWholeModule = false;
  }

public:

  // Infer the expansion for the SIL function.
  TypeExpansionContext(const SILFunction &f);

  TypeExpansionContext(ResilienceExpansion expansion,
                       const DeclContext *inContext, bool isWholeModuleContext)
      : expansion(expansion), inContext(inContext),
        isContextWholeModule(isWholeModuleContext) {}

  ResilienceExpansion getResilienceExpansion() const { return expansion; }

  const DeclContext *getContext() const { return inContext; }

  bool isWholeModuleContext() const { return isContextWholeModule; }

  bool shouldLookThroughOpaqueTypeArchetypes() const {
    return inContext != nullptr;
  }

  bool isMinimal() const { return *this == TypeExpansionContext(); }

  static TypeExpansionContext minimal() {
    return TypeExpansionContext();
  }

  static TypeExpansionContext maximal(const DeclContext *inContext,
                                      bool isContextWholeModule) {
    return TypeExpansionContext(ResilienceExpansion::Maximal, inContext,
                                isContextWholeModule);
  }

  static TypeExpansionContext maximalResilienceExpansionOnly() {
    return maximal(nullptr, false);
  }

  static TypeExpansionContext
  noOpaqueTypeArchetypesSubstitution(ResilienceExpansion expansion) {
    return TypeExpansionContext(expansion, nullptr, false);
  }

  bool operator==(const TypeExpansionContext &other) const {
    assert(other.inContext != this->inContext ||
           other.isContextWholeModule == this->isContextWholeModule);
    return other.inContext == this->inContext &&
      other.expansion == this->expansion;
  }

  bool operator<(const TypeExpansionContext other) const {
    assert(other.inContext != this->inContext ||
           other.isContextWholeModule == this->isContextWholeModule);
    if (this->expansion == other.expansion)
      return this->inContext < other.inContext;
    return this->expansion < other.expansion;
  }

  bool operator>(const TypeExpansionContext other) const {
    assert(other.inContext != this->inContext ||
           other.isContextWholeModule == this->isContextWholeModule);
    if (this->expansion == other.expansion)
      return this->inContext > other.inContext;
    return this->expansion > other.expansion;
  }

  uintptr_t getHashKey() const {
    uintptr_t key = (uintptr_t)inContext | (uintptr_t)expansion;
    return key;
  }
};

} // namespace swift

namespace llvm {
template <> struct DenseMapInfo<swift::TypeExpansionContext> {
  using TypeExpansionContext = swift::TypeExpansionContext;

  static TypeExpansionContext getEmptyKey() {
    return TypeExpansionContext(
        swift::ResilienceExpansion::Minimal,
        reinterpret_cast<swift::DeclContext *>(
            DenseMapInfo<swift::DeclContext *>::getEmptyKey()),
        false);
  }
  static TypeExpansionContext getTombstoneKey() {
    return TypeExpansionContext(
        swift::ResilienceExpansion::Minimal,
        reinterpret_cast<swift::DeclContext *>(
            DenseMapInfo<swift::DeclContext *>::getTombstoneKey()),
        false);
  }

  static unsigned getHashValue(TypeExpansionContext val) {
    return DenseMapInfo<uintptr_t>::getHashValue(val.getHashKey());
  }

  static bool isEqual(TypeExpansionContext LHS, TypeExpansionContext RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

#endif
