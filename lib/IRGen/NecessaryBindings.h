//===--- NecessaryBindings.h - Optimizing archetype bindings ----*- C++ -*-===//
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
//
// This file defines a utility class for saving and restoring the
// archetype metadata necessary in order to carry out value operations
// on a type.
//
// This is a supplemental API of GenProto.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_NECESSARYBINDINGS_H
#define SWIFT_IRGEN_NECESSARYBINDINGS_H

#include "GenericRequirement.h"
#include "llvm/ADT/SetVector.h"
#include "swift/AST/Types.h"

namespace swift {
  class CanType;
  enum class MetadataState : size_t;
  class ProtocolDecl;
  class ProtocolConformanceRef;

namespace irgen {
  class Address;
  class IRGenFunction;
  class IRGenModule;
  class Size;

/// NecessaryBindings - The set of metadata that must be saved in
/// order to perform some set of operations on a type.
class NecessaryBindings {
  llvm::SetVector<GenericRequirement> Requirements;


  /// Are the bindings to be computed for a partial apply forwarder.
  /// In the case this is true we need to store/restore the conformance of a
  /// specialized type with conditional conformance because the conditional
  /// requirements are not available in the partial apply forwarder.
  bool forPartialApply = false;

public:
  NecessaryBindings() = default;
  
  /// Collect the necessary bindings to invoke a function with the given
  /// signature.
  static NecessaryBindings forFunctionInvocations(IRGenModule &IGM,
                                                  CanSILFunctionType origType,
                                                  SubstitutionMap subs);
  static NecessaryBindings forPartialApplyForwarder(IRGenModule &IGM,
                                                    CanSILFunctionType origType,
                                                    SubstitutionMap subs,
                                                    bool considerParameterSources = true);

  /// Add whatever information is necessary to reconstruct type metadata
  /// for the given type.
  void addTypeMetadata(CanType type);

  /// Get the requirement from the bindings at index i.
  const GenericRequirement &operator[](size_t i) const {
    return Requirements[i];
  }

  size_t size() const {
    return Requirements.size();
  }

  /// Add whatever information is necessary to reconstruct a witness table
  /// reference for the given type.
  void addProtocolConformance(CanType type, ProtocolConformanceRef conf);

  /// Is the work to do trivial?
  bool empty() const { return Requirements.empty(); }

  /// Returns the required size of the bindings.
  /// Pointer alignment is sufficient.
  Size getBufferSize(IRGenModule &IGM) const;

  /// Save the necessary bindings to the given buffer.
  void save(IRGenFunction &IGF, Address buffer) const;

  /// Restore the necessary bindings from the given buffer.
  void restore(IRGenFunction &IGF, Address buffer, MetadataState state) const;

  const llvm::SetVector<GenericRequirement> &getRequirements() const {
    return Requirements;
  }
private:
  static NecessaryBindings computeBindings(IRGenModule &IGM,
                                           CanSILFunctionType origType,
                                           SubstitutionMap subs,
                                           bool forPartialApplyForwarder,
                                           bool considerParameterSources = true);
};

} // end namespace irgen
} // end namespace swift

#endif
