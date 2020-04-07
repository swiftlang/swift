//===--- SILDifferentiabilityWitness.h - Differentiability witnesses ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SILDifferentiabilityWitness class, which maps an
// original SILFunction and derivative configuration (parameter indices, result
// indices, derivative generic signature) to derivative functions (JVP and VJP).
//
// SIL differentiability witnesses are generated from the `@differentiable`
// and `@derivative` AST declaration attributes.
//
// Differentiability witnesses are canonicalized by the SIL differentiation
// transform, which fills in missing derivative functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILDIFFERENTIABILITYWITNESS_H
#define SWIFT_SIL_SILDIFFERENTIABILITYWITNESS_H

#include "swift/AST/Attr.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/GenericSignature.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILLinkage.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"

namespace swift {

class SILPrintContext;

class SILDifferentiabilityWitness
    : public llvm::ilist_node<SILDifferentiabilityWitness>,
      public SILAllocated<SILDifferentiabilityWitness> {
private:
  /// The module which contains the differentiability witness.
  SILModule &Module;
  /// The linkage of the differentiability witness.
  SILLinkage Linkage;
  /// The original function.
  SILFunction *OriginalFunction;
  /// The derivative configuration: parameter indices, result indices, and
  /// derivative generic signature (optional). The derivative generic signature
  /// may contain same-type requirements such that all generic parameters are
  /// bound to concrete types.
  AutoDiffConfig Config;
  /// The JVP (Jacobian-vector products) derivative function.
  SILFunction *JVP;
  /// The VJP (vector-Jacobian products) derivative function.
  SILFunction *VJP;
  /// Whether or not this differentiability witness is a declaration.
  bool IsDeclaration;
  /// Whether or not this differentiability witness is serialized, which allows
  /// devirtualization from another module.
  bool IsSerialized;
  /// The AST `@differentiable` or `@derivative` attribute from which the
  /// differentiability witness is generated. Used for diagnostics.
  /// Null if the differentiability witness is parsed from SIL or if it is
  /// deserialized.
  const DeclAttribute *Attribute = nullptr;

  SILDifferentiabilityWitness(
      SILModule &module, SILLinkage linkage, SILFunction *originalFunction,
      IndexSubset *parameterIndices, IndexSubset *resultIndices,
      GenericSignature derivativeGenSig, SILFunction *jvp, SILFunction *vjp,
      bool isDeclaration, bool isSerialized, const DeclAttribute *attribute)
      : Module(module), Linkage(linkage), OriginalFunction(originalFunction),
        Config(parameterIndices, resultIndices, derivativeGenSig.getPointer()),
        JVP(jvp), VJP(vjp), IsDeclaration(isDeclaration),
        IsSerialized(isSerialized), Attribute(attribute) {}

public:
  static SILDifferentiabilityWitness *
  createDeclaration(SILModule &module, SILLinkage linkage,
                    SILFunction *originalFunction,
                    IndexSubset *parameterIndices, IndexSubset *resultIndices,
                    GenericSignature derivativeGenSig,
                    const DeclAttribute *attribute = nullptr);

  static SILDifferentiabilityWitness *createDefinition(
      SILModule &module, SILLinkage linkage, SILFunction *originalFunction,
      IndexSubset *parameterIndices, IndexSubset *resultIndices,
      GenericSignature derivativeGenSig, SILFunction *jvp, SILFunction *vjp,
      bool isSerialized, const DeclAttribute *attribute = nullptr);

  void convertToDefinition(SILFunction *jvp, SILFunction *vjp,
                           bool isSerialized);

  SILDifferentiabilityWitnessKey getKey() const;
  SILModule &getModule() const { return Module; }
  SILLinkage getLinkage() const { return Linkage; }
  SILFunction *getOriginalFunction() const { return OriginalFunction; }
  const AutoDiffConfig &getConfig() const { return Config; }
  IndexSubset *getParameterIndices() const { return Config.parameterIndices; }
  IndexSubset *getResultIndices() const { return Config.resultIndices; }
  GenericSignature getDerivativeGenericSignature() const {
    return Config.derivativeGenericSignature;
  }
  SILFunction *getJVP() const { return JVP; }
  SILFunction *getVJP() const { return VJP; }
  SILFunction *getDerivative(AutoDiffDerivativeFunctionKind kind) const {
    switch (kind) {
    case AutoDiffDerivativeFunctionKind::JVP:
      return JVP;
    case AutoDiffDerivativeFunctionKind::VJP:
      return VJP;
    }
  }
  void setJVP(SILFunction *jvp) { JVP = jvp; }
  void setVJP(SILFunction *vjp) { VJP = vjp; }
  void setDerivative(AutoDiffDerivativeFunctionKind kind,
                     SILFunction *derivative) {
    switch (kind) {
    case AutoDiffDerivativeFunctionKind::JVP:
      JVP = derivative;
      break;
    case AutoDiffDerivativeFunctionKind::VJP:
      VJP = derivative;
      break;
    }
  }
  bool isDeclaration() const { return IsDeclaration; }
  bool isDefinition() const { return !IsDeclaration; }
  bool isSerialized() const { return IsSerialized; }
  const DeclAttribute *getAttribute() const { return Attribute; }

  /// Returns the `SILAutoDiffIndices` corresponding to this config's indices.
  // TODO(TF-893): This is a temporary shim for incremental removal of
  // `SILAutoDiffIndices`. Eventually remove this.
  SILAutoDiffIndices getSILAutoDiffIndices() const;

  /// Verify that the differentiability witness is well-formed.
  void verify(const SILModule &module) const;

  void print(llvm::raw_ostream &os, bool verbose = false) const;
  void dump() const;
};

} // end namespace swift

namespace llvm {

//===----------------------------------------------------------------------===//
// ilist_traits for SILDifferentiabilityWitness
//===----------------------------------------------------------------------===//

template <>
struct ilist_traits<::swift::SILDifferentiabilityWitness>
    : public ilist_node_traits<::swift::SILDifferentiabilityWitness> {
  using SILDifferentiabilityWitness = ::swift::SILDifferentiabilityWitness;

public:
  static void deleteNode(SILDifferentiabilityWitness *DW) {
    DW->~SILDifferentiabilityWitness();
  }

private:
  void createNode(const SILDifferentiabilityWitness &);
};

} // namespace llvm

#endif // SWIFT_SIL_SILDIFFERENTIABILITYWITNESS_H
