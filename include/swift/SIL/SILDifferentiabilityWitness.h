//===--- SILDifferentiabilityWitness.h - Differentiability witnesses ------===//
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
// This file defines the SILDifferentiabilityWitness class, which maps an
// original SILFunction and derivative configuration (parameter indices, result
// indices, derivative generic signature) to derivative functions (JVP and VJP).
//
// SIL differentiability witnesses are generated from the `@differentiable`
// and `@differentiating` attributes AST declaration attributes.
// Differentiability witnesses are canonicalized by the differentiation SIL
// transform, which fills in missing derivative functions. Canonical
// differentiability witnesses from other modules can be deserialized to look up
// derivative functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILDIFFERENTIABILITYWITNESS_H
#define SWIFT_SIL_SILDIFFERENTIABILITYWITNESS_H

#include "swift/AST/AutoDiff.h"
#include "swift/AST/GenericSignature.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace swift {

class SILPrintContext;

class SILDifferentiabilityWitness
    : public llvm::ilist_node<SILDifferentiabilityWitness>,
      public SILAllocated<SILDifferentiabilityWitness>
{
private:
  /// The module which contains the differentiability witness.
  SILModule &module;
  /// The linkage of the differentiability witness.
  SILLinkage linkage;
  /// The original function.
  SILFunction *originalFunction;
  /// The parameter indices.
  IndexSubset *parameterIndices;
  /// The result indices.
  IndexSubset *resultIndices;
  /// The derivative generic signature (optional).
  GenericSignature *derivativeGenericSignature;
  /// The JVP (Jacobian-vector products) derivative function.
  SILFunction *jvp;
  /// The VJP (vector-Jacobian products) derivative function.
  SILFunction *vjp;
  /// Whether or not this differentiability witness is serialized, which allows
  /// devirtualization from another module.
  bool serialized;

  SILDifferentiabilityWitness(SILModule &module, SILLinkage linkage,
                              SILFunction *originalFunction,
                              IndexSubset *parameterIndices,
                              IndexSubset *resultIndices,
                              GenericSignature *derivativeGenSig,
                              SILFunction *jvp, SILFunction *vjp,
                              bool isSerialized)
    : module(module), linkage(linkage), originalFunction(originalFunction),
      parameterIndices(parameterIndices), resultIndices(resultIndices),
      derivativeGenericSignature(derivativeGenSig), jvp(jvp), vjp(vjp),
      serialized(isSerialized) {}

public:
  static SILDifferentiabilityWitness *create(
      SILModule &module, SILLinkage linkage, SILFunction *originalFunction,
      IndexSubset *parameterIndices, IndexSubset *resultIndices,
      GenericSignature *derivativeGenSig, SILFunction *jvp, SILFunction *vjp,
      bool isSerialized);

  SILDifferentiabilityWitnessKey getKey() const;
  SILModule &getModule() const { return module; }
  SILLinkage getLinkage() const { return linkage; }
  SILFunction *getOriginalFunction() const { return originalFunction; }
  IndexSubset *getParameterIndices() const {
    return parameterIndices;
  }
  IndexSubset *getResultIndices() const {
    return resultIndices;
  }
  GenericSignature *getDerivativeGenericSignature() const {
    return derivativeGenericSignature;
  }
  SILFunction *getJVP() const { return jvp; }
  SILFunction *getVJP() const { return vjp; }
  bool isSerialized() const { return serialized; }

  /// Verify that the differentiability witness is well-formed.
  void verify(const SILModule &M) const;

  void print(llvm::raw_ostream &OS, bool verbose = false) const;
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
