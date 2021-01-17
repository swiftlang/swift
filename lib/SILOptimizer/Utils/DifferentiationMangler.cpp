//===----- DifferentiationMangler.cpp --------- differentiation -*- C++ -*-===//
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

#include "swift/SILOptimizer/Utils/DifferentiationMangler.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/SIL/SILGlobalVariable.h"

using namespace swift;
using namespace Mangle;

/// Mangles the generic signature and get its mangling tree. This is necessary
/// because the derivative generic signature's requirements may contain names
/// which repeat the contents of the original function name. To follow Swift's
/// mangling scheme, these repetitions must be mangled as substitutions.
/// Therefore, we build mangling trees in `DifferentiationMangler` and let the
/// remangler take care of substitutions.
static NodePointer mangleGenericSignatureAsNode(GenericSignature sig,
                                                Demangler &demangler) {
  if (!sig)
    return nullptr;
  ASTMangler sigMangler;
  auto mangledGenSig = sigMangler.mangleGenericSignature(sig);
  auto demangledGenSig = demangler.demangleSymbol(mangledGenSig);
  assert(demangledGenSig->getKind() == Node::Kind::Global);
  assert(demangledGenSig->getNumChildren() == 1);
  auto result = demangledGenSig->getFirstChild();
  assert(result->getKind() == Node::Kind::DependentGenericSignature);
  return result;
}

static NodePointer mangleAsNode(
    SILFunction *originalFunction, Demangle::AutoDiffFunctionKind kind,
    AutoDiffConfig config, Demangler &demangler) {
  assert(isMangledName(originalFunction->getName()));
  auto demangledOrig = demangler.demangleSymbol(originalFunction->getName());
  assert(demangledOrig->getKind() == Node::Kind::Global);
  assert(demangledOrig->getNumChildren() == 1);
  auto originalEntityNode = demangledOrig->getFirstChild();
  auto derivativeGenericSignatureNode = mangleGenericSignatureAsNode(
      config.derivativeGenericSignature, demangler);

  auto *adFunc = demangler.createNode(Node::Kind::AutoDiffFunction);
  assert(originalEntityNode && "Should only be called when the original "
         "function has a mangled name");
  adFunc->addChild(originalEntityNode, demangler);
  if (derivativeGenericSignatureNode)
    adFunc->addChild(derivativeGenericSignatureNode, demangler);
  adFunc->addChild(
      demangler.createNode(
          Node::Kind::AutoDiffFunctionKind, (Node::IndexType)kind),
      demangler);
  adFunc->addChild(
      demangler.createNode(
          Node::Kind::IndexSubset, config.parameterIndices->getString()),
      demangler);
  adFunc->addChild(
      demangler.createNode(
          Node::Kind::IndexSubset, config.resultIndices->getString()),
      demangler);
  auto root = demangler.createNode(Node::Kind::Global);
  root->addChild(adFunc, demangler);
  return root;
}

std::string DifferentiationMangler::mangle(
    SILFunction *originalFunction, Demangle::AutoDiffFunctionKind kind,
    AutoDiffConfig config) {
  // If the original function is mangled, mangle the tree.
  if (isMangledName(originalFunction->getName())) {
    Demangler demangler;
    auto node = mangleAsNode(originalFunction, kind, config, demangler);
    return Demangle::mangleNode(node);
  }
  // Otherwise, treat the original function symbol as a black box and just
  // mangle the other parts.
  beginManglingWithoutPrefix();
  appendOperator(originalFunction->getName());
  appendAutoDiffFunctionParts((char)kind, config);
  return finalize();
}

// Returns the mangled name for a derivative function of the given kind.
std::string DifferentiationMangler::mangleDerivativeFunction(
    SILFunction *originalFunction, AutoDiffDerivativeFunctionKind kind,
    AutoDiffConfig config) {
  return mangle(originalFunction, getAutoDiffFunctionKind(kind), config);
}

// Returns the mangled name for a derivative function of the given kind.
std::string DifferentiationMangler::mangleLinearMap(
    SILFunction *originalFunction, AutoDiffLinearMapKind kind,
    AutoDiffConfig config) {
  return mangle(originalFunction, getAutoDiffFunctionKind(kind), config);
}
