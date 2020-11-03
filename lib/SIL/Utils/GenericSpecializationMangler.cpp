//===--- GenericSpecializationMangler.cpp - mangling of specializations ---===//
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

#include "swift/SIL/GenericSpecializationMangler.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Demangling/ManglingMacros.h"

using namespace swift;
using namespace Mangle;

void SpecializationMangler::beginMangling() {
  ASTMangler::beginManglingWithoutPrefix();
  if (Serialized)
    ArgOpBuffer << 'q';
  ArgOpBuffer << char(uint8_t(Pass) + '0');
}

namespace {

/// Utility class for demangling specialization attributes.
class AttributeDemangler : public Demangle::Demangler {
public:
  void demangleAndAddAsChildren(StringRef MangledSpecialization,
                                NodePointer Parent) {
    DemangleInitRAII state(*this, MangledSpecialization, nullptr);
    if (!parseAndPushNodes()) {
      llvm::errs() << "Can't demangle: " << MangledSpecialization << '\n';
      abort();
    }
    for (Node *Nd : NodeStack) {
      addChild(Parent, Nd);
    }
  }
};

} // namespace

std::string SpecializationMangler::finalize() {
  StringRef MangledSpecialization(Storage.data(), Storage.size());
  AttributeDemangler D;
  NodePointer TopLevel = D.createNode(Node::Kind::Global);
  D.demangleAndAddAsChildren(MangledSpecialization, TopLevel);

  StringRef FuncName = Function ? Function->getName() : StringRef(FunctionName);
  NodePointer FuncTopLevel = nullptr;
  if (FuncName.startswith(MANGLING_PREFIX_STR)) {
    FuncTopLevel = D.demangleSymbol(FuncName);
    assert(FuncTopLevel);
  }
  if (!FuncTopLevel) {
    FuncTopLevel = D.createNode(Node::Kind::Global);
    FuncTopLevel->addChild(D.createNode(Node::Kind::Identifier, FuncName), D);
  }
  for (NodePointer FuncChild : *FuncTopLevel) {
    TopLevel->addChild(FuncChild, D);
  }
  std::string mangledName = Demangle::mangleNode(TopLevel);
  verify(mangledName);
  return mangledName;
}

//===----------------------------------------------------------------------===//
//                           Generic Specialization
//===----------------------------------------------------------------------===//

std::string GenericSpecializationMangler::mangle(GenericSignature Sig) {
  beginMangling();

  if (!Sig) {
    assert(Function &&
           "Need a SIL function if no generic signature is provided");
    SILFunctionType *FTy = Function->getLoweredFunctionType();
    Sig = FTy->getInvocationGenericSignature();
  }

  bool First = true;
  Sig->forEachParam([&](GenericTypeParamType *ParamType, bool Canonical) {
    if (Canonical) {
      appendType(Type(ParamType).subst(SubMap)->getCanonicalType());
      appendListSeparator(First);
    }
  });
  assert(!First && "no generic substitutions");

  if (isInlined)
    appendSpecializationOperator("Ti");
  else
    appendSpecializationOperator(
        isPrespecializaton ? "Ts" : (isReAbstracted ? "Tg" : "TG"));
  return finalize();
}

static SubstitutionMap
getSubstitutionMapForPrespecialization(GenericSignature genericSig,
                                       GenericSignature specSig) {
  auto CalleeGenericSig = genericSig;
  auto SpecializedGenericSig = specSig;
  auto SpecializedGenericEnv = specSig->getGenericEnvironment();

  auto CalleeInterfaceToSpecializedInterfaceMap = SubstitutionMap::get(
      CalleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        return type;
      },
      LookUpConformanceInSignature(CalleeGenericSig.getPointer()));

  auto subs = SubstitutionMap::get(
      CalleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        auto SpecializedInterfaceTy =
            Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);
        return SpecializedGenericEnv->mapTypeIntoContext(
            SpecializedInterfaceTy);
      },
      LookUpConformanceInSignature(SpecializedGenericSig.getPointer()));
  return subs;
}

std::string GenericSpecializationMangler::manglePrespecialization(
    std::string unspecializedName, GenericSignature genericSig,
    GenericSignature specializedSig) {
  auto subs =
      getSubstitutionMapForPrespecialization(genericSig, specializedSig);
  GenericSpecializationMangler mangler(unspecializedName, subs);
  return mangler.mangle(genericSig);
}
