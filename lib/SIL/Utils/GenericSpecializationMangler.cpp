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
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Assertions.h"
#include "swift/Demangling/ManglingMacros.h"

using namespace swift;
using namespace Mangle;

void SpecializationMangler::beginMangling() {
  ASTMangler::beginManglingWithoutPrefix();

  if (Serialized)
    ArgOpBuffer << 'q';

  if (RemovedEffects.contains(EffectKind::Async))
    ArgOpBuffer << 'a';

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
  if (FuncName.starts_with(MANGLING_PREFIX_STR)) {
    FuncTopLevel = D.demangleSymbol(FuncName);
    assert(FuncTopLevel);
  }
  else if (FuncName.starts_with(MANGLING_PREFIX_EMBEDDED_STR)) {
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
  auto mangling = Demangle::mangleNode(TopLevel, Flavor);
  assert(mangling.isSuccess());
  std::string mangledName = mangling.result();
  verify(mangledName, Flavor);
  return mangledName;
}

//===----------------------------------------------------------------------===//
//                           Generic Specialization
//===----------------------------------------------------------------------===//

void GenericSpecializationMangler::
appendSubstitutions(GenericSignature sig, SubstitutionMap subs) {
  bool First = true;
  sig->forEachParam([&](GenericTypeParamType *ParamType, bool Canonical) {
    if (Canonical) {
      auto ty = Type(ParamType);
      auto substTy = ty.subst(subs);
      auto canTy = substTy->getCanonicalType();
      appendType(canTy, nullptr);
      appendListSeparator(First);
    }
  });
  assert(!First && "no generic substitutions");
}

std::string GenericSpecializationMangler::
manglePrespecialized(GenericSignature sig, SubstitutionMap subs) {
  beginMangling();
  appendSubstitutions(sig, subs);
  appendSpecializationOperator("Ts");
  return finalize();
}
                                  
std::string GenericSpecializationMangler::
mangleNotReabstracted(SubstitutionMap subs,
                      const SmallBitVector &paramsRemoved) {
  beginMangling();
  appendSubstitutions(getGenericSignature(), subs);
  appendOperator("T");
  appendRemovedParams(paramsRemoved);
  appendSpecializationOperator("G");
  return finalize();
}
                                  
std::string GenericSpecializationMangler::
mangleReabstracted(SubstitutionMap subs, bool alternativeMangling,
                   const SmallBitVector &paramsRemoved) {
  beginMangling();
  appendSubstitutions(getGenericSignature(), subs);
  appendOperator("T");
  appendRemovedParams(paramsRemoved);

  // See ReabstractionInfo::hasConvertedResilientParams for why and when to use
  // the alternative mangling.
  appendSpecializationOperator(alternativeMangling ? "B" : "g");
  return finalize();
}

void GenericSpecializationMangler::appendRemovedParams(const SmallBitVector &paramsRemoved) {
  for (int paramIdx : paramsRemoved.set_bits()) {
    appendOperator("t");
    if (paramIdx != 0)
      Buffer << (paramIdx - 1);
  }
}

std::string GenericSpecializationMangler::
mangleForDebugInfo(GenericSignature sig, SubstitutionMap subs, bool forInlining) {
  beginMangling();
  appendSubstitutions(sig, subs);
  appendSpecializationOperator(forInlining ? "Ti" : "TG");
  return finalize();
}


static SubstitutionMap
getSubstitutionMapForPrespecialization(GenericSignature genericSig,
                                       GenericSignature specSig) {
  auto CalleeGenericSig = genericSig;
  auto SpecializedGenericEnv = specSig.getGenericEnvironment();

  auto CalleeInterfaceToSpecializedInterfaceMap = SubstitutionMap::get(
      CalleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        return type;
      },
      LookUpConformanceInModule());

  auto subs = SubstitutionMap::get(
      CalleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        auto SpecializedInterfaceTy =
            Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);
        return SpecializedGenericEnv->mapTypeIntoContext(
            SpecializedInterfaceTy);
      },
      LookUpConformanceInModule());
  return subs;
}

std::string GenericSpecializationMangler::manglePrespecialization(ASTContext &Ctx,
    std::string unspecializedName, GenericSignature genericSig,
    GenericSignature specializedSig) {
  auto subs =
      getSubstitutionMapForPrespecialization(genericSig, specializedSig);
  GenericSpecializationMangler mangler(Ctx, unspecializedName);
  return mangler.manglePrespecialized(genericSig, subs);
}
