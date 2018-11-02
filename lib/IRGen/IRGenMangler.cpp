//===--- IRGenMangler.cpp - mangling of IRGen symbols ---------------------===//
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

#include "IRGenMangler.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Demangling/Demangle.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace irgen;

const char *getManglingForWitness(swift::Demangle::ValueWitnessKind kind) {
  switch (kind) {
#define VALUE_WITNESS(MANGLING, NAME) \
  case swift::Demangle::ValueWitnessKind::NAME: return #MANGLING;
#include "swift/Demangling/ValueWitnessMangling.def"
  }
  llvm_unreachable("not a function witness");
}

std::string IRGenMangler::mangleValueWitness(Type type, ValueWitness witness) {
  beginMangling();
  appendType(type);

  const char *Code = nullptr;
  switch (witness) {
#define GET_MANGLING(ID) \
    case ValueWitness::ID: Code = getManglingForWitness(swift::Demangle::ValueWitnessKind::ID); break;
    GET_MANGLING(InitializeBufferWithCopyOfBuffer) \
    GET_MANGLING(Destroy) \
    GET_MANGLING(InitializeWithCopy) \
    GET_MANGLING(AssignWithCopy) \
    GET_MANGLING(InitializeWithTake) \
    GET_MANGLING(AssignWithTake) \
    GET_MANGLING(GetEnumTagSinglePayload) \
    GET_MANGLING(StoreEnumTagSinglePayload) \
    GET_MANGLING(StoreExtraInhabitant) \
    GET_MANGLING(GetExtraInhabitantIndex) \
    GET_MANGLING(GetEnumTag) \
    GET_MANGLING(DestructiveProjectEnumData) \
    GET_MANGLING(DestructiveInjectEnumTag)
#undef GET_MANGLING
    case ValueWitness::Size:
    case ValueWitness::Flags:
    case ValueWitness::Stride:
    case ValueWitness::ExtraInhabitantFlags:
      llvm_unreachable("not a function witness");
  }
  appendOperator("w", Code);
  return finalize();
}

std::string IRGenMangler::manglePartialApplyForwarder(StringRef FuncName) {
  if (FuncName.empty()) {
    beginMangling();
  } else {
    if (FuncName.startswith(MANGLING_PREFIX_STR)) {
      Buffer << FuncName;
    } else {
      beginMangling();
      appendIdentifier(FuncName);
    }
  }
  appendOperator("TA");
  return finalize();
}

SymbolicMangling
IRGenMangler::mangleTypeForReflection(IRGenModule &IGM,
                                      Type Ty) {
  Mod = IGM.getSwiftModule();
  OptimizeProtocolNames = false;

  llvm::SaveAndRestore<std::function<bool (const DeclContext *)>>
    SymbolicReferencesForLocalTypes(CanSymbolicReference);
  
  if (IGM.CurSourceFile
      && !isa<ClangModuleUnit>(IGM.CurSourceFile)
      && !IGM.getOptions().IntegratedREPL) {
    CanSymbolicReference = [&](const DeclContext *dc) -> bool {
      // Symbolically reference types that are defined in the same file unit
      // as we're referencing from.
      //
      // We could eventually improve this to reference any type that ends
      // up with its nominal type descriptor in the same linked binary as us,
      // but IRGen doesn't know that with much certainty currently.
      return dc->getModuleScopeContext() == IGM.CurSourceFile
        && isa<NominalTypeDecl>(dc)
        && !isa<ProtocolDecl>(dc);
    };
  }
  
  SymbolicReferences.clear();
  
  appendType(Ty);
  
  return {finalize(), std::move(SymbolicReferences)};
}

std::string IRGenMangler::mangleTypeForLLVMTypeName(CanType Ty) {
  // To make LLVM IR more readable we always add a 'T' prefix so that type names
  // don't start with a digit and don't need to be quoted.
  Buffer << 'T';
  if (auto P = dyn_cast<ProtocolType>(Ty)) {
    appendProtocolName(P->getDecl(), /*allowStandardSubstitution=*/false);
    appendOperator("P");
  } else {
    appendType(Ty);
  }
  return finalize();
}

std::string IRGenMangler::
mangleProtocolForLLVMTypeName(ProtocolCompositionType *type) {
  ExistentialLayout layout = type->getExistentialLayout();

  if (type->isAny()) {
    Buffer << "Any";
  } else if (layout.isAnyObject()) {
    Buffer << "AnyObject";
  } else {
    // To make LLVM IR more readable we always add a 'T' prefix so that type names
    // don't start with a digit and don't need to be quoted.
    Buffer << 'T';
    auto protocols = layout.getProtocols();
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      appendProtocolName(protocols[i]->getDecl());
      if (i == 0)
        appendOperator("_");
    }
    if (auto superclass = layout.explicitSuperclass) {
      // We share type infos for different instantiations of a generic type
      // when the archetypes have the same exemplars.  We cannot mangle
      // archetypes, and the mangling does not have to be unique, so we just
      // mangle the unbound generic form of the type.
      if (superclass->hasArchetype()) {
        superclass = superclass->getClassOrBoundGenericClass()
          ->getDeclaredType();
      }

      appendType(CanType(superclass));
      appendOperator("Xc");
    } else if (layout.getLayoutConstraint()) {
      appendOperator("Xl");
    } else {
      appendOperator("p");
    }
  }
  return finalize();
}

std::string IRGenMangler::
mangleSymbolNameForSymbolicMangling(const SymbolicMangling &mangling) {
  beginManglingWithoutPrefix();
  static const char prefix[] = "symbolic ";
  Buffer << prefix << mangling.String;
  auto prefixLen = sizeof(prefix) - 1;

  for (auto &symbol : mangling.SymbolicReferences) {
    // Fill in the placeholder space with something printable.
    auto dc = symbol.first;
    auto offset = symbol.second;
    Storage[prefixLen + offset] = Storage[prefixLen + offset+1] =
      Storage[prefixLen + offset+2] = Storage[prefixLen + offset+3] = '_';
    Buffer << ' ';
    appendContext(dc);
  }
  
  return finalize();
}
