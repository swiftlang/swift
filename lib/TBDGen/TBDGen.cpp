//===--- TBDGen.cpp - Swift TBD Generation --------------------------------===//
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
//  This file implements the entrypoints into TBD file generation.
//
//===----------------------------------------------------------------------===//

#include "swift/TBDGen/TBDGen.h"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/StringSet.h"

#include "TBDGenVisitor.h"

using namespace swift;
using namespace swift::irgen;
using namespace swift::tbdgen;
using StringSet = llvm::StringSet<>;

static bool isPrivateDecl(ValueDecl *VD) {
  return getDeclLinkage(VD) != FormalLinkage::PublicUnique;
}

static bool isGlobalOrStaticVar(VarDecl *VD) {
  return VD->isStatic() || VD->getDeclContext()->isModuleScopeContext();
}

void TBDGenVisitor::visitPatternBindingDecl(PatternBindingDecl *PBD) {
  for (auto &entry : PBD->getPatternList()) {
    auto *var = entry.getAnchoringVarDecl();
    if (isPrivateDecl(var))
      return;

    // Non-global variables might have an explicit initializer symbol.
    if (entry.getInit() && !isGlobalOrStaticVar(var)) {
      auto declRef =
          SILDeclRef(var, SILDeclRef::Kind::StoredPropertyInitializer);
      // Stored property initializers for public properties are currently
      // public.
      addSymbol(declRef);
    }
  }
}

void TBDGenVisitor::addSymbol(SILDeclRef declRef) {
  bool isPrivate = !hasPublicVisibility(declRef.getLinkage(ForDefinition));
  // Even private methods of open classes (specifically, private methods that
  // are in the vtable) have public symbols, because external subclasses
  // currently need to refer to them by symbol for their own vtable.
  switch (declRef.getSubclassScope()) {
  case SubclassScope::External:
    // Unlike the "truly" public things, private things have public symbols
    // unconditionally, even if they're theoretically SIL only.
    if (isPrivate) {
      isPrivate = false;
    }
    break;
  case SubclassScope::Internal:
  case SubclassScope::NotApplicable:
    break;
  }
  if (isPrivate)
    return;

  // FIXME: this includes too many symbols. There are some that are considered
  // SIL-only, but it isn't obvious how to determine this (e.g. it seems that
  // many, but not all, transparent functions result in object-file symbols)

  addSymbol(declRef.mangle());
}

void TBDGenVisitor::addConformances(DeclContext *DC) {
  for (auto conformance : DC->getLocalConformances()) {
    auto protocol = conformance->getProtocol();
    auto needsWTable =
        Lowering::TypeConverter::protocolRequiresWitnessTable(protocol);
    if (!needsWTable)
      continue;

    // Only normal conformances get symbols; the others get any public symbols
    // from their parent normal conformance.
    auto normalConformance = dyn_cast<NormalProtocolConformance>(conformance);
    if (!normalConformance)
      continue;

    addSymbol(LinkEntity::forDirectProtocolWitnessTable(normalConformance));
    addSymbol(
        LinkEntity::forProtocolWitnessTableAccessFunction(normalConformance));

    // FIXME: the logic around visibility in extensions is confusing, and
    // sometimes witness thunks need to be manually made public.

    auto conformanceIsFixed = SILWitnessTable::conformanceIsSerialized(
        normalConformance, SwiftModule->getResilienceStrategy(),
        SILSerializeWitnessTables);
    auto addSymbolIfNecessary = [&](ValueDecl *valueReq,
                                    SILLinkage witnessLinkage) {
      if (conformanceIsFixed &&
          fixmeWitnessHasLinkageThatNeedsToBePublic(witnessLinkage)) {
        Mangle::ASTMangler Mangler;
        addSymbol(Mangler.mangleWitnessThunk(normalConformance, valueReq));
      }
    };
    normalConformance->forEachValueWitness(nullptr, [&](ValueDecl *valueReq,
                                                        Witness witness) {
      if (isa<AbstractFunctionDecl>(valueReq)) {
        auto witnessLinkage =
            SILDeclRef(witness.getDecl()).getLinkage(ForDefinition);
        addSymbolIfNecessary(valueReq, witnessLinkage);
      } else if (auto VD = dyn_cast<AbstractStorageDecl>(valueReq)) {
        // A var or subscript decl needs extra special handling: the things that
        // end up in the witness table are the accessors, but the compiler only
        // talks about the actual storage decl in the conformance, so we have to
        // manually walk over the members, having pulled out something that will
        // have the right linkage.
        auto witnessVD = cast<AbstractStorageDecl>(witness.getDecl());

        SmallVector<Decl *, 4> members;
        VD->getAllAccessorFunctions(members);

        // Grab one of the accessors, and then use that to pull out which of the
        // getter or setter will have the appropriate linkage.
        FuncDecl *witnessWithRelevantLinkage;
        switch (cast<FuncDecl>(members[0])->getAccessorKind()) {
        case AccessorKind::NotAccessor:
          llvm_unreachable("must be an accessor");
        case AccessorKind::IsGetter:
        case AccessorKind::IsAddressor:
          witnessWithRelevantLinkage = witnessVD->getGetter();
          break;
        case AccessorKind::IsSetter:
        case AccessorKind::IsWillSet:
        case AccessorKind::IsDidSet:
        case AccessorKind::IsMaterializeForSet:
        case AccessorKind::IsMutableAddressor:
          witnessWithRelevantLinkage = witnessVD->getSetter();
          break;
        }
        auto witnessLinkage =
            SILDeclRef(witnessWithRelevantLinkage).getLinkage(ForDefinition);
        for (auto member : members) {
          addSymbolIfNecessary(cast<ValueDecl>(member), witnessLinkage);
        }
      }
    });
  }
}

void TBDGenVisitor::visitValueDecl(ValueDecl *VD) {
  addSymbol(SILDeclRef(VD));
  visitMembers(VD);
}

void TBDGenVisitor::visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
  if (auto FD = dyn_cast<FuncDecl>(AFD)) {
    // Accessors also appear nested inside the storage decl, which we treat as
    // the canonical location, so skip if we've got an accessor that isn't
    // inside the var decl.
    if (FD->getAccessorStorageDecl() && !InsideAbstractStorageDecl)
      return;
  }

  // Default arguments (of public functions) are public symbols, as the default
  // values are computed at the call site.
  auto index = 0;
  auto paramLists = AFD->getParameterLists();
  // Skip the first arguments, which contains Self (etc.), can't be defaulted,
  // and are ignored for the purposes of default argument indices.
  if (AFD->getDeclContext()->isTypeContext())
    paramLists = paramLists.slice(1);
  for (auto *paramList : paramLists) {
    for (auto *param : *paramList) {
      if (param->getDefaultValue())
        addSymbol(SILDeclRef::getDefaultArgGenerator(AFD, index));
      index++;
    }
  }

  visitValueDecl(AFD);
}

void TBDGenVisitor::visitAbstractStorageDecl(AbstractStorageDecl *ASD) {
  assert(!InsideAbstractStorageDecl &&
         "unexpected nesting of abstract storage decls");
  InsideAbstractStorageDecl = true;
  visitMembers(ASD);
  InsideAbstractStorageDecl = false;
}
void TBDGenVisitor::visitVarDecl(VarDecl *VD) {
  // statically/globally stored variables have some special handling.
  if (VD->hasStorage() && isGlobalOrStaticVar(VD)) {
    // The actual variable has a symbol.
    Mangle::ASTMangler mangler;
    addSymbol(mangler.mangleEntity(VD, false));

    // Top-level variables (*not* statics) in the main file don't get accessors,
    // despite otherwise looking like globals.
    if (!FileHasEntryPoint || VD->isStatic())
      addSymbol(SILDeclRef(VD, SILDeclRef::Kind::GlobalAccessor));
  }

  visitAbstractStorageDecl(VD);
}

void TBDGenVisitor::visitNominalTypeDecl(NominalTypeDecl *NTD) {
  auto declaredType = NTD->getDeclaredType()->getCanonicalType();

  addSymbol(LinkEntity::forNominalTypeDescriptor(NTD));

  // Generic types do not get metadata directly, only through the function.
  if (!NTD->isGenericContext()) {
    addSymbol(LinkEntity::forTypeMetadata(declaredType,
                                          TypeMetadataAddress::AddressPoint,
                                          /*isPattern=*/false));
  }
  addSymbol(LinkEntity::forTypeMetadataAccessFunction(declaredType));

  // There are symbols associated with any protocols this type conforms to.
  addConformances(NTD);

  visitMembers(NTD);
}

void TBDGenVisitor::visitClassDecl(ClassDecl *CD) {
  if (isPrivateDecl(CD))
    return;

  auto &ctxt = CD->getASTContext();
  auto isGeneric = CD->isGenericContext();
  auto objCCompatible = ctxt.LangOpts.EnableObjCInterop && !isGeneric;
  auto isObjC = objCCompatible && CD->isObjC();

  // Metaclasses and ObjC class (duh) are a ObjC thing, and so are not needed in
  // build artifacts/for classes which can't touch ObjC.
  if (objCCompatible) {
    if (isObjC)
      addSymbol(LinkEntity::forObjCClass(CD));

    if (CD->getMetaclassKind() == ClassDecl::MetaclassKind::ObjC)
      addSymbol(LinkEntity::forObjCMetaclass(CD));
    else
      addSymbol(LinkEntity::forSwiftMetaclassStub(CD));
  }

  // Some members of classes get extra handling, beyond members of struct/enums,
  // so let's walk over them manually.
  for (auto *member : CD->getMembers()) {
    auto value = dyn_cast<ValueDecl>(member);
    if (!value)
      continue;

    auto var = dyn_cast<VarDecl>(value);
    auto hasFieldOffset = var && var->hasStorage() && !var->isStatic();
    if (hasFieldOffset) {
      // FIXME: a field only has one sort of offset, but it is moderately
      // non-trivial to compute which one. Including both is less painful than
      // missing the correct one (for now), so we do that.
      addSymbol(LinkEntity::forFieldOffset(var, /*isIndirect=*/false));
      addSymbol(LinkEntity::forFieldOffset(var, /*isIndirect=*/true));
    }

    // The non-allocating forms of the destructors.
    if (auto dtor = dyn_cast<DestructorDecl>(value)) {
      // ObjC classes don't have a symbol for their destructor.
      if (!isObjC)
        addSymbol(SILDeclRef(dtor, SILDeclRef::Kind::Destroyer));
    }
  }

  visitNominalTypeDecl(CD);
}

void TBDGenVisitor::visitConstructorDecl(ConstructorDecl *CD) {
  if (CD->getParent()->getAsClassOrClassExtensionContext()) {
    // Class constructors come in two forms, allocating and non-allocating. The
    // default ValueDecl handling gives the allocating one, so we have to
    // manually include the non-allocating one.
    addSymbol(SILDeclRef(CD, SILDeclRef::Kind::Initializer));
  }
  visitAbstractFunctionDecl(CD);
}

void TBDGenVisitor::visitExtensionDecl(ExtensionDecl *ED) {
  if (!ED->getExtendedType()->isExistentialType()) {
    addConformances(ED);
  }

  visitMembers(ED);
}

void TBDGenVisitor::visitProtocolDecl(ProtocolDecl *PD) {
  if (!PD->isObjC())
    addSymbol(LinkEntity::forProtocolDescriptor(PD));

#ifndef NDEBUG
  // There's no (currently) relevant information about members of a protocol at
  // individual protocols, each conforming type has to handle them individually
  // (NB. anything within an active IfConfigDecls also appears outside). Let's
  // assert this fact:
  for (auto *member : PD->getMembers()) {
    auto isExpectedKind =
        isa<TypeAliasDecl>(member) || isa<AssociatedTypeDecl>(member) ||
        isa<AbstractStorageDecl>(member) || isa<PatternBindingDecl>(member) ||
        isa<AbstractFunctionDecl>(member) || isa<IfConfigDecl>(member);
    assert(isExpectedKind &&
           "unexpected member of protocol during TBD generation");
  }
#endif
}

static void enumeratePublicSymbolsAndWrite(ModuleDecl *M, FileUnit *singleFile,
                                           StringSet &symbols,
                                           bool hasMultipleIRGenThreads,
                                           bool silSerializeWitnessTables,
                                           llvm::raw_ostream *os,
                                           StringRef installName) {
  auto isWholeModule = singleFile == nullptr;
  const auto &target = M->getASTContext().LangOpts.Target;
  UniversalLinkageInfo linkInfo(target, hasMultipleIRGenThreads, isWholeModule);

  TBDGenVisitor visitor(symbols, target, linkInfo, M, silSerializeWitnessTables,
                        installName);

  auto visitFile = [&](FileUnit *file) {
    SmallVector<Decl *, 16> decls;
    file->getTopLevelDecls(decls);

    visitor.setFileHasEntryPoint(file->hasEntryPoint());

    for (auto d : decls)
      visitor.visit(d);
  };

  if (singleFile) {
    assert(M == singleFile->getParentModule() && "mismatched file and module");
    visitFile(singleFile);
  } else {
    for (auto *file : M->getFiles()) {
      visitFile(file);
    }
  }

  if (os) {
    // The correct TBD formatting code is temporarily non-open source, so this
    // is just a list of the symbols.
    std::vector<StringRef> sorted;
    for (auto &symbol : symbols)
      sorted.push_back(symbol.getKey());
    std::sort(sorted.begin(), sorted.end());
    for (const auto &symbol : sorted) {
      *os << symbol << "\n";
    }
  }
}

void swift::enumeratePublicSymbols(FileUnit *file, StringSet &symbols,
                                   bool hasMultipleIRGenThreads,
                                   bool silSerializeWitnessTables) {
  enumeratePublicSymbolsAndWrite(
      file->getParentModule(), file, symbols, hasMultipleIRGenThreads,
      silSerializeWitnessTables, nullptr, StringRef());
}
void swift::enumeratePublicSymbols(ModuleDecl *M, StringSet &symbols,
                                   bool hasMultipleIRGenThreads,
                                   bool silSerializeWitnessTables) {
  enumeratePublicSymbolsAndWrite(M, nullptr, symbols, hasMultipleIRGenThreads,
                                 silSerializeWitnessTables, nullptr,
                                 StringRef());
}
void swift::writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                         bool hasMultipleIRGenThreads,
                         bool silSerializeWitnessTables,
                         StringRef installName) {
  StringSet symbols;
  enumeratePublicSymbolsAndWrite(M, nullptr, symbols, hasMultipleIRGenThreads,
                                 silSerializeWitnessTables, &os, installName);
}
