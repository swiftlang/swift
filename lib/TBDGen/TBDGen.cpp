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
#include "swift/SIL/SILVTableVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/StringSet.h"

#include "TBDGenVisitor.h"

using namespace swift;
using namespace swift::irgen;
using namespace swift::tbdgen;
using StringSet = llvm::StringSet<>;

static bool isGlobalOrStaticVar(VarDecl *VD) {
  return VD->isStatic() || VD->getDeclContext()->isModuleScopeContext();
}

void TBDGenVisitor::visitPatternBindingDecl(PatternBindingDecl *PBD) {
  for (auto &entry : PBD->getPatternList()) {
    auto *var = entry.getAnchoringVarDecl();

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
  auto linkage = effectiveLinkageForClassMember(
    declRef.getLinkage(ForDefinition),
    declRef.getSubclassScope());
  if (linkage == SILLinkage::Public)
    addSymbol(declRef.mangle());
}

void TBDGenVisitor::addDispatchThunk(SILDeclRef declRef) {
  auto entity = LinkEntity::forDispatchThunk(declRef);
  addSymbol(entity);
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
    addSymbol(LinkEntity::forProtocolConformanceDescriptor(normalConformance));

    // FIXME: the logic around visibility in extensions is confusing, and
    // sometimes witness thunks need to be manually made public.

    auto conformanceIsFixed = SILWitnessTable::conformanceIsSerialized(
        normalConformance);
    auto addSymbolIfNecessary = [&](SILDeclRef declRef) {
      auto witnessLinkage = declRef.getLinkage(ForDefinition);
      if (conformanceIsFixed &&
          fixmeWitnessHasLinkageThatNeedsToBePublic(witnessLinkage)) {
        Mangle::ASTMangler Mangler;
        addSymbol(Mangler.mangleWitnessThunk(normalConformance,
                                             declRef.getDecl()));
      }
    };
    normalConformance->forEachValueWitness(nullptr, [&](ValueDecl *valueReq,
                                                        Witness witness) {
      if (isa<AbstractFunctionDecl>(valueReq)) {
        addSymbolIfNecessary(SILDeclRef(valueReq));
      } else if (auto *storage = dyn_cast<AbstractStorageDecl>(valueReq)) {
        if (auto *getter = storage->getGetter())
          addSymbolIfNecessary(SILDeclRef(getter));
        if (auto *setter = storage->getGetter())
          addSymbolIfNecessary(SILDeclRef(setter));
        if (auto *materializeForSet = storage->getMaterializeForSetFunc())
          addSymbolIfNecessary(SILDeclRef(materializeForSet));
      }
    });
  }
}

void TBDGenVisitor::visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
  addSymbol(SILDeclRef(AFD));

  if (!SwiftModule->getASTContext().isSwiftVersion3())
    return;

  // In Swift 3, default arguments (of public functions) are public symbols,
  // as the default values are computed at the call site.
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
}

void TBDGenVisitor::visitNominalTypeDecl(NominalTypeDecl *NTD) {
  auto declaredType = NTD->getDeclaredType()->getCanonicalType();

  addSymbol(LinkEntity::forNominalTypeDescriptor(NTD));

  // Generic types do not get metadata directly, only through the function.
  if (!NTD->isGenericContext()) {
    addSymbol(LinkEntity::forTypeMetadata(declaredType,
                                          TypeMetadataAddress::AddressPoint));
  }
  addSymbol(LinkEntity::forTypeMetadataAccessFunction(declaredType));

  // There are symbols associated with any protocols this type conforms to.
  addConformances(NTD);

  for (auto member : NTD->getMembers())
    visit(member);
}

void TBDGenVisitor::visitClassDecl(ClassDecl *CD) {
  if (getDeclLinkage(CD) != FormalLinkage::PublicUnique)
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
    if (hasFieldOffset)
      addSymbol(LinkEntity::forFieldOffset(var));

    // The non-allocating forms of the destructors.
    if (auto dtor = dyn_cast<DestructorDecl>(value)) {
      // ObjC classes don't have a symbol for their destructor.
      if (!isObjC)
        addSymbol(SILDeclRef(dtor, SILDeclRef::Kind::Destroyer));
    }
  }

  visitNominalTypeDecl(CD);

  // The below symbols are only emitted if the class is resilient.
  if (!CD->isResilient())
    return;

  addSymbol(LinkEntity::forClassMetadataBaseOffset(CD));

  // Emit dispatch thunks for every new vtable entry.
  struct VTableVisitor : public SILVTableVisitor<VTableVisitor> {
    TBDGenVisitor &TBD;
    ClassDecl *CD;

  public:
    VTableVisitor(TBDGenVisitor &TBD, ClassDecl *CD)
        : TBD(TBD), CD(CD) {}

    void addMethod(SILDeclRef method) {
      if (method.getDecl()->getDeclContext() == CD)
        TBD.addDispatchThunk(method);
    }

    void addMethodOverride(SILDeclRef baseRef, SILDeclRef derivedRef) {}

    void addPlaceholder(MissingMemberDecl *) {}

    void doIt() {
      addVTableEntries(CD);
    }
  };

  VTableVisitor(*this, CD).doIt();
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

  for (auto member : ED->getMembers())
    visit(member);
}

void TBDGenVisitor::visitProtocolDecl(ProtocolDecl *PD) {
  if (!PD->isObjC()) {
    addSymbol(LinkEntity::forProtocolDescriptor(PD));

    if (PD->isResilient()) {
      for (auto *member : PD->getMembers()) {
        if (auto *funcDecl = dyn_cast<FuncDecl>(member)) {
          addDispatchThunk(SILDeclRef(funcDecl));
        }
        if (auto *ctorDecl = dyn_cast<ConstructorDecl>(member)) {
          addDispatchThunk(SILDeclRef(ctorDecl, SILDeclRef::Kind::Allocator));
        }
      }
    }
  }

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

void TBDGenVisitor::visitEnumDecl(EnumDecl *ED) {
  if (!ED->isResilient())
    return;

  // Emit resilient tags.
  for (auto *elt : ED->getAllElements()) {
    auto entity = LinkEntity::forEnumCase(elt);
    addSymbol(entity);
  }
}

static void enumeratePublicSymbolsAndWrite(ModuleDecl *M, FileUnit *singleFile,
                                           StringSet &symbols,
                                           bool hasMultipleIGMs,
                                           llvm::raw_ostream *os,
                                           StringRef installName) {
  auto isWholeModule = singleFile == nullptr;
  const auto &target = M->getASTContext().LangOpts.Target;
  UniversalLinkageInfo linkInfo(target, hasMultipleIGMs, isWholeModule);

  TBDGenVisitor visitor(symbols, target, linkInfo, M, installName);

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
                                   bool hasMultipleIGMs) {
  enumeratePublicSymbolsAndWrite(file->getParentModule(), file, symbols,
                                 hasMultipleIGMs, nullptr, StringRef());
}
void swift::enumeratePublicSymbols(ModuleDecl *M, StringSet &symbols,
                                   bool hasMultipleIGMs) {
  enumeratePublicSymbolsAndWrite(M, nullptr, symbols, hasMultipleIGMs, nullptr,
                                 StringRef());
}
void swift::writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                         bool hasMultipleIGMs, StringRef installName) {
  StringSet symbols;
  enumeratePublicSymbolsAndWrite(M, nullptr, symbols, hasMultipleIGMs, &os,
                                 installName);
}
