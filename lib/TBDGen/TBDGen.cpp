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
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "swift/SIL/SILWitnessTable.h"
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
    if (entry.getNonLazyInit() && !isGlobalOrStaticVar(var)) {
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
    auto addSymbolIfNecessary = [&](ValueDecl *requirementDecl,
                                    ValueDecl *witnessDecl) {
      auto witnessLinkage = SILDeclRef(witnessDecl).getLinkage(ForDefinition);
      if (conformanceIsFixed &&
          fixmeWitnessHasLinkageThatNeedsToBePublic(witnessLinkage)) {
        Mangle::ASTMangler Mangler;
        addSymbol(
            Mangler.mangleWitnessThunk(normalConformance, requirementDecl));
      }
    };
    normalConformance->forEachValueWitness(
        nullptr, [&](ValueDecl *valueReq, Witness witness) {
          auto witnessDecl = witness.getDecl();
          if (isa<AbstractFunctionDecl>(valueReq)) {
            addSymbolIfNecessary(valueReq, witnessDecl);
          } else if (auto *storage = dyn_cast<AbstractStorageDecl>(valueReq)) {
            auto witnessStorage = cast<AbstractStorageDecl>(witnessDecl);
            if (auto *getter = storage->getGetter())
              addSymbolIfNecessary(getter, witnessStorage->getGetter());
            if (auto *setter = storage->getSetter())
              addSymbolIfNecessary(setter, witnessStorage->getSetter());
            if (auto *materializeForSet = storage->getMaterializeForSetFunc())
              addSymbolIfNecessary(materializeForSet,
                                   witnessStorage->getMaterializeForSetFunc());
          }
        });
  }
}

void TBDGenVisitor::visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
  addSymbol(SILDeclRef(AFD));

  if (AFD->getAttrs().hasAttribute<CDeclAttr>()) {
    // A @_cdecl("...") function has an extra symbol, with the name from the
    // attribute.
    addSymbol(SILDeclRef(AFD).asForeign());
  }

  auto publicDefaultArgGenerators =
      SwiftModule->getASTContext().isSwiftVersion3() ||
      SwiftModule->isTestingEnabled();
  if (!publicDefaultArgGenerators)
    return;

  // In Swift 3 (or under -enable-testing), default arguments (of public
  // functions) are public symbols, as the default values are computed at the
  // call site.
  auto index = 0;
  for (auto *param : *AFD->getParameters()) {
    if (param->getDefaultValue())
      addSymbol(SILDeclRef::getDefaultArgGenerator(AFD, index));
    index++;
  }
}

void TBDGenVisitor::visitAccessorDecl(AccessorDecl *AD) {
  // Do nothing: accessors are always nested within the storage decl, but
  // sometimes appear outside it too. To avoid double-walking them, we
  // explicitly visit them as members of the storage and ignore them when we
  // visit them as part of the main walk, here.
}

void TBDGenVisitor::visitAbstractStorageDecl(AbstractStorageDecl *ASD) {
  // Explicitly look at each accessor here: see visitAccessorDecl.
  for (auto accessor : ASD->getAllAccessors()) {
    visitAbstractFunctionDecl(accessor);
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

  visitAbstractStorageDecl(VD);
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
  }

  visitNominalTypeDecl(CD);

  auto hasResilientAncestor =
      CD->isResilient(SwiftModule, ResilienceExpansion::Minimal);
  auto ancestor = CD->getSuperclassDecl();
  while (ancestor && !hasResilientAncestor) {
    hasResilientAncestor |=
        ancestor->isResilient(SwiftModule, ResilienceExpansion::Maximal);
    ancestor = ancestor->getSuperclassDecl();
  }

  // Types with resilient superclasses have some extra symbols.
  if (!hasResilientAncestor)
    return;

  addSymbol(LinkEntity::forClassMetadataBaseOffset(CD));

  // And classes that are themselves resilient (not just a superclass) have even
  // more.
  if (!CD->isResilient())
    return;

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

void TBDGenVisitor::visitDestructorDecl(DestructorDecl *DD) {
  // Class destructors come in two forms (deallocating and non-deallocating),
  // like constructors above. This is the deallocating one:
  visitAbstractFunctionDecl(DD);

  auto parentClass = DD->getParent()->getAsClassOrClassExtensionContext();

  // But the non-deallocating one doesn't apply to some @objc classes.
  if (!Lowering::usesObjCAllocator(parentClass)) {
    addSymbol(SILDeclRef(DD, SILDeclRef::Kind::Destroyer));
  }
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
  visitNominalTypeDecl(ED);

  if (!ED->isResilient())
    return;

  // Emit resilient tags.
  for (auto *elt : ED->getAllElements()) {
    auto entity = LinkEntity::forEnumCase(elt);
    addSymbol(entity);
  }
}

void TBDGenVisitor::addFirstFileSymbols() {
  if (!Opts.ModuleLinkName.empty()) {
    SmallString<32> buf;
    addSymbol(irgen::encodeForceLoadSymbolName(buf, Opts.ModuleLinkName));
  }
}

static void enumeratePublicSymbolsAndWrite(ModuleDecl *M, FileUnit *singleFile,
                                           StringSet &symbols,
                                           llvm::raw_ostream *os,
                                           TBDGenOptions &opts) {
  auto isWholeModule = singleFile == nullptr;
  const auto &target = M->getASTContext().LangOpts.Target;
  UniversalLinkageInfo linkInfo(target, opts.HasMultipleIGMs, isWholeModule);

  TBDGenVisitor visitor(symbols, target, linkInfo, M, opts);

  auto visitFile = [&](FileUnit *file) {
    if (file == M->getFiles()[0]) {
      visitor.addFirstFileSymbols();
    }

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
                                   TBDGenOptions &opts) {
  enumeratePublicSymbolsAndWrite(file->getParentModule(), file, symbols,
                                 nullptr, opts);
}
void swift::enumeratePublicSymbols(ModuleDecl *M, StringSet &symbols,
                                   TBDGenOptions &opts) {
  enumeratePublicSymbolsAndWrite(M, nullptr, symbols, nullptr, opts);
}
void swift::writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                         TBDGenOptions &opts) {
  StringSet symbols;
  enumeratePublicSymbolsAndWrite(M, nullptr, symbols, &os, opts);
}
