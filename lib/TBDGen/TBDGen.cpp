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
#include "llvm/Support/Error.h"
#include "llvm/Support/YAMLTraits.h"

#include "TBDGenVisitor.h"
#include "tapi/Architecture.h"
#include "tapi/InterfaceFile.h"
#include "tapi/Platform.h"
#include "tapi/TextStub_v3.h"
#include "tapi/YAMLReaderWriter.h"

using namespace swift;
using namespace swift::irgen;
using namespace swift::tbdgen;
using StringSet = llvm::StringSet<>;
using SymbolKind = tapi::internal::SymbolKind;

static bool isGlobalOrStaticVar(VarDecl *VD) {
  return VD->isStatic() || VD->getDeclContext()->isModuleScopeContext();
}

void TBDGenVisitor::addSymbol(StringRef name, SymbolKind kind) {
  Symbols.addSymbol(kind, name, Archs);

  if (StringSymbols && kind == SymbolKind::GlobalSymbol) {
    auto isNewValue = StringSymbols->insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "symbol appears twice");
  }
}

void TBDGenVisitor::addSymbol(SILDeclRef declRef) {
  auto linkage = effectiveLinkageForClassMember(
    declRef.getLinkage(ForDefinition),
    declRef.getSubclassScope());
  if (linkage == SILLinkage::Public)
    addSymbol(declRef.mangle());
}

void TBDGenVisitor::addSymbol(LinkEntity entity) {
  auto linkage =
      LinkInfo::get(UniversalLinkInfo, SwiftModule, entity, ForDefinition);

  auto externallyVisible =
      llvm::GlobalValue::isExternalLinkage(linkage.getLinkage()) &&
      linkage.getVisibility() != llvm::GlobalValue::HiddenVisibility;

  if (externallyVisible)
    addSymbol(linkage.getName());
}

void TBDGenVisitor::addDispatchThunk(SILDeclRef declRef) {
  auto entity = LinkEntity::forDispatchThunk(declRef);
  addSymbol(entity);
}

void TBDGenVisitor::addMethodDescriptor(SILDeclRef declRef) {
  auto entity = LinkEntity::forMethodDescriptor(declRef);
  addSymbol(entity);
}

void TBDGenVisitor::addProtocolRequirementsBaseDescriptor(ProtocolDecl *proto) {
  auto entity = LinkEntity::forProtocolRequirementsBaseDescriptor(proto);
  addSymbol(entity);
}

void TBDGenVisitor::addAssociatedTypeDescriptor(AssociatedTypeDecl *assocType) {
  auto entity = LinkEntity::forAssociatedTypeDescriptor(assocType);
  addSymbol(entity);
}

void TBDGenVisitor::addAssociatedConformanceDescriptor(
                                           AssociatedConformance conformance) {
  auto entity = LinkEntity::forAssociatedConformanceDescriptor(conformance);
  addSymbol(entity);
}

void TBDGenVisitor::addBaseConformanceDescriptor(
                                           BaseConformance conformance) {
  auto entity = LinkEntity::forBaseConformanceDescriptor(conformance);
  addSymbol(entity);
}

void TBDGenVisitor::addConformances(DeclContext *DC) {
  for (auto conformance : DC->getLocalConformances()) {
    auto protocol = conformance->getProtocol();
    auto needsWTable =
        Lowering::TypeConverter::protocolRequiresWitnessTable(protocol);
    if (!needsWTable)
      continue;

    // Only root conformances get symbols; the others get any public symbols
    // from their parent conformances.
    auto rootConformance = dyn_cast<RootProtocolConformance>(conformance);
    if (!rootConformance) {
      continue;
    }

    addSymbol(LinkEntity::forProtocolWitnessTable(rootConformance));
    addSymbol(LinkEntity::forProtocolConformanceDescriptor(rootConformance));

    // FIXME: the logic around visibility in extensions is confusing, and
    // sometimes witness thunks need to be manually made public.

    auto conformanceIsFixed = SILWitnessTable::conformanceIsSerialized(
        rootConformance);
    auto addSymbolIfNecessary = [&](ValueDecl *requirementDecl,
                                    ValueDecl *witnessDecl) {
      auto witnessLinkage = SILDeclRef(witnessDecl).getLinkage(ForDefinition);
      if (conformanceIsFixed &&
          (isa<SelfProtocolConformance>(rootConformance) ||
           fixmeWitnessHasLinkageThatNeedsToBePublic(witnessLinkage))) {
        Mangle::ASTMangler Mangler;
        addSymbol(
            Mangler.mangleWitnessThunk(rootConformance, requirementDecl));
      }
    };

    rootConformance->forEachValueWitness(
        nullptr, [&](ValueDecl *valueReq, Witness witness) {
          auto witnessDecl = witness.getDecl();
          if (isa<AbstractFunctionDecl>(valueReq)) {
            addSymbolIfNecessary(valueReq, witnessDecl);
          } else if (auto *storage = dyn_cast<AbstractStorageDecl>(valueReq)) {
            auto witnessStorage = cast<AbstractStorageDecl>(witnessDecl);
            storage->visitOpaqueAccessors([&](AccessorDecl *reqtAccessor) {
              auto witnessAccessor =
                witnessStorage->getAccessor(reqtAccessor->getAccessorKind());
              assert(witnessAccessor && "no corresponding witness accessor?");
              addSymbolIfNecessary(reqtAccessor, witnessAccessor);
            });
          }
        });
  }
}

void TBDGenVisitor::visitAbstractFunctionDecl(AbstractFunctionDecl *AFD) {
  // A @_silgen_name("...") function without a body only exists
  // to forward-declare a symbol from another library.
  if (!AFD->hasBody() && AFD->getAttrs().hasAttribute<SILGenNameAttr>()) {
    return;
  }

  addSymbol(SILDeclRef(AFD));

  // Add the global function pointer for a dynamically replaceable function.
  if (AFD->isNativeDynamic()) {
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionVariable(AFD));
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionImpl(AFD));
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionKey(AFD));
  }
  if (AFD->getAttrs().hasAttribute<DynamicReplacementAttr>()) {
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionVariable(AFD));
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionImpl(AFD));
  }

  if (AFD->getAttrs().hasAttribute<CDeclAttr>()) {
    // A @_cdecl("...") function has an extra symbol, with the name from the
    // attribute.
    addSymbol(SILDeclRef(AFD).asForeign());
  }

  auto publicDefaultArgGenerators = SwiftModule->isTestingEnabled();
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
  // Add the property descriptor if the decl needs it.
  if (SwiftModule->getASTContext().LangOpts.EnableKeyPathResilience
      && ASD->exportsPropertyDescriptor()) {
    addSymbol(LinkEntity::forPropertyDescriptor(ASD));
  }

  // Explicitly look at each accessor here: see visitAccessorDecl.
  for (auto accessor : ASD->getAllAccessors()) {
    visitAbstractFunctionDecl(accessor);
  }
}

void TBDGenVisitor::visitVarDecl(VarDecl *VD) {
  // Variables inside non-resilient modules have some additional symbols.
  if (!VD->isResilient()) {
    // Non-global variables might have an explicit initializer symbol, in
    // non-resilient modules.
    if (VD->getAttrs().hasAttribute<HasInitialValueAttr>() &&
        !isGlobalOrStaticVar(VD)) {
      auto declRef = SILDeclRef(VD, SILDeclRef::Kind::StoredPropertyInitializer);
      // Stored property initializers for public properties are currently
      // public.
      addSymbol(declRef);
    }

    // statically/globally stored variables have some special handling.
    if (VD->hasStorage() &&
        isGlobalOrStaticVar(VD)) {
      if (getDeclLinkage(VD) == FormalLinkage::PublicUnique) {
        // The actual variable has a symbol.
        Mangle::ASTMangler mangler;
        addSymbol(mangler.mangleEntity(VD, false));
      }

      if (VD->isLazilyInitializedGlobal())
        addSymbol(SILDeclRef(VD, SILDeclRef::Kind::GlobalAccessor));
    }
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
    bool addObjCClass = false;
    if (isObjC) {
      addObjCClass = true;
      addSymbol(LinkEntity::forObjCClass(CD));
    }

    if (CD->getMetaclassKind() == ClassDecl::MetaclassKind::ObjC) {
      addObjCClass = true;
      addSymbol(LinkEntity::forObjCMetaclass(CD));
    } else
      addSymbol(LinkEntity::forSwiftMetaclassStub(CD));

    if (addObjCClass) {
      SmallString<128> buffer;
      addSymbol(CD->getObjCRuntimeName(buffer), SymbolKind::ObjectiveCClass);
    }
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
      CD->hasResilientMetadata(SwiftModule, ResilienceExpansion::Minimal);
  auto ancestor = CD->getSuperclassDecl();
  while (ancestor && !hasResilientAncestor) {
    hasResilientAncestor |=
        ancestor->hasResilientMetadata(SwiftModule, ResilienceExpansion::Maximal);
    ancestor = ancestor->getSuperclassDecl();
  }

  // Types with resilient superclasses have some extra symbols.
  if (hasResilientAncestor)
    addSymbol(LinkEntity::forClassMetadataBaseOffset(CD));

  // Emit dispatch thunks for every new vtable entry.
  struct VTableVisitor : public SILVTableVisitor<VTableVisitor> {
    TBDGenVisitor &TBD;
    ClassDecl *CD;
    bool FirstTime = true;

  public:
    VTableVisitor(TBDGenVisitor &TBD, ClassDecl *CD)
        : TBD(TBD), CD(CD) {}

    void addMethod(SILDeclRef method) {
      assert(method.getDecl()->getDeclContext() == CD);

      if (CD->hasResilientMetadata()) {
        if (FirstTime) {
          FirstTime = false;

          // If the class is itself resilient and has at least one vtable entry,
          // it has a method lookup function.
          TBD.addSymbol(LinkEntity::forMethodLookupFunction(CD));
        }

        TBD.addDispatchThunk(method);
      }

      TBD.addMethodDescriptor(method);
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
  if (CD->getParent()->getSelfClassDecl()) {
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

  auto parentClass = DD->getParent()->getSelfClassDecl();

  // But the non-deallocating one doesn't apply to some @objc classes.
  if (!Lowering::usesObjCAllocator(parentClass)) {
    addSymbol(SILDeclRef(DD, SILDeclRef::Kind::Destroyer));
  }
}

void TBDGenVisitor::visitExtensionDecl(ExtensionDecl *ED) {
  if (!isa<ProtocolDecl>(ED->getExtendedNominal())) {
    addConformances(ED);
  }

  for (auto member : ED->getMembers())
    visit(member);
}

/// Determine whether the protocol descriptor for the given protocol will
/// contain any protocol requirements.
static bool protocolDescriptorHasRequirements(ProtocolDecl *proto) {
  if (!proto->getRequirementSignature().empty())
    return true;

  for (auto *member : proto->getMembers()) {
    if (auto func = dyn_cast<AbstractFunctionDecl>(member)) {
      if (SILDeclRef::requiresNewWitnessTableEntry(func))
        return true;
    }

    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      if (assocType->getOverriddenDecls().empty()) {
        return true;
      }
    }
  }

  return false;
}

void TBDGenVisitor::visitProtocolDecl(ProtocolDecl *PD) {
  if (!PD->isObjC()) {
    addSymbol(LinkEntity::forProtocolDescriptor(PD));

    // If there are any requirements, emit a requirements base descriptor.
    if (protocolDescriptorHasRequirements(PD))
      addProtocolRequirementsBaseDescriptor(PD);

    for (const auto &req : PD->getRequirementSignature()) {
      if (req.getKind() != RequirementKind::Conformance)
        continue;

      if (req.getFirstType()->isEqual(PD->getSelfInterfaceType())) {
        BaseConformance conformance(
          PD,
          req.getSecondType()->castTo<ProtocolType>()->getDecl());
        addBaseConformanceDescriptor(conformance);
      } else {
        AssociatedConformance conformance(
          PD,
          req.getFirstType()->getCanonicalType(),
          req.getSecondType()->castTo<ProtocolType>()->getDecl());
        addAssociatedConformanceDescriptor(conformance);
      }
    }

    for (auto *member : PD->getMembers()) {
      if (PD->isResilient()) {
        if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(member)) {
          if (SILDeclRef::requiresNewWitnessTableEntry(funcDecl)) {
            addDispatchThunk(SILDeclRef(funcDecl));
            addMethodDescriptor(SILDeclRef(funcDecl));
          }
        }
      }

      // Always produce associated type descriptors, because they can
      // be referenced by generic signatures.
      if (auto *assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        if (assocType->getOverriddenDecls().empty())
          addAssociatedTypeDescriptor(assocType);
      }
    }

    // Include the self-conformance.
    addConformances(PD);
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

/// Converts a version tuple into a packed version, ignoring components beyond
/// major, minor, and subminor.
static tapi::internal::PackedVersion
convertToPacked(const version::Version &version) {
  // FIXME: Warn if version is greater than 3 components?
  unsigned major = 0, minor = 0, subminor = 0;
  if (version.size() > 0) major = version[0];
  if (version.size() > 1) minor = version[1];
  if (version.size() > 2) subminor = version[2];
  return tapi::internal::PackedVersion(major, minor, subminor);
}

static bool isApplicationExtensionSafe(const LangOptions &LangOpts) {
  return LangOpts.EnableAppExtensionRestrictions;
}

static void enumeratePublicSymbolsAndWrite(ModuleDecl *M, FileUnit *singleFile,
                                           StringSet *symbols,
                                           llvm::raw_ostream *os,
                                           const TBDGenOptions &opts) {
  auto isWholeModule = singleFile == nullptr;
  const auto &target = M->getASTContext().LangOpts.Target;
  UniversalLinkageInfo linkInfo(target, opts.HasMultipleIGMs, isWholeModule);

  tapi::internal::InterfaceFile file;
  file.setFileType(tapi::internal::FileType::TBD_V3);
  file.setApplicationExtensionSafe(
    isApplicationExtensionSafe(M->getASTContext().LangOpts));
  file.setInstallName(opts.InstallName);
  file.setCurrentVersion(convertToPacked(opts.CurrentVersion));
  file.setCompatibilityVersion(convertToPacked(opts.CompatibilityVersion));
  file.setTwoLevelNamespace();
  file.setSwiftABIVersion(TAPI_SWIFT_ABI_VERSION);
  file.setPlatform(tapi::internal::mapToSinglePlatform(target));
  auto arch = tapi::internal::getArchType(target.getArchName());
  file.setArch(arch);
  file.setInstallAPI();

  TBDGenVisitor visitor(file, arch, symbols, linkInfo, M, opts);

  auto visitFile = [&](FileUnit *file) {
    if (file == M->getFiles()[0]) {
      visitor.addFirstFileSymbols();
    }

    SmallVector<Decl *, 16> decls;
    file->getTopLevelDecls(decls);

    visitor.addMainIfNecessary(file);

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
    tapi::internal::YAMLWriter writer;
    writer.add(
        llvm::make_unique<tapi::internal::stub::v3::YAMLDocumentHandler>());

    assert(writer.canWrite(&file) &&
           "YAML writer should be able to write TBD v3");
    llvm::cantFail(writer.writeFile(*os, &file),
                   "YAML writing should be error-free");
  }
}

void swift::enumeratePublicSymbols(FileUnit *file, StringSet &symbols,
                                   const TBDGenOptions &opts) {
  enumeratePublicSymbolsAndWrite(file->getParentModule(), file, &symbols,
                                 nullptr, opts);
}
void swift::enumeratePublicSymbols(ModuleDecl *M, StringSet &symbols,
                                   const TBDGenOptions &opts) {
  enumeratePublicSymbolsAndWrite(M, nullptr, &symbols, nullptr, opts);
}
void swift::writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                         const TBDGenOptions &opts) {
  enumeratePublicSymbolsAndWrite(M, nullptr, nullptr, &os, opts);
}
