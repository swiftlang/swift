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

#include "swift/AST/Availability.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/SynthesizedFileUnit.h"
#include "swift/AST/TBDGenRequests.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVTableVisitor.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/TextAPI/MachO/InterfaceFile.h"
#include "llvm/TextAPI/MachO/TextAPIReader.h"
#include "llvm/TextAPI/MachO/TextAPIWriter.h"

#include "TBDGenVisitor.h"

using namespace swift;
using namespace swift::irgen;
using namespace swift::tbdgen;
using namespace llvm::yaml;
using StringSet = llvm::StringSet<>;
using SymbolKind = llvm::MachO::SymbolKind;

static bool isGlobalOrStaticVar(VarDecl *VD) {
  return VD->isStatic() || VD->getDeclContext()->isModuleScopeContext();
}

void TBDGenVisitor::addSymbolInternal(StringRef name,
                                      llvm::MachO::SymbolKind kind,
                                      bool isLinkerDirective) {
  if (!isLinkerDirective && Opts.LinkerDirectivesOnly)
    return;
  Symbols.addSymbol(kind, name, Targets);
  if (StringSymbols && kind == SymbolKind::GlobalSymbol) {
    auto isNewValue = StringSymbols->insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "symbol appears twice");
  }
}

static std::vector<OriginallyDefinedInAttr::ActiveVersion>
getAllMovedPlatformVersions(Decl *D) {
  std::vector<OriginallyDefinedInAttr::ActiveVersion> Results;
  for (auto *attr: D->getAttrs()) {
    if (auto *ODA = dyn_cast<OriginallyDefinedInAttr>(attr)) {
      auto Active = ODA->isActivePlatform(D->getASTContext());
      if (Active.hasValue()) {
        Results.push_back(*Active);
      }
    }
  }
  return Results;
}

static StringRef getLinkerPlatformName(uint8_t Id) {
  switch (Id) {
#define LD_PLATFORM(Name, Id) case Id: return #Name;
#include "ldPlatformKinds.def"
  default:
    llvm_unreachable("unrecognized platform id");
  }
}

static Optional<uint8_t> getLinkerPlatformId(StringRef Platform) {
  return llvm::StringSwitch<Optional<uint8_t>>(Platform)
#define LD_PLATFORM(Name, Id) .Case(#Name, Id)
#include "ldPlatformKinds.def"
    .Default(None);
}

StringRef InstallNameStore::getInstallName(LinkerPlatformId Id) const {
  auto It = PlatformInstallName.find((uint8_t)Id);
  if (It == PlatformInstallName.end())
    return InstallName;
  else
    return It->second;
}

void InstallNameStore::remark(ASTContext &Ctx, StringRef ModuleName) const {
  Ctx.Diags.diagnose(SourceLoc(), diag::default_previous_install_name,
                     ModuleName, InstallName);
  for (auto Pair: PlatformInstallName) {
    Ctx.Diags.diagnose(SourceLoc(), diag::platform_previous_install_name,
                       ModuleName, getLinkerPlatformName(Pair.first),
                       Pair.second);
  }
}

static std::string getScalaNodeText(Node *N) {
  SmallString<32> Buffer;
  return cast<ScalarNode>(N)->getValue(Buffer).str();
}

static std::set<int8_t> getSequenceNodePlatformList(ASTContext &Ctx, Node *N) {
  std::set<int8_t> Results;
  for (auto &E: *cast<SequenceNode>(N)) {
    auto Platform = getScalaNodeText(&E);
    auto Id = getLinkerPlatformId(Platform);
    if (Id.hasValue()) {
      Results.insert(*Id);
    } else {
      // Diagnose unrecognized platform name.
      Ctx.Diags.diagnose(SourceLoc(), diag::unknown_platform_name, Platform);
    }
  }
  return Results;
}

/// Parse an entry like this, where the "platforms" key-value pair is optional:
///  {
///     "module": "Foo",
///     "platforms": ["macOS"],
///     "install_name": "/System/MacOS"
///  },
static int
parseEntry(ASTContext &Ctx,
           Node *Node, std::map<std::string, InstallNameStore> &Stores) {
  if (auto *SN = cast<SequenceNode>(Node)) {
    for (auto It = SN->begin(); It != SN->end(); ++It) {
      auto *MN = cast<MappingNode>(&*It);
      std::string ModuleName;
      std::string InstallName;
      Optional<std::set<int8_t>> Platforms;
      for (auto &Pair: *MN) {
        auto Key = getScalaNodeText(Pair.getKey());
        auto* Value = Pair.getValue();
        if (Key == "module") {
          ModuleName = getScalaNodeText(Value);
        } else if (Key == "platforms") {
          Platforms = getSequenceNodePlatformList(Ctx, Value);
        } else if (Key == "install_name") {
          InstallName = getScalaNodeText(Value);
        } else {
          return 1;
        }
      }
      if (ModuleName.empty() || InstallName.empty())
        return 1;
      auto &Store = Stores.insert(std::make_pair(ModuleName,
        InstallNameStore())).first->second;
      if (Platforms.hasValue()) {
        // This install name is platform-specific.
        for (auto Id: Platforms.getValue()) {
          Store.PlatformInstallName[Id] = InstallName;
        }
      } else {
        // The install name is the default one.
        Store.InstallName = InstallName;
      }
    }
  } else {
    return 1;
  }
  return 0;
}

std::unique_ptr<std::map<std::string, InstallNameStore>>
TBDGenVisitor::parsePreviousModuleInstallNameMap() {
  StringRef FileName = Opts.ModuleInstallNameMapPath;
  // Nothing to parse.
  if (FileName.empty())
    return nullptr;
  namespace yaml = llvm::yaml;
  ASTContext &Ctx = SwiftModule->getASTContext();
  std::unique_ptr<std::map<std::string, InstallNameStore>> pResult(
    new std::map<std::string, InstallNameStore>());
  auto &AllInstallNames = *pResult;
  SWIFT_DEFER {
    for (auto Pair: AllInstallNames) {
      Pair.second.remark(Ctx, Pair.first);
    }
  };

  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFile(FileName);
  if (!FileBufOrErr) {
    Ctx.Diags.diagnose(SourceLoc(), diag::previous_installname_map_missing,
                       FileName);
    return nullptr;
  }
  StringRef Buffer = FileBufOrErr->get()->getBuffer();

  // Use a new source manager instead of the one from ASTContext because we
  // don't want the Json file to be persistent.
  SourceManager SM;
  yaml::Stream Stream(llvm::MemoryBufferRef(Buffer, FileName),
                      SM.getLLVMSourceMgr());
  for (auto DI = Stream.begin(); DI != Stream.end(); ++ DI) {
    assert(DI != Stream.end() && "Failed to read a document");
    yaml::Node *N = DI->getRoot();
    assert(N && "Failed to find a root");
    if (parseEntry(Ctx, N, AllInstallNames)) {
      Ctx.Diags.diagnose(SourceLoc(), diag::previous_installname_map_corrupted,
                         FileName);
      return nullptr;
    }
  }
  return pResult;
}

static LinkerPlatformId
getLinkerPlatformId(OriginallyDefinedInAttr::ActiveVersion Ver) {
  switch(Ver.Platform) {
  case swift::PlatformKind::none:
    llvm_unreachable("cannot find platform kind");
  case swift::PlatformKind::iOS:
  case swift::PlatformKind::iOSApplicationExtension:
    return Ver.IsSimulator ? LinkerPlatformId::iOS_sim:
                             LinkerPlatformId::iOS;
  case swift::PlatformKind::tvOS:
  case swift::PlatformKind::tvOSApplicationExtension:
    return Ver.IsSimulator ? LinkerPlatformId::tvOS_sim:
                             LinkerPlatformId::tvOS;
  case swift::PlatformKind::watchOS:
  case swift::PlatformKind::watchOSApplicationExtension:
    return Ver.IsSimulator ? LinkerPlatformId::watchOS_sim:
                             LinkerPlatformId::watchOS;
  case swift::PlatformKind::OSX:
  case swift::PlatformKind::OSXApplicationExtension:
    return LinkerPlatformId::macOS;
  case swift::PlatformKind::macCatalyst:
  case swift::PlatformKind::macCatalystApplicationExtension:
    return LinkerPlatformId::macCatalyst;
  }
}

static StringRef
getLinkerPlatformName(OriginallyDefinedInAttr::ActiveVersion Ver) {
  return getLinkerPlatformName((uint8_t)getLinkerPlatformId(Ver));
}

/// Find the most relevant introducing version of the decl stack we have visted
/// so far.
static Optional<llvm::VersionTuple>
getInnermostIntroVersion(ArrayRef<Decl*> DeclStack, PlatformKind Platform) {
  for (auto It = DeclStack.rbegin(); It != DeclStack.rend(); ++ It) {
    if (auto Result = (*It)->getIntroducedOSVersion(Platform))
      return Result;
  }
  return None;
}

void TBDGenVisitor::addLinkerDirectiveSymbolsLdPrevious(StringRef name,
                                                llvm::MachO::SymbolKind kind) {
  if (kind != llvm::MachO::SymbolKind::GlobalSymbol)
    return;
  if(DeclStack.empty())
    return;
  auto TopLevelDecl = DeclStack.front();
  auto MovedVers = getAllMovedPlatformVersions(TopLevelDecl);
  if (MovedVers.empty())
    return;
  assert(!MovedVers.empty());
  assert(previousInstallNameMap);
  auto &Ctx = TopLevelDecl->getASTContext();
  for (auto &Ver: MovedVers) {
    auto IntroVer = getInnermostIntroVersion(DeclStack, Ver.Platform);
    assert(IntroVer && "cannot find OS intro version");
    if (!IntroVer.hasValue())
      continue;
    // This decl is available after the top-level symbol has been moved here,
    // so we don't need the linker directives.
    if (*IntroVer >= Ver.Version)
      continue;
    auto PlatformNumber = getLinkerPlatformId(Ver);
    auto It = previousInstallNameMap->find(Ver.ModuleName.str());
    if (It == previousInstallNameMap->end()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::cannot_find_install_name,
                         Ver.ModuleName, getLinkerPlatformName(Ver));
      continue;
    }
    auto InstallName = It->second.getInstallName(PlatformNumber);
    if (InstallName.empty()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::cannot_find_install_name,
                         Ver.ModuleName, getLinkerPlatformName(Ver));
      continue;
    }
    llvm::SmallString<64> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    // Empty compatible version indicates using the current compatible version.
    StringRef ComptibleVersion = "";
    OS << "$ld$previous$";
    OS << InstallName << "$";
    OS << ComptibleVersion << "$";
    OS << std::to_string((uint8_t)PlatformNumber) << "$";
    static auto getMinor = [](Optional<unsigned> Minor) {
      return Minor.hasValue() ? *Minor : 0;
    };
    OS << IntroVer->getMajor() << "." << getMinor(IntroVer->getMinor()) << "$";
    OS << Ver.Version.getMajor() << "." << getMinor(Ver.Version.getMinor()) << "$";
    OS << name << "$";
    addSymbolInternal(OS.str(), llvm::MachO::SymbolKind::GlobalSymbol,
                      /*LinkerDirective*/true);
  }
}

void TBDGenVisitor::addLinkerDirectiveSymbolsLdHide(StringRef name,
                                                    llvm::MachO::SymbolKind kind) {
  if (kind != llvm::MachO::SymbolKind::GlobalSymbol)
    return;
  if (DeclStack.empty())
    return;
  auto TopLevelDecl = DeclStack.front();
  auto MovedVers = getAllMovedPlatformVersions(TopLevelDecl);
  if (MovedVers.empty())
    return;
  assert(!MovedVers.empty());

  // Using $ld$add and $ld$hide cannot encode platform name in the version number,
  // so we can only handle one version.
  // FIXME: use $ld$previous instead
  auto MovedVer = MovedVers.front().Version;
  auto Platform = MovedVers.front().Platform;
  unsigned Major[2];
  unsigned Minor[2];
  Major[1] = MovedVer.getMajor();
  Minor[1] = MovedVer.getMinor().hasValue() ? *MovedVer.getMinor(): 0;
  auto IntroVer = getInnermostIntroVersion(DeclStack, Platform);
  assert(IntroVer && "cannot find the start point of availability");
  if (!IntroVer.hasValue())
    return;
  // This decl is available after the top-level symbol has been moved here,
  // so we don't need the linker directives.
  if (*IntroVer >= MovedVer)
    return;
  Major[0] = IntroVer->getMajor();
  Minor[0] = IntroVer->getMinor().hasValue() ? IntroVer->getMinor().getValue() : 0;
  for (auto CurMaj = Major[0]; CurMaj <= Major[1]; ++ CurMaj) {
    unsigned MinRange[2] = {0, 31};
    if (CurMaj == Major[0])
      MinRange[0] = Minor[0];
    if (CurMaj == Major[1])
      MinRange[1] = Minor[1];
    for (auto CurMin = MinRange[0]; CurMin != MinRange[1]; ++ CurMin) {
      llvm::SmallString<64> Buffer;
      llvm::raw_svector_ostream OS(Buffer);
      OS << "$ld$hide$os" << CurMaj << "." << CurMin << "$" << name;
      addSymbolInternal(OS.str(), llvm::MachO::SymbolKind::GlobalSymbol,
                        /*LinkerDirective*/true);
    }
  }
}

void TBDGenVisitor::addSymbol(StringRef name, SymbolKind kind) {
  // The linker expects to see mangled symbol names in TBD files, so make sure
  // to mangle before inserting the symbol.
  SmallString<32> mangled;
  llvm::Mangler::getNameWithPrefix(mangled, name, DataLayout);
  addSymbolInternal(mangled, kind);
  if (previousInstallNameMap) {
    addLinkerDirectiveSymbolsLdPrevious(mangled, kind);
  } else {
    addLinkerDirectiveSymbolsLdHide(mangled, kind);
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
  for (auto conformance : DC->getLocalConformances(
                            ConformanceLookupKind::NonInherited)) {
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
    // We cannot emit the witness table symbol if the protocol is imported from
    // another module and it's resilient, because initialization of that protocol
    // is necessary in this case
    if (!rootConformance->getProtocol()->isResilient(DC->getParentModule(),
                                                     ResilienceExpansion::Maximal))
      addSymbol(LinkEntity::forProtocolWitnessTable(rootConformance));
    addSymbol(LinkEntity::forProtocolConformanceDescriptor(rootConformance));

    // FIXME: the logic around visibility in extensions is confusing, and
    // sometimes witness thunks need to be manually made public.

    auto conformanceIsFixed = SILWitnessTable::conformanceIsSerialized(
        rootConformance);
    auto addSymbolIfNecessary = [&](ValueDecl *requirementDecl,
                                    ValueDecl *witnessDecl) {
      auto witnessRef = SILDeclRef(witnessDecl);
      auto witnessLinkage = witnessRef.getLinkage(ForDefinition);
      if (conformanceIsFixed &&
          (isa<SelfProtocolConformance>(rootConformance) ||
           fixmeWitnessHasLinkageThatNeedsToBePublic(witnessRef))) {
        Mangle::ASTMangler Mangler;
        addSymbol(
            Mangler.mangleWitnessThunk(rootConformance, requirementDecl));
      }
    };

    rootConformance->forEachValueWitness([&](ValueDecl *valueReq,
                                             Witness witness) {
      auto witnessDecl = witness.getDecl();
      if (isa<AbstractFunctionDecl>(valueReq)) {
        addSymbolIfNecessary(valueReq, witnessDecl);
      } else if (auto *storage = dyn_cast<AbstractStorageDecl>(valueReq)) {
        if (auto witnessStorage = dyn_cast<AbstractStorageDecl>(witnessDecl)) {
          storage->visitOpaqueAccessors([&](AccessorDecl *reqtAccessor) {
            auto witnessAccessor = witnessStorage->getSynthesizedAccessor(
                reqtAccessor->getAccessorKind());
            addSymbolIfNecessary(reqtAccessor, witnessAccessor);
          });
        } else if (isa<EnumElementDecl>(witnessDecl)) {
          auto getter = storage->getSynthesizedAccessor(AccessorKind::Get);
          addSymbolIfNecessary(getter, witnessDecl);
        }
      }
    });
  }
}

void TBDGenVisitor::addAutoDiffLinearMapFunction(AbstractFunctionDecl *original,
                                                 AutoDiffConfig config,
                                                 AutoDiffLinearMapKind kind) {
  auto &ctx = original->getASTContext();
  auto declRef =
      SILDeclRef(original).asForeign(requiresForeignEntryPoint(original));

  if (!declRef.isSerialized())
    return;
  // Linear maps are public only when the original function is serialized.
  if (!declRef.isSerialized())
    return;
  // Differential functions are emitted only when forward-mode is enabled.
  if (kind == AutoDiffLinearMapKind::Differential &&
      !ctx.LangOpts.EnableExperimentalForwardModeDifferentiation)
    return;
  auto *loweredParamIndices = autodiff::getLoweredParameterIndices(
      config.parameterIndices,
      original->getInterfaceType()->castTo<AnyFunctionType>());
  Mangle::ASTMangler mangler;
  AutoDiffConfig silConfig{loweredParamIndices, config.resultIndices,
                           config.derivativeGenericSignature};
  std::string linearMapName =
      mangler.mangleAutoDiffLinearMapHelper(declRef.mangle(), kind, silConfig);
  addSymbol(linearMapName);
}

void TBDGenVisitor::addAutoDiffDerivativeFunction(
    AbstractFunctionDecl *original, IndexSubset *parameterIndices,
    GenericSignature derivativeGenericSignature,
    AutoDiffDerivativeFunctionKind kind) {
  auto *assocFnId = AutoDiffDerivativeFunctionIdentifier::get(
      kind, parameterIndices, derivativeGenericSignature,
      original->getASTContext());
  auto declRef =
      SILDeclRef(original).asForeign(requiresForeignEntryPoint(original));
  addSymbol(declRef.asAutoDiffDerivativeFunction(assocFnId));
}

void TBDGenVisitor::addDifferentiabilityWitness(
    AbstractFunctionDecl *original, IndexSubset *astParameterIndices,
    IndexSubset *resultIndices, GenericSignature derivativeGenericSignature) {
  bool foreign = requiresForeignEntryPoint(original);
  auto declRef = SILDeclRef(original).asForeign(foreign);

  // Skip symbol emission for original functions that do not have public
  // linkage. Exclude original functions that require a foreign entry point with
  // `public_external` linkage.
  auto originalLinkage = declRef.getLinkage(ForDefinition);
  if (foreign)
    originalLinkage = stripExternalFromLinkage(originalLinkage);
  if (originalLinkage != SILLinkage::Public)
    return;

  auto *silParamIndices = autodiff::getLoweredParameterIndices(
      astParameterIndices,
      original->getInterfaceType()->castTo<AnyFunctionType>());

  auto originalMangledName = declRef.mangle();
  AutoDiffConfig config{silParamIndices, resultIndices,
                        derivativeGenericSignature};
  SILDifferentiabilityWitnessKey key(originalMangledName, config);

  Mangle::ASTMangler mangler;
  auto mangledName = mangler.mangleSILDifferentiabilityWitnessKey(key);
  addSymbol(mangledName);
}

void TBDGenVisitor::addDerivativeConfiguration(AbstractFunctionDecl *original,
                                               AutoDiffConfig config) {
  auto inserted = AddedDerivatives.insert({original, config});
  if (!inserted.second)
    return;

  addAutoDiffLinearMapFunction(original, config,
                               AutoDiffLinearMapKind::Differential);
  addAutoDiffLinearMapFunction(original, config,
                               AutoDiffLinearMapKind::Pullback);
  addAutoDiffDerivativeFunction(original, config.parameterIndices,
                                config.derivativeGenericSignature,
                                AutoDiffDerivativeFunctionKind::JVP);
  addAutoDiffDerivativeFunction(original, config.parameterIndices,
                                config.derivativeGenericSignature,
                                AutoDiffDerivativeFunctionKind::VJP);
  addDifferentiabilityWitness(original, config.parameterIndices,
                              config.resultIndices,
                              config.derivativeGenericSignature);
}

/// Determine whether dynamic replacement should be emitted for the allocator or
/// the initializer given a decl.
/// The rule is that structs and convenience init of classes emit a
/// dynamic replacement for the allocator.
/// Designated init of classes emit a dynamic replacement for the intializer.
/// This is because the super class init call is emitted to the initializer and
/// needs to be dynamic.
static bool shouldUseAllocatorMangling(const AbstractFunctionDecl *afd) {
  auto constructor = dyn_cast<ConstructorDecl>(afd);
  if (!constructor)
    return false;
  return constructor->getParent()->getSelfClassDecl() == nullptr ||
         constructor->isConvenienceInit();
}

void TBDGenVisitor::visitDefaultArguments(ValueDecl *VD, ParameterList *PL) {
  auto publicDefaultArgGenerators = SwiftModule->isTestingEnabled() ||
                                    SwiftModule->arePrivateImportsEnabled();
  if (!publicDefaultArgGenerators)
    return;

  // In Swift 3 (or under -enable-testing), default arguments (of public
  // functions) are public symbols, as the default values are computed at the
  // call site.
  auto index = 0;
  for (auto *param : *PL) {
    if (param->isDefaultArgument())
      addSymbol(SILDeclRef::getDefaultArgGenerator(VD, index));
    index++;
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
  if (AFD->shouldUseNativeMethodReplacement()) {
    bool useAllocator = shouldUseAllocatorMangling(AFD);
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionVariable(
        AFD, useAllocator));
    addSymbol(
        LinkEntity::forDynamicallyReplaceableFunctionKey(AFD, useAllocator));
  }
  if (AFD->getDynamicallyReplacedDecl()) {
    bool useAllocator = shouldUseAllocatorMangling(AFD);
    addSymbol(LinkEntity::forDynamicallyReplaceableFunctionVariable(
        AFD, useAllocator));
    addSymbol(
        LinkEntity::forDynamicallyReplaceableFunctionImpl(AFD, useAllocator));
  }

  if (AFD->getAttrs().hasAttribute<CDeclAttr>()) {
    // A @_cdecl("...") function has an extra symbol, with the name from the
    // attribute.
    addSymbol(SILDeclRef(AFD).asForeign());
  }

  // Add derivative function symbols.
  for (const auto *differentiableAttr :
       AFD->getAttrs().getAttributes<DifferentiableAttr>())
    addDerivativeConfiguration(
        AFD,
        AutoDiffConfig(differentiableAttr->getParameterIndices(),
                       IndexSubset::get(AFD->getASTContext(), 1, {0}),
                       differentiableAttr->getDerivativeGenericSignature()));
  for (const auto *derivativeAttr :
       AFD->getAttrs().getAttributes<DerivativeAttr>())
    addDerivativeConfiguration(
        derivativeAttr->getOriginalFunction(AFD->getASTContext()),
        AutoDiffConfig(derivativeAttr->getParameterIndices(),
                       IndexSubset::get(AFD->getASTContext(), 1, {0}),
                       AFD->getGenericSignature()));

  visitDefaultArguments(AFD, AFD->getParameters());
}

void TBDGenVisitor::visitFuncDecl(FuncDecl *FD) {
  // If there's an opaque return type, its descriptor is exported.
  if (auto opaqueResult = FD->getOpaqueResultTypeDecl()) {
    addSymbol(LinkEntity::forOpaqueTypeDescriptor(opaqueResult));
    assert(opaqueResult->getNamingDecl() == FD);
    if (FD->shouldUseNativeDynamicDispatch()) {
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessor(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorImpl(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorKey(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorVar(opaqueResult));
    }
    if (FD->getDynamicallyReplacedDecl()) {
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessor(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorVar(opaqueResult));
    }
  }
  visitAbstractFunctionDecl(FD);
}

void TBDGenVisitor::visitAccessorDecl(AccessorDecl *AD) {
  llvm_unreachable("should not see an accessor here");
}

void TBDGenVisitor::visitAbstractStorageDecl(AbstractStorageDecl *ASD) {
  // Add the property descriptor if the decl needs it.
  if (ASD->exportsPropertyDescriptor()) {
    addSymbol(LinkEntity::forPropertyDescriptor(ASD));
  }
  
  // ...and the opaque result decl if it has one.
  if (auto opaqueResult = ASD->getOpaqueResultTypeDecl()) {
    addSymbol(LinkEntity::forOpaqueTypeDescriptor(opaqueResult));
    assert(opaqueResult->getNamingDecl() == ASD);
    if (ASD->hasAnyNativeDynamicAccessors()) {
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessor(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorImpl(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorKey(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorVar(opaqueResult));
    }
    if (ASD->getDynamicallyReplacedDecl()) {
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessor(opaqueResult));
      addSymbol(LinkEntity::forOpaqueTypeDescriptorAccessorVar(opaqueResult));
    }
  }

  // Explicitly look at each accessor here: see visitAccessorDecl.
  ASD->visitEmittedAccessors([&](AccessorDecl *accessor) {
    visitFuncDecl(accessor);
  });

  // Add derivative function symbols.
  for (const auto *differentiableAttr :
       ASD->getAttrs().getAttributes<DifferentiableAttr>())
    addDerivativeConfiguration(
        ASD->getAccessor(AccessorKind::Get),
        AutoDiffConfig(differentiableAttr->getParameterIndices(),
                       IndexSubset::get(ASD->getASTContext(), 1, {0}),
                       differentiableAttr->getDerivativeGenericSignature()));
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
        addSymbol(mangler.mangleEntity(VD));
      }

      if (VD->isLazilyInitializedGlobal())
        addSymbol(SILDeclRef(VD, SILDeclRef::Kind::GlobalAccessor));
    }

    // Wrapped non-static member properties may have a backing initializer.
    if (auto wrapperInfo = VD->getPropertyWrapperBackingPropertyInfo()) {
      if (wrapperInfo.initializeFromOriginal && !VD->isStatic()) {
        addSymbol(
            SILDeclRef(VD, SILDeclRef::Kind::PropertyWrapperBackingInitializer));
      }
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
  for (auto *var : CD->getStoredProperties())
    addSymbol(LinkEntity::forFieldOffset(var));

  visitNominalTypeDecl(CD);

  bool resilientAncestry = CD->checkAncestry(AncestryFlags::ResilientOther);

  // Types with resilient superclasses have some extra symbols.
  if (resilientAncestry || CD->hasResilientMetadata()) {
    addSymbol(LinkEntity::forClassMetadataBaseOffset(CD));
  }

  auto &Ctx = CD->getASTContext();
  if (Ctx.LangOpts.EnableObjCInterop) {
    if (resilientAncestry) {
      addSymbol(LinkEntity::forObjCResilientClassStub(
          CD, TypeMetadataAddress::AddressPoint));
    }
  }

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

#ifndef NDEBUG
static bool isValidProtocolMemberForTBDGen(const Decl *D) {
  switch (D->getKind()) {
  case DeclKind::TypeAlias:
  case DeclKind::AssociatedType:
  case DeclKind::Var:
  case DeclKind::Subscript:
  case DeclKind::PatternBinding:
  case DeclKind::Func:
  case DeclKind::Accessor:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    return true;
  case DeclKind::OpaqueType:
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::GenericTypeParam:
  case DeclKind::Module:
  case DeclKind::Param:
  case DeclKind::EnumElement:
  case DeclKind::Extension:
  case DeclKind::TopLevelCode:
  case DeclKind::Import:
  case DeclKind::PrecedenceGroup:
  case DeclKind::MissingMember:
  case DeclKind::EnumCase:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    return false;
  }
  llvm_unreachable("covered switch");
}
#endif

void TBDGenVisitor::visitProtocolDecl(ProtocolDecl *PD) {
  if (!PD->isObjC()) {
    addSymbol(LinkEntity::forProtocolDescriptor(PD));

    struct WitnessVisitor : public SILWitnessVisitor<WitnessVisitor> {
      TBDGenVisitor &TBD;
      ProtocolDecl *PD;

    public:
      WitnessVisitor(TBDGenVisitor &TBD, ProtocolDecl *PD)
          : TBD(TBD), PD(PD) {}

      void addMethod(SILDeclRef declRef) {
        if (PD->isResilient()) {
          TBD.addDispatchThunk(declRef);
          TBD.addMethodDescriptor(declRef);
        }
      }

      void addAssociatedType(AssociatedType associatedType) {
        TBD.addAssociatedTypeDescriptor(associatedType.getAssociation());
      }

      void addProtocolConformanceDescriptor() {
        TBD.addProtocolRequirementsBaseDescriptor(PD);
      }

      void addOutOfLineBaseProtocol(ProtocolDecl *proto) {
        TBD.addBaseConformanceDescriptor(BaseConformance(PD, proto));
      }

      void addAssociatedConformance(AssociatedConformance associatedConf) {
        TBD.addAssociatedConformanceDescriptor(associatedConf);
      }

      void addPlaceholder(MissingMemberDecl *decl) {}

      void doIt() {
        visitProtocolDecl(PD);
      }
    };

    WitnessVisitor(*this, PD).doIt();

    // Include the self-conformance.
    addConformances(PD);
  }

#ifndef NDEBUG
  // There's no (currently) relevant information about members of a protocol at
  // individual protocols, each conforming type has to handle them individually
  // (NB. anything within an active IfConfigDecls also appears outside). Let's
  // assert this fact:
  for (auto *member : PD->getMembers()) {
    assert(isValidProtocolMemberForTBDGen(member) &&
           "unexpected member of protocol during TBD generation");
  }
#endif
}

void TBDGenVisitor::visitEnumDecl(EnumDecl *ED) {
  visitNominalTypeDecl(ED);
}

void TBDGenVisitor::visitEnumElementDecl(EnumElementDecl *EED) {
  if (EED->getParentEnum()->isResilient())
    addSymbol(LinkEntity::forEnumCase(EED));

  if (auto *PL = EED->getParameterList())
    visitDefaultArguments(EED, PL);
}

void TBDGenVisitor::addFirstFileSymbols() {
  if (!Opts.ModuleLinkName.empty()) {
    SmallString<32> buf;
    addSymbol(irgen::encodeForceLoadSymbolName(buf, Opts.ModuleLinkName));
  }
}

void TBDGenVisitor::visit(Decl *D) {
  DeclStack.push_back(D);
  SWIFT_DEFER { DeclStack.pop_back(); };
  ASTVisitor::visit(D);
}

/// The kind of version being parsed, used for diagnostics.
/// Note: Must match the order in DiagnosticsFrontend.def
enum DylibVersionKind_t: unsigned {
  CurrentVersion,
  CompatibilityVersion
};

/// Converts a version string into a packed version, truncating each component
/// if necessary to fit all 3 into a 32-bit packed structure.
///
/// For example, the version '1219.37.11' will be packed as
///
///  Major (1,219)       Minor (37) Patch (11)
/// ┌───────────────────┬──────────┬──────────┐
/// │ 00001100 11000011 │ 00100101 │ 00001011 │
/// └───────────────────┴──────────┴──────────┘
///
/// If an individual component is greater than the highest number that can be
/// represented in its alloted space, it will be truncated to the maximum value
/// that fits in the alloted space, which matches the behavior of the linker.
static Optional<llvm::MachO::PackedVersion>
parsePackedVersion(DylibVersionKind_t kind, StringRef versionString,
                   ASTContext &ctx) {
  if (versionString.empty())
    return None;

  llvm::MachO::PackedVersion version;
  auto result = version.parse64(versionString);
  if (!result.first) {
    ctx.Diags.diagnose(SourceLoc(), diag::tbd_err_invalid_version,
                       (unsigned)kind, versionString);
    return None;
  }
  if (result.second) {
    ctx.Diags.diagnose(SourceLoc(), diag::tbd_warn_truncating_version,
                       (unsigned)kind, versionString);
  }
  return version;
}

static bool isApplicationExtensionSafe(const LangOptions &LangOpts) {
  // Existing linkers respect these flags to determine app extension safety.
  return LangOpts.EnableAppExtensionRestrictions ||
         llvm::sys::Process::GetEnv("LD_NO_ENCRYPT") ||
         llvm::sys::Process::GetEnv("LD_APPLICATION_EXTENSION_SAFE");
}

static bool hasLinkerDirective(Decl *D) {
  return !getAllMovedPlatformVersions(D).empty();
}

TBDFileAndSymbols
GenerateTBDRequest::evaluate(Evaluator &evaluator,
                             TBDGenDescriptor desc) const {
  auto *M = desc.getParentModule();
  auto &opts = desc.getOptions();

  auto &ctx = M->getASTContext();
  const auto &triple = ctx.LangOpts.Target;
  UniversalLinkageInfo linkInfo(triple, opts.HasMultipleIGMs,
                                /*forcePublicDecls*/ false);

  llvm::MachO::InterfaceFile file;
  file.setFileType(llvm::MachO::FileType::TBD_V4);
  file.setApplicationExtensionSafe(
    isApplicationExtensionSafe(M->getASTContext().LangOpts));
  file.setInstallName(opts.InstallName);
  file.setTwoLevelNamespace();
  file.setSwiftABIVersion(irgen::getSwiftABIVersion());
  file.setInstallAPI(opts.IsInstallAPI);

  if (auto packed = parsePackedVersion(CurrentVersion,
                                       opts.CurrentVersion, ctx)) {
    file.setCurrentVersion(*packed);
  }

  if (auto packed = parsePackedVersion(CompatibilityVersion,
                                       opts.CompatibilityVersion, ctx)) {
    file.setCompatibilityVersion(*packed);
  }

  llvm::MachO::Target target(triple);
  file.addTarget(target);
  // Add target variant
  if (ctx.LangOpts.TargetVariant.hasValue()) {
    llvm::MachO::Target targetVar(*ctx.LangOpts.TargetVariant);
    file.addTarget(targetVar);
  }
  StringSet symbols;
  auto *clang = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  TBDGenVisitor visitor(file, {target}, &symbols,
                        clang->getTargetInfo().getDataLayout(),
                        linkInfo, M, opts);

  auto visitFile = [&](FileUnit *file) {
    if (file == M->getFiles()[0]) {
      visitor.addFirstFileSymbols();
    }

    SmallVector<Decl *, 16> decls;
    file->getTopLevelDecls(decls);

    visitor.addMainIfNecessary(file);

    for (auto d : decls) {
      if (opts.LinkerDirectivesOnly && !hasLinkerDirective(d))
        continue;
      visitor.visit(d);
    }
  };

  if (auto *singleFile = desc.getSingleFile()) {
    assert(M == singleFile->getParentModule() && "mismatched file and module");
    visitFile(singleFile);
    // Visit synthesized file, if it exists.
    if (auto *SF = dyn_cast<SourceFile>(singleFile))
      if (auto *synthesizedFile = SF->getSynthesizedFile())
        visitFile(synthesizedFile);
  } else {
    llvm::SmallVector<ModuleDecl*, 4> Modules;
    Modules.push_back(M);
    for (auto Name: opts.embedSymbolsFromModules) {
      if (auto *MD = ctx.getModuleByName(Name)) {
        // If it is a clang module, the symbols should be collected by TAPI.
        if (!MD->isNonSwiftModule()) {
          Modules.push_back(MD);
          continue;
        }
      }
      // Diagnose module name that cannot be found
      ctx.Diags.diagnose(SourceLoc(), diag::unknown_swift_module_name, Name);
    }
    // Collect symbols in each module.
    llvm::for_each(Modules, [&](ModuleDecl *M) {
      for (auto *file : M->getFiles()) {
        visitFile(file);
      }
    });
  }

  return std::make_pair(std::move(file), std::move(symbols));
}

void swift::enumeratePublicSymbols(FileUnit *file, StringSet &symbols,
                                   const TBDGenOptions &opts) {
  assert(symbols.empty() && "Additive symbol enumeration not supported");
  auto &evaluator = file->getASTContext().evaluator;
  auto desc = TBDGenDescriptor::forFile(file, opts);
  symbols = llvm::cantFail(evaluator(GenerateTBDRequest{desc})).second;
}
void swift::enumeratePublicSymbols(ModuleDecl *M, StringSet &symbols,
                                   const TBDGenOptions &opts) {
  assert(symbols.empty() && "Additive symbol enumeration not supported");
  auto &evaluator = M->getASTContext().evaluator;
  auto desc = TBDGenDescriptor::forModule(M, opts);
  symbols = llvm::cantFail(evaluator(GenerateTBDRequest{desc})).second;
}
void swift::writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                         const TBDGenOptions &opts) {
  auto &evaluator = M->getASTContext().evaluator;
  auto desc = TBDGenDescriptor::forModule(M, opts);
  auto file = llvm::cantFail(evaluator(GenerateTBDRequest{desc})).first;
  llvm::cantFail(llvm::MachO::TextAPIWriter::writeToStream(os, file),
                 "YAML writing should be error-free");
}
