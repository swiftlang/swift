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

#include "swift/IRGen/TBDGen.h"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Availability.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SynthesizedFileUnit.h"
#include "swift/AST/TBDGenRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILSymbolVisitor.h"
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
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/TextAPI/InterfaceFile.h"
#include "llvm/TextAPI/Symbol.h"
#include "llvm/TextAPI/TextAPIReader.h"
#include "llvm/TextAPI/TextAPIWriter.h"

#include "APIGen.h"
#include "TBDGenVisitor.h"

using namespace swift;
using namespace swift::irgen;
using namespace swift::tbdgen;
using namespace llvm::yaml;
using StringSet = llvm::StringSet<>;
using EncodeKind = llvm::MachO::EncodeKind;
using SymbolFlags = llvm::MachO::SymbolFlags;

TBDGenVisitor::TBDGenVisitor(const TBDGenDescriptor &desc,
                             APIRecorder &recorder)
    : TBDGenVisitor(desc.getTarget(), desc.getDataLayoutString(),
                    desc.getParentModule(), desc.getOptions(), recorder) {}

void TBDGenVisitor::addSymbolInternal(StringRef name, EncodeKind kind,
                                      SymbolSource source, SymbolFlags flags) {
  if (!source.isLinkerDirective() && Opts.LinkerDirectivesOnly)
    return;

#ifndef NDEBUG
  if (kind == EncodeKind::GlobalSymbol) {
    if (!DuplicateSymbolChecker.insert(name).second) {
      llvm::dbgs() << "TBDGen duplicate symbol: " << name << '\n';
      assert(false && "TBDGen symbol appears twice");
    }
  }
#endif
  recorder.addSymbol(name, kind, source,
                     DeclStack.empty() ? nullptr : DeclStack.back(), flags);
}

static std::vector<OriginallyDefinedInAttr::ActiveVersion>
getAllMovedPlatformVersions(Decl *D) {
  std::vector<OriginallyDefinedInAttr::ActiveVersion> Results;
  for (auto *attr: D->getAttrs()) {
    if (auto *ODA = dyn_cast<OriginallyDefinedInAttr>(attr)) {
      auto Active = ODA->isActivePlatform(D->getASTContext());
      if (Active.has_value()) {
        Results.push_back(*Active);
      }
    }
  }
  return Results;
}

static StringRef getLinkerPlatformName(LinkerPlatformId Id) {
  switch (Id) {
#define LD_PLATFORM(Name, Id) case LinkerPlatformId::Name: return #Name;
#include "ldPlatformKinds.def"
  }
  llvm_unreachable("unrecognized platform id");
}

static std::optional<LinkerPlatformId> getLinkerPlatformId(StringRef Platform) {
  return llvm::StringSwitch<std::optional<LinkerPlatformId>>(Platform)
#define LD_PLATFORM(Name, Id) .Case(#Name, LinkerPlatformId::Name)
#include "ldPlatformKinds.def"
      .Default(std::nullopt);
}

StringRef InstallNameStore::getInstallName(LinkerPlatformId Id) const {
  auto It = PlatformInstallName.find(Id);
  if (It == PlatformInstallName.end())
    return InstallName;
  else
    return It->second;
}

static std::string getScalaNodeText(Node *N) {
  SmallString<32> Buffer;
  return cast<ScalarNode>(N)->getValue(Buffer).str();
}

static std::set<LinkerPlatformId> getSequenceNodePlatformList(ASTContext &Ctx,
                                                              Node *N) {
  std::set<LinkerPlatformId> Results;
  for (auto &E: *cast<SequenceNode>(N)) {
    auto Platform = getScalaNodeText(&E);
    auto Id = getLinkerPlatformId(Platform);
    if (Id.has_value()) {
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
      std::optional<std::set<LinkerPlatformId>> Platforms;
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
      if (Platforms.has_value()) {
        // This install name is platform-specific.
        for (auto Id: Platforms.value()) {
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
getLinkerPlatformId(OriginallyDefinedInAttr::ActiveVersion Ver,
                    ASTContext &Ctx) {
  auto target =
      Ver.ForTargetVariant ? Ctx.LangOpts.TargetVariant : Ctx.LangOpts.Target;
  bool isSimulator = target ? target->isSimulatorEnvironment() : false;

  switch(Ver.Platform) {
  case swift::PlatformKind::none:
    llvm_unreachable("cannot find platform kind");
  case swift::PlatformKind::OpenBSD:
    llvm_unreachable("not used for this platform");
  case swift::PlatformKind::Windows:
    llvm_unreachable("not used for this platform");
  case swift::PlatformKind::iOS:
  case swift::PlatformKind::iOSApplicationExtension:
    if (target && target->isMacCatalystEnvironment())
      return LinkerPlatformId::macCatalyst;
    return isSimulator ? LinkerPlatformId::iOS_sim : LinkerPlatformId::iOS;
  case swift::PlatformKind::tvOS:
  case swift::PlatformKind::tvOSApplicationExtension:
    return isSimulator ? LinkerPlatformId::tvOS_sim : LinkerPlatformId::tvOS;
  case swift::PlatformKind::watchOS:
  case swift::PlatformKind::watchOSApplicationExtension:
    return isSimulator ? LinkerPlatformId::watchOS_sim
                       : LinkerPlatformId::watchOS;
  case swift::PlatformKind::macOS:
  case swift::PlatformKind::macOSApplicationExtension:
    return LinkerPlatformId::macOS;
  case swift::PlatformKind::macCatalyst:
  case swift::PlatformKind::macCatalystApplicationExtension:
    return LinkerPlatformId::macCatalyst;
  case swift::PlatformKind::visionOS:
  case swift::PlatformKind::visionOSApplicationExtension:
    return isSimulator ? LinkerPlatformId::xrOS_sim:
                         LinkerPlatformId::xrOS;
  }
  llvm_unreachable("invalid platform kind");
}

static StringRef
getLinkerPlatformName(OriginallyDefinedInAttr::ActiveVersion Ver,
                      ASTContext &Ctx) {
  return getLinkerPlatformName(getLinkerPlatformId(Ver, Ctx));
}

/// Find the most relevant introducing version of the decl stack we have visited
/// so far.
static std::optional<llvm::VersionTuple>
getInnermostIntroVersion(ArrayRef<Decl *> DeclStack, PlatformKind Platform) {
  for (auto It = DeclStack.rbegin(); It != DeclStack.rend(); ++ It) {
    if (auto Result = (*It)->getIntroducedOSVersion(Platform))
      return Result;
  }
  return std::nullopt;
}

/// Using the introducing version of a symbol as the start version to redirect
/// linkage path isn't sufficient. This is because the executable can be deployed
/// to OS versions that were before the symbol was introduced. When that happens,
/// strictly using the introductory version can lead to NOT redirecting.
static llvm::VersionTuple calculateLdPreviousVersionStart(ASTContext &ctx,
                                                llvm::VersionTuple introVer) {
  auto minDep = ctx.LangOpts.getMinPlatformVersion();
  if (minDep < introVer)
    return llvm::VersionTuple(1, 0);
  return introVer;
}

void TBDGenVisitor::addLinkerDirectiveSymbolsLdPrevious(
    StringRef name, llvm::MachO::EncodeKind kind) {
  if (kind != llvm::MachO::EncodeKind::GlobalSymbol)
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
    if (!IntroVer.has_value())
      continue;
    // This decl is available after the top-level symbol has been moved here,
    // so we don't need the linker directives.
    if (*IntroVer >= Ver.Version)
      continue;
    auto PlatformNumber = getLinkerPlatformId(Ver, Ctx);
    auto It = previousInstallNameMap->find(Ver.ModuleName.str());
    if (It == previousInstallNameMap->end()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::cannot_find_install_name,
                         Ver.ModuleName, getLinkerPlatformName(Ver, Ctx));
      continue;
    }
    auto InstallName = It->second.getInstallName(PlatformNumber);
    if (InstallName.empty()) {
      Ctx.Diags.diagnose(SourceLoc(), diag::cannot_find_install_name,
                         Ver.ModuleName, getLinkerPlatformName(Ver, Ctx));
      continue;
    }
    llvm::SmallString<64> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    // Empty compatible version indicates using the current compatible version.
    StringRef ComptibleVersion = "";
    OS << "$ld$previous$";
    OS << InstallName << "$";
    OS << ComptibleVersion << "$";
    OS << std::to_string(static_cast<uint8_t>(PlatformNumber)) << "$";
    static auto getMinor = [](std::optional<unsigned> Minor) {
      return Minor.has_value() ? *Minor : 0;
    };
    auto verStart = calculateLdPreviousVersionStart(Ctx, *IntroVer);
    OS << verStart.getMajor() << "." << getMinor(verStart.getMinor()) << "$";
    OS << Ver.Version.getMajor() << "." << getMinor(Ver.Version.getMinor()) << "$";
    OS << name << "$";
    addSymbolInternal(OS.str(), EncodeKind::GlobalSymbol,
                      SymbolSource::forLinkerDirective(), SymbolFlags::Data);
  }
}

void TBDGenVisitor::addLinkerDirectiveSymbolsLdHide(
    StringRef name, llvm::MachO::EncodeKind kind) {
  if (kind != llvm::MachO::EncodeKind::GlobalSymbol)
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
  Minor[1] = MovedVer.getMinor().has_value() ? *MovedVer.getMinor(): 0;
  auto IntroVer = getInnermostIntroVersion(DeclStack, Platform);
  assert(IntroVer && "cannot find the start point of availability");
  if (!IntroVer.has_value())
    return;
  // This decl is available after the top-level symbol has been moved here,
  // so we don't need the linker directives.
  if (*IntroVer >= MovedVer)
    return;
  Major[0] = IntroVer->getMajor();
  Minor[0] = IntroVer->getMinor().has_value() ? IntroVer->getMinor().value() : 0;
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
      addSymbolInternal(OS.str(), EncodeKind::GlobalSymbol,
                        SymbolSource::forLinkerDirective(), SymbolFlags::Data);
    }
  }
}

void TBDGenVisitor::addSymbol(StringRef name, SymbolSource source,
                              SymbolFlags flags, EncodeKind kind) {
  // The linker expects to see mangled symbol names in TBD files,
  // except when being passed objective c classes,
  // so make sure to mangle before inserting the symbol.
  SmallString<32> mangled;
  if (kind == EncodeKind::ObjectiveCClass) {
    mangled = name;
  } else {
    if (!DataLayout)
      DataLayout = llvm::DataLayout(DataLayoutDescription);
    llvm::Mangler::getNameWithPrefix(mangled, name, *DataLayout);
  }

  addSymbolInternal(mangled, kind, source, flags);
  if (previousInstallNameMap) {
    addLinkerDirectiveSymbolsLdPrevious(mangled, kind);
  } else {
    addLinkerDirectiveSymbolsLdHide(mangled, kind);
  }
}

bool TBDGenVisitor::willVisitDecl(Decl *D) {
  if (!D->isAvailableDuringLowering())
    return false;

  // A @_silgen_name("...") function without a body only exists to
  // forward-declare a symbol from another library.
  if (auto AFD = dyn_cast<AbstractFunctionDecl>(D))
    if (!AFD->hasBody() && AFD->getAttrs().hasAttribute<SILGenNameAttr>())
      return false;

  DeclStack.push_back(D);
  return true;
}

void TBDGenVisitor::didVisitDecl(Decl *D) {
  assert(DeclStack.back() == D);
  DeclStack.pop_back();
}

void TBDGenVisitor::addFunction(SILDeclRef declRef) {
  addSymbol(declRef.mangle(), SymbolSource::forSILDeclRef(declRef),
            SymbolFlags::Text);
}

void TBDGenVisitor::addFunction(StringRef name, SILDeclRef declRef) {
  addSymbol(name, SymbolSource::forSILDeclRef(declRef), SymbolFlags::Text);
}

void TBDGenVisitor::addGlobalVar(VarDecl *VD) {
  Mangle::ASTMangler mangler(VD->getASTContext());
  addSymbol(mangler.mangleEntity(VD), SymbolSource::forGlobal(VD),
            SymbolFlags::Data);
}

void TBDGenVisitor::addLinkEntity(LinkEntity entity) {
  auto linkage =
      LinkInfo::get(UniversalLinkInfo, SwiftModule, entity, ForDefinition);

  SymbolFlags flags = entity.isData() ? SymbolFlags::Data : SymbolFlags::Text;
  addSymbol(linkage.getName(), SymbolSource::forIRLinkEntity(entity), flags);
}

void TBDGenVisitor::addObjCInterface(ClassDecl *CD) {
  // FIXME: We ought to have a symbol source for this.
  SmallString<128> buffer;
  addSymbol(CD->getObjCRuntimeName(buffer), SymbolSource::forUnknown(),
            SymbolFlags::Data, EncodeKind::ObjectiveCClass);
  recorder.addObjCInterface(CD);
}

void TBDGenVisitor::addObjCMethod(AbstractFunctionDecl *AFD) {
  if (auto *CD = dyn_cast<ClassDecl>(AFD->getDeclContext()))
    recorder.addObjCMethod(CD, SILDeclRef(AFD));
  else if (auto *ED = dyn_cast<ExtensionDecl>(AFD->getDeclContext()))
    recorder.addObjCMethod(ED, SILDeclRef(AFD));
}

void TBDGenVisitor::addProtocolWitnessThunk(RootProtocolConformance *C,
                                            ValueDecl *requirementDecl) {
  Mangle::ASTMangler Mangler(requirementDecl->getASTContext());

  std::string decorated = Mangler.mangleWitnessThunk(C, requirementDecl);
  // FIXME: We should have a SILDeclRef SymbolSource for this.
  addSymbol(decorated, SymbolSource::forUnknown(), SymbolFlags::Text);

  if (requirementDecl->isProtocolRequirement()) {
    ValueDecl *PWT = C->getWitness(requirementDecl).getDecl();
    if (const auto *AFD = dyn_cast<AbstractFunctionDecl>(PWT))
      if (AFD->hasAsync())
        addSymbol(decorated + "Tu", SymbolSource::forUnknown(),
                  SymbolFlags::Text);
  }
}

void TBDGenVisitor::addFirstFileSymbols() {
  if (!Opts.ModuleLinkName.empty()) {
    // FIXME: We ought to have a symbol source for this.
    SmallString<32> buf;
    addSymbol(irgen::encodeForceLoadSymbolName(buf, Opts.ModuleLinkName),
              SymbolSource::forUnknown(), SymbolFlags::Data);
  }
}

void TBDGenVisitor::visit(const TBDGenDescriptor &desc) {
  SILSymbolVisitorOptions opts;
  opts.VisitMembers = true;
  opts.LinkerDirectivesOnly = Opts.LinkerDirectivesOnly;
  opts.PublicOrPackageSymbolsOnly = Opts.PublicOrPackageSymbolsOnly;
  opts.WitnessMethodElimination = Opts.WitnessMethodElimination;
  opts.VirtualFunctionElimination = Opts.VirtualFunctionElimination;
  opts.FragileResilientProtocols = Opts.FragileResilientProtocols;

  auto silVisitorCtx = SILSymbolVisitorContext(SwiftModule, opts);
  auto visitorCtx = IRSymbolVisitorContext{UniversalLinkInfo, silVisitorCtx};

  // Add any autolinking force_load symbols.
  addFirstFileSymbols();
  
  if (auto *singleFile = desc.getSingleFile()) {
    assert(SwiftModule == singleFile->getParentModule() &&
           "mismatched file and module");
    visitFile(singleFile, visitorCtx);
    return;
  }

  llvm::SmallVector<ModuleDecl*, 4> Modules;
  Modules.push_back(SwiftModule);

  auto &ctx = SwiftModule->getASTContext();
  for (auto Name: Opts.embedSymbolsFromModules) {
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
  visitModules(Modules, visitorCtx);
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
static std::optional<llvm::MachO::PackedVersion>
parsePackedVersion(DylibVersionKind_t kind, StringRef versionString,
                   ASTContext &ctx) {
  if (versionString.empty())
    return std::nullopt;

  llvm::MachO::PackedVersion version;
  auto result = version.parse64(versionString);
  if (!result.first) {
    ctx.Diags.diagnose(SourceLoc(), diag::tbd_err_invalid_version,
                       (unsigned)kind, versionString);
    return std::nullopt;
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

TBDFile GenerateTBDRequest::evaluate(Evaluator &evaluator,
                                     TBDGenDescriptor desc) const {
  auto *M = desc.getParentModule();
  auto &opts = desc.getOptions();
  auto &ctx = M->getASTContext();

  llvm::MachO::InterfaceFile file;
  file.setFileType(llvm::MachO::FileType::TBD_V5);
  file.setApplicationExtensionSafe(isApplicationExtensionSafe(ctx.LangOpts));
  file.setInstallName(opts.InstallName);
  file.setTwoLevelNamespace();
  file.setSwiftABIVersion(irgen::getSwiftABIVersion());

  if (auto packed = parsePackedVersion(CurrentVersion,
                                       opts.CurrentVersion, ctx)) {
    file.setCurrentVersion(*packed);
  }

  if (auto packed = parsePackedVersion(CompatibilityVersion,
                                       opts.CompatibilityVersion, ctx)) {
    file.setCompatibilityVersion(*packed);
  }

  llvm::MachO::Target target(ctx.LangOpts.Target);
  file.addTarget(target);
  llvm::MachO::TargetList targets{target};
  // Add target variant
  if (ctx.LangOpts.TargetVariant.has_value()) {
    llvm::MachO::Target targetVar(*ctx.LangOpts.TargetVariant);
    file.addTarget(targetVar);
    targets.push_back(targetVar);
  }

  auto addSymbol = [&](StringRef symbol, EncodeKind kind, SymbolSource source,
                       Decl *decl, SymbolFlags flags) {
    file.addSymbol(kind, symbol, targets, flags);
  };
  SimpleAPIRecorder recorder(addSymbol);
  TBDGenVisitor visitor(desc, recorder);
  visitor.visit(desc);
  return file;
}

std::vector<std::string>
PublicSymbolsRequest::evaluate(Evaluator &evaluator,
                               TBDGenDescriptor desc) const {
  std::vector<std::string> symbols;
  auto addSymbol = [&](StringRef symbol, EncodeKind kind, SymbolSource source,
                       Decl *decl, SymbolFlags flags) {
    if (kind == EncodeKind::GlobalSymbol)
      symbols.push_back(symbol.str());
    // TextAPI ObjC Class Kinds represents two symbols.
    else if (kind == EncodeKind::ObjectiveCClass) {
      symbols.push_back((llvm::MachO::ObjC2ClassNamePrefix + symbol).str());
      symbols.push_back((llvm::MachO::ObjC2MetaClassNamePrefix + symbol).str());
    }
  };
  SimpleAPIRecorder recorder(addSymbol);
  TBDGenVisitor visitor(desc, recorder);
  visitor.visit(desc);
  return symbols;
}

std::vector<std::string> swift::getPublicSymbols(TBDGenDescriptor desc) {
  auto &evaluator = desc.getParentModule()->getASTContext().evaluator;
  return evaluateOrFatal(evaluator, PublicSymbolsRequest{desc});
}
void swift::writeTBDFile(ModuleDecl *M, llvm::raw_ostream &os,
                         const TBDGenOptions &opts) {
  auto &evaluator = M->getASTContext().evaluator;
  auto desc = TBDGenDescriptor::forModule(M, opts);
  auto file = evaluateOrFatal(evaluator, GenerateTBDRequest{desc});
  llvm::cantFail(llvm::MachO::TextAPIWriter::writeToStream(os, file),
                 "TBD writing should be error-free");
}

class APIGenRecorder final : public APIRecorder {
  static bool isSPI(const Decl *decl) {
    assert(decl);
    return decl->isSPI() || decl->isAvailableAsSPI();
  }

public:
  APIGenRecorder(apigen::API &api, ModuleDecl *module)
      : api(api), module(module) {
    // If we're working with a serialized module, make the default location
    // for symbols the path to the binary module.
    if (FileUnit *MainFile = module->getFiles().front()) {
      if (MainFile->getKind() == FileUnitKind::SerializedAST)
        defaultLoc =
            apigen::APILoc(MainFile->getModuleDefiningPath().str(), 0, 0);
    }
  }
  ~APIGenRecorder() {}

  void addSymbol(StringRef symbol, EncodeKind kind, SymbolSource source,
                 Decl *decl, SymbolFlags flags) override {
    if (kind != EncodeKind::GlobalSymbol)
      return;

    apigen::APIAvailability availability;
    auto access = apigen::APIAccess::Public;
    if (decl) {
      availability = getAvailability(decl);
      if (isSPI(decl))
        access = apigen::APIAccess::Private;
    }

    api.addSymbol(symbol, getAPILocForDecl(decl), apigen::APILinkage::Exported,
                  apigen::APIFlags::None, access, availability);
  }

  void addObjCInterface(const ClassDecl *decl) override {
    addOrGetObjCInterface(decl);
  }

  void addObjCCategory(const ExtensionDecl *decl) override {
    addOrGetObjCCategory(decl);
  }

  void addObjCMethod(const GenericContext *ctx, SILDeclRef method) override {
    SmallString<128> buffer;
    StringRef name = getSelectorName(method, buffer);
    apigen::APIAvailability availability;
    bool isInstanceMethod = true;
    auto access = apigen::APIAccess::Public;
    auto decl = method.hasDecl() ? method.getDecl() : nullptr;
    if (decl) {
      availability = getAvailability(decl);
      if (decl->getDescriptiveKind() == DescriptiveDeclKind::ClassMethod)
        isInstanceMethod = false;
      if (isSPI(decl))
        access = apigen::APIAccess::Private;
    }

    apigen::ObjCContainerRecord *record = nullptr;
    if (auto *cls = dyn_cast<ClassDecl>(ctx))
      record = addOrGetObjCInterface(cls);
    else if (auto *ext = dyn_cast<ExtensionDecl>(ctx))
      record = addOrGetObjCCategory(ext);

    if (record)
      api.addObjCMethod(record, name, getAPILocForDecl(decl), access,
                        isInstanceMethod, false, availability);
  }

private:
  apigen::APILoc getAPILocForDecl(const Decl *decl) {
    if (!decl)
      return defaultLoc;

    SourceLoc loc = decl->getLoc();
    if (!loc.isValid())
      return defaultLoc;

    auto &SM = decl->getASTContext().SourceMgr;
    unsigned line, col;
    std::tie(line, col) = SM.getPresumedLineAndColumnForLoc(loc);
    auto displayName = SM.getDisplayNameForLoc(loc);

    return apigen::APILoc(std::string(displayName), line, col);
  }

  /// Follow the naming schema that IRGen uses for Categories (see
  /// ClassDataBuilder).
  using CategoryNameKey = std::pair<const ClassDecl *, const ModuleDecl *>;
  llvm::DenseMap<CategoryNameKey, unsigned> CategoryCounts;

  apigen::APIAvailability getAvailability(const Decl *decl) {
    std::optional<bool> unavailable;
    std::string introduced, obsoleted;
    bool hasFallbackUnavailability = false;
    auto platform = targetPlatform(module->getASTContext().LangOpts);
    for (auto *attr : decl->getAttrs()) {
      if (auto *ava = dyn_cast<AvailableAttr>(attr)) {
        if (ava->getPlatform() == PlatformKind::none) {
          hasFallbackUnavailability = ava->isUnconditionallyUnavailable();
          continue;
        }
        if (ava->getPlatform() != platform)
          continue;
        unavailable = ava->isUnconditionallyUnavailable();
        if (ava->Introduced)
          introduced = ava->Introduced->getAsString();
        if (ava->Obsoleted)
          obsoleted = ava->Obsoleted->getAsString();
      }
    }
    return {introduced, obsoleted,
            unavailable.value_or(hasFallbackUnavailability)};
  }

  StringRef getSelectorName(SILDeclRef method, SmallString<128> &buffer) {
    auto methodOrCtorOrDtor = method.getDecl();
    if (methodOrCtorOrDtor) {
      if (auto *method = dyn_cast<FuncDecl>(methodOrCtorOrDtor))
        return method->getObjCSelector().getString(buffer);
      else if (auto *ctor = dyn_cast<ConstructorDecl>(methodOrCtorOrDtor))
        return ctor->getObjCSelector().getString(buffer);
      else if (isa<DestructorDecl>(methodOrCtorOrDtor))
        return "dealloc";
    }
    llvm_unreachable("cannot get selector name from decl");
  }

  apigen::ObjCInterfaceRecord *addOrGetObjCInterface(const ClassDecl *decl) {
    auto entry = classMap.find(decl);
    if (entry != classMap.end())
      return entry->second;

    SmallString<128> nameBuffer;
    auto name = decl->getObjCRuntimeName(nameBuffer);
    StringRef superCls;
    SmallString<128> buffer;
    if (auto *super = decl->getSuperclassDecl())
      superCls = super->getObjCRuntimeName(buffer);
    apigen::APIAvailability availability = getAvailability(decl);
    apigen::APIAccess access =
        isSPI(decl) ? apigen::APIAccess::Private : apigen::APIAccess::Public;
    apigen::APILinkage linkage =
        decl->getFormalAccess() == AccessLevel::Public && decl->isObjC()
            ? apigen::APILinkage::Exported
            : apigen::APILinkage::Internal;
    auto cls = api.addObjCClass(name, linkage, getAPILocForDecl(decl), access,
                                availability, superCls);
    classMap.try_emplace(decl, cls);
    return cls;
  }

  void buildCategoryName(const ExtensionDecl *ext, const ClassDecl *cls,
                         SmallVectorImpl<char> &s) {
    llvm::raw_svector_ostream os(s);
    if (!ext->getObjCCategoryName().empty()) {
      os << ext->getObjCCategoryName();
      return;
    }
    ModuleDecl *module = ext->getParentModule();
    os << module->getName();
    unsigned categoryCount = CategoryCounts[{cls, module}]++;
    if (categoryCount > 0)
      os << categoryCount;
  }

  apigen::ObjCCategoryRecord *addOrGetObjCCategory(const ExtensionDecl *decl) {
    auto entry = categoryMap.find(decl);
    if (entry != categoryMap.end())
      return entry->second;

    SmallString<128> interfaceBuffer;
    SmallString<128> nameBuffer;
    ClassDecl *cls = decl->getSelfClassDecl();
    auto interface = cls->getObjCRuntimeName(interfaceBuffer);
    buildCategoryName(decl, cls, nameBuffer);
    apigen::APIAvailability availability = getAvailability(decl);
    apigen::APIAccess access =
        isSPI(decl) ? apigen::APIAccess::Private : apigen::APIAccess::Public;
    apigen::APILinkage linkage =
        decl->getMaxAccessLevel() == AccessLevel::Public
            ? apigen::APILinkage::Exported
            : apigen::APILinkage::Internal;
    auto category =
        api.addObjCCategory(nameBuffer, linkage, getAPILocForDecl(decl), access,
                            availability, interface);
    categoryMap.try_emplace(decl, category);
    return category;
  }

  apigen::API &api;
  ModuleDecl *module;
  apigen::APILoc defaultLoc;

  llvm::DenseMap<const ClassDecl*, apigen::ObjCInterfaceRecord*> classMap;
  llvm::DenseMap<const ExtensionDecl *, apigen::ObjCCategoryRecord *>
      categoryMap;
};

apigen::API APIGenRequest::evaluate(Evaluator &evaluator,
                                    TBDGenDescriptor desc) const {
  auto *M = desc.getParentModule();
  apigen::API api(M->getASTContext().LangOpts.Target);
  APIGenRecorder recorder(api, M);

  TBDGenVisitor visitor(desc, recorder);
  visitor.visit(desc);

  return api;
}

void swift::writeAPIJSONFile(ModuleDecl *M, llvm::raw_ostream &os,
                             bool PrettyPrint) {
  TBDGenOptions opts;
  auto &evaluator = M->getASTContext().evaluator;
  auto desc = TBDGenDescriptor::forModule(M, opts);
  auto api = evaluateOrFatal(evaluator, APIGenRequest{desc});
  api.writeAPIJSONFile(os, PrettyPrint);
}

/// NOTE: This is part of an incomplete experimental feature. There are
/// currently no clients that depend on its output.
const SymbolSourceMap *
SymbolSourceMapRequest::evaluate(Evaluator &evaluator,
                                 TBDGenDescriptor desc) const {

  // FIXME: Once the evaluator supports returning a reference to a cached value
  // in storage, this won't be necessary.
  auto &Ctx = desc.getParentModule()->getASTContext();
  auto *SymbolSources = Ctx.Allocate<SymbolSourceMap>();

  auto addSymbol = [=](StringRef symbol, EncodeKind kind, SymbolSource source,
                       Decl *decl, SymbolFlags flags) {
    SymbolSources->insert({symbol, source});
  };

  SimpleAPIRecorder recorder(addSymbol);
  TBDGenVisitor visitor(desc, recorder);
  visitor.visit(desc);

  Ctx.addCleanup([SymbolSources]() { SymbolSources->~SymbolSourceMap(); });

  return SymbolSources;
}
