//===-------- FrontendSourceFileDepGraphFactory.cpp -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file holds the code to build a SourceFileDepGraph in the frontend.
// This graph captures relationships between definitions and uses, and
// it is written to a file which is read by the driver in order to decide which
// source files require recompilation.

#include "swift/AST/FrontendSourceFileDepGraphFactory.h"

// may not all be needed
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/AbstractSourceFileDepGraphFactory.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/FineGrainedDependencyFormat.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Types.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace fine_grained_dependencies;

//==============================================================================
// MARK: Constructing from a SourceFile
//==============================================================================

//==============================================================================
// MARK: Helpers for key construction that must be in frontend
//==============================================================================

template <typename DeclT> static std::string getBaseName(const DeclT *decl) {
  return decl->getBaseName().userFacingName().str();
}

static std::string mangleTypeAsContext(const NominalTypeDecl *NTD) {
  Mangle::ASTMangler Mangler;
  return !NTD ? "" : Mangler.mangleTypeAsContextUSR(NTD);
}

//==============================================================================
// MARK: Privacy queries
//==============================================================================

/// Return true if \ref ED does not contain a member that can affect other
/// files.
static bool allMembersArePrivate(const ExtensionDecl *ED) {
  return std::all_of(
      ED->getMembers().begin(), ED->getMembers().end(),
      [](const Decl *d) { return d->isPrivateToEnclosingFile(); });
}

/// \ref inheritedType, an inherited protocol, return true if this inheritance
/// cannot affect other files.
static bool extendedTypeIsPrivate(TypeLoc inheritedType) {
  auto type = inheritedType.getType();
  if (!type)
    return true;

  if (!type->isExistentialType()) {
    // Be conservative. We don't know how to deal with other extended types.
    return false;
  }

  auto layout = type->getExistentialLayout();
  assert(!layout.explicitSuperclass &&
         "Should not have a subclass existential "
         "in the inheritance clause of an extension");
  for (auto protoTy : layout.getProtocols()) {
    if (!protoTy->getDecl()->isPrivateToEnclosingFile())
      return false;
  }

  return true;
}

/// Return true if \ref ED does not inherit a protocol that can affect other
/// files. Was called "justMembers" in ReferenceDependencies.cpp
/// \ref ED might be null.
static bool allInheritedProtocolsArePrivate(const ExtensionDecl *ED) {
  return std::all_of(ED->getInherited().begin(), ED->getInherited().end(),
                     extendedTypeIsPrivate);
}

//==============================================================================
// MARK: DependencyKey - creation for Decls
//==============================================================================

template <NodeKind kindArg, typename Entity>
DependencyKey DependencyKey::createForProvidedEntityInterface(Entity entity) {
  return DependencyKey(
      kindArg, DeclAspect::interface,
      DependencyKey::computeContextForProvidedEntity<kindArg>(entity),
      DependencyKey::computeNameForProvidedEntity<kindArg>(entity));
}

//==============================================================================
// MARK: computeContextForProvidedEntity
//==============================================================================

template <NodeKind kind, typename Entity>
std::string DependencyKey::computeContextForProvidedEntity(Entity) {
  // Context field is not used for most kinds
  return "";
}

// \ref nominal dependencies are created from a Decl and use the context field.
template <>
std::string DependencyKey::computeContextForProvidedEntity<
    NodeKind::nominal, NominalTypeDecl const *>(NominalTypeDecl const *D) {
  return mangleTypeAsContext(D);
}

/// \ref potentialMember dependencies are created from a Decl and use the
/// context field.
template <>
std::string
DependencyKey::computeContextForProvidedEntity<NodeKind::potentialMember,
                                               NominalTypeDecl const *>(
    const NominalTypeDecl *D) {
  return mangleTypeAsContext(D);
}

template <>
std::string DependencyKey::computeContextForProvidedEntity<
    NodeKind::member, const NominalTypeDecl *>(const NominalTypeDecl *holder) {
  return mangleTypeAsContext(holder);
}

/// \ref member dependencies are created from a pair and use the context field.
template <>
std::string DependencyKey::computeContextForProvidedEntity<
    NodeKind::member, std::pair<const NominalTypeDecl *, const ValueDecl *>>(
    std::pair<const NominalTypeDecl *, const ValueDecl *> holderAndMember) {
  return computeContextForProvidedEntity<NodeKind::member>(
      holderAndMember.first);
}

// Linux compiler requires the following:
template std::string
    DependencyKey::computeContextForProvidedEntity<NodeKind::sourceFileProvide,
                                                   StringRef>(StringRef);

//==============================================================================
// MARK: computeNameForProvidedEntity
//==============================================================================

template <>
std::string
DependencyKey::computeNameForProvidedEntity<NodeKind::sourceFileProvide,
                                            StringRef>(StringRef swiftDeps) {
  assert(!swiftDeps.empty());
  return swiftDeps.str();
}

template <>
std::string
DependencyKey::computeNameForProvidedEntity<NodeKind::topLevel,
                                            PrecedenceGroupDecl const *>(
    const PrecedenceGroupDecl *D) {
  return D->getName().str().str();
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, FuncDecl const *>(const FuncDecl *D) {
  return getBaseName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, OperatorDecl const *>(const OperatorDecl *D) {
  return D->getName().str().str();
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return D->getName().str().str();
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::topLevel, ValueDecl const *>(const ValueDecl *D) {
  return getBaseName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::dynamicLookup, ValueDecl const *>(const ValueDecl *D) {
  return getBaseName(D);
}
template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::nominal, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return "";
}
template <>
std::string
DependencyKey::computeNameForProvidedEntity<NodeKind::potentialMember,
                                            NominalTypeDecl const *>(
    const NominalTypeDecl *D) {
  return "";
}

template <>
std::string DependencyKey::computeNameForProvidedEntity<
    NodeKind::member, std::pair<const NominalTypeDecl *, const ValueDecl *>>(
    std::pair<const NominalTypeDecl *, const ValueDecl *> holderAndMember) {
  return getBaseName(holderAndMember.second);
}

//==============================================================================
// MARK: createDependedUponKey
//==============================================================================

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::topLevel>(StringRef name) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       name.str());
}

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::dynamicLookup>(StringRef name) {
  return DependencyKey(NodeKind::dynamicLookup, DeclAspect::interface, "",
                       name.str());
}

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::externalDepend>(StringRef name) {
  return DependencyKey(NodeKind::externalDepend, DeclAspect::interface, "",
                       name.str());
}

template <>
DependencyKey
DependencyKey::createDependedUponKey<NodeKind::nominal>(StringRef mangledName) {
  return DependencyKey(NodeKind::nominal, DeclAspect::interface,
                       mangledName.str(), "");
}

DependencyKey DependencyKey::createDependedUponKey(StringRef mangledHolderName,
                                                   StringRef memberBaseName) {
  const bool isMemberBlank = memberBaseName.empty();
  const auto kind =
      isMemberBlank ? NodeKind::potentialMember : NodeKind::member;
  return DependencyKey(kind, DeclAspect::interface, mangledHolderName.str(),
                       isMemberBlank ? "" : memberBaseName.str());
}

//==============================================================================
// MARK: Entry point into frontend graph construction
//==============================================================================

bool fine_grained_dependencies::emitReferenceDependencies(
    DiagnosticEngine &diags, SourceFile *const SF,
    const DependencyTracker &depTracker,
    StringRef outputPath,
    const bool alsoEmitDotFile) {

  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");

  SourceFileDepGraph g = FrontendSourceFileDepGraphFactory(
                             SF, outputPath, depTracker, alsoEmitDotFile)
                              .construct();

  bool hadError = writeFineGrainedDependencyGraph(diags, outputPath, g);

  // If path is stdout, cannot read it back, so check for "-"
  assert(outputPath == "-" || g.verifyReadsWhatIsWritten(outputPath));

  if (alsoEmitDotFile)
    g.emitDotFile(outputPath, diags);

  return hadError;
}

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory
//==============================================================================

FrontendSourceFileDepGraphFactory::FrontendSourceFileDepGraphFactory(
    SourceFile *SF, StringRef outputPath, const DependencyTracker &depTracker,
    const bool alsoEmitDotFile)
    : AbstractSourceFileDepGraphFactory(
          computeIncludePrivateDeps(SF), SF->getASTContext().hadError(),
          outputPath, getInterfaceHash(SF), alsoEmitDotFile,
          SF->getASTContext().Diags),
      SF(SF), depTracker(depTracker) {}

bool FrontendSourceFileDepGraphFactory::computeIncludePrivateDeps(
    SourceFile *SF) {
  // Since, when fingerprints are enabled,
  // the parser diverts token hashing into per-body fingerprints
  // before it can know if a difference is in a private type,
  // in order to be able to test the changed  fingerprints
  // we force the inclusion of private declarations when fingerprints
  // are enabled.
  return SF->getASTContext()
             .LangOpts.FineGrainedDependenciesIncludeIntrafileOnes ||
         SF->getASTContext().LangOpts.EnableTypeFingerprints;
}

/// Centralize the invariant that the fingerprint of the whole file is the
/// interface hash
std::string FrontendSourceFileDepGraphFactory::getFingerprint(SourceFile *SF) {
  return getInterfaceHash(SF);
}

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory - adding collections of defined Decls
//==============================================================================
//==============================================================================
// MARK: SourceFileDeclFinder
//==============================================================================

namespace {
/// Takes all the Decls in a SourceFile, and collects them into buckets by
/// groups of DeclKinds. Also casts them to more specific types
/// TODO: Factor with SourceFileDeclFinder
struct SourceFileDeclFinder {

public:
  /// Existing system excludes private decls in some cases.
  /// In the future, we might not want to do this, so use bool to decide.
  const bool includePrivateDecls;

  // The extracted Decls:
  ConstPtrVec<ExtensionDecl> extensions;
  ConstPtrVec<OperatorDecl> operators;
  ConstPtrVec<PrecedenceGroupDecl> precedenceGroups;
  ConstPtrVec<NominalTypeDecl> topNominals;
  ConstPtrVec<ValueDecl> topValues;
  ConstPtrVec<NominalTypeDecl> allNominals;
  ConstPtrVec<NominalTypeDecl> potentialMemberHolders;
  ConstPtrVec<FuncDecl> memberOperatorDecls;
  ConstPtrPairVec<NominalTypeDecl, ValueDecl> valuesInExtensions;
  ConstPtrVec<ValueDecl> classMembers;

  /// Construct me and separates the Decls.
  // clang-format off
    SourceFileDeclFinder(const SourceFile *const SF, const bool includePrivateDecls)
    : includePrivateDecls(includePrivateDecls) {
      for (const Decl *const D : SF->getTopLevelDecls()) {
        select<ExtensionDecl, DeclKind::Extension>(D, extensions, false) ||
        select<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator,
        DeclKind::PostfixOperator>(D, operators, false) ||
        select<PrecedenceGroupDecl, DeclKind::PrecedenceGroup>(
                                                               D, precedenceGroups, false) ||
        select<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct,
        DeclKind::Class, DeclKind::Protocol>(D, topNominals, true) ||
        select<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func,
        DeclKind::Accessor>(D, topValues, true);
      }
    // clang-format on
    // The order is important because some of these use instance variables
    // computed by others.
    findNominalsFromExtensions();
    findNominalsInTopNominals();
    findValuesInExtensions();
    findClassMembers(SF);
  }

private:
  /// Extensions may contain nominals and operators.
  void findNominalsFromExtensions() {
    for (auto *ED : extensions) {
      const auto *const NTD = ED->getExtendedNominal();
      if (NTD)
        findNominalsAndOperatorsIn(NTD, ED);
    }
  }
  /// Top-level nominals may contain nominals and operators.
  void findNominalsInTopNominals() {
    for (const auto *const NTD : topNominals)
      findNominalsAndOperatorsIn(NTD);
  }
  /// Any nominal may contain nominals and operators.
  /// (indirectly recursive)
  void findNominalsAndOperatorsIn(const NominalTypeDecl *const NTD,
                                  const ExtensionDecl *ED = nullptr) {
    if (excludeIfPrivate(NTD))
      return;
    const bool exposedProtocolIsExtended =
        ED && !allInheritedProtocolsArePrivate(ED);
    if (ED && !includePrivateDecls && !exposedProtocolIsExtended &&
        std::all_of(
            ED->getMembers().begin(), ED->getMembers().end(),
            [&](const Decl *D) { return D->isPrivateToEnclosingFile(); })) {
      return;
    }
    if (includePrivateDecls || !ED || exposedProtocolIsExtended)
      allNominals.push_back(NTD);
    potentialMemberHolders.push_back(NTD);
    findNominalsAndOperatorsInMembers(ED ? ED->getMembers()
                                         : NTD->getMembers());
  }

  /// Search through the members to find nominals and operators.
  /// (indirectly recursive)
  /// TODO: clean this up, maybe recurse separately for each purpose.
  void findNominalsAndOperatorsInMembers(const DeclRange members) {
    for (const Decl *const D : members) {
      auto *VD = dyn_cast<ValueDecl>(D);
      if (!VD || excludeIfPrivate(VD))
        continue;
      if (VD->getName().isOperator())
        memberOperatorDecls.push_back(cast<FuncDecl>(D));
      else if (const auto *const NTD = dyn_cast<NominalTypeDecl>(D))
        findNominalsAndOperatorsIn(NTD);
    }
  }

  /// Extensions may contain ValueDecls.
  void findValuesInExtensions() {
    for (const auto *ED : extensions) {
      const auto *const NTD = ED->getExtendedNominal();
      if (!NTD || excludeIfPrivate(NTD))
        continue;
      if (!includePrivateDecls &&
          (!allInheritedProtocolsArePrivate(ED) || allMembersArePrivate(ED)))
        continue;
      for (const auto *member : ED->getMembers())
        if (const auto *VD = dyn_cast<ValueDecl>(member))
          if (VD->hasName() &&
              (includePrivateDecls || !VD->isPrivateToEnclosingFile())) {
            const auto *const NTD = ED->getExtendedNominal();
            if (NTD)
              valuesInExtensions.push_back(std::make_pair(NTD, VD));
          }
    }
  }

  /// Class members are needed for dynamic lookup dependency nodes.
  void findClassMembers(const SourceFile *const SF) {
    struct Collector : public VisibleDeclConsumer {
      ConstPtrVec<ValueDecl> &classMembers;
      Collector(ConstPtrVec<ValueDecl> &classMembers)
          : classMembers(classMembers) {}
      void foundDecl(ValueDecl *VD, DeclVisibilityKind,
                     DynamicLookupInfo) override {
        classMembers.push_back(VD);
      }
    } collector{classMembers};
    SF->lookupClassMembers({}, collector);
  }

  /// Check \p D to see if it is one of the DeclKinds in the template
  /// arguments. If so, cast it to DesiredDeclType and add it to foundDecls.
  /// \returns true if successful.
  template <typename DesiredDeclType, DeclKind firstKind,
            DeclKind... restOfKinds>
  bool select(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls,
              const bool canExcludePrivateDecls) {
    if (D->getKind() == firstKind) {
      auto *dd = cast<DesiredDeclType>(D);
      const bool exclude = canExcludePrivateDecls && excludeIfPrivate(dd);
      if (!exclude)
        foundDecls.push_back(cast<DesiredDeclType>(D));
      return true;
    }
    return select<DesiredDeclType, restOfKinds...>(D, foundDecls,
                                                   canExcludePrivateDecls);
  }

  /// Terminate the template recursion.
  template <typename DesiredDeclType>
  bool select(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls,
              bool) {
    return false;
  }

  /// Return true if \param D should be excluded on privacy grounds.
  bool excludeIfPrivate(const Decl *const D) {
    return !includePrivateDecls && D->isPrivateToEnclosingFile();
  }
};
} // namespace

void FrontendSourceFileDepGraphFactory::addAllDefinedDecls() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates

  // Many kinds of Decls become top-level depends.

  SourceFileDeclFinder declFinder(SF, includePrivateDeps);

  addAllDefinedDeclsOfAGivenType<NodeKind::topLevel>(
      declFinder.precedenceGroups);
  addAllDefinedDeclsOfAGivenType<NodeKind::topLevel>(
      declFinder.memberOperatorDecls);
  addAllDefinedDeclsOfAGivenType<NodeKind::topLevel>(declFinder.operators);
  addAllDefinedDeclsOfAGivenType<NodeKind::topLevel>(declFinder.topNominals);
  addAllDefinedDeclsOfAGivenType<NodeKind::topLevel>(declFinder.topValues);
  addAllDefinedDeclsOfAGivenType<NodeKind::nominal>(declFinder.allNominals);
  addAllDefinedDeclsOfAGivenType<NodeKind::potentialMember>(
      declFinder.potentialMemberHolders);
  addAllDefinedDeclsOfAGivenType<NodeKind::member>(
      declFinder.valuesInExtensions);
  addAllDefinedDeclsOfAGivenType<NodeKind::dynamicLookup>(
      declFinder.classMembers);
}

/// Given an array of Decls or pairs of them in \p declsOrPairs
/// create node pairs for context and name
template <NodeKind kind, typename ContentsT>
void FrontendSourceFileDepGraphFactory::addAllDefinedDeclsOfAGivenType(
    std::vector<ContentsT> &contentsVec) {
  for (const auto &declOrPair : contentsVec) {
    Optional<std::string> fp = getFingerprintIfAny(declOrPair);
    addADefinedDecl(
        DependencyKey::createForProvidedEntityInterface<kind>(declOrPair),
        fp ? StringRef(fp.getValue()) : Optional<StringRef>());
  }
}

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory - adding collections of used Decls
//==============================================================================

namespace {
/// Extracts uses out of a SourceFile
class UsedDeclEnumerator {
  SourceFile *SF;
  const DependencyTracker &depTracker;
  StringRef swiftDeps;

  /// Cache these for efficiency
  const DependencyKey sourceFileInterface;
  const DependencyKey sourceFileImplementation;

  const bool includeIntrafileDeps;

  function_ref<void(const DependencyKey &, const DependencyKey &)> createDefUse;

public:
  UsedDeclEnumerator(
      SourceFile *SF, const DependencyTracker &depTracker, StringRef swiftDeps,
      bool includeIntrafileDeps,
      function_ref<void(const DependencyKey &, const DependencyKey &)>
          createDefUse)
      : SF(SF), depTracker(depTracker), swiftDeps(swiftDeps),
        sourceFileInterface(DependencyKey::createKeyForWholeSourceFile(
            DeclAspect::interface, swiftDeps)),
        sourceFileImplementation(DependencyKey::createKeyForWholeSourceFile(
            DeclAspect::implementation, swiftDeps)),
        includeIntrafileDeps(includeIntrafileDeps), createDefUse(createDefUse) {
  }

public:
  void enumerateAllUses() {
    auto &Ctx = SF->getASTContext();
    std::unordered_set<std::string> holdersOfCascadingMembers;
    Ctx.evaluator.enumerateReferencesInFile(SF, [&](const auto &ref) {
      const auto cascades = ref.cascades;
      std::string name = ref.name.userFacingName().str();
      const auto *nominal = ref.subject;
      using Kind = evaluator::DependencyCollector::Reference::Kind;

      switch (ref.kind) {
      case Kind::Empty:
      case Kind::Tombstone:
        llvm_unreachable("Cannot enumerate dead reference!");
      case Kind::TopLevel:
        return enumerateUse<NodeKind::topLevel>("", name, cascades);
      case Kind::Dynamic:
        return enumerateUse<NodeKind::dynamicLookup>("", name, cascades);
      case Kind::PotentialMember: {
        std::string context = DependencyKey::computeContextForProvidedEntity<
            NodeKind::potentialMember>(nominal);
        appendHolderOfCascadingMembers(holdersOfCascadingMembers, nominal,
                                       cascades);
        return enumerateUse<NodeKind::potentialMember>(context, "", cascades);
      }
      case Kind::UsedMember: {
        std::string context =
            DependencyKey::computeContextForProvidedEntity<NodeKind::member>(
                nominal);
        appendHolderOfCascadingMembers(holdersOfCascadingMembers, nominal,
                                       cascades);
        return enumerateUse<NodeKind::member>(context, name, cascades);
      }
      }
    });
    enumerateExternalUses();
    enumerateNominalUses(std::move(holdersOfCascadingMembers));
  }

private:
  template <NodeKind kind>
  void enumerateUse(StringRef context, StringRef name, bool isCascadingUse) {
    // Assume that what is depended-upon is the interface
    createDefUse(
        DependencyKey(kind, DeclAspect::interface, context.str(), name.str()),
        isCascadingUse ? sourceFileInterface : sourceFileImplementation);
  }

  void appendHolderOfCascadingMembers(std::unordered_set<std::string> &holders,
                                      const NominalTypeDecl *subject,
                                      bool isCascading) {
    bool isPrivate = subject->isPrivateToEnclosingFile();
    if (isPrivate && !includeIntrafileDeps)
      return;

    std::string context =
        DependencyKey::computeContextForProvidedEntity<NodeKind::nominal>(
            subject);
    if (isCascading) {
      holders.insert(context);
    }
  }

  void enumerateNominalUses(
      const std::unordered_set<std::string> &&holdersOfCascadingMembers) {
    auto &Ctx = SF->getASTContext();
    Ctx.evaluator.enumerateReferencesInFile(SF, [&](const auto &ref) {
      const NominalTypeDecl *subject = ref.subject;
      if (!subject) {
        return;
      }

      bool isPrivate = subject->isPrivateToEnclosingFile();
      if (isPrivate && !includeIntrafileDeps) {
        return;
      }

      std::string context =
          DependencyKey::computeContextForProvidedEntity<NodeKind::nominal>(
              subject);
      const bool isCascadingUse = holdersOfCascadingMembers.count(context) != 0;
      enumerateUse<NodeKind::nominal>(context, "", isCascadingUse);
    });
  }

  void enumerateExternalUses() {
    // external dependencies always cascade
    for (StringRef s : depTracker.getDependencies())
      enumerateUse<NodeKind::externalDepend>("", s, true);
  }
};
} // end namespace

void FrontendSourceFileDepGraphFactory::addAllUsedDecls() {
  UsedDeclEnumerator(SF, depTracker, swiftDeps, includePrivateDeps,
                     [&](const DependencyKey &def, const DependencyKey &use) {
                       addAUsedDecl(def, use);
                     })
    .enumerateAllUses();
}

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory - adding individual defined Decls
//==============================================================================

std::string
FrontendSourceFileDepGraphFactory::getInterfaceHash(SourceFile *SF) {
  llvm::SmallString<32> interfaceHash;
  SF->getInterfaceHash(interfaceHash);
  return interfaceHash.str().str();
}

/// At present, only \c NominalTypeDecls have (body) fingerprints
Optional<std::string> FrontendSourceFileDepGraphFactory::getFingerprintIfAny(
    std::pair<const NominalTypeDecl *, const ValueDecl *>) {
  return None;
}
Optional<std::string>
FrontendSourceFileDepGraphFactory::getFingerprintIfAny(const Decl *d) {
  if (const auto *idc = dyn_cast<IterableDeclContext>(d)) {
    auto result = idc->getBodyFingerprint();
    assert((!result || !result->empty()) &&
           "Fingerprint should never be empty");
    return result;
  }
  return None;
}
