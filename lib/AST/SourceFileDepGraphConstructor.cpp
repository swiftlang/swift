//===--- FineGrainedDependenciesSourceFileDepGraphConstructor.cpp ---------===//
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

#include <stdio.h>

// may not all be needed
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/FineGrainedDependencies.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SourceFileDepGraphConstructor.h"
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

// This file holds the code to build a SourceFileDepGraph in the frontend.
// This graph captures relationships between definitions and uses, and
// it is written to a file which is read by the driver in order to decide which
// source files require recompilation.

using namespace swift;
using namespace fine_grained_dependencies;

//==============================================================================
// MARK: Helpers for key construction that must be in frontend
//==============================================================================

template <typename DeclT> static std::string getBaseName(const DeclT *decl) {
  return decl->getBaseName().userFacingName();
}

template <typename DeclT> static std::string getName(const DeclT *decl) {
  return DeclBaseName(decl->getName()).userFacingName();
}

static std::string mangleTypeAsContext(const NominalTypeDecl *NTD) {
  Mangle::ASTMangler Mangler;
  return !NTD ? "" : Mangler.mangleTypeAsContextUSR(NTD);
}

//==============================================================================
// MARK: Privacy queries
//==============================================================================

static bool declIsPrivate(const ValueDecl *VD) {
  return VD->getFormalAccess() <= AccessLevel::FilePrivate;
}

/// Return true if \param D cannot affect other files.
static bool declIsPrivate(const Decl *D) {
  if (auto *VD = dyn_cast<ValueDecl>(D))
    return declIsPrivate(VD);
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    return true;

  case DeclKind::Extension:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    return false;

  default:
    llvm_unreachable("everything else is a ValueDecl");
  }
}

/// Return true if \ref ED does not contain a member that can affect other
/// files.
static bool allMembersArePrivate(const ExtensionDecl *ED) {
  return std::all_of(ED->getMembers().begin(), ED->getMembers().end(),
                     [](const Decl *d) { return declIsPrivate(d); });
  //                     declIsPrivate);
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
    if (!declIsPrivate(protoTy->getDecl()))
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
        std::all_of(ED->getMembers().begin(), ED->getMembers().end(),
                    [&](const Decl *D) { return declIsPrivate(D); })) {
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
      if (VD->getFullName().isOperator())
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
          if (VD->hasName() && (includePrivateDecls || !declIsPrivate(VD))) {
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
    return !includePrivateDecls && declIsPrivate(D);
  }
};
} // namespace

//==============================================================================
// MARK: creating keys
//==============================================================================

DependencyKey DependencyKey::createInterfaceKey(NodeKind kind,
                                                StringRef context,
                                                StringRef name) {
  return DependencyKey(kind, DeclAspect::interface, context, name);
}

DependencyKey DependencyKey::createNominalOrMemberInterfaceKeyNominal(
    StringRef mangledHolderName, StringRef memberBaseNameIfAny) {
  const bool isMemberBlank = memberBaseNameIfAny.empty();
  const auto kind =
      isMemberBlank ? NodeKind::potentialMember : NodeKind::member;
  return DependencyKey::createInterfaceKey(kind, mangledHolderName,
                                           memberBaseNameIfAny);
}

//==============================================================================
// MARK: Start of SourceFileDepGraph building, specific to status quo
//==============================================================================

//==============================================================================
// MARK: createProvidedInterfaceKey
//==============================================================================

template <>
DependencyKey
DependencyKey::createProvidedInterfaceKey<NodeKind::topLevel,
                                          PrecedenceGroupDecl const *>(
    PrecedenceGroupDecl const *D) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       ::getName(D));
}

template <>
DependencyKey
DependencyKey::createProvidedInterfaceKey<NodeKind::topLevel, FuncDecl const *>(
    FuncDecl const *D) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       ::getName(D));
}

template <>
DependencyKey DependencyKey::createProvidedInterfaceKey<
    NodeKind::topLevel, OperatorDecl const *>(const OperatorDecl *D) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       ::getName(D));
}

template <>
DependencyKey DependencyKey::createProvidedInterfaceKey<
    NodeKind::topLevel, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       ::getName(D));
}

template <>
DependencyKey DependencyKey::createProvidedInterfaceKey<
    NodeKind::topLevel, ValueDecl const *>(const ValueDecl *D) {
  return DependencyKey(NodeKind::topLevel, DeclAspect::interface, "",
                       getBaseName(D));
}

template <>
DependencyKey DependencyKey::createProvidedInterfaceKey<
    NodeKind::dynamicLookup, ValueDecl const *>(const ValueDecl *D) {
  return DependencyKey(NodeKind::dynamicLookup, DeclAspect::interface, "",
                       getBaseName(D));
}

template <>
DependencyKey DependencyKey::createProvidedInterfaceKey<
    NodeKind::nominal, NominalTypeDecl const *>(const NominalTypeDecl *D) {
  return DependencyKey(NodeKind::nominal, DeclAspect::interface,
                       mangleTypeAsContext(D), "");
}

template <>
DependencyKey
DependencyKey::createProvidedInterfaceKey<NodeKind::potentialMember,
                                          NominalTypeDecl const *>(
    const NominalTypeDecl *D) {
  return DependencyKey(NodeKind::potentialMember, DeclAspect::interface,
                       mangleTypeAsContext(D), "");
}

template <>
DependencyKey
DependencyKey::createProvidedInterfaceKey<NodeKind::sourceFileProvide,
                                          StringRef>(StringRef swiftDeps) {
  return DependencyKey(NodeKind::sourceFileProvide, DeclAspect::interface, "",
                       swiftDeps);
}

template <>
DependencyKey DependencyKey::createProvidedInterfaceKey<
    NodeKind::member, std::pair<const NominalTypeDecl *, const ValueDecl *>>(
    std::pair<const NominalTypeDecl *, const ValueDecl *> holderAndMember) {
  return DependencyKey(NodeKind::member, DeclAspect::interface,
                       mangleTypeAsContext(holderAndMember.first),
                       getBaseName(holderAndMember.second));
}

//==============================================================================
// MARK: SourceFileDepGraphConstructor
//==============================================================================

// clang-format off
/// Used by the unit tests
SourceFileDepGraphConstructor::SourceFileDepGraphConstructor(
    StringRef swiftDeps,
    bool includePrivateDeps,
    bool hadCompilationError,
    const std::string &interfaceHash,

    ArrayRef<SerializableUse> topLevelDepends,
    ArrayRef<SerializableUse> nominalMemberPotentialMemberDepends,
    ArrayRef<SerializableUse> dynamicLookupDepends,
    ArrayRef<SerializableUse> externalDependencies,

    ArrayRef<SerializableDecl> precedenceGroups,
    ArrayRef<SerializableDecl> memberOperatorDecls,
    ArrayRef<SerializableDecl> operators,
    ArrayRef<SerializableDecl> topNominals,
    ArrayRef<SerializableDecl> topValues,
    ArrayRef<SerializableDecl> allNominals,
    ArrayRef<SerializableDecl> potentialMemberHolders,
    ArrayRef<SerializableDecl> valuesInExtensions,
    ArrayRef<SerializableDecl> classMembers
    ) :
    swiftDeps(swiftDeps),
    includePrivateDeps(includePrivateDeps),
    hadCompilationError(hadCompilationError),

    interfaceHash(interfaceHash),
    topLevelDepends(topLevelDepends),
    nominalMemberPotentialMemberDepends(nominalMemberPotentialMemberDepends),
    dynamicLookupDepends(dynamicLookupDepends),
    externalDependencies(externalDependencies),

    precedenceGroups(precedenceGroups),
    memberOperatorDecls(memberOperatorDecls),
    operators(operators),
    topNominals(topNominals),
    topValues(topValues),
    allNominals(allNominals),
    potentialMemberHolders(potentialMemberHolders),
    valuesInExtensions(valuesInExtensions),
    classMembers(classMembers)
    {}
// clang-format on

// clang-format off
SourceFileDepGraphConstructor
SourceFileDepGraphConstructor::forSourceFile(
  SourceFile *SF,
  const DependencyTracker &depTracker,
  StringRef swiftDeps,
  const bool includePrivateDeps,
  const bool hadCompilationError) {
// clang-format on

  SourceFileDeclFinder declFinder(SF, includePrivateDeps);
  std::vector<SerializableUse> topLevelDepends;
  for (const auto &p : SF->getReferencedNameTracker()->getTopLevelNames())
    topLevelDepends.push_back(SerializableUse{
        p.getSecond(), None, "", p.getFirst().userFacingName(), None});

  std::vector<SerializableUse> dynamicLookupDepends;
  for (const auto &p : SF->getReferencedNameTracker()->getDynamicLookupNames())
    dynamicLookupDepends.push_back(SerializableUse{
        p.getSecond(), None, "", p.getFirst().userFacingName(), None});

  std::vector<SerializableUse> nominalMemberPotentialMemberDepends;
  for (const auto &p : SF->getReferencedNameTracker()->getUsedMembers()) {
    const auto &member = p.getFirst().second;
    StringRef emptyOrUserFacingName =
        member.empty() ? "" : member.userFacingName();
    nominalMemberPotentialMemberDepends.push_back(SerializableUse{
        declIsPrivate(p.getFirst().first), p.getSecond(),
        mangleTypeAsContext(p.getFirst().first), emptyOrUserFacingName, None});
  }

  std::vector<SerializableUse> externalDepends;
  for (StringRef s : depTracker.getDependencies())
    externalDepends.push_back(SerializableUse{false, None, "", s, None});

  // clang-format off
  return SourceFileDepGraphConstructor(
      swiftDeps,
      includePrivateDeps,
      hadCompilationError,

      getInterfaceHash(SF),
      topLevelDepends,
      nominalMemberPotentialMemberDepends,
      dynamicLookupDepends,
      externalDepends,

      serializableDeclsForProvidersOfAGivenType<NodeKind::topLevel>(
          declFinder.precedenceGroups),
      serializableDeclsForProvidersOfAGivenType<NodeKind::topLevel>(
          declFinder.memberOperatorDecls),
      serializableDeclsForProvidersOfAGivenType<NodeKind::topLevel>(declFinder.operators),
      serializableDeclsForProvidersOfAGivenType<NodeKind::topLevel>(declFinder.topNominals),
      serializableDeclsForProvidersOfAGivenType<NodeKind::topLevel>(declFinder.topValues),
      serializableDeclsForProvidersOfAGivenType<NodeKind::nominal>(declFinder.allNominals),
      serializableDeclsForProvidersOfAGivenType<NodeKind::potentialMember>(
          declFinder.potentialMemberHolders),
      serializableDeclsForProvidersOfAGivenType<NodeKind::member>(
          declFinder.valuesInExtensions),
      serializableDeclsForProvidersOfAGivenType<NodeKind::dynamicLookup>(
          declFinder.classMembers));
}
// clang-format on

std::string SourceFileDepGraphConstructor::getInterfaceHash(SourceFile *SF) {
  llvm::SmallString<32> interfaceHash;
  SF->getInterfaceHash(interfaceHash);
  return interfaceHash.str().str();
}

Optional<std::string>
SourceFileDepGraphConstructor::getFingerprintIfAny(const Decl *d) {
  if (const auto *idc = dyn_cast<IterableDeclContext>(d))
    return idc->getBodyFingerprint();
  return None;
}

template <NodeKind kind>
void SourceFileDepGraphConstructor::addAllDependenciesFrom(
    ArrayRef<SerializableUse> depends) {
  for (const auto &d : depends) {
    recordDefUse(DependencyKey::createInterfaceKey(kind, d.context, d.name),
                 d.isCascadingUse, d.use);
  }
}

template <>
void SourceFileDepGraphConstructor::addAllDependenciesFrom<NodeKind::member>(
    ArrayRef<SerializableUse> nominalMemberPotentialMemberDepends) {

  llvm::StringSet<> holdersOfCascadingMembers;
  for (const auto &entry : nominalMemberPotentialMemberDepends) {
    if (!includePrivateDeps && entry.isPrivate.getValueOr(false))
      continue;
    if (entry.isCascadingUse)
      holdersOfCascadingMembers.insert(entry.context);
  }
  for (const auto &entry : nominalMemberPotentialMemberDepends) {
    if (!includePrivateDeps && entry.isPrivate.getValueOr(false))
      continue;
    recordDefUse(
        DependencyKey::createInterfaceKey(NodeKind::nominal, entry.context, ""),
        holdersOfCascadingMembers.count(entry.context) != 0, entry.use);
    recordDefUse(DependencyKey::createNominalOrMemberInterfaceKeyNominal(
                     entry.context, entry.name),
                 entry.isCascadingUse, entry.use);
  }
}


//==============================================================================
// MARK: SourceFileDepGraphConstructor: Adding nodes to the graph
//==============================================================================

void SourceFileDepGraphConstructor::addSourceFileNodesToGraph() {
  g.findExistingNodePairOrCreateAndAddIfNew(
      SerializableDecl{DependencyKey::createInterfaceKey(
                           NodeKind::sourceFileProvide, "", swiftDeps),
                       getSourceFileFingerprint()});
}

void SourceFileDepGraphConstructor::addProviderNodesToGraph() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates

  // Many kinds of Decls become top-level depends.
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(precedenceGroups);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(memberOperatorDecls);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(operators);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(topNominals);
  addAllProviderNodesOfAGivenType<NodeKind::topLevel>(topValues);

  addAllProviderNodesOfAGivenType<NodeKind::nominal>(allNominals);

  addAllProviderNodesOfAGivenType<NodeKind::potentialMember>(
      potentialMemberHolders);
  addAllProviderNodesOfAGivenType<NodeKind::member>(valuesInExtensions);

  addAllProviderNodesOfAGivenType<NodeKind::dynamicLookup>(classMembers);
}

void SourceFileDepGraphConstructor::addDependencyArcsToGraph() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates
  addAllDependenciesFrom<NodeKind::topLevel>(topLevelDepends);
  addAllDependenciesFrom<NodeKind::member>(nominalMemberPotentialMemberDepends);
  addAllDependenciesFrom<NodeKind::dynamicLookup>(dynamicLookupDepends);
  addAllDependenciesFrom<NodeKind::externalDepend>(externalDependencies);
}

void SourceFileDepGraphConstructor::recordDefUse(
    const DependencyKey &defKey, bool isCascadingUse,
    Optional<SerializableDecl> use) {
  SourceFileDepGraphNode *defNode =
      g.findExistingNodeOrCreateIfNew(defKey, None, false /* = !isProvides */);
  auto useNodePair =
      use ? g.findExistingNodePairOrCreateAndAddIfNew(use.getValue())
          : g.getSourceFileNodePair();
  g.addArc(defNode, useNodePair.useDependingOnCascading(isCascadingUse));
}

//==============================================================================
// Entry point from the Frontend to this whole system
//==============================================================================

bool fine_grained_dependencies::emitReferenceDependencies(
    DiagnosticEngine &diags, SourceFile *const SF,
    const DependencyTracker &depTracker, StringRef outputPath,
    const bool alsoEmitDotFile) {

  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  // Since, when fingerprints are enabled,
  // the parser diverts token hashing into per-body fingerprints
  // before it can know if a difference is in a private type,
  // in order to be able to test the changed  fingerprints
  // we force the inclusion of private declarations when fingerprints
  // are enabled.
  const bool includeIntrafileDeps =
      SF->getASTContext()
          .LangOpts.FineGrainedDependenciesIncludeIntrafileOnes ||
      SF->getASTContext().LangOpts.EnableTypeFingerprints;
  const bool hadCompilationError = SF->getASTContext().hadError();
  auto gc = SourceFileDepGraphConstructor::forSourceFile(
      SF, depTracker, outputPath, includeIntrafileDeps, hadCompilationError);
  SourceFileDepGraph g = gc.construct();

  const bool hadError =
      withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
        out << g.yamlProlog(hadCompilationError);
        llvm::yaml::Output yamlWriter(out);
        yamlWriter << g;
        return false;
      });

  // If path is stdout, cannot read it back, so check for "-"
  assert(outputPath == "-" || g.verifyReadsWhatIsWritten(outputPath));

  if (alsoEmitDotFile)
    g.emitDotFile(outputPath, diags);

  return hadError;
}
