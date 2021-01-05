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

#include "FrontendSourceFileDepGraphFactory.h"

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
// MARK: Entry point into frontend graph construction
//==============================================================================

bool fine_grained_dependencies::withReferenceDependencies(
    llvm::PointerUnion<const ModuleDecl *, const SourceFile *> MSF,
    const DependencyTracker &depTracker, StringRef outputPath,
    bool alsoEmitDotFile,
    llvm::function_ref<bool(SourceFileDepGraph &&)> cont) {
  if (auto *MD = MSF.dyn_cast<const ModuleDecl *>()) {
    SourceFileDepGraph g =
        ModuleDepGraphFactory(MD, alsoEmitDotFile).construct();
    return cont(std::move(g));
  } else {
    auto *SF = MSF.get<const SourceFile *>();
    SourceFileDepGraph g = FrontendSourceFileDepGraphFactory(
                               SF, outputPath, depTracker, alsoEmitDotFile)
                               .construct();
    return cont(std::move(g));
  }
}

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory
//==============================================================================

FrontendSourceFileDepGraphFactory::FrontendSourceFileDepGraphFactory(
    const SourceFile *SF, StringRef outputPath,
    const DependencyTracker &depTracker, const bool alsoEmitDotFile)
    : AbstractSourceFileDepGraphFactory(
          SF->getASTContext().hadError(), outputPath, SF->getInterfaceHash(),
          alsoEmitDotFile, SF->getASTContext().Diags),
      SF(SF), depTracker(depTracker) {}

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory - adding collections of defined Decls
//==============================================================================
//==============================================================================
// MARK: DeclFinder
//==============================================================================

namespace {
/// Takes all the Decls in a SourceFile, and collects them into buckets by
/// groups of DeclKinds. Also casts them to more specific types
struct DeclFinder {
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

  using LookupClassMember = llvm::function_ref<void(VisibleDeclConsumer &)>;

public:
  /// Construct me and separates the Decls.
  // clang-format off
    DeclFinder(ArrayRef<Decl *> topLevelDecls,
               LookupClassMember lookupClassMember) {
      for (const Decl *const D : topLevelDecls) {
        select<ExtensionDecl, DeclKind::Extension>(D, extensions) ||
        select<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator,
        DeclKind::PostfixOperator>(D, operators) ||
        select<PrecedenceGroupDecl, DeclKind::PrecedenceGroup>(
                                                               D, precedenceGroups) ||
        select<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct,
        DeclKind::Class, DeclKind::Protocol>(D, topNominals) ||
        select<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func,
        DeclKind::Accessor>(D, topValues);
      }
    // clang-format on
    // The order is important because some of these use instance variables
    // computed by others.
    findNominalsFromExtensions();
    findNominalsInTopNominals();
    findValuesInExtensions();
    findClassMembers(lookupClassMember);
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
      if (!VD)
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
      if (!NTD) {
        continue;
      }

      for (const auto *member : ED->getMembers()) {
        const auto *VD = dyn_cast<ValueDecl>(member);
        if (!VD || !VD->hasName()) {
          continue;
        }

        if (const auto *const NTD = ED->getExtendedNominal()) {
          valuesInExtensions.push_back(std::make_pair(NTD, VD));
        }
      }
    }
  }

  /// Class members are needed for dynamic lookup dependency nodes.
  void findClassMembers(LookupClassMember lookup) {
    struct Collector : public VisibleDeclConsumer {
      ConstPtrVec<ValueDecl> &classMembers;
      Collector(ConstPtrVec<ValueDecl> &classMembers)
          : classMembers(classMembers) {}
      void foundDecl(ValueDecl *VD, DeclVisibilityKind,
                     DynamicLookupInfo) override {
        classMembers.push_back(VD);
      }
    } collector{classMembers};
    lookup(collector);
  }

  /// Check \p D to see if it is one of the DeclKinds in the template
  /// arguments. If so, cast it to DesiredDeclType and add it to foundDecls.
  /// \returns true if successful.
  template <typename DesiredDeclType, DeclKind firstKind,
            DeclKind... restOfKinds>
  bool select(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls) {
    if (D->getKind() == firstKind) {
      foundDecls.push_back(cast<DesiredDeclType>(D));
      return true;
    }
    return select<DesiredDeclType, restOfKinds...>(D, foundDecls);
  }

  /// Terminate the template recursion.
  template <typename DesiredDeclType>
  bool select(const Decl *const D, ConstPtrVec<DesiredDeclType> &foundDecls) {
    return false;
  }
};
} // namespace

void FrontendSourceFileDepGraphFactory::addAllDefinedDecls() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates

  // Many kinds of Decls become top-level depends.

  DeclFinder declFinder(SF->getTopLevelDecls(),
                        [this](VisibleDeclConsumer &consumer) {
    SF->lookupClassMembers({}, consumer);
  });

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

//==============================================================================
// MARK: FrontendSourceFileDepGraphFactory - adding collections of used Decls
//==============================================================================

namespace {
/// Extracts uses out of a SourceFile
class UsedDeclEnumerator {
  const SourceFile *SF;
  const DependencyTracker &depTracker;
  StringRef swiftDeps;

  /// Cache these for efficiency
  const DependencyKey sourceFileImplementation;

public:
  UsedDeclEnumerator(const SourceFile *SF, const DependencyTracker &depTracker,
                     StringRef swiftDeps)
      : SF(SF), depTracker(depTracker), swiftDeps(swiftDeps),
        sourceFileImplementation(DependencyKey::createKeyForWholeSourceFile(
            DeclAspect::implementation, swiftDeps)) {}

public:
  using UseEnumerator =
      llvm::function_ref<void(const DependencyKey &, const DependencyKey &)>;
  void enumerateAllUses(UseEnumerator enumerator) {
    auto &Ctx = SF->getASTContext();
    Ctx.evaluator.enumerateReferencesInFile(SF, [&](const auto &ref) {
      std::string name = ref.name.userFacingName().str();
      const auto *nominal = ref.subject;
      using Kind = evaluator::DependencyCollector::Reference::Kind;

      switch (ref.kind) {
      case Kind::Empty:
      case Kind::Tombstone:
        llvm_unreachable("Cannot enumerate dead reference!");
      case Kind::TopLevel:
        return enumerateUse<NodeKind::topLevel>("", name, enumerator);
      case Kind::Dynamic:
        return enumerateUse<NodeKind::dynamicLookup>("", name, enumerator);
      case Kind::PotentialMember: {
        std::string context = DependencyKey::computeContextForProvidedEntity<
            NodeKind::potentialMember>(nominal);
        return enumerateUse<NodeKind::potentialMember>(context, "", enumerator);
      }
      case Kind::UsedMember: {
        std::string context =
            DependencyKey::computeContextForProvidedEntity<NodeKind::member>(
                nominal);
        return enumerateUse<NodeKind::member>(context, name, enumerator);
      }
      }
    });
    enumerateExternalUses(enumerator);
    enumerateNominalUses(enumerator);
  }

private:
  template <NodeKind kind>
  void enumerateUse(StringRef context, StringRef name,
                    UseEnumerator createDefUse) {
    // Assume that what is depended-upon is the interface
    createDefUse(
        DependencyKey(kind, DeclAspect::interface, context.str(), name.str()),
        sourceFileImplementation);
  }

  void enumerateNominalUses(UseEnumerator enumerator) {
    auto &Ctx = SF->getASTContext();
    Ctx.evaluator.enumerateReferencesInFile(SF, [&](const auto &ref) {
      const NominalTypeDecl *subject = ref.subject;
      if (!subject) {
        return;
      }

      std::string context =
          DependencyKey::computeContextForProvidedEntity<NodeKind::nominal>(
              subject);
      enumerateUse<NodeKind::nominal>(context, "", enumerator);
    });
  }

  void enumerateExternalUses(UseEnumerator enumerator) {
    for (StringRef s : depTracker.getIncrementalDependencies())
      enumerateUse<NodeKind::incrementalExternalDepend>("", s, enumerator);

    for (StringRef s : depTracker.getDependencies())
      enumerateUse<NodeKind::externalDepend>("", s, enumerator);
  }
};
} // end namespace

void FrontendSourceFileDepGraphFactory::addAllUsedDecls() {
  UsedDeclEnumerator(SF, depTracker, swiftDeps)
      .enumerateAllUses(
          [&](const DependencyKey &def, const DependencyKey &use) {
            addAUsedDecl(def, use);
          });
}

//==============================================================================
// MARK: ModuleDepGraphFactory
//==============================================================================

ModuleDepGraphFactory::ModuleDepGraphFactory(const ModuleDecl *Mod,
                                             bool emitDot)
    : AbstractSourceFileDepGraphFactory(Mod->getASTContext().hadError(),
                                        Mod->getNameStr(), Fingerprint::ZERO(),
                                        emitDot, Mod->getASTContext().Diags),

      Mod(Mod) {
  // Since a fingerprint only summarizes the state of the module but not
  // the state of its fingerprinted sub-declarations, and since a module
  // contains no state other than sub-declarations, its fingerprint does not
  // matter and can just be some arbitrary value. Should it be the case that a
  // change in a declaration that does not have a fingerprint must cause
  // a rebuild of a file outside of the module, this assumption will need
  // to be revisited.
}

void ModuleDepGraphFactory::addAllDefinedDecls() {
  // TODO: express the multiple provides and depends streams with variadic
  // templates

  // Many kinds of Decls become top-level depends.

  SmallVector<Decl *, 32> TopLevelDecls;
  Mod->getTopLevelDecls(TopLevelDecls);
  DeclFinder declFinder(TopLevelDecls,
                        [this](VisibleDeclConsumer &consumer) {
                          return Mod->lookupClassMembers({}, consumer);
                        });

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
}
