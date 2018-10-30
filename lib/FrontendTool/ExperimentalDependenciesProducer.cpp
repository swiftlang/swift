//===--- ExperimentalDependenciesProducer.cpp - Generates swiftdeps files -===//
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

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace experimental_dependencies;

namespace {
enum class ReferendeDependenciesKind { StatusQuo };
/// Emits the reference dependencies from the frontend so that the driver
/// can compute a dependency graph for the whole module, and use it to decide
/// which files need to be recompiled when doing incremental compilation.
template <ReferendeDependenciesKind> class ReferenceDependenciesEmitter {
protected:
  SourceFile *const SF;
  const DependencyTracker &depTracker;
  llvm::raw_ostream &out;

public:
  ReferenceDependenciesEmitter(SourceFile *const SF,
                               const DependencyTracker &depTracker,
                               llvm::raw_ostream &out)
      : SF(SF), depTracker(depTracker), out(out) {}
  void emit();
};
} // end namespace

template <>
void ReferenceDependenciesEmitter<ReferendeDependenciesKind::StatusQuo>::emit();

bool swift::experimental_dependencies::emitReferenceDependencies(
    DiagnosticEngine &diags, SourceFile *SF,
    const DependencyTracker &depTracker, StringRef outputPath) {

  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
    ReferenceDependenciesEmitter<ReferendeDependenciesKind::StatusQuo>(
        SF, depTracker, out)
        .emit();
    return false;
  });
}

// Status quo
namespace SQ {
class YAMLEmitter {
private:
  llvm::raw_ostream &out;

public:
  YAMLEmitter(llvm::raw_ostream &out) : out(out) {}

  void emitSectionStart(StringRef section) const {
    out << reference_dependency_keys::providesMember << ":\n";
  }
  void emitName(StringRef name) const {
    out << "- \"" << llvm::yaml::escape(name) << "\"\n";
  }
  void emitSingleValueSection(StringRef section, StringRef value) const {
    out << section << ": \"" << llvm::yaml::escape(value) << "\"\n";
  }
  void emitDoubleNameLine(StringRef name1, StringRef name2) const {
    out << "- [\"" << llvm::yaml::escape(name1) << "\", \""
        << (name2.empty() ? std::string() : llvm::yaml::escape(name2))
        << "\"]\n";
  }
  };

  class ProviderDeclarations {
  public:
    const std::vector<const ValueDecl *> classMembers;
    const std::vector<const NominalTypeDecl *> extendedNominals;
    const std::vector<const NominalTypeDecl *>
        extendedNominalsThatCouldNotChangeTheShapeOfTheType;
    const std::vector<std::pair<const NominalTypeDecl *, const ValueDecl *>>
        holdersAndMembers;

  public:
    ProviderDeclarations(const SourceFile *SF);

  private:
    static std::vector<const ValueDecl *> getClassMembers(const SourceFile *);
    static std::vector<const NominalTypeDecl *>
    getExtendedNominals(const SourceFile *);
    static std::set<const NominalTypeDecl *>
    getExtendedNominalsThatCouldNotChangeTheShapeOfTheType(const SourceFile *);
    static std::vector<std::pair<const NominalTypeDecl *, const ValueDecl *>>
    getHoldersAndMembers(const SourceFile *);
    static std::vector<const ExtensionDecl *>
    getExtensionsWithJustMembers(const SourceFile *);
    static std::vector<const ExtensionDecl *>
    getTopLevelVisibleExtensions(const SourceFile *SF);
    static std::vector<const NominalTypeDecl *>
    getTopLevelVisibleNominals(const SourceFile *SF);
    static void addExtendedNominalMembers(
        const DeclRange members,
        std::vector<const NominalTypeDecl *> &extendedNominals);

    template <DeclKind, typename DeclT>
    static std::vector<const DeclT *> getTopLevelDecls(const SourceFile *);

    static bool isVisibleOutsideItsFile(const ValueDecl *const VD) {
      return VD->getFormalAccess() > AccessLevel::FilePrivate;
    }
    static bool
    isExtendedTypeVisibleOutsideThisFile(const TypeLoc inheritedType);
    static bool isMemberVisibleOutsideThisFile(const Decl *const member);
    static bool
    areAnyExtendedTypesVisibleOutsideThisFile(const ExtensionDecl *const);
    static bool areAnyMembersVisibleOutsideThisFile(const ExtensionDecl *const);
  };

  class ProviderNames {
  public:
    const std::vector<std::string> dynamicLookupNames;
    const std::vector<std::string>
        extendedNominalContextualNamesThatCouldAddMembers;
    const std::vector<std::pair<std::string, std::string>> holdersAndMembers;

  private:
    static std::vector<std::string>
    getDynamicLookupNames(const ProviderDeclarations &);
    static std::vector<std::string>
    getExtendedNominalContextualNamesThatCouldAddMembers(
        const ProviderDeclarations &);
    static std::vector<std::pair<std::string, std::string>>
    getHoldersAndMembers(const ProviderDeclarations &);

    static std::string getContextualName(const NominalTypeDecl *const NTD) {
      Mangle::ASTMangler Mangler;
      return Mangler.mangleTypeAsContextUSR(NTD);
    }

  public:
    ProviderNames(const ProviderDeclarations &);
  };

  class InterfaceHash {
  public:
    llvm::SmallString<32> contents;
    InterfaceHash(SourceFile *SF) { SF->getInterfaceHash(contents); }
  };

  class Provides {
    ProviderNames pns;
    InterfaceHash interfaceHash;
    YAMLEmitter &emitter;

    void emitTopLevelNames() const;
    void emitNominalTypes() const;
    void emitMembers() const;
    void emitDynamicLookupMembers() const;
    void emitInterfaceHash() const;

  public:
    Provides(const ProviderNames &, const InterfaceHash &, YAMLEmitter &);
    void emit() const;
  };

  class Depends {
    YAMLEmitter &emitter;

  public:
    Depends(const SourceFile *const, const DependencyTracker &, YAMLEmitter &);
    void emit() const;
  };
  } // namespace SQ

  template <>
  void
  ReferenceDependenciesEmitter<ReferendeDependenciesKind::StatusQuo>::emit() {
    SQ::YAMLEmitter e(out);
    SQ::ProviderDeclarations pd(SF);
    SQ::ProviderNames pn(pd);
    SQ::InterfaceHash sqh(SF);
    SQ::Provides sqp(pn, sqh, e);

    SQ::Depends sqd(SF, depTracker, e);

    sqp.emit();
    sqd.emit();
}

SQ::Provides::Provides(const ProviderNames &pns,
                       const InterfaceHash &interfaceHash, YAMLEmitter &emitter)
    : pns(pns), interfaceHash(interfaceHash), emitter(emitter) {}

void SQ::Provides::emit() const {
  emitTopLevelNames();
  emitNominalTypes();
  emitMembers();
  emitDynamicLookupMembers();
  emitInterfaceHash();
}

void SQ::Provides::emitTopLevelNames() const {}
void SQ::Provides::emitNominalTypes() const {
  emitter.emitSectionStart(reference_dependency_keys::providesNominal);
  for (StringRef n : pns.extendedNominalsContextualNamesThatCanChangeShape)
    emitter.emitName(n);
}
void SQ::Provides::emitMembers() const {
  emitter.emitSectionStart(reference_dependency_keys::providesMember);
  for (StringRef n : pns.extendedNominalContextualNamesThatCouldAddMembers)
    emitter.emitDoubleNameLine(n, "");
  for (std::pair<StringRef, StringRef> hm : pns.holdersAndMembers)
    emitter.emitDoubleNameLine(hm.first, hm.second);
}
void SQ::Provides::emitDynamicLookupMembers() const {
  emitter.emitSectionStart(reference_dependency_keys::providesDynamicLookup);
  for (StringRef n : pns.dynamicLookupNames)
    emitter.emitName(n);
}
void SQ::Provides::emitInterfaceHash() const {
  emitter.emitSingleValueSection(reference_dependency_keys::interfaceHash,
                                 interfaceHash.contents);
}

SQ::ProviderDeclarations::ProviderDeclarations(const SourceFile *SF)
    : classMembers(getClassMembers(SF)),
      extendedNominals(getExtendedNominals(SF)),
      extendedNominalsThatCouldNotChangeTheShapeOfTheType(
          getExtendedNominalsThatCouldNotChangeTheShapeOfTheType(SF)),
      holdersAndMembers(getHoldersAndMembers(SF)) {}

std::vector<const ValueDecl *>
SQ::ProviderDeclarations::getClassMembers(const SourceFile *SF) {
  struct NameCollector : public VisibleDeclConsumer {
    std::vector<const ValueDecl *> classMembers;
    void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
      classMembers.push_back(VD);
    }
  } collect;
  SF->lookupClassMembers({}, collect);
  return collect.classMembers;
}

std::vector<const NominalTypeDecl *>
SQ::ProviderDeclarations::getExtendedNominals(const SourceFile *SF) {
  std::vector<const NominalTypeDecl *> extendedNominals;
  for (const ExtensionDecl *const ED : getTopLevelVisibleExtensions(SF)) {
    extendedNominals.push_back(ED->getExtendedNominal());
    addExtendedNominalMembers(ED->getMembers(), extendedNominals);
  }
  for (const NominalTypeDecl *const NTD : getTopLevelVisibleNominals(SF)) {
    extendedNominals.push_back(NTD);
    addExtendedNominalMembers(NTD->getMembers(), extendedNominals);
  }
  return extendedNominals;
}

std::set<const NominalTypeDecl *> SQ::ProviderDeclarations::
    getExtendedNominalsThatCouldNotChangeTheShapeOfTheType(
        const SourceFile *SF) {
  getTopLevelVisibleExtensions(SF) XXXX
}

void SQ::ProviderDeclarations::addExtendedNominalMembers(
    const DeclRange members,
    std::vector<const NominalTypeDecl *> &extendedNominals) {
  for (const Decl *D : members) {
    if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      extendedNominals.push_back(NTD);
      addExtendedNominalMembers(NTD->getMembers(), extendedNominals);
    }
  }
}
// FINDING OPERATORS???
std::vector<const ExtensionDecl *>
SQ::ProviderDeclarations::getTopLevelVisibleExtensions(const SourceFile *SF) {
  std::vector<const ExtensionDecl *> extensions;
  for (const ExtensionDecl *const ED :
       getTopLevelDecls<DeclKind::Extension, ExtensionDecl>(SF))
    if (const NominalTypeDecl *const NTD = ED->getExtendedNominal())
      if (isVisibleOutsideItsFile(NTD))
        extensions.push_back(ED);
  return extensions;
}

std::vector<const NominalTypeDecl *>
SQ::ProviderDeclarations::getTopLevelVisibleNominals(const SourceFile *SF) {
  std::vector<const NominalTypeDecl *> nominals;
  for (const Decl *const D : SF->Decls)
    switch (D->getKind()) {
    default:
      break;

    case DeclKind::Enum:
    case DeclKind::Struct:
    case DeclKind::Class:
    case DeclKind::Protocol: {
      const NominalTypeDecl *const NTD = cast<NominalTypeDecl>(D);
      if (NTD->hasName() && isVisibleOutsideItsFile(NTD))
        nominals.push_back(NTD);
      break;
    }
    }
  return nominals;
}

// getnonchanging
// for (const ExtensionDecl *const ED: getTopLevelDecls<DeclKind::Extension,
// ExtensionDecl>(SF))  if (const NominalTypeDecl *const NTD =
// ED->getExtendedNominal())  if (isVisibleOutsideItsFile(NTD))
// extendedNominalsThatCouldAddMembers.push_back(NTD);
//
//
//
// return extendedNominalsThatCouldAddMembers;
//}

std::vector<std::pair<const NominalTypeDecl *, const ValueDecl *>>
SQ::ProviderDeclarations::getHoldersAndMembers(const SourceFile *SF) {
  std::vector<std::pair<const NominalTypeDecl *, const ValueDecl *>>
      holdersAndMembers;
  for (const ExtensionDecl *const ED : getExtensionsWithJustMembers(SF)) {
    for (const Decl *const D : ED->getMembers())
      if (const ValueDecl *const VD = dyn_cast<ValueDecl>(D))
        if (VD->hasName() && isVisibleOutsideItsFile(VD))
          holdersAndMembers.push_back(
              std::make_pair(ED->getExtendedNominal(), VD));
  }
  return holdersAndMembers;
}

std::vector<const ExtensionDecl *>
SQ::ProviderDeclarations::getExtensionsWithJustMembers(const SourceFile *SF) {
  std::vector<const ExtensionDecl *> extensionsWithJustMembers;
  for (const ExtensionDecl *const ED :
       getTopLevelDecls<DeclKind::Extension, ExtensionDecl>(SF))
    if (!areAnyExtendedTypesVisibleOutsideThisFile(ED) &&
        areAnyMembersVisibleOutsideThisFile(ED))
      extensionsWithJustMembers.push_back(ED);
  return extensionsWithJustMembers;
}

bool SQ::ProviderDeclarations::areAnyExtendedTypesVisibleOutsideThisFile(
    const ExtensionDecl *const ED) {
  return std::any_of(ED->getInherited().begin(), ED->getInherited().end(),
                     isExtendedTypeVisibleOutsideThisFile);
}
bool SQ::ProviderDeclarations::areAnyMembersVisibleOutsideThisFile(
    const ExtensionDecl *const ED) {
  return std::any_of(ED->getMembers().begin(), ED->getMembers().end(),
                     isMemberVisibleOutsideThisFile);
}

bool SQ::ProviderDeclarations::isExtendedTypeVisibleOutsideThisFile(
    const TypeLoc inheritedType) {
  auto type = inheritedType.getType();
  if (!type)
    return false;

  if (!type->isExistentialType()) {
    // Be conservative. We don't know how to deal with other extended types.
    return true;
  }

  auto layout = type->getExistentialLayout();
  assert(!layout.explicitSuperclass && "Should not have a subclass existential "
         "in the inheritance clause of an extension");
  for (auto protoTy : layout.getProtocols()) {
    if (isMemberVisibleOutsideThisFile(protoTy->getDecl()))
      return true;
  }
  return false;
}

bool SQ::ProviderDeclarations::isMemberVisibleOutsideThisFile(
    const Decl *const member) {
  if (auto *VD = dyn_cast<ValueDecl>(member))
    return isVisibleOutsideItsFile(VD);

  switch (member->getKind()) {
  case DeclKind::Import:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    return false;

  case DeclKind::Extension:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    return true;

  default:
    llvm_unreachable("everything else is a ValueDecl");
  }
}

template <DeclKind kind, typename DeclT>
std::vector<const DeclT *>
SQ::ProviderDeclarations::getTopLevelDecls(const SourceFile *const SF) {
  std::vector<const DeclT *> tops;
  for (const Decl *const D : SF->Decls) {
    if (D->getKind() == kind) {
      assert(cast<DeclT>(D));
      tops.push_back(cast<DeclT>(D));
    }
  }
  return tops;
}

SQ::ProviderNames::ProviderNames(const SQ::ProviderDeclarations &pd)
    : dynamicLookupNames(getDynamicLookupNames(pd)),
      extendedNominalContextualNamesThatCouldAddMembers(
          getExtendedNominalContextualNamesThatCouldAddMembers(pd)),
      holdersAndMembers(getHoldersAndMembers(pd)) {}

std::vector<std::string>
SQ::ProviderNames::getDynamicLookupNames(const ProviderDeclarations &pd) {
  std::vector<DeclBaseName> names;
  for (const auto *VD : pd.classMembers)
    names.push_back(VD->getBaseName());
  llvm::array_pod_sort(names.begin(), names.end(),
                       [](const DeclBaseName *lhs, const DeclBaseName *rhs) {
                         return lhs->compare(*rhs);
                       });
  names.erase(std::unique(names.begin(), names.end()), names.end());
  std::vector<std::string> nameStrings;
  for (auto &dbn : names)
    nameStrings.push_back(dbn.userFacingName().str());
  return nameStrings;
}

std::vector<std::string>
SQ::ProviderNames::getExtendedNominalContextualNamesThatCouldAddMembers(
    const ProviderDeclarations &pd) {
  std::vector<std::string> extendedNominalContextualNamesThatCouldAddMembers;
  for (const NominalTypeDecl *const NTD :
       pd.extendedNominalsThatCouldAddMembers)
    extendedNominalContextualNamesThatCouldAddMembers.push_back(
        getContextualName(NTD));
  return extendedNominalContextualNamesThatCouldAddMembers;
}

std::vector<std::pair<std::string, std::string>>
SQ::ProviderNames::getHoldersAndMembers(const ProviderDeclarations &pd) {
  std::vector<std::pair<std::string, std::string>> holdersAndMembers;
  for (auto p : pd.holdersAndMembers)
    holdersAndMembers.push_back(std::make_pair(
        getContextualName(p.first), p.second->getBaseName().userFacingName()));
  return holdersAndMembers;
}

SQ::Depends::Depends(const SourceFile *const SF,
                     const DependencyTracker &depTracker, YAMLEmitter &emitter)
    : emitter(emitter) {}

void SQ::Depends::emit() const {
  (void)emitter;
  abort();
}

// namespace {
//
//
//
//  /// Emits the reference dependencies from the frontend so that the driver
//  /// can compute a dependency graph for the whole module, and use it to
//  decide
//  /// which files need to be recompiled when doing incremental compilation.
//  class ReferenceDependenciesEmitter {
//    SourceFile *const SF;
//    const DependencyTracker &depTracker;
//    llvm::raw_ostream &out;
//
//    ReferenceDependenciesEmitter(SourceFile *const SF,
//                                 const DependencyTracker &depTracker,
//                                 llvm::raw_ostream &out)
//    : SF(SF), depTracker(depTracker), out(out) {}
//
//  public:
//    /// Emits the provided and depended-upon dependencies to a file
//    ///
//    /// \param diags Where problems opening the file are emitted
//    /// \param SF The SourceFile containing the code with the dependences
//    /// \param depTracker The entities depended-upon
//    /// \param outputPath Where the dependencies are written
//    ///
//    /// \return true on error
//    static bool emit(DiagnosticEngine &diags, SourceFile *SF,
//                     const DependencyTracker &depTracker, StringRef
//                     outputPath);
//
//    /// Emit the dependencies.
//    static void emit(SourceFile *SF, const DependencyTracker &depTracker,
//                     llvm::raw_ostream &out);
//
//  private:
//    /// Emits all the dependency information.
//    void emit() const;
//  };
//
//  class CollectedDeclarations {
//    /// All of these used to be provided as top-level names
//    struct StatusQuoTopLevels {
//      /// Used to be provided as top-level, getName()->userFacingName()
//      llvm::SmallVector<const NominalTypeDecl*, 16> nominals;
//
//      /// Used to be provided as top-levels,
//      element->getName()->userFacingName() llvm::SmallVector<const
//      OperatorDecl *, 8> operators;
//
//      /// Used to be provided as top-levels,
//      element->getName()->userFacingName() llvm::SmallVector<const FuncDecl *,
//      8> memberOperatorsTreatedAsTop;
//
//      /// Used to be provided as top level,
//      element->getName()->userFacingName() llvm::SmallVector<const
//      PrecedenceGroupDecl *, 4> precedenceGroups;
//
//      /// Used to be provided as element->getBaseName().getMemberFacingName()
//      llvm::SmallVector<const ValueDecl *, 32> values;
//
//      template<typename DeclT>
//      static std::string getProvidedTopLevelName(const DeclT *const D) {
//        return DeclBaseName(D->getName()).userFacingName();
//      }
//
//    } top;
//
//
//
//    /// Records every nominal declaration, and whether or not the declaration
//    /// changes the externally-observable shape of the type.
//    /// Used to be provided as:
//    /// member type with empty member,
//    llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
//
//
//
//    /// Records extension declarations which are not introducing a conformance
//    /// to a public protocol and add a public member.
//    /// Used to be provided as:
//    /// member type with empty member,
//    llvm::SmallVector<const ExtensionDecl *, 8> extensionsWithJustMembers;
//
//    /// Records types for which members must be emitted.
//    /// Right now, only extension members are emitted individually.
//    /// Also emit with empty member as a wildcard.
//    std::vector<const std::pair<const NominalTypeDecl*, std::vector<const
//    ValueDecl*>>> holdersAndMembers; static std::string
//    getProvidedContextualName(const ExtensionDecl *const ED) {
//      return getProvidedContextualName(ED->getExtendedNominal());
//    }
//    static std::string getProvidedContextualName(const NominalTypeDecl *const
//    NTD) {
//      Mangle::ASTMangler Mangler;
//      return Mangler.mangleTypeAsContextUSR(NTD);
//    }
//
//    /// used for dynamicLookupMembers
//    std::vector<const ValueDecl *>classMembers;
//
//
//    /// Recursively computes the transitive closure over members
//    /// adding memberOperatorsTreatedAsTop and extendedNominals to the
//    receiver. void findNominalsAndOperators(const DeclRange members);
//
//    static bool extendedTypeIsPrivate(const TypeLoc inheritedType);
//    static bool declIsPrivate(const Decl *const member);
//
//    void collectExtensionAndContents(const ExtensionDecl *const ED);
//    void collectNominalAndContents(const NominalTypeDecl *const NTD);
//    void collectTopLevelValue(const ValueDecl *const VD);
//    void collectTopLevel(const Decl *const);
//
//    void collectMembers();
//    void collectExtendedNominal(const NominalTypeDecl *const NTD, bool
//    changesShape); void collectExtensionWithJustMembers(const ExtensionDecl
//    *const ED);
//
//    void collectClassMembers(const SourceFile *const);
//  public:
//    void collectDeclarationsFrom(const SourceFile *const SF);
//  };
//
//} // namespace
//
// template<>
// std::string CollectedDeclarations::StatusQuoTopLevels::
// getProvidedTopLevelName<ValueDecl>(const ValueDecl *const VD) {
//  return VD->getBaseName().userFacingName();
//}
//
//
// static bool isVisibleOutsideItsFile(const ValueDecl *const VD) {
//  return VD->getFormalAccess() > AccessLevel::FilePrivate;
//}
// static void unimplemented() { assert(false && "experimental dependencies
// unimplemented"); }
//
// bool ReferenceDependenciesEmitter::emit(DiagnosticEngine &diags,
//                                        SourceFile *const SF,
//                                        const DependencyTracker &depTracker,
//                                        StringRef outputPath) {
//  // Before writing to the dependencies file path, preserve any previous file
//  // that may have been there. No error handling -- this is just a nicety, it
//  // doesn't matter if it fails.
//  llvm::sys::fs::rename(outputPath, outputPath + "~");
//  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
//    ReferenceDependenciesEmitter::emit(SF, depTracker, out);
//    return false;
//  });
//}
//
// void ReferenceDependenciesEmitter::emit(SourceFile *SF,
//                                        const DependencyTracker &depTracker,
//                                        llvm::raw_ostream &out) {
//  ReferenceDependenciesEmitter(SF, depTracker, out).emit();
//}
//
// void ReferenceDependenciesEmitter::emit() const {
//  assert(SF && "Cannot emit reference dependencies without a SourceFile");
//  out << "### Swift experimental dependencies file v0 ###\n";
//  CollectedDeclarations cpd;
//  cpd.collectDeclarationsFrom(SF);
//  (void)depTracker;
//  unimplemented();
//}
//
//
//
//// stubs
//
// void CollectedDeclarations::collectDeclarationsFrom(const SourceFile *const
// SF) {
//  for (const Decl *D : SF->Decls)
//    collectTopLevel(D);
//  collectMembers();
//  collectClassMembers(SF);
//}
//
// void CollectedDeclarations::collectTopLevel(const Decl *const D) {
//  switch (D->getKind()) {
//    case DeclKind::Module:
//      break;
//
//    case DeclKind::Import:
//      // FIXME: Handle re-exported decls.
//      break;
//
//    case DeclKind::Extension:
//      collectExtensionAndContents(cast<ExtensionDecl>(D));
//      break;
//
//    case DeclKind::InfixOperator:
//    case DeclKind::PrefixOperator:
//    case DeclKind::PostfixOperator:
//      top.operators.push_back(cast<OperatorDecl>(D));
//      break;
//
//    case DeclKind::PrecedenceGroup:
//      top.precedenceGroups.push_back(cast<PrecedenceGroupDecl>(D));
//      break;
//
//    case DeclKind::Enum:
//    case DeclKind::Struct:
//    case DeclKind::Class:
//    case DeclKind::Protocol:
//      collectNominalAndContents(cast<NominalTypeDecl>(D));
//      break;
//
//    case DeclKind::TypeAlias:
//    case DeclKind::Var:
//    case DeclKind::Func:
//    case DeclKind::Accessor:
//      collectTopLevelValue(cast<ValueDecl>(D));
//      break;
//
//    case DeclKind::PatternBinding:
//    case DeclKind::TopLevelCode:
//    case DeclKind::IfConfig:
//    case DeclKind::PoundDiagnostic:
//      // No action necessary.
//      break;
//
//    case DeclKind::EnumCase:
//    case DeclKind::GenericTypeParam:
//    case DeclKind::AssociatedType:
//    case DeclKind::Param:
//    case DeclKind::Subscript:
//    case DeclKind::Constructor:
//    case DeclKind::Destructor:
//    case DeclKind::EnumElement:
//    case DeclKind::MissingMember:
//      // These can occur in malformed ASTs.
//      break;
//  }
//}
//
// void CollectedDeclarations::collectExtensionAndContents(const ExtensionDecl
// *const ED) {
//  const NominalTypeDecl *const NTD = ED->getExtendedNominal();
//  if (!NTD || !isVisibleOutsideItsFile(NTD))
//    return;
//
//  // Check if the extension is just adding members, or if it is
//  // introducing a conformance to a public protocol.
//  bool justMembers =
//  std::all_of(ED->getInherited().begin(), ED->getInherited().end(),
//              extendedTypeIsPrivate);
//  if (justMembers) {
//    if (std::all_of(ED->getMembers().begin(), ED->getMembers().end(),
//                    declIsPrivate)) {
//      return;
//    }
//    extensionsWithJustMembers.push_back(ED);
//  }
//  extendedNominals[NTD] |= !justMembers;
//  findNominalsAndOperators(ED->getMembers());
//}
//
// void CollectedDeclarations::collectNominalAndContents(const NominalTypeDecl
// *const NTD) {
//  if (!NTD->hasName() || !isVisibleOutsideItsFile(NTD)) {
//    return;
//  }
//  top.nominals.push_back(NTD);
//  extendedNominals[NTD] |= true;
//  findNominalsAndOperators(NTD->getMembers());
//}
//
// void CollectedDeclarations::collectTopLevelValue(const ValueDecl *const VD) {
//  if (!VD->hasName() || !isVisibleOutsideItsFile(VD))
//    return;
//  top.values.push_back(VD);
//}
//
// void CollectedDeclarations::CollectedDeclarations::findNominalsAndOperators(
//                                                                      const
//                                                                      DeclRange
//                                                                      members)
//                                                                      {
//  for (const Decl *D : members) {
//    auto *VD = dyn_cast<ValueDecl>(D);
//    if (!VD || !isVisibleOutsideItsFile(VD))
//      continue;
//
//    if (VD->getFullName().isOperator()) {
//      top.memberOperatorsTreatedAsTop.push_back(cast<FuncDecl>(VD));
//      continue;
//    }
//
//    auto nominal = dyn_cast<NominalTypeDecl>(D);
//    if (!nominal)
//      continue;
//    extendedNominals[nominal] |= true;
//    findNominalsAndOperators(nominal->getMembers());
//  }
//}
//
// bool CollectedDeclarations::extendedTypeIsPrivate(const TypeLoc
// inheritedType) {
//  auto type = inheritedType.getType();
//  if (!type)
//    return true;
//
//  if (!type->isExistentialType()) {
//    // Be conservative. We don't know how to deal with other extended types.
//    return false;
//  }
//
//  auto layout = type->getExistentialLayout();
//  assert(!layout.explicitSuperclass && "Should not have a subclass existential
//  "
//         "in the inheritance clause of an extension");
//  for (auto protoTy : layout.getProtocols()) {
//    if (!declIsPrivate(protoTy->getDecl()))
//      return false;
//  }
//
//  return true;
//}
//
// bool CollectedDeclarations::declIsPrivate(const Decl *const member) {
//  auto *VD = dyn_cast<ValueDecl>(member);
//  if (!VD) {
//    switch (member->getKind()) {
//      case DeclKind::Import:
//      case DeclKind::PatternBinding:
//      case DeclKind::EnumCase:
//      case DeclKind::TopLevelCode:
//      case DeclKind::IfConfig:
//      case DeclKind::PoundDiagnostic:
//        return true;
//
//      case DeclKind::Extension:
//      case DeclKind::InfixOperator:
//      case DeclKind::PrefixOperator:
//      case DeclKind::PostfixOperator:
//        return false;
//
//      default:
//        llvm_unreachable("everything else is a ValueDecl");
//    }
//  }
//
//  return !isVisibleOutsideItsFile(VD);
//}
//
// void CollectedDeclarations::collectMembers() {
//  for (auto entry: extendedNominals)
//    collectExtendedNominal(entry.first, entry.second);
//  for (auto *ED: extensionsWithJustMembers)
//    collectExtensionWithJustMembers(ED);
//}
//
// void CollectedDeclarations::collectClassMembers(const SourceFile *const SF) {
//  class NameCollector : public VisibleDeclConsumer {
//  private:
//    std::vector<const ValueDecl*> &classMembers;
//
//  public:
//    NameCollector(std::vector<const ValueDecl*> &classMembers)
//    : classMembers(classMembers) {}
//
//    void foundDecl(ValueDecl *VD, DeclVisibilityKind) override {
//      classMembers.push_back(VD);
//    }
//  };
//  NameCollector collector(classMembers);
//  SF->lookupClassMembers({}, collector);
//}
//
// void CollectedDeclarations::collectExtendedNominal(const NominalTypeDecl
// *const NTD, bool changesShape) {
//  holdersAndMembers.push_back(std::make_pair(NTD, std::vector<const
//  ValueDecl*>()));
//}
//
// void CollectedDeclarations::collectExtensionWithJustMembers(const
// ExtensionDecl *const ED) {
//  std::vector<const ValueDecl *>members;
//  for (auto *member : ED->getMembers()) {
//    auto *VD = dyn_cast<ValueDecl>(member);
//    if (!VD || !VD->hasName() || !isVisibleOutsideItsFile(VD))
//      continue;
//    members.push_back(VD);
//  }
//  holdersAndMembers.push_back(std::make_pair(ED->getExtendedNominal(),
//  members));
//}
