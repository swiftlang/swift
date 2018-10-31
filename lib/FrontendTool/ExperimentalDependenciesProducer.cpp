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
    const std::vector<const PrecedenceGroupDecl *> precedenceGroups;
    const std::vector<const NominalTypeDecl *> topLevelVisibleNominals;
    const std::vector<const ValueDecl *> topLevelVisibleValues;
    const std::vector<const OperatorDecl *> topLevelOperators;
    const std::vector<const FuncDecl *> nestedOperators;

    /// Records every nominal declaration, and whether or not the declaration
    /// changes the externally-observable shape of the type.
    llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
    const std::vector<std::pair<const NominalTypeDecl *, const ValueDecl *>>
        holdersAndMembers;

    const std::vector<const ValueDecl *> classMembers;

  public:
    ProviderDeclarations(const SourceFile *SF);

  private:
    static std::vector<const ValueDecl *> getClassMembers(const SourceFile *);

    static llvm::MapVector<const NominalTypeDecl *, bool>
    getExtendedNominals(const SourceFile *);

    static std::vector<std::pair<const NominalTypeDecl *, const ValueDecl *>>
    getHoldersAndMembers(const SourceFile *);

    static std::vector<const ExtensionDecl *>
    getExtensionsWithJustMembers(const SourceFile *);

    static std::vector<const ExtensionDecl *>
    getTopLevelVisibleExtensions(const SourceFile *SF);

    static std::vector<const NominalTypeDecl *>
    getTopLevelVisibleNominals(const SourceFile *SF);

    static std::vector<const ValueDecl *>
    getTopLevelVisibleValues(const SourceFile *SF);

    static std::vector<const OperatorDecl *>
    getTopLevelOperators(const SourceFile *SF);

    static std::vector<const FuncDecl *>
    getNestedOperators(const SourceFile *SF);

    static void addExtendedNominalMembers(
        const DeclRange members,
        llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals);

    static void
    addNestedOperators(const DeclRange members,
                       std::vector<const FuncDecl *> &nestedOperators);

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
    const std::vector<std::string> topLevelNames;
    const std::vector<std::string>
        extendedNominalContextualNamesThatCouldAddMembers;
    const std::vector<std::pair<std::string, std::string>>
        holdersAndMaybeMembers;
    const std::vector<std::string> dynamicLookupNames;

  private:
    static std::vector<std::string>
    getTopLevelNames(const ProviderDeclarations &);

    static std::vector<std::string>
    getExtendedNominalContextualNames(const ProviderDeclarations &,
                                      bool onlyCouldAddMembers);

    static std::vector<std::pair<std::string, std::string>>
    getHoldersAndMembers(const ProviderDeclarations &);

    static std::vector<std::pair<std::string, std::string>>
    getHoldersAndMaybeMembers(const ProviderDeclarations &);

    static std::vector<std::string>
    getDynamicLookupNames(const ProviderDeclarations &);

    static std::string getContextualName(const NominalTypeDecl *const NTD) {
      Mangle::ASTMangler Mangler;
      return Mangler.mangleTypeAsContextUSR(NTD);
    }
    
    template<typename DeclT> static void addTopLevelNamesFromDecls(std::vector<std::string>::iterator,
                                                                   const std::vector<const DeclT*>&
                                                                   );

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

    void emitSectionOfIndividualNames(StringRef sectionKey,
                                      ArrayRef<std::string> names) const;
    void emitSectionOfNamePairs(
        StringRef sectionKey,
        ArrayRef<std::pair<std::string, std::string>> pairs) const;

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

void SQ::Provides::emitTopLevelNames() const {
  emitSectionOfIndividualNames(reference_dependency_keys::providesTopLevel,
                               pns.topLevelNames);
}

void SQ::Provides::emitNominalTypes() const {
  emitSectionOfIndividualNames(
      reference_dependency_keys::providesNominal,
      pns.extendedNominalContextualNamesThatCouldAddMembers);
}

void SQ::Provides::emitMembers() const {
  emitSectionOfNamePairs(reference_dependency_keys::providesMember,
                         pns.holdersAndMaybeMembers);
}

void SQ::Provides::emitDynamicLookupMembers() const {
  emitSectionOfIndividualNames(reference_dependency_keys::providesDynamicLookup,
                               pns.dynamicLookupNames);
}
void SQ::Provides::emitInterfaceHash() const {
  emitter.emitSingleValueSection(reference_dependency_keys::interfaceHash,
                                 interfaceHash.contents);
}

void SQ::Provides::emitSectionOfIndividualNames(
    StringRef sectionKey, ArrayRef<std::string> names) const {
  emitter.emitSectionStart(sectionKey);
  for (StringRef n : names)
    emitter.emitName(n);
}

void SQ::Provides::emitSectionOfNamePairs(
    StringRef sectionKey,
    ArrayRef<std::pair<std::string, std::string>> pairs) const {
  emitter.emitSectionStart(sectionKey);
  for (std::pair<StringRef, StringRef> p : pairs)
    emitter.emitDoubleNameLine(p.first, p.second);
}

SQ::ProviderDeclarations::ProviderDeclarations(const SourceFile *SF)
    : precedenceGroups(
          getTopLevelDecls<DeclKind::PrecedenceGroup, PrecedenceGroupDecl>(SF)),
      topLevelVisibleNominals(getTopLevelVisibleNominals(SF)),
      topLevelVisibleValues(getTopLevelVisibleValues(SF)),
      topLevelOperators(getTopLevelOperators(SF)),
      nestedOperators(getNestedOperators(SF)),
      extendedNominals(getExtendedNominals(SF)),
      holdersAndMembers(getHoldersAndMembers(SF)),
      classMembers(getClassMembers(SF)) {}

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

std::vector<const ValueDecl *>
SQ::ProviderDeclarations::getTopLevelVisibleValues(const SourceFile *SF) {
  std::vector<const ValueDecl *> values;
  for (const Decl *const D : SF->Decls)
    switch (D->getKind()) {
    default:
      break;

    case DeclKind::TypeAlias:
    case DeclKind::Var:
    case DeclKind::Func:
    case DeclKind::Accessor: {
      const ValueDecl *const VD = cast<ValueDecl>(D);
      if (VD->hasName() && isVisibleOutsideItsFile(VD))
        values.push_back(VD);
      break;
    }
    }
  return values;
}

std::vector<const OperatorDecl *>
SQ::ProviderDeclarations::getTopLevelOperators(const SourceFile *SF) {
  std::vector<const OperatorDecl *> operators;
  for (const Decl *const D : SF->Decls)
    switch (D->getKind()) {
    default:
      break;

    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      operators.push_back(cast<OperatorDecl>(D));
      break;
    }
  return operators;
}

std::vector<const FuncDecl *>
SQ::ProviderDeclarations::getNestedOperators(const SourceFile *SF) {
  std::vector<const FuncDecl *> nestedOps;
  for (const ExtensionDecl *const ED : getTopLevelVisibleExtensions(SF))
    addNestedOperators(ED->getMembers(), nestedOps);
  for (const NominalTypeDecl *const NTD : getTopLevelVisibleNominals(SF))
    addNestedOperators(NTD->getMembers(), nestedOps);
  return nestedOps;
}

llvm::MapVector<const NominalTypeDecl *, bool>
SQ::ProviderDeclarations::getExtendedNominals(const SourceFile *SF) {
  llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
  for (const ExtensionDecl *const ED : getTopLevelVisibleExtensions(SF)) {
    const bool extendsVisibleTypes =
        areAnyExtendedTypesVisibleOutsideThisFile(ED);
    if (!extendsVisibleTypes && !areAnyMembersVisibleOutsideThisFile(ED))
      continue;
    extendedNominals[ED->getExtendedNominal()] |= extendsVisibleTypes;
    addExtendedNominalMembers(ED->getMembers(), extendedNominals);
  }
  for (const NominalTypeDecl *const NTD : getTopLevelVisibleNominals(SF)) {
    extendedNominals[NTD] |= true;
    addExtendedNominalMembers(NTD->getMembers(), extendedNominals);
  }
  return extendedNominals;
}

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

void SQ::ProviderDeclarations::addExtendedNominalMembers(
    const DeclRange members,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals) {
  for (const Decl *D : members) {
    if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      extendedNominals[NTD] |= true;
      addExtendedNominalMembers(NTD->getMembers(), extendedNominals);
    }
  }
}

void SQ::ProviderDeclarations::addNestedOperators(
    const DeclRange members, std::vector<const FuncDecl *> &nestedOperators) {
  for (const Decl *D : members) {
    if (const ValueDecl *const VD = dyn_cast<ValueDecl>(D)) {
      if (isVisibleOutsideItsFile(VD) && VD->getFullName().isOperator()) {
        nestedOperators.push_back(cast<FuncDecl>(VD));
        continue;
      }
    }
    if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      addNestedOperators(NTD->getMembers(), nestedOperators);
    }
  }
}

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
    : topLevelNames(getTopLevelNames(pd)),
      extendedNominalContextualNamesThatCouldAddMembers(
          getExtendedNominalContextualNames(pd, true)),
      holdersAndMaybeMembers(getHoldersAndMaybeMembers(pd)),
      dynamicLookupNames(getDynamicLookupNames(pd)) {}

template<>
void SQ::ProviderNames::addTopLevelNamesFromDecls<ValueDecl>(std::vector<std::string>::iterator dest,
                                                             const std::vector<const ValueDecl*> &decls) {
  for (const ValueDecl *const D : decls)
    *dest++ = DeclBaseName(D->getBaseName()).userFacingName();
}

template<typename DeclT>
void SQ::ProviderNames::addTopLevelNamesFromDecls(std::vector<std::string>::iterator dest,
                                                  const std::vector<const DeclT*> &decls
                                                  ) {
  for (const DeclT *const D : decls)
    *dest++ = DeclBaseName(D->getName()).userFacingName();
}

std::vector<std::string>
SQ::ProviderNames::getTopLevelNames(const ProviderDeclarations &pd) {
  std::vector<std::string> tops;
  addTopLevelNamesFromDecls(tops.end(), pd.precedenceGroups);
  addTopLevelNamesFromDecls(tops.end(), pd.topLevelVisibleNominals);
  addTopLevelNamesFromDecls(tops.end(), pd.topLevelVisibleValues);
  addTopLevelNamesFromDecls(tops.end(), pd.topLevelOperators);
  addTopLevelNamesFromDecls(tops.end(), pd.nestedOperators);

  return tops;
}





std::vector<std::string> SQ::ProviderNames::getExtendedNominalContextualNames(
    const ProviderDeclarations &pd, const bool onlyIfCouldAddMembers) {
  std::vector<std::string> extendedNominalContextualNames;
  for (const auto &p : pd.extendedNominals) {
    if (!onlyIfCouldAddMembers || p.second)
      extendedNominalContextualNames.push_back(getContextualName(p.first));
  }
  return extendedNominalContextualNames;
}

std::vector<std::pair<std::string, std::string>>
SQ::ProviderNames::getHoldersAndMaybeMembers(const ProviderDeclarations &pd) {
  std::vector<std::pair<std::string, std::string>> holdersAndMaybeMembers;
  for (std::string n : getExtendedNominalContextualNames(pd, false))
    holdersAndMaybeMembers.push_back(std::make_pair(n, ""));
  for (auto p : getHoldersAndMembers(pd))
    holdersAndMaybeMembers.push_back(p);
  return holdersAndMaybeMembers;
}

std::vector<std::pair<std::string, std::string>>
SQ::ProviderNames::getHoldersAndMembers(const ProviderDeclarations &pd) {
  std::vector<std::pair<std::string, std::string>> holdersAndMembers;
  
  for (auto p : pd.holdersAndMembers)
    holdersAndMembers.push_back(std::make_pair(
        getContextualName(p.first), p.second->getBaseName().userFacingName()));
  
//  std::transform(pd.holdersAndMembers.begin(), pd.holdersAndMembers.end(),
//                 holdersAndMembers.begin(),
//                 [](std::pair<const NominalTypeDecl *, const ValueDecl *> &p)
//                 -> std::pair<std::string, std::string> {
//                   return std::make_pair(
//                                         getContextualName(p.first),
//                                         p.second->getBaseName().userFacingName()); } );
  
  return holdersAndMembers;
}

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

SQ::Depends::Depends(const SourceFile *const SF,
                     const DependencyTracker &depTracker, YAMLEmitter &emitter)
    : emitter(emitter) {}

void SQ::Depends::emit() const {
  (void)emitter;
  abort();
}

namespace SQ2 {
  class Subsection {};
  class TopLevelPrecedenceGroups: Subsection {};
  class TopLevelVisibleNominals: Subsection {};
  class TopLevelVisibleValues: Subsection {};
  class TopLevelNestOperators: Subsection {};
  class NominalTypeExtendedNominalContextualNames: Subsection {};
  class MemberExtendedNominalContextualNames: Subsection {};
  class MemberHoldersAndMembers: Subsection {};
  class DynamicLookupNames: Subsection {};
  class InterfaceHash: Subsection {};
  
  template <typename SubsectionT, typename InputT, typename OutputT> class Stage {
  private:
    const InputT input;
    std::vector<OutputT> _output;
    void computeOutput() = 0;
  public:
    Stage(const InputT input): input(input) {
      computeOutput();
    }
    std::vector<OutputT> output() const { return _output; }
    typename std::vector<OutputT>::const_iterator begin() { return output().begin(); }
    typename std::vector<OutputT>::const_iterator end() { return output().end(); }
 };
  
  template <typename SubsectionT, typename DeclT>
  class DeclarationGatherer:
  Stage<SubsectionT, const SourceFile*, std::forward_iterator_tag> {};
  
  template <typename SubsectionT> class Namer: Stage<SubsectionT> {};
  
  
  
}
