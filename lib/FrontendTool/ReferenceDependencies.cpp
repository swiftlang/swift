//===--- ReferenceDependencies.cpp - Generates swiftdeps files ------------===//
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

#include "ReferenceDependencies.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
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
using namespace reference_dependency_keys;

namespace {
/// Emits the reference dependencies from the frontend so that the driver
/// can compute a dependency graph for the whole module, and use it to decide
/// which files need to be recompiled when doing incremental compilation.
class ReferenceDependenciesEmitter {
  SourceFile *const SF;
  const DependencyTracker &depTracker;
  llvm::raw_ostream &out;

  ReferenceDependenciesEmitter(SourceFile *const SF,
                               const DependencyTracker &depTracker,
                               llvm::raw_ostream &out)
      : SF(SF), depTracker(depTracker), out(out) {}

public:
  /// Emits the provided and depended-upon dependencies to a file
  ///
  /// \param diags Where problems opening the file are emitted
  /// \param SF The SourceFile containing the code with the dependences
  /// \param depTracker The entities depended-upon
  /// \param outputPath Where the dependencies are written
  ///
  /// \return true on error
  static bool emit(DiagnosticEngine &diags, SourceFile *SF,
                   const DependencyTracker &depTracker, StringRef outputPath);

  /// Emit the dependencies.
  static void emit(SourceFile *SF, const DependencyTracker &depTracker,
                   llvm::raw_ostream &out);

private:
  /// Emits all the dependency information.
  void emit() const;

  void emitProvides() const;
  void emitDepends() const;
  void emitInterfaceHash() const;
};

/// Emits the declarations provided by a source file.
class ProvidesEmitter {
  const SourceFile *const SF;
  const bool EnableExperimentalDependencies;
  llvm::raw_ostream &out;

  ProvidesEmitter(const SourceFile *const SF, llvm::raw_ostream &out)
      : SF(SF),
        EnableExperimentalDependencies(SF->getEnableExperimentalDependencies()),
        out(out) {}

public:
  static void emit(const SourceFile *SF, llvm::raw_ostream &out);

private:
  /// Aggregates declarations which are collected first and emitted later.
  struct CollectedDeclarations {
    /// Records every nominal declaration, and whether or not the declaration
    /// changes the externally-observable shape of the type.
    llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;

    /// Records operator declarations so they can be included as top-level
    /// declarations.
    llvm::SmallVector<const FuncDecl *, 8> memberOperatorDecls;

    /// Records extension declarations which are not introducing a conformance
    /// to a public protocol and add a public member.
    llvm::SmallVector<const ExtensionDecl *, 8> extensionsWithJustMembers;

    /// Recursively computes the transitive closure over members
    /// adding memberOperatorDecls and extendedNominals to the receiver.
    void findNominalsAndOperators(const DeclRange members);
  };

  /// Emit all provided declartions.
  void emit() const;

  CollectedDeclarations emitTopLevelNames() const;
  void emitNominalTypes(const llvm::MapVector<const NominalTypeDecl *, bool>
                            &extendedNominals) const;
  void emitMembers(const CollectedDeclarations &cpd) const;
  void emitDynamicLookupMembers() const;

  void emitTopLevelDecl(const Decl *D, CollectedDeclarations &cpd) const;
  void emitExtensionDecl(const ExtensionDecl *D,
                         CollectedDeclarations &cpd) const;
  void emitNominalTypeDecl(const NominalTypeDecl *NTD,
                           CollectedDeclarations &cpd) const;
  void emitValueDecl(const ValueDecl *VD) const;

  static bool extendedTypeIsPrivate(TypeLoc inheritedType);
  static bool declIsPrivate(const Decl *member);

  void emitNormalOrExperimentalTopLevelDecl(const DeclBaseName &, const Decl *) const;
  void emitNameAndTopLevelDeclHash(StringRef name, const Decl *) const;
  
  template<ExperimentalDependencies::ProvidesKind, typename DeclT>
  std::string nameAndMaybeProvidesHash(StringRef name, const DeclT*) const;
  
  std::pair<std::string, std::string>
  namesAndMaybeProvidesHashes(const NominalTypeDecl *holder, const ValueDecl *member) const;
};

/// Emit the depended-upon declartions.
class DependsEmitter {
  /// The file that dependes upon the declarations.
  const SourceFile *const SF;
  /// The dependencies collected by the compiler.
  const DependencyTracker &depTracker;

  llvm::raw_ostream &out;

  DependsEmitter(const SourceFile *SF, const DependencyTracker &depTracker,
                 llvm::raw_ostream &out)
      : SF(SF), depTracker(depTracker), out(out) {}

public:
  /// A NominalTypeDecl, its DeclBaseName, and whether it is externally-visible.
  using MemberTableEntryTy = std::pair<ReferencedNameTracker::MemberPair, bool>;

  /// Emit the dependencies
  ///
  /// \param SF SourceFile containing the dependent code
  /// \param depTracker Contains the dependencies found during compilation
  /// \param out Where the dependencies are emitted
  static void emit(const SourceFile *SF, const DependencyTracker &depTracker,
                   llvm::raw_ostream &out);

private:
  /// Emit all the dependencies.
  void emit() const;

  void emitTopLevelNames(const ReferencedNameTracker *const tracker) const;
  void emitMembers(const ArrayRef<MemberTableEntryTy> sortedMembers) const;
  void emitNominalTypes(const ArrayRef<MemberTableEntryTy> sortedMembers) const;
  void emitDynamicLookup(const ReferencedNameTracker *const tracker) const;
  void emitExternal(const DependencyTracker &depTracker) const;

  static SmallVector<std::pair<DeclBaseName, bool>, 16>
  sortedByName(const llvm::DenseMap<DeclBaseName, bool> map);
};
} // namespace

static std::string mangleTypeAsContext(const NominalTypeDecl *type) {
  Mangle::ASTMangler Mangler;
  return Mangler.mangleTypeAsContextUSR(type);
}

std::vector<std::string>
swift::reversePathSortedFilenames(const ArrayRef<std::string> elts) {
  std::vector<std::string> tmp(elts.begin(), elts.end());
  std::sort(tmp.begin(), tmp.end(), [](const std::string &a,
                                       const std::string &b) -> bool {
              return std::lexicographical_compare(a.rbegin(), a.rend(),
                                                  b.rbegin(), b.rend());
            });
  return tmp;
}

static std::string escape(DeclBaseName name) {
  return llvm::yaml::escape(name.userFacingName());
}

bool ReferenceDependenciesEmitter::emit(DiagnosticEngine &diags,
                                        SourceFile *const SF,
                                        const DependencyTracker &depTracker,
                                        StringRef outputPath) {
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  std::error_code EC =
      swift::atomicallyWritingToFile(outputPath,
                                     [&](llvm::raw_pwrite_stream &out) {
    ReferenceDependenciesEmitter::emit(SF, depTracker, out);
  });
  if (EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                   EC.message());
    return true;
  }
  return false;
}

void ReferenceDependenciesEmitter::emit(SourceFile *SF,
                                        const DependencyTracker &depTracker,
                                        llvm::raw_ostream &out) {
  ReferenceDependenciesEmitter(SF, depTracker, out).emit();
}

void ReferenceDependenciesEmitter::emit() const {
  assert(SF && "Cannot emit reference dependencies without a SourceFile");
  out << "### Swift dependencies file v0 ###\n";
  emitProvides();
  emitDepends();
  emitInterfaceHash();
}

bool swift::emitReferenceDependencies(DiagnosticEngine &diags, SourceFile *SF,
                                      const DependencyTracker &depTracker,
                                      StringRef outputPath) {
  return ReferenceDependenciesEmitter::emit(diags, SF, depTracker, outputPath);
}

void ProvidesEmitter::emit() const {
  CollectedDeclarations cpd = emitTopLevelNames();
  emitNominalTypes(cpd.extendedNominals);
  emitMembers(cpd);
  emitDynamicLookupMembers();
}

void ProvidesEmitter::emit(const SourceFile *SF, llvm::raw_ostream &out) {
  ProvidesEmitter(SF, out).emit();
}

void ReferenceDependenciesEmitter::emitProvides() const {
  ProvidesEmitter::emit(SF, out);
}

void ReferenceDependenciesEmitter::emitDepends() const {
  DependsEmitter::emit(SF, depTracker, out);
}

void ReferenceDependenciesEmitter::emitInterfaceHash() const {
  llvm::SmallString<32> interfaceHash;
  SF->getInterfaceHash(interfaceHash);
  out << reference_dependency_keys::interfaceHash << ": \"" << interfaceHash
  << "\"\n";
}

ProvidesEmitter::CollectedDeclarations
ProvidesEmitter::emitTopLevelNames() const {
  out << providesTopLevel << ":\n";

  CollectedDeclarations cpd;
  for (const Decl *D : SF->Decls)
    emitTopLevelDecl(D, cpd);
  for (auto *operatorFunction : cpd.memberOperatorDecls)
    emitNormalOrExperimentalTopLevelDecl(operatorFunction->getName(), operatorFunction);
  return cpd;
}

void ProvidesEmitter::emitTopLevelDecl(const Decl *const D,
                                       CollectedDeclarations &cpd) const {

  switch (D->getKind()) {
  case DeclKind::Module:
    break;

  case DeclKind::Import:
    // FIXME: Handle re-exported decls.
    break;

  case DeclKind::Extension:
    emitExtensionDecl(cast<ExtensionDecl>(D), cpd);
    break;

  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    emitNormalOrExperimentalTopLevelDecl(cast<OperatorDecl>(D)->getName(),
                             cast<OperatorDecl>(D));
    break;

  case DeclKind::PrecedenceGroup:
    emitNormalOrExperimentalTopLevelDecl(cast<PrecedenceGroupDecl>(D)->getName(),
                             cast<PrecedenceGroupDecl>(D));
    break;

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
    emitNominalTypeDecl(cast<NominalTypeDecl>(D), cpd);
    break;

  case DeclKind::TypeAlias:
  case DeclKind::Var:
  case DeclKind::Func:
  case DeclKind::Accessor:
    emitValueDecl(cast<ValueDecl>(D));
    break;

  case DeclKind::PatternBinding:
  case DeclKind::TopLevelCode:
  case DeclKind::IfConfig:
  case DeclKind::PoundDiagnostic:
    // No action necessary.
    break;

  case DeclKind::EnumCase:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Param:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::EnumElement:
  case DeclKind::MissingMember:
    // These can occur in malformed ASTs.
    break;
  }
}

void ProvidesEmitter::emitNameAndTopLevelDeclHash(StringRef name, const Decl *D) const {
  std::string stringToEmit = nameAndMaybeProvidesHash<ExperimentalDependencies::ProvidesKind::topLevel, Decl>(name, D);
  out << "- \""
  << llvm::yaml::escape(stringToEmit)
  << "\"\n";
}



void ProvidesEmitter::emitNormalOrExperimentalTopLevelDecl(const DeclBaseName &N,
                                               const Decl *D) const {
  emitNameAndTopLevelDeclHash(N.userFacingName(), D);
}

template<ExperimentalDependencies::ProvidesKind kind, typename DeclT>
std::string ProvidesEmitter::nameAndMaybeProvidesHash(StringRef name, const DeclT* D) const {
  return !EnableExperimentalDependencies
  ? name.str()
  : ExperimentalDependencies::getCombinedNameAndProvidesHash<kind>(name, D);
}

void ProvidesEmitter::emitExtensionDecl(const ExtensionDecl *const ED,
                                        CollectedDeclarations &cpd) const {
  auto *NTD = ED->getExtendedNominal();
  if (!NTD)
    return;
  if (NTD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }

  // Check if the extension is just adding members, or if it is
  // introducing a conformance to a public protocol.
  bool justMembers =
      std::all_of(ED->getInherited().begin(), ED->getInherited().end(),
                  extendedTypeIsPrivate);
  if (justMembers) {
    if (std::all_of(ED->getMembers().begin(), ED->getMembers().end(),
                    declIsPrivate)) {
      return;
    }
    cpd.extensionsWithJustMembers.push_back(ED);
  }
  cpd.extendedNominals[NTD] |= !justMembers;
  cpd.findNominalsAndOperators(ED->getMembers());
}

void ProvidesEmitter::emitNominalTypeDecl(const NominalTypeDecl *const NTD,
                                          CollectedDeclarations &cpd) const {
  if (!NTD->hasName())
    return;
  if (NTD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }
  emitNormalOrExperimentalTopLevelDecl(NTD->getName(), NTD);

  cpd.extendedNominals[NTD] |= true;
  cpd.findNominalsAndOperators(NTD->getMembers());
}

void ProvidesEmitter::CollectedDeclarations::findNominalsAndOperators(
    const DeclRange members) {
  for (const Decl *D : members) {
    auto *VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (VD->getFormalAccess() <= AccessLevel::FilePrivate) {
      continue;
    }

    if (VD->getFullName().isOperator()) {
      memberOperatorDecls.push_back(cast<FuncDecl>(VD));
      continue;
    }

    auto nominal = dyn_cast<NominalTypeDecl>(D);
    if (!nominal)
      continue;
    extendedNominals[nominal] |= true;
    findNominalsAndOperators(nominal->getMembers());
  }
}

void ProvidesEmitter::emitValueDecl(const ValueDecl *const VD) const {
  if (!VD->hasName())
    return;
  if (VD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }
  emitNormalOrExperimentalTopLevelDecl(VD->getBaseName(), VD);
}

void ProvidesEmitter::emitNominalTypes(
     const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals)
const {
  out << providesNominal << ":\n";
  for (auto entry : extendedNominals) {
    if (!entry.second)
      continue;

    std::string toEmit = nameAndMaybeProvidesHash<ExperimentalDependencies::ProvidesKind::nominal, NominalTypeDecl>(mangleTypeAsContext(entry.first), entry.first);
    out << "- \"" << llvm::yaml::escape(toEmit) << "\"\n";
  }
}

void ProvidesEmitter::emitMembers(const CollectedDeclarations &cpd) const {
  typedef std::pair<std::string, std::string> Names;
  out << providesMember << ":\n";
  for (auto entry : cpd.extendedNominals) {
    Names names = namesAndMaybeProvidesHashes(entry.first, nullptr);
    out << "- [\"";
    out << llvm::yaml::escape(names.first);
    out << "\", \"" << llvm::yaml::escape(names.second) << "\"]\n";
  }

  // This is also part of providesMember.
  for (auto *ED : cpd.extensionsWithJustMembers) {
    for (auto *member : ED->getMembers()) {
      auto *VD = dyn_cast<ValueDecl>(member);
      if (!VD || !VD->hasName() ||
          VD->getFormalAccess() <= AccessLevel::FilePrivate) {
        continue;
      }
      Names names = namesAndMaybeProvidesHashes(ED->getExtendedNominal(), VD);
      out << "- [\"" << llvm::yaml::escape(names.first) << "\", \""
      << llvm::yaml::escape(names.second)
      << "\"]\n";
    }
  }
}

std::pair<std::string, std::string>
ProvidesEmitter::namesAndMaybeProvidesHashes(const NominalTypeDecl *extendedDecl, const ValueDecl *member) const {
  const std::string holderName = llvm::yaml::escape(mangleTypeAsContext(extendedDecl));
  const std::string memberName = llvm::yaml::escape(
                                              !member
                                              ? std::string()
                                              : member->getBaseName().userFacingName().str()
                                              );
  return !EnableExperimentalDependencies
  ? std::make_pair(holderName, memberName)
  : std::make_pair(
                   ExperimentalDependencies::getCombinedNameAndProvidesHash<ExperimentalDependencies::ProvidesKind::memberHolder, NominalTypeDecl>(holderName, extendedDecl),
                   ExperimentalDependencies::getCombinedNameAndProvidesHash<ExperimentalDependencies::ProvidesKind::member, ValueDecl>(memberName, member));
}

void ProvidesEmitter::emitDynamicLookupMembers() const {
  if (SF->getASTContext().LangOpts.EnableObjCInterop) {
    // FIXME: This requires a traversal of the whole file to compute.
    // We should (a) see if there's a cheaper way to keep it up to date,
    // and/or (b) see if we can fast-path cases where there's no ObjC
    // involved.
    out << providesDynamicLookup << ":\n";
 
    class NameCollector : public VisibleDeclConsumer {
    private:
      const ProvidesEmitter &e;
      SmallVector<std::string, 16> stringsToEmit;

    public:
      NameCollector(const ProvidesEmitter &e) : e(e) {}
      
      void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
        std::string s = e.nameAndMaybeProvidesHash<
           ExperimentalDependencies::ProvidesKind::dynamicLookup,
           ValueDecl
        >(VD->getBaseName().userFacingName(), VD);
        stringsToEmit.push_back(s);
      }
      ArrayRef<std::string> getStringsToEmit() {
        llvm::array_pod_sort(
            stringsToEmit.begin(), stringsToEmit.end(),
                             [](const std::string *lhs, const std::string *rhs) {
                               return ExperimentalDependencies::CompoundProvides(*lhs).name .compare(ExperimentalDependencies::CompoundProvides(*rhs).name);
            });
        stringsToEmit.erase(std::unique(stringsToEmit.begin(), stringsToEmit.end()), stringsToEmit.end());
        return stringsToEmit;
      }
    };
    NameCollector collector(*this);
    SF->lookupClassMembers({}, collector);
    for (std::string s : collector.getStringsToEmit()) {
      out << "- \"" << llvm::yaml::escape(s) << "\"\n";
    }
  }
}

bool ProvidesEmitter::extendedTypeIsPrivate(TypeLoc inheritedType) {
  auto type = inheritedType.getType();
  if (!type)
    return true;

  if (!type->isExistentialType()) {
    // Be conservative. We don't know how to deal with other extended types.
    return false;
  }

  auto layout = type->getExistentialLayout();
  assert(!layout.explicitSuperclass && "Should not have a subclass existential "
                                       "in the inheritance clause of an extension");
  for (auto protoTy : layout.getProtocols()) {
    if (!declIsPrivate(protoTy->getDecl()))
      return false;
  }

  return true;
}

bool ProvidesEmitter::declIsPrivate(const Decl *member) {
  auto *VD = dyn_cast<ValueDecl>(member);
  if (!VD) {
    switch (member->getKind()) {
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

  return VD->getFormalAccess() <= AccessLevel::FilePrivate;
}

void DependsEmitter::emit(const SourceFile *SF,
                          const DependencyTracker &depTracker,
                          llvm::raw_ostream &out) {
  DependsEmitter(SF, depTracker, out).emit();
}

void DependsEmitter::emit() const {
  const ReferencedNameTracker *const tracker = SF->getReferencedNameTracker();
  assert(tracker && "Cannot emit reference dependencies without a tracker");

  emitTopLevelNames(tracker);

  auto &memberLookupTable = tracker->getUsedMembers();
  std::vector<MemberTableEntryTy> sortedMembers{
    memberLookupTable.begin(), memberLookupTable.end()
  };
  llvm::array_pod_sort(sortedMembers.begin(), sortedMembers.end(),
                       [](const MemberTableEntryTy *lhs,
                          const MemberTableEntryTy *rhs) -> int {
    if (auto cmp = lhs->first.first->getName().compare(rhs->first.first->getName()))
      return cmp;

    if (auto cmp = lhs->first.second.compare(rhs->first.second))
      return cmp;

    // We can have two entries with the same member name if one of them
    // was the special 'init' name and the other is the plain 'init' token.
    if (lhs->second != rhs->second)
      return lhs->second ? -1 : 1;

    // Break type name ties by mangled name.
    auto lhsMangledName = mangleTypeAsContext(lhs->first.first);
    auto rhsMangledName = mangleTypeAsContext(rhs->first.first);
    return lhsMangledName.compare(rhsMangledName);
  });

  emitMembers(sortedMembers);
  emitNominalTypes(sortedMembers);
  emitDynamicLookup(tracker);
  emitExternal(depTracker);
}

void DependsEmitter::emitTopLevelNames(
    const ReferencedNameTracker *const tracker) const {
  out << dependsTopLevel << ":\n";
  for (auto &entry : sortedByName(tracker->getTopLevelNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }
}

void DependsEmitter::emitMembers(
    ArrayRef<MemberTableEntryTy> sortedMembers) const {
  out << dependsMember << ":\n";
  for (auto &entry : sortedMembers) {
    assert(entry.first.first != nullptr);
    if (entry.first.first->getFormalAccess() <= AccessLevel::FilePrivate)
      continue;

    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "[\"";
    out << mangleTypeAsContext(entry.first.first);
    out << "\", \"";
    if (!entry.first.second.empty())
      out << escape(entry.first.second);
    out << "\"]\n";
  }
}

void DependsEmitter::emitNominalTypes(
    ArrayRef<MemberTableEntryTy> sortedMembers) const {
  out << dependsNominal << ":\n";
  for (auto i = sortedMembers.begin(), e = sortedMembers.end(); i != e; ++i) {
    bool isCascading = i->second;
    while (i+1 != e && i[0].first.first == i[1].first.first) {
      ++i;
      isCascading |= i->second;
    }

    if (i->first.first->getFormalAccess() <= AccessLevel::FilePrivate)
      continue;

    out << "- ";
    if (!isCascading)
      out << "!private ";
    out << "\"";
    out <<  mangleTypeAsContext(i->first.first);
    out << "\"\n";
  }
}

void DependsEmitter::emitDynamicLookup(
    const ReferencedNameTracker *const tracker) const {
  out << dependsDynamicLookup << ":\n";
  for (auto &entry : sortedByName(tracker->getDynamicLookupNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }
}

void DependsEmitter::emitExternal(const DependencyTracker &depTracker) const {
  out << dependsExternal << ":\n";
  for (auto &entry : reversePathSortedFilenames(depTracker.getDependencies())) {
    out << "- \"" << llvm::yaml::escape(entry) << "\"\n";
  }
}

SmallVector<std::pair<DeclBaseName, bool>, 16>
DependsEmitter::sortedByName(const llvm::DenseMap<DeclBaseName, bool> map) {
  SmallVector<std::pair<DeclBaseName, bool>, 16> pairs{map.begin(), map.end()};
  llvm::array_pod_sort(pairs.begin(), pairs.end(),
                       [](const std::pair<DeclBaseName, bool> *first,
                          const std::pair<DeclBaseName, bool> *second) -> int {
                         return first->first.compare(second->first);
                       });
  return pairs;
}
