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
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;

static void findNominalsAndOperators(
    llvm::MapVector<const NominalTypeDecl *, bool> &foundNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &foundOperators,
    DeclRange members) {
  for (const Decl *D : members) {
    auto *VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (VD->hasAccess() &&
        VD->getFormalAccess() <= AccessLevel::FilePrivate) {
      continue;
    }

    if (VD->getFullName().isOperator()) {
      foundOperators.push_back(cast<FuncDecl>(VD));
      continue;
    }

    auto nominal = dyn_cast<NominalTypeDecl>(D);
    if (!nominal)
      continue;
    foundNominals[nominal] |= true;
    findNominalsAndOperators(foundNominals, foundOperators,
                             nominal->getMembers());
  }
}

static bool declIsPrivate(const Decl *member) {
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

static bool extendedTypeIsPrivate(TypeLoc inheritedType) {
  auto type = inheritedType.getType();
  if (!type)
    return true;

  if (!type->isExistentialType()) {
    // Be conservative. We don't know how to deal with other extended types.
    return false;
  }

  auto layout = type->getExistentialLayout();
  assert(!layout.superclass && "Should not have a subclass existential "
         "in the inheritance clause of an extension");
  for (auto protoTy : layout.getProtocols()) {
    if (!declIsPrivate(protoTy->getDecl()))
      return false;
  }

  return true;
}

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

namespace {
  class ReferenceDependenciesEmitter {
    SourceFile * const SF;
    const DependencyTracker &depTracker;
    llvm::raw_ostream &out;
    
    ReferenceDependenciesEmitter(SourceFile *const SF, const DependencyTracker &depTracker, llvm::raw_ostream &out) :
    SF(SF), depTracker(depTracker), out(out) {}
    
  public:
    /// \return true on error
    static bool emit(DiagnosticEngine &diags, SourceFile *const SF,
                     const DependencyTracker &depTracker,
                     StringRef outputPath);
    static void emit(SourceFile *const SF, const DependencyTracker &depTracker, llvm::raw_ostream &out);
    
  private:
    static std::unique_ptr<llvm::raw_fd_ostream> openFile(DiagnosticEngine &diags,
                                                          StringRef OutputPath);
    void emit();
    void emitProvides();
    void emitDepends();
    void emitInterfaceHash();
  };
} // end anon namespace

std::unique_ptr<llvm::raw_fd_ostream>ReferenceDependenciesEmitter::openFile(DiagnosticEngine &diags, StringRef outputPath) {
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  
  std::error_code EC;
  auto out = llvm::make_unique<llvm::raw_fd_ostream>(outputPath, EC, llvm::sys::fs::F_None);
  
  if (out->has_error() || EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output, outputPath,
                   EC.message());
    out->clear_error();
    return nullptr;
  }
  return out;
}

bool ReferenceDependenciesEmitter::emit(DiagnosticEngine &diags,
                                        SourceFile *const SF,
                                        const DependencyTracker &depTracker,
                                        StringRef outputPath) {
  std::unique_ptr<llvm::raw_ostream> out = openFile(diags, outputPath);
  if (!out.get())
    return true;
  ReferenceDependenciesEmitter::emit(SF, depTracker, *out);
  return false;
}

void ReferenceDependenciesEmitter::emit(SourceFile *const SF,
                                        const DependencyTracker &depTracker,
                                        llvm::raw_ostream &out) {
  ReferenceDependenciesEmitter(SF, depTracker, out).emit();
}

void ReferenceDependenciesEmitter::emit() {
  assert(SF && "Cannot emit reference dependencies without a SourceFile");
  out << "### Swift dependencies file v0 ###\n";
  emitProvides();
  emitDepends();
  emitInterfaceHash();
}

bool swift::emitReferenceDependencies(DiagnosticEngine &diags,
                                      SourceFile *const SF,
                                      const DependencyTracker &depTracker,
                                      StringRef outputPath) {
  return ReferenceDependenciesEmitter::emit(diags, SF, depTracker, outputPath);
}

static void emitProvidesTopLevelNames(
    const SourceFile *SF, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const ExtensionDecl *> &extensionsWithJustMembers);

static void emitProvidesNominalTypes(
    const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::raw_ostream &out);

static void emitProvidesMembers(
    const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    const llvm::SmallVectorImpl<const ExtensionDecl *>
        &extensionsWithJustMembers,
    llvm::raw_ostream &out);

static void emitProvidesDynamicLookupMembers(const SourceFile *SF,
                                             llvm::raw_ostream &out);

void ReferenceDependenciesEmitter::emitProvides() {
  llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
  llvm::SmallVector<const ExtensionDecl *, 8> extensionsWithJustMembers;

  out << "provides-top-level:\n";

  emitProvidesTopLevelNames(SF, out, extendedNominals,
                            extensionsWithJustMembers);
  emitProvidesNominalTypes(extendedNominals, out);
  emitProvidesMembers(extendedNominals, extensionsWithJustMembers, out);
  emitProvidesDynamicLookupMembers(SF, out);
}

static void emitProvidesTopLevelDecl(
    const Decl *const D, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &memberOperatorDecls,
    llvm::SmallVectorImpl<const ExtensionDecl *> &extensionsWithJustMembers);

static void emitProvidesTopLevelNames(
    const SourceFile *const SF, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const ExtensionDecl *> &extensionsWithJustMembers) {
  llvm::SmallVector<const FuncDecl *, 8> memberOperatorDecls;

  for (const Decl *D : SF->Decls)
    emitProvidesTopLevelDecl(D, out, extendedNominals, memberOperatorDecls,
                             extensionsWithJustMembers);
  for (auto *operatorFunction : memberOperatorDecls)
    out << "- \"" << escape(operatorFunction->getName()) << "\"\n";
}

static void emitProvidesExtensionDecl(
    const ExtensionDecl *ED, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &memberOperatorDecls,
    llvm::SmallVectorImpl<const ExtensionDecl *> &extensionsWithJustMembers);

static void emitProvidesNominalTypeDecl(
    const NominalTypeDecl *NTD, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &memberOperatorDecls);

static void emitProvidesValueDecl(const ValueDecl *VD,
                                  llvm::raw_ostream &out);

static void emitProvidesTopLevelDecl(
    const Decl *const D, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &memberOperatorDecls,
    llvm::SmallVectorImpl<const ExtensionDecl *> &extensionsWithJustMembers) {
  switch (D->getKind()) {
  case DeclKind::Module:
    break;

  case DeclKind::Import:
    // FIXME: Handle re-exported decls.
    break;

  case DeclKind::Extension:
    emitProvidesExtensionDecl(cast<ExtensionDecl>(D), out, extendedNominals,
                              memberOperatorDecls, extensionsWithJustMembers);
    break;

  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    out << "- \"" << escape(cast<OperatorDecl>(D)->getName()) << "\"\n";
    break;

  case DeclKind::PrecedenceGroup:
    out << "- \"" << escape(cast<PrecedenceGroupDecl>(D)->getName()) << "\"\n";
    break;

  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
    emitProvidesNominalTypeDecl(cast<NominalTypeDecl>(D), out, extendedNominals,
                                memberOperatorDecls);
    break;

  case DeclKind::TypeAlias:
  case DeclKind::Var:
  case DeclKind::Func:
  case DeclKind::Accessor:
    emitProvidesValueDecl(cast<ValueDecl>(D), out);
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

static void emitProvidesExtensionDecl(
    const ExtensionDecl *const ED, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &memberOperatorDecls,
    llvm::SmallVectorImpl<const ExtensionDecl *> &extensionsWithJustMembers) {
  auto *NTD = ED->getExtendedType()->getAnyNominal();
  if (!NTD)
    return;
  if (NTD->hasAccess() && NTD->getFormalAccess() <= AccessLevel::FilePrivate) {
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
    extensionsWithJustMembers.push_back(ED);
  }
  extendedNominals[NTD] |= !justMembers;
  findNominalsAndOperators(extendedNominals, memberOperatorDecls,
                           ED->getMembers());
}

static void emitProvidesNominalTypeDecl(
    const NominalTypeDecl *const NTD, llvm::raw_ostream &out,
    llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::SmallVectorImpl<const FuncDecl *> &memberOperatorDecls) {
  if (!NTD->hasName())
    return;
  if (NTD->hasAccess() && NTD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }
  out << "- \"" << escape(NTD->getName()) << "\"\n";
  extendedNominals[NTD] |= true;
  findNominalsAndOperators(extendedNominals, memberOperatorDecls,
                           NTD->getMembers());
}

static void emitProvidesValueDecl(const ValueDecl *const VD,
                                  llvm::raw_ostream &out) {
  if (!VD->hasName())
    return;
  if (VD->hasAccess() && VD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }
  out << "- \"" << escape(VD->getBaseName()) << "\"\n";
}

static void emitProvidesNominalTypes(
    const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    llvm::raw_ostream &out) {
  out << "provides-nominal:\n";
  for (auto entry : extendedNominals) {
    if (!entry.second)
      continue;
    out << "- \"";
    out << mangleTypeAsContext(entry.first);
    out << "\"\n";
  }
}

static void emitProvidesMembers(
    const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals,
    const llvm::SmallVectorImpl<const ExtensionDecl *>
        &extensionsWithJustMembers,
    llvm::raw_ostream &out) {
  out << "provides-member:\n";
  for (auto entry : extendedNominals) {
    out << "- [\"";
    out << mangleTypeAsContext(entry.first);
    out << "\", \"\"]\n";
  }

  // This is also part of "provides-member".
  for (auto *ED : extensionsWithJustMembers) {
    auto mangledName =
        mangleTypeAsContext(ED->getExtendedType()->getAnyNominal());

    for (auto *member : ED->getMembers()) {
      auto *VD = dyn_cast<ValueDecl>(member);
      if (!VD || !VD->hasName() ||
          VD->getFormalAccess() <= AccessLevel::FilePrivate) {
        continue;
      }
      out << "- [\"" << mangledName << "\", \"" << escape(VD->getBaseName())
          << "\"]\n";
    }
  }
}

static void emitProvidesDynamicLookupMembers(const SourceFile *const SF,
                                             llvm::raw_ostream &out) {
  if (SF->getASTContext().LangOpts.EnableObjCInterop) {
    // FIXME: This requires a traversal of the whole file to compute.
    // We should (a) see if there's a cheaper way to keep it up to date,
    // and/or (b) see if we can fast-path cases where there's no ObjC
    // involved.
    out << "provides-dynamic-lookup:\n";
    class NameCollector : public VisibleDeclConsumer {
    private:
      SmallVector<DeclBaseName, 16> names;

    public:
      void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
        names.push_back(VD->getBaseName());
      }
      ArrayRef<DeclBaseName> getNames() {
        llvm::array_pod_sort(
            names.begin(), names.end(),
            [](const DeclBaseName *lhs, const DeclBaseName *rhs) {
              return lhs->compare(*rhs);
            });
        names.erase(std::unique(names.begin(), names.end()), names.end());
        return names;
      }
    };
    NameCollector collector;
    SF->lookupClassMembers({}, collector);
    for (DeclBaseName name : collector.getNames()) {
      out << "- \"" << escape(name) << "\"\n";
    }
  }
}

static SmallVector<std::pair<DeclBaseName, bool>, 16>
sortedByName(const llvm::DenseMap<DeclBaseName, bool> map) {
  SmallVector<std::pair<DeclBaseName, bool>, 16> pairs{map.begin(), map.end()};
  llvm::array_pod_sort(pairs.begin(), pairs.end(),
                       [](const std::pair<DeclBaseName, bool> *first,
                          const std::pair<DeclBaseName, bool> *second) -> int {
                         return first->first.compare(second->first);
                       });
  return pairs;
}

static void emitDependsTopLevelNames(const ReferencedNameTracker *tracker,
                                     llvm::raw_ostream &out);

using TableEntryTy = std::pair<ReferencedNameTracker::MemberPair, bool>;

static void emitDependsMember(ArrayRef<TableEntryTy> sortedMembers,
                              llvm::raw_ostream &out);

static void emitDependsNominal(ArrayRef<TableEntryTy> sortedMembers,
                               llvm::raw_ostream &out);

static void emitDependsDynamicLookup(const ReferencedNameTracker *tracker,
                                     llvm::raw_ostream &out);

static void emitDependsExternal(const DependencyTracker &depTracker,
                                llvm::raw_ostream &out);

void ReferenceDependenciesEmitter::emitDepends() {

  const ReferencedNameTracker *const tracker = SF->getReferencedNameTracker();
  assert(tracker && "Cannot emit reference dependencies without a tracker");

  emitDependsTopLevelNames(tracker, out);

  auto &memberLookupTable = tracker->getUsedMembers();
  std::vector<TableEntryTy> sortedMembers{
    memberLookupTable.begin(), memberLookupTable.end()
  };
  llvm::array_pod_sort(sortedMembers.begin(), sortedMembers.end(),
                       [](const TableEntryTy *lhs,
                          const TableEntryTy *rhs) -> int {
    if (lhs->first.first == rhs->first.first)
      return lhs->first.second.compare(rhs->first.second);

    if (lhs->first.first->getName() != rhs->first.first->getName())
      return lhs->first.first->getName().compare(rhs->first.first->getName());

    // Break type name ties by mangled name.
    auto lhsMangledName = mangleTypeAsContext(lhs->first.first);
    auto rhsMangledName = mangleTypeAsContext(rhs->first.first);
    return lhsMangledName.compare(rhsMangledName);
  });

  emitDependsMember(sortedMembers, out);
  emitDependsNominal(sortedMembers, out);
  emitDependsDynamicLookup(tracker, out);
  emitDependsExternal(depTracker, out);
}

static void emitDependsTopLevelNames(const ReferencedNameTracker *const tracker,
                                     llvm::raw_ostream &out) {
  out << "depends-top-level:\n";
  for (auto &entry : sortedByName(tracker->getTopLevelNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }
}

static void emitDependsMember(ArrayRef<TableEntryTy> sortedMembers,
                              llvm::raw_ostream &out) {
  out << "depends-member:\n";
  for (auto &entry : sortedMembers) {
    assert(entry.first.first != nullptr);
    if (entry.first.first->hasAccess() &&
        entry.first.first->getFormalAccess() <= AccessLevel::FilePrivate)
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

static void emitDependsNominal(ArrayRef<TableEntryTy> sortedMembers,
                               llvm::raw_ostream &out) {
  out << "depends-nominal:\n";
  for (auto i = sortedMembers.begin(), e = sortedMembers.end(); i != e; ++i) {
    bool isCascading = i->second;
    while (i+1 != e && i[0].first.first == i[1].first.first) {
      ++i;
      isCascading |= i->second;
    }

    if (i->first.first->hasAccess() &&
        i->first.first->getFormalAccess() <= AccessLevel::FilePrivate)
      continue;

    out << "- ";
    if (!isCascading)
      out << "!private ";
    out << "\"";
    out <<  mangleTypeAsContext(i->first.first);
    out << "\"\n";
  }
}

static void emitDependsDynamicLookup(const ReferencedNameTracker *const tracker,
                                     llvm::raw_ostream &out) {
  out << "depends-dynamic-lookup:\n";
  for (auto &entry : sortedByName(tracker->getDynamicLookupNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }
}

static void emitDependsExternal(const DependencyTracker &depTracker,
                                llvm::raw_ostream &out) {
  out << "depends-external:\n";
  for (auto &entry : reversePathSortedFilenames(depTracker.getDependencies())) {
    out << "- \"" << llvm::yaml::escape(entry) << "\"\n";
  }
}

void ReferenceDependenciesEmitter::emitInterfaceHash() {
  llvm::SmallString<32> interfaceHash;
  SF->getInterfaceHash(interfaceHash);
  out << "interface-hash: \"" << interfaceHash << "\"\n";
}
