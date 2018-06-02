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

namespace {
class ReferenceDependenciesEmitter {
  SourceFile * const SF;
  const DependencyTracker &depTracker;
  llvm::raw_ostream &out;
  
  ReferenceDependenciesEmitter(SourceFile *const SF, const DependencyTracker &depTracker, llvm::raw_ostream &out) :
  SF(SF), depTracker(depTracker), out(out) {}
  
public:
  /// \return true on error
  static bool emit(DiagnosticEngine &diags, SourceFile *SF,
                   const DependencyTracker &depTracker,
                   StringRef outputPath);
  static void emit(SourceFile *SF, const DependencyTracker &depTracker, llvm::raw_ostream &out);
  
private:
  static std::unique_ptr<llvm::raw_fd_ostream> openFile(DiagnosticEngine &diags,
                                                        StringRef OutputPath);
  void emit() const;
  void emitProvides() const;
  void emitDepends() const;
  void emitInterfaceHash() const;
};
  
class ProvidesEmitter {
  const SourceFile * const SF;
  llvm::raw_ostream &out;
  
  ProvidesEmitter(const SourceFile * const SF, llvm::raw_ostream &out)
  : SF(SF), out(out) {}
  
public:
  static void emit(const SourceFile * SF, llvm::raw_ostream &out);
  
private:
  /// Collected and later written information.
  struct CollectedProvidedDeclarations {
    llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
    llvm::SmallVector<const FuncDecl *, 8> memberOperatorDecls;
    llvm::SmallVector<const ExtensionDecl *, 8> extensionsWithJustMembers;
    
    void findNominalsAndOperators(DeclRange members);
  };
  
  void emit() const;
  CollectedProvidedDeclarations emitTopLevelNames() const;
  void emitNominalTypes(const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals) const;
  void emitMembers(const CollectedProvidedDeclarations &cpd) const;
  void emitDynamicLookupMembers() const;
  
  void emitTopLevelDecl(const Decl *D, CollectedProvidedDeclarations &cpd) const;
  void emitExtensionDecl(const ExtensionDecl *D, CollectedProvidedDeclarations &cpd) const;
  void emitNominalTypeDecl(const NominalTypeDecl *NTD, CollectedProvidedDeclarations &cpd) const;
  void emitValueDecl(const ValueDecl *VD) const;
  
  static bool extendedTypeIsPrivate(TypeLoc inheritedType);
  static bool declIsPrivate(const Decl *member);
};

  
class DependsEmitter {
  const SourceFile *const SF;
  const DependencyTracker &depTracker;
  llvm::raw_ostream &out;
  
  DependsEmitter(const SourceFile *SF, const DependencyTracker &depTracker,
                 llvm::raw_ostream &out)
  : SF(SF), depTracker(depTracker), out(out) {}
  
public:
  using MemberTableEntryTy = std::pair<ReferencedNameTracker::MemberPair, bool>;
  
  static void emit(const SourceFile *SF, const DependencyTracker &depTracker,
                   llvm::raw_ostream &out);
  
private:
  void emit() const;
  void emitTopLevelNames(const ReferencedNameTracker *const tracker) const;
  void emitMember(const ArrayRef<MemberTableEntryTy> sortedMembers) const;
  void emitNominal(const ArrayRef<MemberTableEntryTy> sortedMembers) const;
  void emitDynamicLookup(const ReferencedNameTracker *const tracker) const;
  void emitExternal(const DependencyTracker &depTracker) const;
  
  
  static SmallVector<std::pair<DeclBaseName, bool>, 16>
  sortedByName(const llvm::DenseMap<DeclBaseName, bool> map);
};
} // end anon namespace

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
  const std::unique_ptr<llvm::raw_ostream> out = openFile(diags, outputPath);
  if (!out.get())
    return true;
  ReferenceDependenciesEmitter::emit(SF, depTracker, *out);
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

bool swift::emitReferenceDependencies(DiagnosticEngine &diags,
                                      SourceFile *SF,
                                      const DependencyTracker &depTracker,
                                      StringRef outputPath) {
  return ReferenceDependenciesEmitter::emit(diags, SF, depTracker, outputPath);
}

void ProvidesEmitter::emit() const {
  out << "provides-top-level" << ":\n";

  CollectedProvidedDeclarations cpd = emitTopLevelNames();
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
  out << "interface-hash" << ": \"" << interfaceHash << "\"\n";
}

ProvidesEmitter::CollectedProvidedDeclarations ProvidesEmitter::emitTopLevelNames() const {
    CollectedProvidedDeclarations cpd;
  for (const Decl *D : SF->Decls)
    emitTopLevelDecl(D, cpd);
  for (auto *operatorFunction : cpd.memberOperatorDecls)
    out << "- \"" << escape(operatorFunction->getName()) << "\"\n";
  return cpd;
}

void ProvidesEmitter::emitTopLevelDecl(
    const Decl *const D,
    CollectedProvidedDeclarations &cpd) const {
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
    out << "- \"" << escape(cast<OperatorDecl>(D)->getName()) << "\"\n";
    break;

  case DeclKind::PrecedenceGroup:
    out << "- \"" << escape(cast<PrecedenceGroupDecl>(D)->getName()) << "\"\n";
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

void ProvidesEmitter::emitExtensionDecl(
    const ExtensionDecl *const ED,
    CollectedProvidedDeclarations &cpd) const {
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
    cpd.extensionsWithJustMembers.push_back(ED);
  }
  cpd.extendedNominals[NTD] |= !justMembers;
  cpd.findNominalsAndOperators(ED->getMembers());
}

void ProvidesEmitter::emitNominalTypeDecl(
    const NominalTypeDecl *const NTD,
    CollectedProvidedDeclarations &cpd) const {
  if (!NTD->hasName())
    return;
  if (NTD->hasAccess() && NTD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }
  out << "- \"" << escape(NTD->getName()) << "\"\n";
  cpd.extendedNominals[NTD] |= true;
  cpd.findNominalsAndOperators(NTD->getMembers());
}

void ProvidesEmitter::CollectedProvidedDeclarations::findNominalsAndOperators(DeclRange members) {
  for (const Decl *D : members) {
    auto *VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;
    
    if (VD->hasAccess() &&
        VD->getFormalAccess() <= AccessLevel::FilePrivate) {
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
  if (VD->hasAccess() && VD->getFormalAccess() <= AccessLevel::FilePrivate) {
    return;
  }
  out << "- \"" << escape(VD->getBaseName()) << "\"\n";
}

void ProvidesEmitter::emitNominalTypes(
    const llvm::MapVector<const NominalTypeDecl *, bool> &extendedNominals) const {
  out << "provides-nominal" << ":\n";
  for (auto entry : extendedNominals) {
    if (!entry.second)
      continue;
    out << "- \"";
    out << mangleTypeAsContext(entry.first);
    out << "\"\n";
  }
}

void ProvidesEmitter::emitMembers(
    const CollectedProvidedDeclarations &cpd) const {
  out << "provides-member" << ":\n";
  for (auto entry : cpd.extendedNominals) {
    out << "- [\"";
    out << mangleTypeAsContext(entry.first);
    out << "\", \"\"]\n";
  }

  // This is also part of "provides-member".
  for (auto *ED : cpd.extensionsWithJustMembers) {
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

void ProvidesEmitter::emitDynamicLookupMembers() const {
  if (SF->getASTContext().LangOpts.EnableObjCInterop) {
    // FIXME: This requires a traversal of the whole file to compute.
    // We should (a) see if there's a cheaper way to keep it up to date,
    // and/or (b) see if we can fast-path cases where there's no ObjC
    // involved.
    out << "provides-dynamic-lookup" << ":\n";
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

bool ProvidesEmitter::extendedTypeIsPrivate(TypeLoc inheritedType) {
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

void DependsEmitter::emit(const SourceFile *SF, const DependencyTracker &depTracker,
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

  emitMember(sortedMembers);
  emitNominal(sortedMembers);
  emitDynamicLookup(tracker);
  emitExternal(depTracker);
}

void DependsEmitter::emitTopLevelNames(const ReferencedNameTracker *const tracker) const {
  out << "depends-top-level" << ":\n";
  for (auto &entry : sortedByName(tracker->getTopLevelNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }
}

void DependsEmitter::emitMember(ArrayRef<MemberTableEntryTy> sortedMembers) const {
  out << "depends-member" << ":\n";
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

void DependsEmitter::emitNominal(ArrayRef<MemberTableEntryTy> sortedMembers) const {
  out << "depends-nominal" << ":\n";
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

void DependsEmitter::emitDynamicLookup(const ReferencedNameTracker *const tracker) const {
  out << "depends-dynamic-lookup" << ":\n";
  for (auto &entry : sortedByName(tracker->getDynamicLookupNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }
}

void DependsEmitter::emitExternal(const DependencyTracker &depTracker) const {
  out << "depends-external" << ":\n";
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
