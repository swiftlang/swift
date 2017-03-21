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

    if (VD->hasAccessibility() &&
        VD->getFormalAccess() <= Accessibility::FilePrivate) {
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

  return VD->getFormalAccess() <= Accessibility::FilePrivate;
}

static bool extendedTypeIsPrivate(TypeLoc inheritedType) {
  if (!inheritedType.getType())
    return true;

  SmallVector<ProtocolDecl *, 2> protocols;
  if (!inheritedType.getType()->isAnyExistentialType(protocols)) {
    // Be conservative. We don't know how to deal with other extended types.
    return false;
  }

  return std::all_of(protocols.begin(), protocols.end(), declIsPrivate);
}

static std::string mangleTypeAsContext(const NominalTypeDecl *type) {
  Mangle::ASTMangler Mangler;
  return Mangler.mangleTypeAsContextUSR(type);
}

bool swift::emitReferenceDependencies(DiagnosticEngine &diags,
                                      SourceFile *SF,
                                      DependencyTracker &depTracker,
                                      const FrontendOptions &opts) {
  if (!SF) {
    diags.diagnose(SourceLoc(),
                   diag::emit_reference_dependencies_without_primary_file);
    return true;
  }

  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(opts.ReferenceDependenciesFilePath,
                        opts.ReferenceDependenciesFilePath + "~");

  std::error_code EC;
  llvm::raw_fd_ostream out(opts.ReferenceDependenciesFilePath, EC,
                           llvm::sys::fs::F_None);

  if (out.has_error() || EC) {
    diags.diagnose(SourceLoc(), diag::error_opening_output,
                   opts.ReferenceDependenciesFilePath, EC.message());
    out.clear_error();
    return true;
  }

  auto escape = [](Identifier name) -> std::string {
    return llvm::yaml::escape(name.str());
  };

  out << "### Swift dependencies file v0 ###\n";

  llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
  llvm::SmallVector<const FuncDecl *, 8> memberOperatorDecls;
  llvm::SmallVector<const ExtensionDecl *, 8> extensionsWithJustMembers;

  out << "provides-top-level:\n";
  for (const Decl *D : SF->Decls) {
    switch (D->getKind()) {
    case DeclKind::Module:
      break;

    case DeclKind::Import:
      // FIXME: Handle re-exported decls.
      break;

    case DeclKind::Extension: {
      auto *ED = cast<ExtensionDecl>(D);
      auto *NTD = ED->getExtendedType()->getAnyNominal();
      if (!NTD)
        break;
      if (NTD->hasAccessibility() &&
          NTD->getFormalAccess() <= Accessibility::FilePrivate) {
        break;
      }

      bool justMembers = std::all_of(ED->getInherited().begin(),
                                     ED->getInherited().end(),
                                     extendedTypeIsPrivate);
      if (justMembers) {
        if (std::all_of(ED->getMembers().begin(), ED->getMembers().end(),
                        declIsPrivate)) {
          break;
        } else {
          extensionsWithJustMembers.push_back(ED);
        }
      }
      extendedNominals[NTD] |= !justMembers;
      findNominalsAndOperators(extendedNominals, memberOperatorDecls,
                               ED->getMembers());
      break;
    }

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
    case DeclKind::Protocol: {
      auto *NTD = cast<NominalTypeDecl>(D);
      if (!NTD->hasName())
        break;
      if (NTD->hasAccessibility() &&
          NTD->getFormalAccess() <= Accessibility::FilePrivate) {
        break;
      }
      out << "- \"" << escape(NTD->getName()) << "\"\n";
      extendedNominals[NTD] |= true;
      findNominalsAndOperators(extendedNominals, memberOperatorDecls,
                               NTD->getMembers());
      break;
    }

    case DeclKind::TypeAlias:
    case DeclKind::Var:
    case DeclKind::Func: {
      auto *VD = cast<ValueDecl>(D);
      if (!VD->hasName())
        break;
      if (VD->hasAccessibility() &&
          VD->getFormalAccess() <= Accessibility::FilePrivate) {
        break;
      }
      out << "- \"" << escape(VD->getName()) << "\"\n";
      break;
    }

    case DeclKind::PatternBinding:
    case DeclKind::TopLevelCode:
    case DeclKind::IfConfig:
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
      llvm_unreachable("cannot appear at the top level of a file");
    }
  }

  // This is also part of "provides-top-level".
  for (auto *operatorFunction : memberOperatorDecls)
    out << "- \"" << escape(operatorFunction->getName()) << "\"\n";

  out << "provides-nominal:\n";
  for (auto entry : extendedNominals) {
    if (!entry.second)
      continue;
    out << "- \"";
    out << mangleTypeAsContext(entry.first);
    out << "\"\n";
  }

  out << "provides-member:\n";
  for (auto entry : extendedNominals) {
    out << "- [\"";
    out << mangleTypeAsContext(entry.first);
    out << "\", \"\"]\n";
  }

  // This is also part of "provides-member".
  for (auto *ED : extensionsWithJustMembers) {
    auto mangledName = mangleTypeAsContext(
                                        ED->getExtendedType()->getAnyNominal());

    for (auto *member : ED->getMembers()) {
      auto *VD = dyn_cast<ValueDecl>(member);
      if (!VD || !VD->hasName() ||
          VD->getFormalAccess() <= Accessibility::FilePrivate) {
        continue;
      }
      out << "- [\"" << mangledName << "\", \""
          << escape(VD->getName()) << "\"]\n";
    }
  }

  if (SF->getASTContext().LangOpts.EnableObjCInterop) {
    // FIXME: This requires a traversal of the whole file to compute.
    // We should (a) see if there's a cheaper way to keep it up to date,
    // and/or (b) see if we can fast-path cases where there's no ObjC involved.
    out << "provides-dynamic-lookup:\n";
    class NameCollector : public VisibleDeclConsumer {
    private:
      SmallVector<Identifier, 16> names;
    public:
      void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
        names.push_back(VD->getName());
      }
      ArrayRef<Identifier> getNames() {
        llvm::array_pod_sort(names.begin(), names.end(),
                             [](const Identifier *lhs, const Identifier *rhs) {
          return lhs->compare(*rhs);
        });
        names.erase(std::unique(names.begin(), names.end()), names.end());
        return names;
      }
    };
    NameCollector collector;
    SF->lookupClassMembers({}, collector);
    for (Identifier name : collector.getNames()) {
      out << "- \"" << escape(name) << "\"\n";
    }
  }

  ReferencedNameTracker *tracker = SF->getReferencedNameTracker();

  auto sortedByIdentifier =
      [](const llvm::DenseMap<Identifier, bool> map) ->
        SmallVector<std::pair<Identifier, bool>, 16> {
    SmallVector<std::pair<Identifier, bool>, 16> pairs{map.begin(), map.end()};
    llvm::array_pod_sort(pairs.begin(), pairs.end(),
                         [](const std::pair<Identifier, bool> *first,
                            const std::pair<Identifier, bool> *second) -> int {
      return first->first.compare(second->first);
    });
    return pairs;
  };

  out << "depends-top-level:\n";
  for (auto &entry : sortedByIdentifier(tracker->getTopLevelNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }

  out << "depends-member:\n";
  auto &memberLookupTable = tracker->getUsedMembers();
  using TableEntryTy = std::pair<ReferencedNameTracker::MemberPair, bool>;
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

  for (auto &entry : sortedMembers) {
    assert(entry.first.first != nullptr);
    if (entry.first.first->hasAccessibility() &&
        entry.first.first->getFormalAccess() <= Accessibility::FilePrivate)
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

  out << "depends-nominal:\n";
  for (auto i = sortedMembers.begin(), e = sortedMembers.end(); i != e; ++i) {
    bool isCascading = i->second;
    while (i+1 != e && i[0].first.first == i[1].first.first) {
      ++i;
      isCascading |= i->second;
    }

    if (i->first.first->hasAccessibility() &&
        i->first.first->getFormalAccess() <= Accessibility::FilePrivate)
      continue;

    out << "- ";
    if (!isCascading)
      out << "!private ";
    out << "\"";
    out <<  mangleTypeAsContext(i->first.first);
    out << "\"\n";
  }

  out << "depends-dynamic-lookup:\n";
  for (auto &entry : sortedByIdentifier(tracker->getDynamicLookupNames())) {
    assert(!entry.first.empty());
    out << "- ";
    if (!entry.second)
      out << "!private ";
    out << "\"" << escape(entry.first) << "\"\n";
  }

  out << "depends-external:\n";
  for (auto &entry : depTracker.getDependencies()) {
    out << "- \"" << llvm::yaml::escape(entry) << "\"\n";
  }

  llvm::SmallString<32> interfaceHash;
  SF->getInterfaceHash(interfaceHash);
  out << "interface-hash: \"" << interfaceHash << "\"\n";

  return false;
}
