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
  };
  
  class CollectedDeclarations {
    /// Records every nominal declaration, and whether or not the declaration
    /// changes the externally-observable shape of the type.
    llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
    
    llvm::SmallVector<const OperatorDecl *, 8> topLevelOperatorDecls;

    /// Records operator declarations so they can be included as top-level
    /// declarations.
    llvm::SmallVector<const FuncDecl *, 8> memberOperatorDecls;
    
    /// Records extension declarations which are not introducing a conformance
    /// to a public protocol and add a public member.
    llvm::SmallVector<const ExtensionDecl *, 8> extensionsWithJustMembers;
    
    
    /// Recursively computes the transitive closure over members
    /// adding memberOperatorDecls and extendedNominals to the receiver.
    void findNominalsAndOperators(const DeclRange members);
    
    static bool extendedTypeIsPrivate(const TypeLoc inheritedType);
    static bool declIsPrivate(const Decl *const member);
    
    void collectExtensionAndContents(const ExtensionDecl *const ED);
    void collectNominalAndContents(const NominalTypeDecl *const NTD);
  public:
    void collectDeclarationsFrom(const SourceFile *const SF);
  };
  
} // namespace

static bool isVisibleOutsideItsFile(const ValueDecl *const VD) {
  return VD->getFormalAccess() > AccessLevel::FilePrivate;
}
static void unimplemented() { assert(false && "experimental dependencies unimplemented"); }

bool ReferenceDependenciesEmitter::emit(DiagnosticEngine &diags,
                                        SourceFile *const SF,
                                        const DependencyTracker &depTracker,
                                        StringRef outputPath) {
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
    ReferenceDependenciesEmitter::emit(SF, depTracker, out);
    return false;
  });
}

void ReferenceDependenciesEmitter::emit(SourceFile *SF,
                                        const DependencyTracker &depTracker,
                                        llvm::raw_ostream &out) {
  ReferenceDependenciesEmitter(SF, depTracker, out).emit();
}

void ReferenceDependenciesEmitter::emit() const {
  assert(SF && "Cannot emit reference dependencies without a SourceFile");
  out << "### Swift experimental dependencies file v0 ###\n";
  CollectedDeclarations cpd;
  cpd.collectDeclarationsFrom(SF);
  unimplemented();
}

bool swift::experimental_dependencies::emitReferenceDependencies(
                                                                 DiagnosticEngine &diags, SourceFile *SF,
                                                                 const DependencyTracker &depTracker, StringRef outputPath) {
  return ReferenceDependenciesEmitter::emit(diags, SF, depTracker, outputPath);
}

// stubs

void CollectedDeclarations::collectDeclarationsFrom(const SourceFile *const SF) {
  for (const Decl *D : SF->Decls)
    switch (D->getKind()) {
      case DeclKind::Module:
        break;
        
      case DeclKind::Import:
        // FIXME: Handle re-exported decls.
        break;
        
      case DeclKind::Extension:
        collectExtensionAndContents(cast<ExtensionDecl>(D));
        break;
        
      case DeclKind::InfixOperator:
      case DeclKind::PrefixOperator:
      case DeclKind::PostfixOperator:
        topLevelOperatorDecls.push_back(cast<OperatorDecl>(D));
        break;
        
      case DeclKind::PrecedenceGroup:
        break;
        
      case DeclKind::Enum:
      case DeclKind::Struct:
      case DeclKind::Class:
      case DeclKind::Protocol:
        collectNominalAndContents(cast<NominalTypeDecl>(D));
        break;
        
      case DeclKind::TypeAlias:
      case DeclKind::Var:
      case DeclKind::Func:
      case DeclKind::Accessor:
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

void CollectedDeclarations::collectExtensionAndContents(const ExtensionDecl *const ED) {
  const NominalTypeDecl *const NTD = ED->getExtendedNominal();
  if (!NTD || !isVisibleOutsideItsFile(NTD))
    return;

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
  findNominalsAndOperators(ED->getMembers());
}

void CollectedDeclarations::collectNominalAndContents(const NominalTypeDecl *const NTD) {
  if (!NTD->hasName() || !isVisibleOutsideItsFile(NTD)) {
    return;
  }
  extendedNominals[NTD] |= true;
  findNominalsAndOperators(NTD->getMembers());
}

void CollectedDeclarations::CollectedDeclarations::findNominalsAndOperators(
                                                                      const DeclRange members) {
  for (const Decl *D : members) {
    auto *VD = dyn_cast<ValueDecl>(D);
    if (!VD || !isVisibleOutsideItsFile(VD))
      continue;
    
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

bool CollectedDeclarations::extendedTypeIsPrivate(const TypeLoc inheritedType) {
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

bool CollectedDeclarations::declIsPrivate(const Decl *const member) {
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
  
  return !isVisibleOutsideItsFile(VD);
}
