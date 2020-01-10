//===--- SymbolGraphASTWalker.cpp - Symbol Graph AST Walker ---------------===//
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

#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/StringSwitch.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Markup/Markup.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "DeclarationFragmentPrinter.h"
#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

SymbolGraphASTWalker::SymbolGraphASTWalker(ModuleDecl &M,
                                           const SymbolGraphOptions &Options)
  : Options(Options),
    M(M),
    Graph(M, Options.Target) {}

/// Returns `true` if the symbol should be included as a node in the graph.
bool SymbolGraphASTWalker::shouldIncludeNode(const Decl *D) const {
  // If this decl isn't in this module, don't record it,
  // as it will appear elsewhere in its module's symbol graph.
  if (D->getModuleContext()->getName() != M.getName()) {
    return false;
  }

  // Implicit declarations are probably not going to have documentation,
  // so don't record it in the symbol graph.
  if (D->isImplicit()) {
    return false;
  }
  
  // At this point, the declaration must be a ValueDecl.
  auto VD = cast<ValueDecl>(D);

  // Don't record unconditionally private declarations
  if (VD->isPrivateStdlibDecl(/*treatNonBuiltinProtocolsAsPublic=*/false)) {
    return false;
  }

  // Don't record effectively internal declarations if specified
  if (Options.MinimumAccessLevel > AccessLevel::Internal &&
      VD->hasUnderscoredNaming()) {
    return false;
  }

  // Symbols must meet the minimum access level to be included in the graph.
  if (VD->getFormalAccess() < Options.MinimumAccessLevel) {
    return false;
  }

  // Special cases

  auto BaseName = VD->getBaseName().userFacingName();

  // ${MODULE}Version{Number,String} in ${Module}.h
  SmallString<32> VersionNameIdentPrefix { M.getName().str() };
  VersionNameIdentPrefix.append("Version");

  if (BaseName.startswith(VersionNameIdentPrefix.str())) {
    return false;
  }

  // Automatically mapped SIMD types 
  bool ShouldInclude = llvm::StringSwitch<bool>(BaseName)
#define MAP_SIMD_TYPE(C_TYPE, _, __) \
    .Case("swift_" #C_TYPE "2", false) \
    .Case("swift_" #C_TYPE "3", false) \
    .Case("swift_" #C_TYPE "4", false)
#include "swift/ClangImporter/SIMDMappedTypes.def"
    .Case("SWIFT_TYPEDEFS", false)
    .Case("char16_t", false)
    .Case("char32_t", false)
    .Default(true);

  return ShouldInclude;
}

bool SymbolGraphASTWalker::walkToDeclPre(Decl *D, CharSourceRange Range) {

    switch (D->getKind()) {
    // We'll record nodes for the following kinds of declarations.
    case swift::DeclKind::Class:
    case swift::DeclKind::Struct:
    case swift::DeclKind::Enum:
    case swift::DeclKind::EnumElement:
    case swift::DeclKind::Protocol:
    case swift::DeclKind::Constructor:
    case swift::DeclKind::Func:
    case swift::DeclKind::Var:
    case swift::DeclKind::TypeAlias:
      break;
      
    // We'll descend into everything else.
    default:
      return true;
  }

  if (!shouldIncludeNode(D)) {
    return false;
  }

  auto *VD = cast<ValueDecl>(D);
  Graph.Nodes.insert(VD);
  
  // Record all of the possible relationships (edges) originating
  // with this declaration.
  recordMemberRelationship(VD);
  recordConformanceRelationships(VD);
  recordInheritanceRelationships(VD);
  recordDefaultImplementationRelationships(VD);
  recordOverrideRelationship(VD);
  recordRequirementRelationships(VD);
  recordOptionalRequirementRelationships(VD);
  
  return true;
}

StringRef SymbolGraphASTWalker::getUSR(const ValueDecl *VD) {
  auto Found = USRCache.find(VD);
  if (Found != USRCache.end()) {
    return Found->second;
  }
  llvm::SmallString<32> Scratch;
  llvm::raw_svector_ostream OS(Scratch);
  ide::printDeclUSR(VD, OS);
  auto USR = Ctx.allocateCopy(Scratch.str());
  USRCache.insert({VD, USR});
  return USR;
}

SymbolIdentifier
SymbolGraphASTWalker::getSymbolIdentifier(const ValueDecl *VD) {
  // Look in the symbol identifier cache for this declartion.
  auto Found = SymbolIdentifierCache.find(VD);
  if (Found != SymbolIdentifierCache.end()) {
    return Found->getSecond();
  }

  // Not found; need to build a symbol identifier and add it to the cache.
  auto PreciseIdentifier = getUSR(VD);
  llvm::SmallVector<llvm::StringRef, 4> SimpleIdentifierChain;
  
  // Collect the spellings of the fully qualified identifier components.
  auto Decl = VD;
  while (Decl && !isa<ModuleDecl>(Decl)) {
    SmallString<32> Scratch;
    Decl->getFullName().getString(Scratch);
    SimpleIdentifierChain.push_back(Ctx.allocateCopy(Scratch.str()));
    if (const auto *DC = Decl->getDeclContext()) {
      if (const auto *Proto = DC->getExtendedProtocolDecl()) {
        Decl = Proto;
      } else if (const auto *Ext = dyn_cast_or_null<ExtensionDecl>(DC->getAsDecl())) {
        Decl = Ext->getExtendedNominal();
      } else {
        Decl = dyn_cast_or_null<ValueDecl>(DC->getAsDecl());
      }
    } else {
      Decl = nullptr;
    }
  }
  
  // The list is leaf-to-root, but our list is root-to-leaf, so reverse it.
  std::reverse(SimpleIdentifierChain.begin(), SimpleIdentifierChain.end());

  SymbolIdentifier Identifier {
    PreciseIdentifier,
    Ctx.allocateCopy(llvm::makeArrayRef(SimpleIdentifierChain))
  };

  SymbolIdentifierCache.insert({VD, Identifier});
  return Identifier;
}

PrintOptions SymbolGraphASTWalker::getDeclarationFragmentsPrintOptions() const {
  PrintOptions Opts;
  Opts.FunctionDefinitions = false;
  Opts.ArgAndParamPrinting =
    PrintOptions::ArgAndParamPrintingMode::ArgumentOnly;
  Opts.PrintGetSetOnRWProperties = false;
  Opts.PrintPropertyAccessors = false;
  Opts.PrintSubscriptAccessors = false;
  Opts.SkipUnderscoredKeywords = true;
  Opts.SkipAttributes = true;
  Opts.PrintOverrideKeyword = true;
  Opts.PrintImplicitAttrs = false;
  Opts.PrintFunctionRepresentationAttrs =
    PrintOptions::FunctionRepresentationMode::None;
  Opts.PrintUserInaccessibleAttrs = false;
  Opts.SkipPrivateStdlibDecls = true;
  Opts.SkipUnderscoredStdlibProtocols = true;

  Opts.ExclusiveAttrList.clear();

#define DECL_ATTR(SPELLING, CLASS, OPTIONS, CODE) Opts.ExcludeAttrList.push_back(DAK_##CLASS);
#define TYPE_ATTR(X) Opts.ExcludeAttrList.push_back(TAK_##X);
#include "swift/AST/Attr.def"

  return Opts;
}

void
SymbolGraphASTWalker::serializeDeclarationFragments(StringRef Key,
                                                    const ValueDecl *VD,
                                                    llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(*this, OS, Key);
  VD->print(Printer, getDeclarationFragmentsPrintOptions());
}

void
SymbolGraphASTWalker::serializeSubheadingDeclarationFragments(StringRef Key,
    const ValueDecl *VD,
    llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(*this, OS, Key);
  auto Options = getDeclarationFragmentsPrintOptions();
  Options.VarInitializers = false;
  Options.PrintDefaultArgumentValue = false;
  Options.PrintEmptyArgumentNames = false;
  Options.PrintOverrideKeyword = false;
  VD->print(Printer, Options);
}

void
SymbolGraphASTWalker::serializeDeclarationFragments(StringRef Key, Type T,
                                                    llvm::json::OStream &OS) {
  DeclarationFragmentPrinter Printer(*this, OS, Key);
  T->print(Printer, getDeclarationFragmentsPrintOptions());
}

void SymbolGraphASTWalker::recordEdge(const ValueDecl *Source,
                                      const ValueDecl *Target,
                                      RelationshipKind Kind) {
  if (Target->isPrivateStdlibDecl(
      /*treatNonBuiltinProtocolsAsPublic = */false)) {
    return;
  }

  // There might be relationships on implicit declarations,
  // such as overriding implicit @objc init().
  if (Target->isImplicit()) {
    return;
  }

  Graph.Nodes.insert(Source);
  Graph.Nodes.insert(Target);

  Graph.Edges.insert({this, Kind, Source, Target});
}

void SymbolGraphASTWalker::recordMemberRelationship(const ValueDecl *VD) {
  auto *DC = VD->getDeclContext();
  switch (DC->getContextKind()) {
    case DeclContextKind::GenericTypeDecl:
    case DeclContextKind::ExtensionDecl:
    case swift::DeclContextKind::EnumElementDecl:
      return recordEdge(VD, VD->getDeclContext()->getSelfNominalTypeDecl(),
                        RelationshipKind::MemberOf());
    case swift::DeclContextKind::AbstractClosureExpr:
    case swift::DeclContextKind::Initializer:
    case swift::DeclContextKind::TopLevelCodeDecl:
    case swift::DeclContextKind::SubscriptDecl:
    case swift::DeclContextKind::AbstractFunctionDecl:
    case swift::DeclContextKind::SerializedLocal:
    case swift::DeclContextKind::Module:
    case swift::DeclContextKind::FileUnit:
      break;
  }
}

void
SymbolGraphASTWalker::recordInheritanceRelationships(const ValueDecl *VD) {
  if (const auto *NTD = dyn_cast<NominalTypeDecl>(VD)) {
    for (const auto &InheritanceLoc : NTD->getInherited()) {
      auto Ty = InheritanceLoc.getType();
      if (!Ty) {
        continue;
      }
      auto *InheritedTypeDecl =
          dyn_cast_or_null<ClassDecl>(Ty->getAnyNominal());
      if (!InheritedTypeDecl) {
        continue;
      }
      
      recordEdge(VD, InheritedTypeDecl, RelationshipKind::InheritsFrom());
    }
  }
}

void SymbolGraphASTWalker::recordDefaultImplementationRelationships(
    const ValueDecl *VD) {
  if (const auto *Extension = dyn_cast<ExtensionDecl>(VD->getDeclContext())) {
    if (const auto *Protocol = Extension->getExtendedProtocolDecl()) {
      for (const auto *Member : Protocol->getMembers()) {
        if (const auto *MemberVD = dyn_cast<ValueDecl>(Member)) {
          if (MemberVD->getFullName().compare(VD->getFullName()) == 0) {
            recordEdge(VD, MemberVD,
                       RelationshipKind::DefaultImplementationOf());
          }
        }
      }
    }
  }
}

void
SymbolGraphASTWalker::recordRequirementRelationships(const ValueDecl *VD) {
  if (const auto *Protocol = dyn_cast<ProtocolDecl>(VD->getDeclContext())) {
    if (VD->isProtocolRequirement()) {
      recordEdge(VD, Protocol, RelationshipKind::RequirementOf());
    }
  }
}

void SymbolGraphASTWalker::recordOptionalRequirementRelationships(
    const ValueDecl *VD) {
  if (const auto *Protocol = dyn_cast<ProtocolDecl>(VD->getDeclContext())) {
    if (VD->isProtocolRequirement()) {
      if (const auto *ClangDecl = VD->getClangDecl()) {
        if (const auto *Method = dyn_cast<clang::ObjCMethodDecl>(ClangDecl)) {
          if (Method->isOptional()) {
            recordEdge(VD, Protocol,
                       RelationshipKind::OptionalRequirementOf());
          }
        }
      }
    }
  }
}

void
SymbolGraphASTWalker::recordConformanceRelationships(const ValueDecl *VD) {
  if (const auto *NTD = dyn_cast<NominalTypeDecl>(VD)) {
    for (const auto *Conformance : NTD->getAllConformances()) {
      recordEdge(VD, Conformance->getProtocol(),
                 RelationshipKind::ConformsTo());
    }
  }
}

void SymbolGraphASTWalker::recordOverrideRelationship(const ValueDecl *VD) {
  if (const auto *Override = VD->getOverriddenDecl()) {
    recordEdge(VD, Override, RelationshipKind::Overrides());
  }
}
