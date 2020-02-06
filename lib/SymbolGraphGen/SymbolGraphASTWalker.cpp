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

#include "llvm/ADT/StringSwitch.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/SymbolGraphGen/SymbolGraphGen.h"

#include "SymbolGraphASTWalker.h"

using namespace swift;
using namespace symbolgraphgen;

SymbolGraphASTWalker::SymbolGraphASTWalker(ModuleDecl &M,
                                           const SymbolGraphOptions &Options)
  : Options(Options),
    M(M),
    Graph(M, Options.Target, Ctx) {}

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
  Graph.recordMemberRelationship(VD);
  Graph.recordConformanceRelationships(VD);
  Graph.recordInheritanceRelationships(VD);
  Graph.recordDefaultImplementationRelationships(VD);
  Graph.recordOverrideRelationship(VD);
  Graph.recordRequirementRelationships(VD);
  Graph.recordOptionalRequirementRelationships(VD);
  
  return true;
}
