//===--- ITCDecl.cpp - Iterative Type Checker for Declarations ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the portions of the IterativeTypeChecker
//  class that involve declarations.
//
//===----------------------------------------------------------------------===//
#include "GenericTypeResolver.h"
#include "TypeChecker.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include <tuple>
using namespace swift;

//----------------------------------------------------------------------------//
// Inheritance clause handling
//----------------------------------------------------------------------------//
static std::tuple<TypeResolutionOptions, DeclContext *,
                  MutableArrayRef<TypeLoc>>
decomposeInheritedClauseDecl(
  llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl) {
  TypeResolutionOptions options;
  DeclContext *dc;
  MutableArrayRef<TypeLoc> inheritanceClause;
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>()) {
    inheritanceClause = typeDecl->getInherited();
    if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
      dc = nominal;
      options |= TR_GenericSignature | TR_InheritanceClause;
    } else {
      dc = typeDecl->getDeclContext();

      if (isa<GenericTypeParamDecl>(typeDecl)) {
        // For generic parameters, we want name lookup to look at just the
        // signature of the enclosing entity.
        if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
          dc = nominal;
          options |= TR_GenericSignature;
        } else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
          dc = ext;
          options |= TR_GenericSignature;
        } else if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
          dc = func;
          options |= TR_GenericSignature;
        } else if (!dc->isModuleScopeContext()) {
          // Skip the generic parameter's context entirely.
          dc = dc->getParent();
        }
      }
    }
  } else {
    auto ext = decl.get<ExtensionDecl *>();
    inheritanceClause = ext->getInherited();
    dc = ext;
    options |= TR_GenericSignature | TR_InheritanceClause;
  }

  return std::make_tuple(options, dc, inheritanceClause);
}

static std::tuple<TypeResolutionOptions, DeclContext *, TypeLoc *>
decomposeInheritedClauseEntry(
  TypeCheckRequest::InheritedClauseEntryPayloadType entry) {
  TypeResolutionOptions options;
  DeclContext *dc;
  MutableArrayRef<TypeLoc> inheritanceClause;
  std::tie(options, dc, inheritanceClause)
    = decomposeInheritedClauseDecl(entry.first);
  return std::make_tuple(options, dc, &inheritanceClause[entry.second]);
}

bool IterativeTypeChecker::isTypeCheckInheritedClauseEntrySatisfied(
       TypeCheckRequest::InheritedClauseEntryPayloadType payload) {
  TypeLoc &inherited = *std::get<2>(decomposeInheritedClauseEntry(payload));
  return !inherited.getType().isNull();
}

void IterativeTypeChecker::enumerateDependenciesOfTypeCheckInheritedClauseEntry(
       TypeCheckRequest::InheritedClauseEntryPayloadType payload,
       llvm::function_ref<void(TypeCheckRequest)>) {
  // FIXME: depends on type checking the TypeRepr for this inheritance
  // clause entry.
}

void IterativeTypeChecker::satisfyTypeCheckInheritedClauseEntry(
       TypeCheckRequest::InheritedClauseEntryPayloadType payload) {
  TypeResolutionOptions options;
  DeclContext *dc;
  TypeLoc *inherited;
  std::tie(options, dc, inherited) = decomposeInheritedClauseEntry(payload);

  // FIXME: Declaration validation is overkill. Sink it down into type
  // resolution when it is actually needed.
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc))
    TC.validateDecl(nominal);
  else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
    TC.validateExtension(ext);
  }

  // Validate the type of this inherited clause entry.
  // FIXME: Recursion into existing type checker.
  PartialGenericTypeToArchetypeResolver resolver(TC);
  if (TC.validateType(*inherited, dc, options, &resolver)) {
    inherited->setInvalidType(getASTContext());
  }
}

//----------------------------------------------------------------------------//
// Superclass handling
//----------------------------------------------------------------------------//
bool IterativeTypeChecker::isTypeCheckSuperclassSatisfied(ClassDecl *payload) {
  return payload->LazySemanticInfo.Superclass.getInt();
}

void IterativeTypeChecker::enumerateDependenciesOfTypeCheckSuperclass(
       ClassDecl *payload,
       llvm::function_ref<void(TypeCheckRequest)> fn) {
  // The superclass should be the first inherited type. However, so
  // long as we see already-resolved types that refer to protocols,
  // skip over them to keep looking for a misplaced superclass. The
  // actual error will be diagnosed when we perform full semantic
  // analysis on the class itself.
  auto inheritedClause = payload->getInherited();
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    TypeLoc &inherited = inheritedClause[i];

    // If this inherited type has not been resolved, we depend on it.
    if (!inherited.getType()) {
      fn(TypeCheckRequest(TypeCheckRequest::TypeCheckInheritedClauseEntry,
                          { payload, i }));
      return;
    }

    // If this resolved inherited type is existential, keep going.
    if (inherited.getType()->isExistentialType()) continue;

    break;
  }
}

void IterativeTypeChecker::satisfyTypeCheckSuperclass(ClassDecl *classDecl) {
  // Loop through the inheritance clause looking for a class type.
  Type superclassType;
  for (const auto &inherited : classDecl->getInherited()) {
    if (inherited.getType()->getClassOrBoundGenericClass()) {
      superclassType = inherited.getType();
      break;
    }
  }

  // Set the superclass type.
  classDecl->setSuperclass(superclassType);
}

//----------------------------------------------------------------------------//
// Raw type handling
//----------------------------------------------------------------------------//
bool IterativeTypeChecker::isTypeCheckRawTypeSatisfied(EnumDecl *payload) {
  return payload->LazySemanticInfo.RawType.getInt();
}

void IterativeTypeChecker::enumerateDependenciesOfTypeCheckRawType(
       EnumDecl *payload,
       llvm::function_ref<void(TypeCheckRequest)> fn) {
  // The raw type should be the first inherited type. However, so
  // long as we see already-resolved types that refer to protocols,
  // skip over them to keep looking for a misplaced raw type. The
  // actual error will be diagnosed when we perform full semantic
  // analysis on the enum itself.
  auto inheritedClause = payload->getInherited();
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    TypeLoc &inherited = inheritedClause[i];

    // If this inherited type has not been resolved, we depend on it.
    if (!inherited.getType()) {
      fn(TypeCheckRequest(TypeCheckRequest::TypeCheckInheritedClauseEntry,
                          { payload, i }));
      return;
    }

    // If this resolved inherited type is existential, keep going.
    if (inherited.getType()->isExistentialType()) continue;

    break;
  }
}

void IterativeTypeChecker::satisfyTypeCheckRawType(EnumDecl *enumDecl) {
  // Loop through the inheritance clause looking for a non-existential
  // nominal type.
  Type rawType;
  for (const auto &inherited : enumDecl->getInherited()) {
    if (!inherited.getType()) break;

    // Skip existential types.
    if (inherited.getType()->isExistentialType()) continue;

    // Record this raw type.
    rawType = inherited.getType();
    break;
  }

  // Set the raw type.
  enumDecl->setRawType(rawType);
}

//----------------------------------------------------------------------------//
// Inherited protocols
//----------------------------------------------------------------------------//
bool IterativeTypeChecker::isInheritedProtocolsSatisfied(ProtocolDecl *payload){
  return payload->isInheritedProtocolsValid();
}

void IterativeTypeChecker::enumerateDependenciesOfInheritedProtocols(
       ProtocolDecl *payload,
       llvm::function_ref<void(TypeCheckRequest)> fn) {
  // Computing the set of inherited protocols depends on the complete
  // inheritance clause.
  // FIXME: Technically, we only need very basic name binding.
  auto inheritedClause = payload->getInherited();
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    TypeLoc &inherited = inheritedClause[i];

    // If this inherited type has not been resolved, we depend on it.
    if (!inherited.getType()) {
      fn(TypeCheckRequest(TypeCheckRequest::TypeCheckInheritedClauseEntry,
                          { payload, i }));
    }
  }
}

void IterativeTypeChecker::satisfyInheritedProtocols(ProtocolDecl *protocol) {
  // Gather all of the existential types in the inherited list.
  // Loop through the inheritance clause looking for a non-existential
  // nominal type.
  llvm::SmallSetVector<ProtocolDecl *, 4> allProtocols;
  for (const auto &inherited : protocol->getInherited()) {
    if (!inherited.getType()) continue;

    // Collect existential types.
    // FIXME: We'd prefer to keep what the user wrote here.
    SmallVector<ProtocolDecl *, 4> protocols;
    if (inherited.getType()->isExistentialType(protocols)) {
      allProtocols.insert(protocols.begin(), protocols.end());
    }
  }

  // FIXME: Hack to deal with recursion elsewhere.
  if (protocol->isInheritedProtocolsValid())
    return;

  // Check for circular inheritance.
  // FIXME: The diagnostics here should be improved.
  bool diagnosedCircularity = false;
  for (unsigned i = 0, n = allProtocols.size(); i != n; /*in loop*/) {
    if (allProtocols[i] == protocol ||
        allProtocols[i]->inheritsFrom(protocol)) {
      if (!diagnosedCircularity) {
        diagnose(protocol,
                 diag::circular_protocol_def, protocol->getName().str());
        diagnosedCircularity = true;
      }

      allProtocols.remove(allProtocols[i]);
      --n;
      continue;
    }

    ++i;
  }

  protocol->setInheritedProtocols(getASTContext().AllocateCopy(allProtocols));
}
