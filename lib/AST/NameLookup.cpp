//===--- NameLookup.cpp - Swift Name Lookup Routines ----------------------===//
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
//
// This file implements interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "namelookup"

using namespace swift;
using namespace swift::namelookup;

void VisibleDeclConsumer::anchor() {}
void VectorDeclConsumer::anchor() {}
void NamedDeclConsumer::anchor() {}

ValueDecl *LookupResultEntry::getBaseDecl() const {
  if (BaseDC == nullptr)
    return nullptr;

  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(BaseDC))
    return AFD->getImplicitSelfDecl();

  if (auto *PBI = dyn_cast<PatternBindingInitializer>(BaseDC)) {
    auto *selfDecl = PBI->getImplicitSelfDecl();
    assert(selfDecl);
    return selfDecl;
  }

  auto *nominalDecl = BaseDC->getSelfNominalTypeDecl();
  assert(nominalDecl);
  return nominalDecl;
}

void LookupResult::filter(
    llvm::function_ref<bool(LookupResultEntry, bool)> pred) {
  size_t index = 0;
  size_t originalFirstOuter = IndexOfFirstOuterResult;
  Results.erase(std::remove_if(Results.begin(), Results.end(),
                               [&](LookupResultEntry result) -> bool {
                                 auto isInner = index < originalFirstOuter;
                                 index++;
                                 if (pred(result, !isInner))
                                   return false;

                                 // Need to remove this, which means, if it is
                                 // an inner result, the outer results need to
                                 // shift down.
                                 if (isInner)
                                   IndexOfFirstOuterResult--;
                                 return true;
                               }),
                Results.end());
}

void LookupResult::shiftDownResults() {
  // Remove inner results.
  Results.erase(Results.begin(), Results.begin() + IndexOfFirstOuterResult);
  IndexOfFirstOuterResult = 0;

  if (Results.empty())
    return;

  // Compute IndexOfFirstOuterResult.
  const DeclContext *dcInner = Results.front().getValueDecl()->getDeclContext();
  for (auto &&result : Results) {
    const DeclContext *dc = result.getValueDecl()->getDeclContext();
    if (dc == dcInner ||
        (dc->isModuleScopeContext() && dcInner->isModuleScopeContext()))
      ++IndexOfFirstOuterResult;
    else
      break;
  }
}

void swift::simple_display(llvm::raw_ostream &out,
                           UnqualifiedLookupOptions options) {
  using Flag = std::pair<UnqualifiedLookupFlags, StringRef>;
  Flag possibleFlags[] = {
      {UnqualifiedLookupFlags::AllowProtocolMembers, "AllowProtocolMembers"},
      {UnqualifiedLookupFlags::IgnoreAccessControl, "IgnoreAccessControl"},
      {UnqualifiedLookupFlags::IncludeOuterResults, "IncludeOuterResults"},
      {UnqualifiedLookupFlags::KnownPrivate, "KnownPrivate"},
      {UnqualifiedLookupFlags::TypeLookup, "TypeLookup"},
  };

  auto flagsToPrint = llvm::make_filter_range(
      possibleFlags, [&](Flag flag) { return options.contains(flag.first); });

  out << "{ ";
  interleave(
      flagsToPrint, [&](Flag flag) { out << flag.second; },
      [&] { out << ", "; });
  out << " }";
}

void DebuggerClient::anchor() {}

void AccessFilteringDeclConsumer::foundDecl(
    ValueDecl *D, DeclVisibilityKind reason,
    DynamicLookupInfo dynamicLookupInfo) {
  if (D->hasInterfaceType() && D->isInvalid())
    return;
  if (!D->isAccessibleFrom(DC))
    return;

  ChainedConsumer.foundDecl(D, reason, dynamicLookupInfo);
}

void LookupResultEntry::print(llvm::raw_ostream& out) const {
  getValueDecl()->print(out);
  if (auto dc = getBaseDecl()) {
    out << "\nbase: ";
    dc->print(out);
    out << "\n";
  } else
    out << "\n(no-base)\n";
}


bool swift::removeOverriddenDecls(SmallVectorImpl<ValueDecl*> &decls) {
  if (decls.size() < 2)
    return false;

  llvm::SmallPtrSet<ValueDecl*, 8> overridden;
  for (auto decl : decls) {
    // Don't look at the overrides of operators in protocols. The global
    // lookup of operators means that we can find overriding operators that
    // aren't relevant to the types in hand, and will fail to type check.
    if (isa<ProtocolDecl>(decl->getDeclContext())) {
      if (auto func = dyn_cast<FuncDecl>(decl))
        if (func->isOperator())
          continue;
    }

    while (auto overrides = decl->getOverriddenDecl()) {
      overridden.insert(overrides);

      // Because initializers from Objective-C base classes have greater
      // visibility than initializers written in Swift classes, we can
      // have a "break" in the set of declarations we found, where
      // C.init overrides B.init overrides A.init, but only C.init and
      // A.init are in the chain. Make sure we still remove A.init from the
      // set in this case.
      if (decl->getFullName().getBaseName() == DeclBaseName::createConstructor()) {
        /// FIXME: Avoid the possibility of an infinite loop by fixing the root
        ///        cause instead (incomplete circularity detection).
        assert(decl != overrides && "Circular class inheritance?");
        decl = overrides;
        continue;
      }

      break;
    }
  }

  // If no methods were overridden, we're done.
  if (overridden.empty()) return false;

  // Erase any overridden declarations
  bool anyOverridden = false;
  decls.erase(std::remove_if(decls.begin(), decls.end(),
                             [&](ValueDecl *decl) -> bool {
                               if (overridden.count(decl) > 0) {
                                 anyOverridden = true;
                                 return true;
                               }

                               return false;
                             }),
              decls.end());

  return anyOverridden;
}

enum class ConstructorComparison {
  Worse,
  Same,
  Better,
};

/// Determines whether \p ctor1 is a "better" initializer than \p ctor2.
static ConstructorComparison compareConstructors(ConstructorDecl *ctor1,
                                                 ConstructorDecl *ctor2,
                                                 const swift::ASTContext &ctx) {
  bool available1 = !ctor1->getAttrs().isUnavailable(ctx);
  bool available2 = !ctor2->getAttrs().isUnavailable(ctx);

  // An unavailable initializer is always worse than an available initializer.
  if (available1 < available2)
    return ConstructorComparison::Worse;

  if (available1 > available2)
    return ConstructorComparison::Better;

  CtorInitializerKind kind1 = ctor1->getInitKind();
  CtorInitializerKind kind2 = ctor2->getInitKind();

  if (kind1 > kind2)
    return ConstructorComparison::Worse;

  if (kind1 < kind2)
    return ConstructorComparison::Better;

  return ConstructorComparison::Same;
}

/// Given a set of declarations whose names and signatures have matched,
/// figure out which of these declarations have been shadowed by others.
static void recordShadowedDeclsAfterSignatureMatch(
                              ArrayRef<ValueDecl *> decls,
                              const DeclContext *dc,
                              llvm::SmallPtrSetImpl<ValueDecl *> &shadowed) {
  assert(decls.size() > 1 && "Nothing collided");

  // Compare each declaration to every other declaration. This is
  // unavoidably O(n^2) in the number of declarations, but because they
  // all have the same signature, we expect n to remain small.
  auto *curModule = dc->getParentModule();
  ASTContext &ctx = curModule->getASTContext();
  auto &imports = ctx.getImportCache();

  for (unsigned firstIdx : indices(decls)) {
    auto firstDecl = decls[firstIdx];
    auto firstModule = firstDecl->getModuleContext();
    bool firstTopLevel = firstDecl->getDeclContext()->isModuleScopeContext();

    auto name = firstDecl->getBaseName();

    auto isShadowed = [&](ArrayRef<ModuleDecl::AccessPathTy> paths) {
      for (auto path : paths) {
        if (ModuleDecl::matchesAccessPath(path, name))
          return false;
      }

      return true;
    };

    auto isScopedImport = [&](ArrayRef<ModuleDecl::AccessPathTy> paths) {
      for (auto path : paths) {
        if (path.empty())
          continue;
        if (ModuleDecl::matchesAccessPath(path, name))
          return true;
      }

      return false;
    };

    for (unsigned secondIdx : range(firstIdx + 1, decls.size())) {
      // Determine whether one module takes precedence over another.
      auto secondDecl = decls[secondIdx];
      auto secondModule = secondDecl->getModuleContext();
      bool secondTopLevel = secondDecl->getDeclContext()->isModuleScopeContext();

      // For member types, we skip most of the below rules. Instead, we allow
      // member types defined in a subclass to shadow member types defined in
      // a superclass.
      if (isa<TypeDecl>(firstDecl) &&
          isa<TypeDecl>(secondDecl) &&
          !firstTopLevel &&
          !secondTopLevel) {
        auto *firstClass = firstDecl->getDeclContext()->getSelfClassDecl();
        auto *secondClass = secondDecl->getDeclContext()->getSelfClassDecl();
        if (firstClass && secondClass && firstClass != secondClass) {
          if (firstClass->isSuperclassOf(secondClass)) {
            shadowed.insert(firstDecl);
            continue;
          } else if (secondClass->isSuperclassOf(firstClass)) {
            shadowed.insert(secondDecl);
            continue;
          }
        }

        continue;
      }

      // Top-level type declarations in a module shadow other declarations
      // visible through the module's imports.
      //
      // [Backward compatibility] Note that members of types have the same
      // shadowing check, but we do it after dropping unavailable members.
      if (firstModule != secondModule &&
          firstTopLevel && secondTopLevel) {
        auto firstPaths = imports.getAllAccessPathsNotShadowedBy(
          firstModule, secondModule, dc);
        auto secondPaths = imports.getAllAccessPathsNotShadowedBy(
          secondModule, firstModule, dc);

        // Check if one module shadows the other.
        if (isShadowed(firstPaths)) {
          shadowed.insert(firstDecl);
          break;
        } else if (isShadowed(secondPaths)) {
          shadowed.insert(secondDecl);
          continue;
        }

        // We might be in a situation where neither module shadows the
        // other, but one declaration is visible via a scoped import.
        bool firstScoped = isScopedImport(firstPaths);
        bool secondScoped = isScopedImport(secondPaths);
        if (!firstScoped && secondScoped) {
          shadowed.insert(firstDecl);
          break;
        } else if (firstScoped && !secondScoped) {
          shadowed.insert(secondDecl);
          continue;
        }
      }

      // Swift 4 compatibility hack: Don't shadow properties defined in
      // extensions of generic types with properties defined elsewhere.
      // This is due to the fact that in Swift 4, we only gave custom overload
      // types to properties in extensions of generic types, otherwise we
      // used the null type.
      if (!ctx.isSwiftVersionAtLeast(5)) {
        auto secondSig = secondDecl->getOverloadSignature();
        auto firstSig = firstDecl->getOverloadSignature();
        if (firstSig.IsVariable && secondSig.IsVariable)
          if (firstSig.InExtensionOfGenericType !=
              secondSig.InExtensionOfGenericType)
            continue;
      }

      // If one declaration is in a protocol or extension thereof and the
      // other is not, prefer the one that is not.
      if ((bool)firstDecl->getDeclContext()->getSelfProtocolDecl() !=
            (bool)secondDecl->getDeclContext()->getSelfProtocolDecl()) {
        if (firstDecl->getDeclContext()->getSelfProtocolDecl()) {
          shadowed.insert(firstDecl);
          break;
        } else {
          shadowed.insert(secondDecl);
          continue;
        }
      }

      // If one declaration is available and the other is not, prefer the
      // available one.
      if (firstDecl->getAttrs().isUnavailable(ctx) !=
            secondDecl->getAttrs().isUnavailable(ctx)) {
       if (firstDecl->getAttrs().isUnavailable(ctx)) {
         shadowed.insert(firstDecl);
         break;
       } else {
         shadowed.insert(secondDecl);
         continue;
       }
      }

      // Don't apply module-shadowing rules to members of protocol types.
      if (isa<ProtocolDecl>(firstDecl->getDeclContext()) ||
          isa<ProtocolDecl>(secondDecl->getDeclContext()))
        continue;

      // [Backward compatibility] For members of types, the general module
      // shadowing check is performed after unavailable candidates have
      // already been dropped.
      if (firstModule != secondModule &&
          !firstTopLevel && !secondTopLevel) {
        auto firstPaths = imports.getAllAccessPathsNotShadowedBy(
          firstModule, secondModule, dc);
        auto secondPaths = imports.getAllAccessPathsNotShadowedBy(
          secondModule, firstModule, dc);

        // Check if one module shadows the other.
        if (isShadowed(firstPaths)) {
          shadowed.insert(firstDecl);
          break;
        } else if (isShadowed(secondPaths)) {
          shadowed.insert(secondDecl);
          continue;
        }
      }

      // Prefer declarations in the any module over those in the standard
      // library module.
      if (auto swiftModule = ctx.getStdlibModule()) {
        if ((firstModule == swiftModule) != (secondModule == swiftModule)) {
          // If the second module is the standard library module, the second
          // declaration is shadowed by the first.
          if (secondModule == swiftModule) {
            shadowed.insert(secondDecl);
            continue;
          }

          // Otherwise, the first declaration is shadowed by the second. There is
          // no point in continuing to compare the first declaration to others.
          shadowed.insert(firstDecl);
          break;
        }
      }

      // The Foundation overlay introduced Data.withUnsafeBytes, which is
      // treated as being ambiguous with SwiftNIO's Data.withUnsafeBytes
      // extension. Apply a special-case name shadowing rule to use the
      // latter rather than the former, which be the consequence of a more
      // significant change to name shadowing in the future.
      if (auto owningStruct1
            = firstDecl->getDeclContext()->getSelfStructDecl()) {
        if (auto owningStruct2
              = secondDecl->getDeclContext()->getSelfStructDecl()) {
          if (owningStruct1 == owningStruct2 &&
              owningStruct1->getName().is("Data") &&
              isa<FuncDecl>(firstDecl) && isa<FuncDecl>(secondDecl) &&
              firstDecl->getFullName() == secondDecl->getFullName() &&
              firstDecl->getBaseName().userFacingName() == "withUnsafeBytes") {
            // If the second module is the Foundation module and the first
            // is the NIOFoundationCompat module, the second is shadowed by the
            // first.
            if (firstDecl->getModuleContext()->getName()
                  .is("NIOFoundationCompat") &&
                secondDecl->getModuleContext()->getName().is("Foundation")) {
              shadowed.insert(secondDecl);
              continue;
            }

            // If it's the other way around, the first declaration is shadowed
            // by the second.
            if (secondDecl->getModuleContext()->getName()
                  .is("NIOFoundationCompat") &&
                firstDecl->getModuleContext()->getName().is("Foundation")) {
              shadowed.insert(firstDecl);
              break;
            }
          }
        }
      }

      // Prefer declarations in an overlay to similar declarations in
      // the Clang module it customizes.
      if (firstDecl->hasClangNode() != secondDecl->hasClangNode()) {
        auto clangLoader = ctx.getClangModuleLoader();
        if (!clangLoader) continue;

        if (clangLoader->isInOverlayModuleForImportedModule(
                                              firstDecl->getDeclContext(),
                                              secondDecl->getDeclContext())) {
          shadowed.insert(secondDecl);
          continue;
        }

        if (clangLoader->isInOverlayModuleForImportedModule(
                                               secondDecl->getDeclContext(),
                                               firstDecl->getDeclContext())) {
          shadowed.insert(firstDecl);
          break;
        }
      }
    }
  }
}

/// Look through the given set of declarations (that all have the same name),
/// recording those that are shadowed by another declaration in the
/// \c shadowed set.
static void recordShadowedDeclsForImportedInits(
                                ArrayRef<ConstructorDecl *> ctors,
                                llvm::SmallPtrSetImpl<ValueDecl *> &shadowed) {
  assert(ctors.size() > 1 && "No collisions");

  ASTContext &ctx = ctors.front()->getASTContext();

  // Find the "best" constructor with this signature.
  ConstructorDecl *bestCtor = ctors[0];
  for (auto ctor : ctors.slice(1)) {
    auto comparison = compareConstructors(ctor, bestCtor, ctx);
    if (comparison == ConstructorComparison::Better)
      bestCtor = ctor;
  }

  // Shadow any initializers that are worse.
  for (auto ctor : ctors) {
    auto comparison = compareConstructors(ctor, bestCtor, ctx);
    if (comparison == ConstructorComparison::Worse)
      shadowed.insert(ctor);
  }
}

/// Look through the given set of declarations (that all have the same name),
/// recording those that are shadowed by another declaration in the
/// \c shadowed set.
static void recordShadowedDecls(ArrayRef<ValueDecl *> decls,
                                const DeclContext *dc,
                                llvm::SmallPtrSetImpl<ValueDecl *> &shadowed) {
  if (decls.size() < 2)
    return;

  // Categorize all of the declarations based on their overload signatures.
  llvm::SmallDenseMap<CanType, llvm::TinyPtrVector<ValueDecl *>> collisions;
  llvm::SmallVector<CanType, 2> collisionTypes;
  llvm::SmallDenseMap<NominalTypeDecl *, llvm::TinyPtrVector<ConstructorDecl *>>
    importedInitializerCollisions;
  llvm::TinyPtrVector<NominalTypeDecl *> importedInitializerCollectionTypes;

  for (auto decl : decls) {
    // Specifically keep track of imported initializers, which can come from
    // Objective-C init methods, Objective-C factory methods, renamed C
    // functions, or be synthesized by the importer.
    if (decl->hasClangNode() ||
        (isa<NominalTypeDecl>(decl->getDeclContext()) &&
         cast<NominalTypeDecl>(decl->getDeclContext())->hasClangNode())) {
      if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
        auto nominal = ctor->getDeclContext()->getSelfNominalTypeDecl();
        auto &knownInits = importedInitializerCollisions[nominal];
        if (knownInits.size() == 1) {
          importedInitializerCollectionTypes.push_back(nominal);
        }
        knownInits.push_back(ctor);
      }
    }

    CanType signature;

    if (!isa<TypeDecl>(decl)) {
      // If the decl is currently being validated, this is likely a recursive
      // reference and we'll want to skip ahead so as to avoid having its type
      // attempt to desugar itself.
      if (decl->isRecursiveValidation())
        continue;
      auto ifaceType = decl->getInterfaceType();

      // FIXME: the canonical type makes a poor signature, because we don't
      // canonicalize away default arguments.
      signature = ifaceType->getCanonicalType();

      // FIXME: The type of a variable or subscript doesn't include
      // enough context to distinguish entities from different
      // constrained extensions, so use the overload signature's
      // type. This is layering a partial fix upon a total hack.
      if (auto asd = dyn_cast<AbstractStorageDecl>(decl))
        signature = asd->getOverloadSignatureType();
    }

    // Record this declaration based on its signature.
    auto &known = collisions[signature];
    if (known.size() == 1) {
      collisionTypes.push_back(signature);
    }
    known.push_back(decl);
  }

  // Check whether we have shadowing for signature collisions.
  for (auto signature : collisionTypes) {
    recordShadowedDeclsAfterSignatureMatch(collisions[signature], dc,
                                           shadowed);
  }

  // Check whether we have shadowing for imported initializer collisions.
  for (auto nominal : importedInitializerCollectionTypes) {
    recordShadowedDeclsForImportedInits(importedInitializerCollisions[nominal],
                                        shadowed);
  }
}

bool swift::removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                                const DeclContext *dc) {
  // Collect declarations with the same (full) name.
  llvm::SmallDenseMap<DeclName, llvm::TinyPtrVector<ValueDecl *>>
    collidingDeclGroups;
  bool anyCollisions = false;
  for (auto decl : decls) {
    // Record this declaration based on its full name.
    auto &knownDecls = collidingDeclGroups[decl->getFullName()];
    if (!knownDecls.empty())
      anyCollisions = true;

    knownDecls.push_back(decl);
  }

  // If nothing collided, we're done.
  if (!anyCollisions)
    return false;

  // Walk through the declarations again, marking any declarations that shadow.
  llvm::SmallPtrSet<ValueDecl *, 4> shadowed;
  for (auto decl : decls) {
    auto known = collidingDeclGroups.find(decl->getFullName());
    if (known == collidingDeclGroups.end()) {
      // We already handled this group.
      continue;
    }

    recordShadowedDecls(known->second, dc, shadowed);
    collidingDeclGroups.erase(known);
  }

  // If no declarations were shadowed, we're done.
  if (shadowed.empty())
    return false;

  // Remove shadowed declarations from the list of declarations.
  bool anyRemoved = false;
  decls.erase(std::remove_if(decls.begin(), decls.end(),
                             [&](ValueDecl *vd) {
                               if (shadowed.count(vd) > 0) {
                                 anyRemoved = true;
                                 return true;
                               }

                               return false;
                             }),
              decls.end());

  return anyRemoved;
}

namespace {
enum class DiscriminatorMatch {
  NoDiscriminator,
  Matches,
  Different
};
} // end anonymous namespace

static DiscriminatorMatch matchDiscriminator(Identifier discriminator,
                                             const ValueDecl *value) {
  if (value->getFormalAccess() > AccessLevel::FilePrivate)
    return DiscriminatorMatch::NoDiscriminator;

  auto containingFile =
    dyn_cast<FileUnit>(value->getDeclContext()->getModuleScopeContext());
  if (!containingFile)
    return DiscriminatorMatch::Different;

  if (discriminator == containingFile->getDiscriminatorForPrivateValue(value))
    return DiscriminatorMatch::Matches;

  return DiscriminatorMatch::Different;
}

static DiscriminatorMatch
matchDiscriminator(Identifier discriminator,
                   LookupResultEntry lookupResult) {
  return matchDiscriminator(discriminator, lookupResult.getValueDecl());
}

template <typename Result>
void namelookup::filterForDiscriminator(SmallVectorImpl<Result> &results,
                                        DebuggerClient *debugClient) {
  if (debugClient == nullptr)
    return;
  Identifier discriminator = debugClient->getPreferredPrivateDiscriminator();
  if (discriminator.empty())
    return;

  auto lastMatchIter = std::find_if(results.rbegin(), results.rend(),
                                    [discriminator](Result next) -> bool {
    return
      matchDiscriminator(discriminator, next) == DiscriminatorMatch::Matches;
  });
  if (lastMatchIter == results.rend())
    return;

  Result lastMatch = *lastMatchIter;

  auto newEnd = std::remove_if(results.begin(), lastMatchIter.base()-1,
                               [discriminator](Result next) -> bool {
    return
      matchDiscriminator(discriminator, next) == DiscriminatorMatch::Different;
  });
  results.erase(newEnd, results.end());
  results.push_back(lastMatch);
}

template void namelookup::filterForDiscriminator<LookupResultEntry>(
    SmallVectorImpl<LookupResultEntry> &results, DebuggerClient *debugClient);

void namelookup::recordLookupOfTopLevelName(DeclContext *topLevelContext,
                                            DeclName name, bool isCascading) {
  auto SF = dyn_cast<SourceFile>(topLevelContext);
  if (!SF)
    return;
  auto *nameTracker = SF->getReferencedNameTracker();
  if (!nameTracker)
    return;
  nameTracker->addTopLevelName(name.getBaseName(), isCascading);
}

namespace {
  /// Whether we're looking up outer results or not.
  enum class LookupOuterResults {
    Excluded,
    Included
  };
}

/// Retrieve the set of type declarations that are directly referenced from
/// the given parsed type representation.
static DirectlyReferencedTypeDecls
directReferencesForTypeRepr(Evaluator &evaluator,
                            ASTContext &ctx, TypeRepr *typeRepr,
                            DeclContext *dc);

/// Retrieve the set of type declarations that are directly referenced from
/// the given type.
static DirectlyReferencedTypeDecls directReferencesForType(Type type);

/// Given a set of type declarations, find all of the nominal type declarations
/// that they reference, looking through typealiases as appropriate.
static TinyPtrVector<NominalTypeDecl *>
resolveTypeDeclsToNominal(Evaluator &evaluator,
                          ASTContext &ctx,
                          ArrayRef<TypeDecl *> typeDecls,
                          SmallVectorImpl<ModuleDecl *> &modulesFound,
                          bool &anyObject);

SelfBounds
SelfBoundsFromWhereClauseRequest::evaluate(
    Evaluator &evaluator,
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl) const {
  auto *typeDecl = decl.dyn_cast<TypeDecl *>();
  auto *protoDecl = dyn_cast_or_null<ProtocolDecl>(typeDecl);
  auto *extDecl = decl.dyn_cast<ExtensionDecl *>();

  DeclContext *dc = protoDecl ? (DeclContext *)protoDecl : (DeclContext *)extDecl;

  // A protocol or extension 'where' clause can reference associated types of
  // the protocol itself, so we have to start unqualified lookup from 'dc'.
  //
  // However, the right hand side of a 'Self' conformance constraint must be
  // resolved before unqualified lookup into 'dc' can work, so we make an
  // exception here and begin lookup from the parent context instead.
  auto *lookupDC = dc->getParent();
  auto requirements = protoDecl ? protoDecl->getTrailingWhereClause()
                                : extDecl->getTrailingWhereClause();

  ASTContext &ctx = dc->getASTContext();

  SelfBounds result;

  if (requirements == nullptr)
    return result;

  for (const auto &req : requirements->getRequirements()) {
    // We only care about type constraints.
    if (req.getKind() != RequirementReprKind::TypeConstraint)
      continue;

    // The left-hand side of the type constraint must be 'Self'.
    bool isSelfLHS = false;
    if (auto typeRepr = req.getSubjectRepr()) {
      if (auto identTypeRepr = dyn_cast<SimpleIdentTypeRepr>(typeRepr))
        isSelfLHS = (identTypeRepr->getIdentifier() == ctx.Id_Self);
    } else if (Type type = req.getSubject()) {
      isSelfLHS = type->isEqual(dc->getSelfInterfaceType());
    }
    if (!isSelfLHS)
      continue;

    // Resolve the right-hand side.
    DirectlyReferencedTypeDecls rhsDecls;
    if (auto typeRepr = req.getConstraintRepr()) {
      rhsDecls = directReferencesForTypeRepr(evaluator, ctx, typeRepr, lookupDC);
    } else if (Type type = req.getConstraint()) {
      rhsDecls = directReferencesForType(type);
    }

    SmallVector<ModuleDecl *, 2> modulesFound;
    auto rhsNominals = resolveTypeDeclsToNominal(evaluator, ctx, rhsDecls,
                                                 modulesFound,
                                                 result.anyObject);
    result.decls.insert(result.decls.end(),
                        rhsNominals.begin(),
                        rhsNominals.end());
  }

  return result;
}

SelfBounds swift::getSelfBoundsFromWhereClause(
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl) {
  auto *typeDecl = decl.dyn_cast<TypeDecl *>();
  auto *extDecl = decl.dyn_cast<ExtensionDecl *>();
  auto &ctx = typeDecl ? typeDecl->getASTContext()
                       : extDecl->getASTContext();
  return evaluateOrDefault(ctx.evaluator,
                           SelfBoundsFromWhereClauseRequest{decl}, {});
}

TinyPtrVector<TypeDecl *>
TypeDeclsFromWhereClauseRequest::evaluate(Evaluator &evaluator,
                                          ExtensionDecl *ext) const {
  ASTContext &ctx = ext->getASTContext();

  TinyPtrVector<TypeDecl *> result;
  for (const auto &req : ext->getGenericParams()->getTrailingRequirements()) {
    auto resolve = [&](TypeLoc loc) {
      DirectlyReferencedTypeDecls decls;
      if (auto *typeRepr = loc.getTypeRepr())
        decls = directReferencesForTypeRepr(evaluator, ctx, typeRepr, ext);
      else if (Type type = loc.getType())
        decls = directReferencesForType(type);

      result.insert(result.end(), decls.begin(), decls.end());
    };

    switch (req.getKind()) {
    case RequirementReprKind::TypeConstraint:
      resolve(req.getSubjectLoc());
      resolve(req.getConstraintLoc());
      break;

    case RequirementReprKind::SameType:
      resolve(req.getFirstTypeLoc());
      resolve(req.getSecondTypeLoc());
      break;

    case RequirementReprKind::LayoutConstraint:
      resolve(req.getSubjectLoc());
      break;
    }
  }

  return result;
}




#pragma mark Member lookup table

void LazyMemberLoader::anchor() {}

void LazyConformanceLoader::anchor() {}

/// Lookup table used to store members of a nominal type (and its extensions)
/// for fast retrieval.
class swift::MemberLookupTable {
  /// The last extension that was included within the member lookup table's
  /// results.
  ExtensionDecl *LastExtensionIncluded = nullptr;

  /// The type of the internal lookup table.
  typedef llvm::DenseMap<DeclName, llvm::TinyPtrVector<ValueDecl *>>
    LookupTable;

  /// Lookup table mapping names to the set of declarations with that name.
  LookupTable Lookup;

public:
  /// Create a new member lookup table.
  explicit MemberLookupTable(ASTContext &ctx);

  /// Update a lookup table with members from newly-added extensions.
  void updateLookupTable(NominalTypeDecl *nominal);

  /// Add the given member to the lookup table.
  void addMember(Decl *members);

  /// Add the given members to the lookup table.
  void addMembers(DeclRange members);

  /// Iterator into the lookup table.
  typedef LookupTable::iterator iterator;

  iterator begin() { return Lookup.begin(); }
  iterator end() { return Lookup.end(); }

  iterator find(DeclName name) {
    return Lookup.find(name);
  }

  void dump(llvm::raw_ostream &os) const {
    os << "LastExtensionIncluded:\n";
    if (LastExtensionIncluded)
      LastExtensionIncluded->printContext(os, 2);
    else
      os << "  nullptr\n";

    os << "Lookup:\n  ";
    for (auto &pair : Lookup) {
      pair.getFirst().print(os) << ":\n  ";
      for (auto &decl : pair.getSecond()) {
        os << "- ";
        decl->dumpRef(os);
        os << "\n  ";
      }
    }
    os << "\n";
  }

  SWIFT_DEBUG_DUMP {
    dump(llvm::errs());
  }

  // Mark all Decls in this table as not-resident in a table, drop
  // references to them. Should only be called when this was not fully-populated
  // from an IterableDeclContext.
  void clear() {
    // LastExtensionIncluded would only be non-null if this was populated from
    // an IterableDeclContext (though it might still be null in that case).
    assert(LastExtensionIncluded == nullptr);
    for (auto const &i : Lookup) {
      for (auto d : i.getSecond()) {
        d->setAlreadyInLookupTable(false);
      }
    }
    Lookup.clear();
  }

  // Only allow allocation of member lookup tables using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(MemberLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

namespace {
  /// Stores the set of Objective-C methods with a given selector within the
  /// Objective-C method lookup table.
  struct StoredObjCMethods {
    /// The generation count at which this list was last updated.
    unsigned Generation = 0;

    /// The set of methods with the given selector.
    llvm::TinyPtrVector<AbstractFunctionDecl *> Methods;
  };
} // end anonymous namespace

/// Class member lookup table, which is a member lookup table with a second
/// table for lookup based on Objective-C selector.
class ClassDecl::ObjCMethodLookupTable
        : public llvm::DenseMap<std::pair<ObjCSelector, char>,
                                StoredObjCMethods>
{
public:
  // Only allow allocation of member lookup tables using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(MemberLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

MemberLookupTable::MemberLookupTable(ASTContext &ctx) {
  // Register a cleanup with the ASTContext to call the lookup table
  // destructor.
  ctx.addCleanup([this]() {
    this->~MemberLookupTable();
  });
}

void MemberLookupTable::addMember(Decl *member) {
  // Only value declarations matter.
  auto vd = dyn_cast<ValueDecl>(member);
  if (!vd)
    return;

  // @_implements members get added under their declared name.
  auto A = vd->getAttrs().getAttribute<ImplementsAttr>();

  // Unnamed entities w/o @_implements synonyms cannot be found by name lookup.
  if (!A && !vd->hasName())
    return;

  // If this declaration is already in the lookup table, don't add it
  // again.
  if (vd->isAlreadyInLookupTable()) {
    return;
  }
  vd->setAlreadyInLookupTable();

  // Add this declaration to the lookup set under its compound name and simple
  // name.
  vd->getFullName().addToLookupTable(Lookup, vd);

  // And if given a synonym, under that name too.
  if (A)
    A->getMemberName().addToLookupTable(Lookup, vd);
}

void MemberLookupTable::addMembers(DeclRange members) {
  for (auto member : members) {
    addMember(member);
  }
}

void MemberLookupTable::updateLookupTable(NominalTypeDecl *nominal) {
  // If the last extension we included is the same as the last known extension,
  // we're already up-to-date.
  if (LastExtensionIncluded == nominal->LastExtension)
    return;

  // Add members from each of the extensions that we have not yet visited.
  for (auto next = LastExtensionIncluded
                     ? LastExtensionIncluded->NextExtension.getPointer()
                     : nominal->FirstExtension;
       next;
       (LastExtensionIncluded = next,next = next->NextExtension.getPointer())) {
    addMembers(next->getMembers());
  }
}

void NominalTypeDecl::addedMember(Decl *member) {
  auto *vd = dyn_cast<ValueDecl>(member);
  if (!vd)
    return;

  // If we have a lookup table, add the new member to it.
  auto *lookup = LookupTable.getPointer();
  if (lookup && isLookupTablePopulated()) {
    if (hasLazyMembers()) {
      // If we have lazy members, only add the new member to the lookup
      // table if we already have another member with the same name.
      // The presence of a lookup table entry indicates that the
      // nominal as well as all extensions have already been searched.
      if (lookup->find(vd->getBaseName()) == lookup->end())
        return;
    }

    lookup->addMember(vd);
  }
}

void NominalTypeDecl::addedExtension(ExtensionDecl *ext) {
  if (hasLazyMembers())
    setLookupTablePopulated(false);
}

void ExtensionDecl::addedMember(Decl *member) {
  // If this extension has already been bound to a nominal, add the new member
  // to the nominal's lookup table.
  if (NextExtension.getInt()) {
    auto nominal = getExtendedNominal();
    if (nominal)
      nominal->addedMember(member);
  }
}

// For lack of anywhere more sensible to put it, here's a diagram of the pieces
// involved in finding members and extensions of a NominalTypeDecl.
//
// ┌────────────────────────────┬─┐
// │IterableDeclContext         │ │     ┌─────────────────────────────┐
// │-------------------         │ │     │┌───────────────┬┐           ▼
// │Decl *LastDecl   ───────────┼─┼─────┘│Decl           ││  ┌───────────────┬┐
// │Decl *FirstDecl  ───────────┼─┼─────▶│----           ││  │Decl           ││
// │                            │ │      │Decl  *NextDecl├┼─▶│----           ││
// │bool HasLazyMembers         │ │      ├───────────────┘│  │Decl *NextDecl ││
// │IterableDeclContextKind Kind│ │      │                │  ├───────────────┘│
// │                            │ │      │ValueDecl       │  │                │
// ├────────────────────────────┘ │      │---------       │  │ValueDecl       │
// │                              │      │DeclName Name   │  │---------       │
// │NominalTypeDecl               │      └────────────────┘  │DeclName Name   │
// │---------------               │               ▲          └────────────────┘
// │ExtensionDecl *FirstExtension─┼────────┐      │                   ▲
// │ExtensionDecl *LastExtension ─┼───────┐│      │                   └───┐
// │                              │       ││      └──────────────────────┐│
// │MemberLookupTable *LookupTable├─┐     ││                             ││
// │bool LookupTableComplete      │ │     ││     ┌─────────────────┐     ││
// └──────────────────────────────┘ │     ││     │ExtensionDecl    │     ││
//                                  │     ││     │-------------    │     ││
//                    ┌─────────────┘     │└────▶│ExtensionDecl    │     ││
//                    │                   │      │  *NextExtension ├──┐  ││
//                    ▼                   │      └─────────────────┘  │  ││
// ┌─────────────────────────────────────┐│      ┌─────────────────┐  │  ││
// │MemberLookupTable                    ││      │ExtensionDecl    │  │  ││
// │-----------------                    ││      │-------------    │  │  ││
// │ExtensionDecl *LastExtensionIncluded ├┴─────▶│ExtensionDecl    │◀─┘  ││
// │                                     │       │  *NextExtension │     ││
// │┌───────────────────────────────────┐│       └─────────────────┘     ││
// ││DenseMap<Declname, ...> LookupTable││                               ││
// ││-----------------------------------││  ┌──────────────────────────┐ ││
// ││[NameA] TinyPtrVector<ValueDecl *> ││  │TinyPtrVector<ValueDecl *>│ ││
// ││[NameB] TinyPtrVector<ValueDecl *> ││  │--------------------------│ ││
// ││[NameC] TinyPtrVector<ValueDecl *>─┼┼─▶│[0] ValueDecl *      ─────┼─┘│
// │└───────────────────────────────────┘│  │[1] ValueDecl *      ─────┼──┘
// └─────────────────────────────────────┘  └──────────────────────────┘
//
// The HasLazyMembers, Kind, and LookupTableComplete fields are packed into
// PointerIntPairs so don't go grepping for them; but for purposes of
// illustration they are effectively their own fields.
//
// MemberLookupTable is populated en-masse when the IterableDeclContext's
// (IDC's) list of Decls is populated. But MemberLookupTable can also be
// populated incrementally by one-name-at-a-time lookups by lookupDirect, in
// which case those Decls are _not_ added to the IDC's list. They are cached in
// the loader they come from, lifecycle-wise, and are added to the
// MemberLookupTable to accelerate subsequent retrieval, but the IDC is not
// considered populated until someone calls getMembers().
//
// If the IDC list is later populated and/or an extension is added _after_
// MemberLookupTable is constructed (and possibly has entries in it),
// MemberLookupTable is purged and reconstructed from IDC's list.

static bool
populateLookupTableEntryFromLazyIDCLoader(ASTContext &ctx,
                                          MemberLookupTable &LookupTable,
                                          DeclName name,
                                          IterableDeclContext *IDC) {
  if (IDC->isLoadingLazyMembers()) {
    return false;
  }
  IDC->setLoadingLazyMembers(true);
  auto ci = ctx.getOrCreateLazyIterableContextData(IDC,
                                                   /*lazyLoader=*/nullptr);
  if (auto res = ci->loader->loadNamedMembers(IDC, name.getBaseName(),
                                              ci->memberData)) {
    IDC->setLoadingLazyMembers(false);
    if (auto s = ctx.Stats) {
      ++s->getFrontendCounters().NamedLazyMemberLoadSuccessCount;
    }
    for (auto d : *res) {
      LookupTable.addMember(d);
    }
    return false;
  } else {
    IDC->setLoadingLazyMembers(false);
    if (auto s = ctx.Stats) {
      ++s->getFrontendCounters().NamedLazyMemberLoadFailureCount;
    }
    return true;
  }
}

static void populateLookupTableEntryFromCurrentMembers(
    ASTContext &ctx, MemberLookupTable &LookupTable, DeclName name,
    IterableDeclContext *IDC) {
  for (auto m : IDC->getMembers()) {
    if (auto v = dyn_cast<ValueDecl>(m)) {
      if (v->getFullName().matchesRef(name.getBaseName())) {
        LookupTable.addMember(m);
      }
    }
  }
}

static void
populateLookupTableEntryFromExtensions(ASTContext &ctx,
                                       MemberLookupTable &table,
                                       NominalTypeDecl *nominal,
                                       DeclName name) {
  for (auto e : nominal->getExtensions()) {
    if (e->wasDeserialized() || e->hasClangNode()) {
      assert(!e->hasUnparsedMembers());
      if (populateLookupTableEntryFromLazyIDCLoader(ctx, table,
                                                    name, e)) {
        populateLookupTableEntryFromCurrentMembers(ctx, table, name, e);
      }
    } else {
      populateLookupTableEntryFromCurrentMembers(ctx, table, name, e);
    }
  }
}

bool NominalTypeDecl::isLookupTablePopulated() const {
  return LookupTable.getInt();
}

void NominalTypeDecl::setLookupTablePopulated(bool value) {
  LookupTable.setInt(value);
}

void NominalTypeDecl::prepareLookupTable() {
  // If we haven't allocated the lookup table yet, do so now.
  if (!LookupTable.getPointer()) {
    auto &ctx = getASTContext();
    LookupTable.setPointer(new (ctx) MemberLookupTable(ctx));
  }

  if (hasLazyMembers()) {
    assert(!hasUnparsedMembers());

    // Lazy members: if the table needs population, populate the table _only
    // from those members already in the IDC member list_ such as implicits or
    // globals-as-members, then update table entries from the extensions that
    // have the same names as any such initial-population members.
    if (!isLookupTablePopulated()) {
      setLookupTablePopulated(true);
      LookupTable.getPointer()->addMembers(getCurrentMembersWithoutLoading());

      llvm::SetVector<DeclName> baseNamesPresent;
      for (auto entry : *LookupTable.getPointer()) {
        baseNamesPresent.insert(entry.getFirst().getBaseName());
      }

      for (auto baseName : baseNamesPresent) {
        populateLookupTableEntryFromExtensions(getASTContext(),
                                               *LookupTable.getPointer(),
                                               this, baseName);
      }
    }

  } else {
    // No lazy members: if the table needs population, populate the table
    // en-masse; and in either case update the extensions.
    if (!isLookupTablePopulated()) {
      setLookupTablePopulated(true);
      LookupTable.getPointer()->addMembers(getMembers());
    }
    LookupTable.getPointer()->updateLookupTable(this);
  }
}

static TinyPtrVector<ValueDecl *>
maybeFilterOutAttrImplements(TinyPtrVector<ValueDecl *> decls,
                             DeclName name,
                             bool includeAttrImplements) {
  if (includeAttrImplements)
    return decls;
  TinyPtrVector<ValueDecl*> result;
  for (auto V : decls) {
    // Filter-out any decl that doesn't have the name we're looking for
    // (asserting as a consistency-check that such entries all have
    // @_implements attrs for the name!)
    if (V->getFullName().matchesRef(name)) {
      result.push_back(V);
    } else {
      auto A = V->getAttrs().getAttribute<ImplementsAttr>();
      (void)A;
      assert(A && A->getMemberName().matchesRef(name));
    }
  }
  return result;
}

TinyPtrVector<ValueDecl *> NominalTypeDecl::lookupDirect(
                                                  DeclName name,
                                                  OptionSet<LookupDirectFlags> flags) {
  ASTContext &ctx = getASTContext();
  if (auto s = ctx.Stats) {
    ++s->getFrontendCounters().NominalTypeLookupDirectCount;
  }

  // We only use NamedLazyMemberLoading when a user opts-in and we have
  // not yet loaded all the members into the IDC list in the first place.
  bool useNamedLazyMemberLoading = (ctx.LangOpts.NamedLazyMemberLoading &&
                                    hasLazyMembers());

  bool includeAttrImplements =
      flags.contains(LookupDirectFlags::IncludeAttrImplements);

  // FIXME: At present, lazy member is not able to find inherited constructors
  // in imported classes, because SwiftDeclConverter::importInheritedConstructors()
  // is only called via ClangImporter::Implementation::loadAllMembers().
  if (hasClangNode() &&
      name.getBaseName() == DeclBaseName::createConstructor())
    useNamedLazyMemberLoading = false;

  LLVM_DEBUG(llvm::dbgs() << getNameStr() << ".lookupDirect("
             << name << ")"
        << ", isLookupTablePopulated()=" << isLookupTablePopulated()
        << ", hasLazyMembers()=" << hasLazyMembers()
        << ", useNamedLazyMemberLoading=" << useNamedLazyMemberLoading
        << "\n");

  // We check the LookupTable at most twice, possibly treating a miss in the
  // first try as a cache-miss that we then do a cache-fill on, and retry.
  for (int i = 0; i < 2; ++i) {

    // First, if we're _not_ doing NamedLazyMemberLoading, we make sure we've
    // populated the IDC and brought it up to date with any extensions. This
    // will flip the hasLazyMembers() flag to false as well.
    if (!useNamedLazyMemberLoading) {
      // It's possible that the lookup table exists but has information in it
      // that is either currently out of date or soon to be out of date.
      // This can happen two ways:
      //
      //   - We've not yet indexed the members we have (isLookupTablePopulated()
      //     is zero).
      //
      //   - We've still got more lazy members left to load; this can happen
      //     even if we _did_ index some members.
      //
      // In either of these cases, we want to reset the table to empty and
      // mark it as needing reconstruction.
      if (LookupTable.getPointer() &&
          (hasLazyMembers() || !isLookupTablePopulated())) {
        LookupTable.getPointer()->clear();
        setLookupTablePopulated(false);
      }

      (void)getMembers();

      // Make sure we have the complete list of members (in this nominal and in
      // all extensions).
      for (auto E : getExtensions())
        (void)E->getMembers();
    }

    // Next, in all cases, prepare the lookup table for use, possibly
    // repopulating it from the IDC if the IDC member list has just grown.
    prepareLookupTable();

    // Look for a declaration with this name.
    auto known = LookupTable.getPointer()->find(name);

    // We found something; return it.
    if (known != LookupTable.getPointer()->end())
      return maybeFilterOutAttrImplements(known->second, name,
                                          includeAttrImplements);

    // If we have no more second chances, stop now.
    if (!useNamedLazyMemberLoading || i > 0)
      break;

    // If we get here, we had a cache-miss and _are_ using
    // NamedLazyMemberLoading. Try to populate a _single_ entry in the
    // MemberLookupTable from both this nominal and all of its extensions, and
    // retry. Any failure to load here flips the useNamedLazyMemberLoading to
    // false, and we fall back to loading all members during the retry.
    auto &Table = *LookupTable.getPointer();
    if (populateLookupTableEntryFromLazyIDCLoader(ctx, Table,
                                                  name, this)) {
      useNamedLazyMemberLoading = false;
    } else {
      populateLookupTableEntryFromExtensions(ctx, Table, this, name);
    }
  }

  // None of our attempts found anything.
  return { };
}

void ClassDecl::createObjCMethodLookup() {
  assert(!ObjCMethodLookup && "Already have an Objective-C member table");
  auto &ctx = getASTContext();
  ObjCMethodLookup = new (ctx) ObjCMethodLookupTable();

  // Register a cleanup with the ASTContext to call the lookup table
  // destructor.
  ctx.addCleanup([this]() {
    this->ObjCMethodLookup->~ObjCMethodLookupTable();
  });
}

MutableArrayRef<AbstractFunctionDecl *>
ClassDecl::lookupDirect(ObjCSelector selector, bool isInstance) {
  if (!ObjCMethodLookup) {
    createObjCMethodLookup();
  }

  // If any modules have been loaded since we did the search last (or if we
  // hadn't searched before), look in those modules, too.
  auto &stored = (*ObjCMethodLookup)[{selector, isInstance}];
  ASTContext &ctx = getASTContext();
  if (ctx.getCurrentGeneration() > stored.Generation) {
    ctx.loadObjCMethods(this, selector, isInstance, stored.Generation,
                        stored.Methods);
    stored.Generation = ctx.getCurrentGeneration();
  }

  return { stored.Methods.begin(), stored.Methods.end() };
}

void ClassDecl::recordObjCMethod(AbstractFunctionDecl *method,
                                 ObjCSelector selector) {
  if (!ObjCMethodLookup) {
    createObjCMethodLookup();
  }

  // Record the method.
  bool isInstanceMethod = method->isObjCInstanceMethod();
  auto &vec = (*ObjCMethodLookup)[{selector, isInstanceMethod}].Methods;

  // Check whether we have a duplicate. This only checks more than one
  // element in ill-formed code, so the linear search is acceptable.
  if (std::find(vec.begin(), vec.end(), method) != vec.end())
    return;

  if (auto *sf = method->getParentSourceFile()) {
    if (vec.size() == 1) {
      // We have a conflict.
      sf->ObjCMethodConflicts.push_back(std::make_tuple(this, selector,
                                                        isInstanceMethod));
    } if (vec.empty()) {
      sf->ObjCMethodList.push_back(method);
    }
  }

  vec.push_back(method);
}

/// Configure name lookup for the given declaration context and options.
///
/// This utility is used by qualified name lookup.
static void configureLookup(const DeclContext *dc,
                            NLOptions &options,
                            ReferencedNameTracker *&tracker,
                            bool &isLookupCascading) {
  auto &ctx = dc->getASTContext();
  if (ctx.isAccessControlDisabled())
    options |= NL_IgnoreAccessControl;

  // Find the dependency tracker we'll need for this lookup.
  tracker = nullptr;
  if (auto containingSourceFile =
          dyn_cast<SourceFile>(dc->getModuleScopeContext())) {
    tracker = containingSourceFile->getReferencedNameTracker();
  }

  auto checkLookupCascading = [dc, options]() -> Optional<bool> {
    switch (static_cast<unsigned>(options & NL_KnownDependencyMask)) {
    case 0:
      return dc->isCascadingContextForLookup(
               /*functionsAreNonCascading=*/false);
    case NL_KnownNonCascadingDependency:
      return false;
    case NL_KnownCascadingDependency:
      return true;
    case NL_KnownNoDependency:
      return None;
    default:
      // FIXME: Use llvm::CountPopulation_64 when that's declared constexpr.
#if defined(__clang__) || defined(__GNUC__)
      static_assert(__builtin_popcountll(NL_KnownDependencyMask) == 2,
                    "mask should only include four values");
#endif
      llvm_unreachable("mask only includes four values");
    }
  };

  // Determine whether a lookup here will cascade.
  isLookupCascading = false;
  if (tracker) {
    if (auto maybeLookupCascade = checkLookupCascading())
      isLookupCascading = maybeLookupCascade.getValue();
    else
      tracker = nullptr;
  }
}

/// Determine whether the given declaration is an acceptable lookup
/// result when searching from the given DeclContext.
static bool isAcceptableLookupResult(const DeclContext *dc,
                                     NLOptions options,
                                     ValueDecl *decl,
                                     bool onlyCompleteObjectInits) {
  // Filter out designated initializers, if requested.
  if (onlyCompleteObjectInits) {
    if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
      if (isa<ClassDecl>(ctor->getDeclContext()) && !ctor->isInheritable())
        return false;
    } else {
      return false;
    }
  }

  // Ignore stub implementations.
  if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    if (ctor->hasStubImplementation())
      return false;
  }

  // Check access.
  if (!(options & NL_IgnoreAccessControl)) {
    return decl->isAccessibleFrom(dc);
  }

  return true;
}

void namelookup::pruneLookupResultSet(const DeclContext *dc, NLOptions options,
                                      SmallVectorImpl<ValueDecl *> &decls) {
  // If we're supposed to remove overridden declarations, do so now.
  if (options & NL_RemoveOverridden)
    removeOverriddenDecls(decls);

  // If we're supposed to remove shadowed/hidden declarations, do so now.
  if (options & NL_RemoveNonVisible)
    removeShadowedDecls(decls, dc);

  ModuleDecl *M = dc->getParentModule();
  filterForDiscriminator(decls, M->getDebugClient());
}

/// Inspect the given type to determine which nominal type declarations it
/// directly references, to facilitate name lookup into those types.
static void extractDirectlyReferencedNominalTypes(
              Type type, SmallVectorImpl<NominalTypeDecl *> &decls) {
  if (auto nominal = type->getAnyNominal()) {
    decls.push_back(nominal);
    return;
  }

  if (auto unbound = type->getAs<UnboundGenericType>()) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(unbound->getDecl()))
      decls.push_back(nominal);
    return;
  }

  if (auto archetypeTy = type->getAs<ArchetypeType>()) {
    // Look in the protocols to which the archetype conforms (always).
    for (auto proto : archetypeTy->getConformsTo())
      decls.push_back(proto);

    // Look into the superclasses of this archetype.
    if (auto superclass = archetypeTy->getSuperclass()) {
      if (auto superclassDecl = superclass->getClassOrBoundGenericClass())
        decls.push_back(superclassDecl);
    }

    return;
  }

  if (auto compositionTy = type->getAs<ProtocolCompositionType>()) {
    auto layout = compositionTy->getExistentialLayout();

    for (auto proto : layout.getProtocols()) {
      auto *protoDecl = proto->getDecl();
      decls.push_back(protoDecl);
    }

    if (auto superclass = layout.explicitSuperclass) {
      auto *superclassDecl = superclass->getClassOrBoundGenericClass();
      if (superclassDecl)
        decls.push_back(superclassDecl);
    }

    return;
  }

  llvm_unreachable("Not a type containing nominal types?");
}

bool DeclContext::lookupQualified(Type type,
                                  DeclName member,
                                  NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  // Handle AnyObject lookup.
  if (type->isAnyObject())
    return lookupAnyObject(member, options, decls);

  // Handle lookup in a module.
  if (auto moduleTy = type->getAs<ModuleType>())
    return lookupQualified(moduleTy->getModule(), member, options, decls);

  // Figure out which nominal types we will look into.
  SmallVector<NominalTypeDecl *, 4> nominalTypesToLookInto;
  extractDirectlyReferencedNominalTypes(type, nominalTypesToLookInto);

  return lookupQualified(nominalTypesToLookInto, member, options, decls);
}

static void installPropertyWrapperMembersIfNeeded(NominalTypeDecl *target,
                                                  DeclName member) {
  auto &Context = target->getASTContext();
  auto baseName = member.getBaseName();
  if (!member.isSimpleName() || baseName.isSpecial())
    return;

  if ((!baseName.getIdentifier().str().startswith("$") &&
       !baseName.getIdentifier().str().startswith("_")) ||
      baseName.getIdentifier().str().size() <= 1) {
    return;
  }

  // $- and _-prefixed variables can be generated by properties that have
  // attached property wrappers.
  auto originalPropertyName =
      Context.getIdentifier(baseName.getIdentifier().str().substr(1));
  for (auto member : target->lookupDirect(originalPropertyName)) {
    if (auto var = dyn_cast<VarDecl>(member)) {
      if (var->hasAttachedPropertyWrapper()) {
        auto sourceFile = var->getDeclContext()->getParentSourceFile();
        if (sourceFile && sourceFile->Kind != SourceFileKind::Interface)
          (void)var->getPropertyWrapperBackingProperty();
      }
    }
  }
}

bool DeclContext::lookupQualified(ArrayRef<NominalTypeDecl *> typeDecls,
                                  DeclName member,
                                  NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  auto *stats = getASTContext().Stats;
  if (stats)
    stats->getFrontendCounters().NumLookupQualifiedInNominal++;

  // Configure lookup and dig out the tracker.
  ReferencedNameTracker *tracker = nullptr;
  bool isLookupCascading;
  configureLookup(this, options, tracker, isLookupCascading);

  // Tracking for the nominal types we'll visit.
  SmallVector<NominalTypeDecl *, 4> stack;
  llvm::SmallPtrSet<NominalTypeDecl *, 4> visited;
  bool sawClassDecl = false;

  // Add the given nominal type to the stack.
  auto addNominalType = [&](NominalTypeDecl *nominal) {
    if (!visited.insert(nominal).second)
      return false;

    if (isa<ClassDecl>(nominal))
      sawClassDecl = true;

    stack.push_back(nominal);
    return true;
  };

  // Add all of the nominal types to the stack.
  for (auto nominal : typeDecls) {
    addNominalType(nominal);
  }

  // Whether we only want to return complete object initializers.
  bool onlyCompleteObjectInits = false;

  // Visit all of the nominal types we know about, discovering any others
  // we need along the way.
  auto &ctx = getASTContext();
  bool wantProtocolMembers = (options & NL_ProtocolMembers);
  while (!stack.empty()) {
    auto current = stack.back();
    stack.pop_back();

    if (tracker)
      tracker->addUsedMember({current, member.getBaseName()},isLookupCascading);

    // Make sure we've resolved implicit members, if we need them.
    if (ctx.areSemanticQueriesEnabled()) {
      current->synthesizeSemanticMembersIfNeeded(member);
      installPropertyWrapperMembersIfNeeded(current, member);
    }

    // Look for results within the current nominal type and its extensions.
    bool currentIsProtocol = isa<ProtocolDecl>(current);
    auto flags = OptionSet<NominalTypeDecl::LookupDirectFlags>();
    if (options & NL_IncludeAttributeImplements)
      flags |= NominalTypeDecl::LookupDirectFlags::IncludeAttrImplements;
    for (auto decl : current->lookupDirect(member, flags)) {
      // If we're performing a type lookup, don't even attempt to validate
      // the decl if its not a type.
      if ((options & NL_OnlyTypes) && !isa<TypeDecl>(decl))
        continue;

      if (isAcceptableLookupResult(this, options, decl,
                                   onlyCompleteObjectInits))
        decls.push_back(decl);
    }

    // Visit superclass.
    if (auto classDecl = dyn_cast<ClassDecl>(current)) {
      // If we're looking for initializers, only look at the superclass if the
      // current class permits inheritance. Even then, only find complete
      // object initializers.
      bool visitSuperclass = true;
      if (member.getBaseName() == DeclBaseName::createConstructor()) {
        if (classDecl->inheritsSuperclassInitializers())
          onlyCompleteObjectInits = true;
        else
          visitSuperclass = false;
      }

      if (visitSuperclass) {
        if (auto superclassDecl = classDecl->getSuperclassDecl())
          if (visited.insert(superclassDecl).second)
            stack.push_back(superclassDecl);
      }
    }

    // If we're not looking at a protocol and we're not supposed to
    // visit the protocols that this type conforms to, skip the next
    // step.
    if (!wantProtocolMembers && !currentIsProtocol)
      continue;

    SmallVector<ProtocolDecl *, 4> protocols;

    if (auto *protoDecl = dyn_cast<ProtocolDecl>(current)) {
      // If we haven't seen a class declaration yet, look into the protocol.
      if (!sawClassDecl) {
        if (auto superclassDecl = protoDecl->getSuperclassDecl()) {
          visited.insert(superclassDecl);
          stack.push_back(superclassDecl);
        }
      }

      // Collect inherited protocols.
      for (auto inheritedProto : protoDecl->getInheritedProtocols()) {
        addNominalType(inheritedProto);
      }
    } else {
      // Collect the protocols to which the nominal type conforms.
      for (auto proto : current->getAllProtocols()) {
        if (visited.insert(proto).second) {
          stack.push_back(proto);
        }
      }

      // For a class, we don't need to visit the protocol members of the
      // superclass: that's already handled.
      if (isa<ClassDecl>(current))
        wantProtocolMembers = false;
    }
  }

  pruneLookupResultSet(this, options, decls);
  if (auto *debugClient = this->getParentModule()->getDebugClient()) {
    debugClient->finishLookupInNominals(this, typeDecls, member, options,
                                        decls);
  }
  // We're done. Report success/failure.
  return !decls.empty();
}

bool DeclContext::lookupQualified(ModuleDecl *module, DeclName member,
                                  NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;

  auto &ctx = getASTContext();
  auto *stats = ctx.Stats;
  if (stats)
    stats->getFrontendCounters().NumLookupQualifiedInModule++;

  // Configure lookup and dig out the tracker.
  ReferencedNameTracker *tracker = nullptr;
  bool isLookupCascading;
  configureLookup(this, options, tracker, isLookupCascading);

  auto kind = (options & NL_OnlyTypes
               ? ResolutionKind::TypesOnly
               : ResolutionKind::Overloadable);
  auto topLevelScope = getModuleScopeContext();
  if (module == topLevelScope->getParentModule()) {
    if (tracker) {
      recordLookupOfTopLevelName(topLevelScope, member, isLookupCascading);
    }
    lookupInModule(module, member, decls,
                   NLKind::QualifiedLookup, kind, topLevelScope);
  } else {
    // Note: This is a lookup into another module. Unless we're compiling
    // multiple modules at once, or if the other module re-exports this one,
    // it shouldn't be possible to have a dependency from that module on
    // anything in this one.

    // Perform the lookup in all imports of this module.
    auto accessPaths = ctx.getImportCache().getAllVisibleAccessPaths(
        module, topLevelScope);
    if (llvm::any_of(accessPaths,
                     [&](ModuleDecl::AccessPathTy accessPath) {
                       return ModuleDecl::matchesAccessPath(accessPath, member);
                     })) {
      lookupInModule(module, member, decls,
                     NLKind::QualifiedLookup, kind, topLevelScope);
    }
  }

  pruneLookupResultSet(this, options, decls);

  if (auto *debugClient = this->getParentModule()->getDebugClient()) {
    debugClient->finishLookupInModule(this, module, member, options, decls);
  }
  // We're done. Report success/failure.
  return !decls.empty();
}

bool DeclContext::lookupAnyObject(DeclName member, NLOptions options,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  using namespace namelookup;
  assert(decls.empty() && "additive lookup not supported");

  // Configure lookup and dig out the tracker.
  ReferencedNameTracker *tracker = nullptr;
  bool isLookupCascading;
  configureLookup(this, options, tracker, isLookupCascading);

  // Record this lookup.
  if (tracker)
    tracker->addDynamicLookupName(member.getBaseName(), isLookupCascading);

  // Type-only lookup won't find anything on AnyObject.
  if (options & NL_OnlyTypes)
    return false;

  auto *stats = getASTContext().Stats;
  if (stats)
    stats->getFrontendCounters().NumLookupQualifiedInAnyObject++;

  // Collect all of the visible declarations.
  SmallVector<ValueDecl *, 4> allDecls;
  for (auto import : namelookup::getAllImports(this)) {
    import.second->lookupClassMember(import.first, member, allDecls);
  }

  // For each declaration whose context is not something we've
  // already visited above, add it to the list of declarations.
  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  for (auto decl : allDecls) {
    // If the declaration is not @objc, it cannot be called dynamically.
    if (!decl->isObjC())
      continue;

    // If the declaration has an override, name lookup will also have
    // found the overridden method. Skip this declaration, because we
    // prefer the overridden method.
    if (decl->getOverriddenDecl())
      continue;

    auto dc = decl->getDeclContext();
    auto nominal = dc->getSelfNominalTypeDecl();
    assert(nominal && "Couldn't find nominal type?");
    (void)nominal;

    // If we didn't see this declaration before, and it's an acceptable
    // result, add it to the list.
    // declaration to the list.
    if (knownDecls.insert(decl).second &&
        isAcceptableLookupResult(this, options, decl,
                                 /*onlyCompleteObjectInits=*/false))
      decls.push_back(decl);
  }

  pruneLookupResultSet(this, options, decls);
  if (auto *debugClient = this->getParentModule()->getDebugClient()) {
    debugClient->finishLookupInAnyObject(this, member, options, decls);
  }
  // We're done. Report success/failure.
  return !decls.empty();
}

void DeclContext::lookupAllObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  // Collect all of the methods with this selector.
  for (auto import : namelookup::getAllImports(this)) {
    import.second->lookupObjCMethods(selector, results);
  }

  // Filter out duplicates.
  llvm::SmallPtrSet<AbstractFunctionDecl *, 8> visited;
  results.erase(
    std::remove_if(results.begin(), results.end(),
                   [&](AbstractFunctionDecl *func) -> bool {
                     return !visited.insert(func).second;
                   }),
    results.end());
}

/// Given a set of type declarations, find all of the nominal type declarations
/// that they reference, looking through typealiases as appropriate.
static TinyPtrVector<NominalTypeDecl *>
resolveTypeDeclsToNominal(Evaluator &evaluator,
                          ASTContext &ctx,
                          ArrayRef<TypeDecl *> typeDecls,
                          SmallVectorImpl<ModuleDecl *> &modulesFound,
                          bool &anyObject,
                          llvm::SmallPtrSetImpl<TypeAliasDecl *> &typealiases) {
  SmallPtrSet<NominalTypeDecl *, 4> knownNominalDecls;
  TinyPtrVector<NominalTypeDecl *> nominalDecls;
  auto addNominalDecl = [&](NominalTypeDecl *nominal) {
    if (knownNominalDecls.insert(nominal).second)
      nominalDecls.push_back(nominal);
  };

  for (auto typeDecl : typeDecls) {
    // Nominal type declarations get copied directly.
    if (auto nominalDecl = dyn_cast<NominalTypeDecl>(typeDecl)) {
      addNominalDecl(nominalDecl);
      continue;
    }

    // Recursively resolve typealiases.
    if (auto typealias = dyn_cast<TypeAliasDecl>(typeDecl)) {
      // FIXME: Ad hoc recursion breaking, so we don't look through the
      // same typealias multiple times.
      if (!typealiases.insert(typealias).second)
        continue;

      auto underlyingTypeReferences = evaluateOrDefault(evaluator,
        UnderlyingTypeDeclsReferencedRequest{typealias}, {});

      auto underlyingNominalReferences
        = resolveTypeDeclsToNominal(evaluator, ctx, underlyingTypeReferences,
                                    modulesFound, anyObject, typealiases);
      std::for_each(underlyingNominalReferences.begin(),
                    underlyingNominalReferences.end(),
                    addNominalDecl);

      // Recognize Swift.AnyObject directly.
      if (typealias->getName().is("AnyObject")) {
        // TypeRepr version: Builtin.AnyObject
        if (auto typeRepr = typealias->getUnderlyingTypeRepr()) {
          if (auto compound = dyn_cast<CompoundIdentTypeRepr>(typeRepr)) {
            auto components = compound->getComponents();
            if (components.size() == 2 &&
                components[0]->getIdentifier().is("Builtin") &&
                components[1]->getIdentifier().is("AnyObject")) {
              anyObject = true;
            }
          }
        }

        // Type version: an empty class-bound existential.
        if (typealias->hasInterfaceType()) {
          if (auto type = typealias->getUnderlyingType())
            if (type->isAnyObject())
              anyObject = true;
        }
      }

      continue;
    }

    // Keep track of modules we see.
    if (auto module = dyn_cast<ModuleDecl>(typeDecl)) {
      modulesFound.push_back(module);
      continue;
    }

    // Make sure we didn't miss some interesting kind of type declaration.
    assert(isa<AbstractTypeParamDecl>(typeDecl));
  }

  return nominalDecls;
}

static TinyPtrVector<NominalTypeDecl *>
resolveTypeDeclsToNominal(Evaluator &evaluator,
                          ASTContext &ctx,
                          ArrayRef<TypeDecl *> typeDecls,
                          SmallVectorImpl<ModuleDecl *> &modulesFound,
                          bool &anyObject) {
  llvm::SmallPtrSet<TypeAliasDecl *, 4> typealiases;
  return resolveTypeDeclsToNominal(evaluator, ctx, typeDecls, modulesFound,
                                   anyObject, typealiases);
}

/// Perform unqualified name lookup for types at the given location.
static DirectlyReferencedTypeDecls
directReferencesForUnqualifiedTypeLookup(DeclName name,
                                         SourceLoc loc, DeclContext *dc,
                                         LookupOuterResults lookupOuter) {
  DirectlyReferencedTypeDecls results;
  UnqualifiedLookupOptions options =
      UnqualifiedLookupFlags::TypeLookup |
      UnqualifiedLookupFlags::AllowProtocolMembers;
  if (lookupOuter == LookupOuterResults::Included)
    options |= UnqualifiedLookupFlags::IncludeOuterResults;

  auto &ctx = dc->getASTContext();
  auto descriptor = UnqualifiedLookupDescriptor(name, dc, loc, options);
  auto lookup = evaluateOrDefault(ctx.evaluator,
                                  UnqualifiedLookupRequest{descriptor}, {});
  for (const auto &result : lookup.allResults()) {
    if (auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl()))
      results.push_back(typeDecl);
  }

  return results;
}

/// Perform qualified name lookup for types.
static DirectlyReferencedTypeDecls
directReferencesForQualifiedTypeLookup(Evaluator &evaluator,
                                       ASTContext &ctx,
                                       ArrayRef<TypeDecl *> baseTypes,
                                       DeclName name,
                                       DeclContext *dc) {
  DirectlyReferencedTypeDecls result;
  auto addResults = [&result](ArrayRef<ValueDecl *> found){
    for (auto decl : found){
      assert(isa<TypeDecl>(decl) &&
             "Lookup should only have found type declarations");
      result.push_back(cast<TypeDecl>(decl));
    }
  };

  {
    // Look into the base types.
    SmallVector<ValueDecl *, 4> members;
    auto options = NL_RemoveNonVisible | NL_OnlyTypes;

    // Look through the type declarations we were given, resolving them down
    // to nominal type declarations, module declarations, and
    SmallVector<ModuleDecl *, 2> moduleDecls;
    bool anyObject = false;
    auto nominalTypeDecls =
      resolveTypeDeclsToNominal(ctx.evaluator, ctx, baseTypes, moduleDecls,
                                anyObject);

    dc->lookupQualified(nominalTypeDecls, name, options, members);

    // Search all of the modules.
    for (auto module : moduleDecls) {
      auto innerOptions = options;
      innerOptions &= ~NL_RemoveOverridden;
      innerOptions &= ~NL_RemoveNonVisible;
      dc->lookupQualified(module, name, innerOptions, members);
    }

    addResults(members);
  }

  return result;
}

/// Determine the types directly referenced by the given identifier type.
static DirectlyReferencedTypeDecls
directReferencesForIdentTypeRepr(Evaluator &evaluator,
                                 ASTContext &ctx, IdentTypeRepr *ident,
                                 DeclContext *dc) {
  DirectlyReferencedTypeDecls current;

  bool firstComponent = true;
  for (const auto &component : ident->getComponentRange()) {
    // If we already set a declaration, use it.
    if (auto typeDecl = component->getBoundDecl()) {
      current = {1, typeDecl};
      continue;
    }

    // For the first component, perform unqualified name lookup.
    if (current.empty()) {
      current =
        directReferencesForUnqualifiedTypeLookup(component->getIdentifier(),
                                                 component->getIdLoc(),
                                                 dc,
                                                 LookupOuterResults::Excluded);

      // If we didn't find anything, fail now.
      if (current.empty())
        return current;

      firstComponent = false;
      continue;
    }

    // For subsequent components, perform qualified name lookup.
    current =
        directReferencesForQualifiedTypeLookup(evaluator, ctx, current,
                                               component->getIdentifier(), dc);
    if (current.empty())
      return current;
  }

  return current;
}

static DirectlyReferencedTypeDecls
directReferencesForTypeRepr(Evaluator &evaluator,
                            ASTContext &ctx, TypeRepr *typeRepr,
                            DeclContext *dc) {
  switch (typeRepr->getKind()) {
  case TypeReprKind::Array:
    return {1, ctx.getArrayDecl()};

  case TypeReprKind::Attributed: {
    auto attributed = cast<AttributedTypeRepr>(typeRepr);
    return directReferencesForTypeRepr(evaluator, ctx,
                                       attributed->getTypeRepr(), dc);
  }

  case TypeReprKind::Composition: {
    DirectlyReferencedTypeDecls result;
    auto composition = cast<CompositionTypeRepr>(typeRepr);
    for (auto component : composition->getTypes()) {
      auto componentResult =
          directReferencesForTypeRepr(evaluator, ctx, component, dc);
      result.insert(result.end(),
                    componentResult.begin(),
                    componentResult.end());
    }
    return result;
  }

  case TypeReprKind::CompoundIdent:
  case TypeReprKind::GenericIdent:
  case TypeReprKind::SimpleIdent:
    return directReferencesForIdentTypeRepr(evaluator, ctx,
                                            cast<IdentTypeRepr>(typeRepr), dc);

  case TypeReprKind::Dictionary:
    return { 1, ctx.getDictionaryDecl()};

  case TypeReprKind::Tuple: {
    auto tupleRepr = cast<TupleTypeRepr>(typeRepr);
    if (tupleRepr->isParenType()) {
      return directReferencesForTypeRepr(evaluator, ctx,
                                         tupleRepr->getElementType(0), dc);
    }
    return { };
  }

  case TypeReprKind::Error:
  case TypeReprKind::Function:
  case TypeReprKind::InOut:
  case TypeReprKind::Metatype:
  case TypeReprKind::Owned:
  case TypeReprKind::Protocol:
  case TypeReprKind::Shared:
  case TypeReprKind::SILBox:
    return { };
      
  case TypeReprKind::OpaqueReturn:
    return { };

  case TypeReprKind::Fixed:
    llvm_unreachable("Cannot get fixed TypeReprs in name lookup");

  case TypeReprKind::Optional:
  case TypeReprKind::ImplicitlyUnwrappedOptional:
    return { 1, ctx.getOptionalDecl() };
  }
  llvm_unreachable("unhandled kind");
}

static DirectlyReferencedTypeDecls directReferencesForType(Type type) {
  // If it's a typealias, return that.
  if (auto aliasType = dyn_cast<TypeAliasType>(type.getPointer()))
    return { 1, aliasType->getDecl() };

  // If there is a generic declaration, return it.
  if (auto genericDecl = type->getAnyGeneric())
    return { 1, genericDecl };

  if (type->isExistentialType()) {
    DirectlyReferencedTypeDecls result;
    const auto &layout = type->getExistentialLayout();

    // Superclass.
    if (auto superclassType = layout.explicitSuperclass) {
      if (auto superclassDecl = superclassType->getAnyGeneric()) {
        result.push_back(superclassDecl);
      }
    }

    // Protocols.
    for (auto protocolTy : layout.getProtocols())
      result.push_back(protocolTy->getDecl());
    return result;
  }

  return { };
}

DirectlyReferencedTypeDecls InheritedDeclsReferencedRequest::evaluate(
    Evaluator &evaluator,
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
    unsigned index) const {

  // Prefer syntactic information when we have it.
  TypeLoc &typeLoc = getInheritedTypeLocAtIndex(decl, index);
  if (auto typeRepr = typeLoc.getTypeRepr()) {
    // Figure out the context in which name lookup will occur.
    DeclContext *dc;
    if (auto typeDecl = decl.dyn_cast<TypeDecl *>())
      dc = typeDecl->getInnermostDeclContext();
    else
      dc = decl.get<ExtensionDecl *>();

    return directReferencesForTypeRepr(evaluator, dc->getASTContext(), typeRepr,
                                       dc);
  }

  // Fall back to semantic types.
  // FIXME: In the long run, we shouldn't need this. Non-syntactic results
  // should be cached.
  if (auto type = typeLoc.getType()) {
    return directReferencesForType(type);
  }

  return { };
}

DirectlyReferencedTypeDecls UnderlyingTypeDeclsReferencedRequest::evaluate(
    Evaluator &evaluator,
    TypeAliasDecl *typealias) const {
  // Prefer syntactic information when we have it.
  if (auto typeRepr = typealias->getUnderlyingTypeRepr()) {
    return directReferencesForTypeRepr(evaluator, typealias->getASTContext(),
                                       typeRepr, typealias);
  }

  // Fall back to semantic types.
  // FIXME: In the long run, we shouldn't need this. Non-syntactic results
  // should be cached.
  if (auto type = typealias->getUnderlyingType()) {
    return directReferencesForType(type);
  }

  return { };
}

/// Evaluate a superclass declaration request.
llvm::Expected<ClassDecl *>
SuperclassDeclRequest::evaluate(Evaluator &evaluator,
                                NominalTypeDecl *subject) const {
  auto &Ctx = subject->getASTContext();

  // Protocols may get their superclass bound from a `where Self : Superclass`
  // clause.
  if (auto *proto = dyn_cast<ProtocolDecl>(subject)) {
    // If the protocol came from a serialized module, compute the superclass via
    // its generic signature.
    if (proto->wasDeserialized()) {
      auto superTy = proto->getGenericSignature()
          ->getSuperclassBound(proto->getSelfInterfaceType());
      if (superTy)
        return superTy->getClassOrBoundGenericClass();
    }

    // Otherwise check the where clause.
    auto selfBounds = getSelfBoundsFromWhereClause(proto);
    for (auto inheritedNominal : selfBounds.decls)
      if (auto classDecl = dyn_cast<ClassDecl>(inheritedNominal))
        return classDecl;
  }

  for (unsigned i : indices(subject->getInherited())) {
    // Find the inherited declarations referenced at this position.
    auto inheritedTypes = evaluateOrDefault(evaluator,
      InheritedDeclsReferencedRequest{subject, i}, {});

    // Resolve those type declarations to nominal type declarations.
    SmallVector<ModuleDecl *, 2> modulesFound;
    bool anyObject = false;
    auto inheritedNominalTypes
      = resolveTypeDeclsToNominal(evaluator, Ctx,
                                  inheritedTypes, modulesFound, anyObject);

    // Look for a class declaration.
    for (auto inheritedNominal : inheritedNominalTypes) {
      if (auto classDecl = dyn_cast<ClassDecl>(inheritedNominal))
        return classDecl;
    }
  }


  return nullptr;
}

llvm::Expected<NominalTypeDecl *>
ExtendedNominalRequest::evaluate(Evaluator &evaluator,
                                 ExtensionDecl *ext) const {
  auto typeRepr = ext->getExtendedTypeRepr();
  if (!typeRepr)
    // We must've seen 'extension { ... }' during parsing.
    return nullptr;

  ASTContext &ctx = ext->getASTContext();
  DirectlyReferencedTypeDecls referenced =
    directReferencesForTypeRepr(evaluator, ctx, typeRepr, ext->getParent());

  // Resolve those type declarations to nominal type declarations.
  SmallVector<ModuleDecl *, 2> modulesFound;
  bool anyObject = false;
  auto nominalTypes
    = resolveTypeDeclsToNominal(evaluator, ctx, referenced, modulesFound,
                                anyObject);

  // If there is more than 1 element, we will emit a warning or an error
  // elsewhere, so don't handle that case here.
  return nominalTypes.empty() ? nullptr : nominalTypes[0];
}

/// Whether there are only associated types in the set of declarations.
static bool declsAreAssociatedTypes(ArrayRef<TypeDecl *> decls) {
  if (decls.empty())
    return false;

  for (auto decl : decls) {
    if (!isa<AssociatedTypeDecl>(decl))
      return false;
  }

  return true;
}

llvm::Expected<NominalTypeDecl *>
CustomAttrNominalRequest::evaluate(Evaluator &evaluator,
                                   CustomAttr *attr, DeclContext *dc) const {
  // Find the types referenced by the custom attribute.
  auto &ctx = dc->getASTContext();
  TypeLoc &typeLoc = attr->getTypeLoc();
  DirectlyReferencedTypeDecls decls;
  if (auto typeRepr = typeLoc.getTypeRepr()) {
    decls = directReferencesForTypeRepr(
        evaluator, ctx, typeRepr, dc);
  } else if (Type type = typeLoc.getType()) {
    decls = directReferencesForType(type);
  }

  // Dig out the nominal type declarations.
  SmallVector<ModuleDecl *, 2> modulesFound;
  bool anyObject = false;
  auto nominals = resolveTypeDeclsToNominal(evaluator, ctx, decls,
                                            modulesFound, anyObject);
  if (nominals.size() == 1 && !isa<ProtocolDecl>(nominals.front()))
    return nominals.front();

  // If we found declarations that are associated types, look outside of
  // the current context to see if we can recover.
  if (declsAreAssociatedTypes(decls)) {
    if (auto typeRepr = typeLoc.getTypeRepr()) {
      if (auto identTypeRepr = dyn_cast<SimpleIdentTypeRepr>(typeRepr)) {
        auto assocType = cast<AssociatedTypeDecl>(decls.front());

        modulesFound.clear();
        anyObject = false;
        decls = directReferencesForUnqualifiedTypeLookup(
            identTypeRepr->getIdentifier(), identTypeRepr->getIdLoc(), dc,
            LookupOuterResults::Included);
        nominals = resolveTypeDeclsToNominal(evaluator, ctx, decls,
                                             modulesFound, anyObject);
        if (nominals.size() == 1 && !isa<ProtocolDecl>(nominals.front())) {
          auto nominal = nominals.front();
          if (nominal->getDeclContext()->isModuleScopeContext()) {
            // Complain, producing module qualification in a Fix-It.
            auto moduleName = nominal->getParentModule()->getName();
            ctx.Diags.diagnose(typeRepr->getLoc(),
                               diag::warn_property_wrapper_module_scope,
                               identTypeRepr->getIdentifier(),
                               moduleName)
              .fixItInsert(typeRepr->getLoc(),
                           moduleName.str().str() + ".");
            ctx.Diags.diagnose(assocType, diag::kind_declname_declared_here,
                               assocType->getDescriptiveKind(),
                               assocType->getFullName());

            ComponentIdentTypeRepr *components[2] = {
              new (ctx) SimpleIdentTypeRepr(typeRepr->getLoc(), moduleName),
              identTypeRepr
            };

            typeLoc = TypeLoc(IdentTypeRepr::create(ctx, components));
            return nominal;
          }
        }
      }
    }
  }

  return nullptr;
}

void swift::getDirectlyInheritedNominalTypeDecls(
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
    unsigned i,
    llvm::SmallVectorImpl<std::pair<SourceLoc, NominalTypeDecl *>> &result,
    bool &anyObject) {
  auto typeDecl = decl.dyn_cast<TypeDecl *>();
  auto extDecl = decl.dyn_cast<ExtensionDecl *>();

  ASTContext &ctx = typeDecl ? typeDecl->getASTContext()
                             : extDecl->getASTContext();

  // Find inherited declarations.
  auto referenced = evaluateOrDefault(ctx.evaluator,
    InheritedDeclsReferencedRequest{decl, i}, {});

  // Resolve those type declarations to nominal type declarations.
  SmallVector<ModuleDecl *, 2> modulesFound;
  auto nominalTypes
    = resolveTypeDeclsToNominal(ctx.evaluator, ctx, referenced, modulesFound,
                                anyObject);

  // Dig out the source location
  // FIXME: This is a hack. We need cooperation from
  // InheritedDeclsReferencedRequest to make this work.
  SourceLoc loc;
  if (TypeRepr *typeRepr = typeDecl ? typeDecl->getInherited()[i].getTypeRepr()
                                    : extDecl->getInherited()[i].getTypeRepr()){
    loc = typeRepr->getLoc();
  }

  // Form the result.
  for (auto nominal : nominalTypes) {
    result.push_back({loc, nominal});
  }
}

SmallVector<std::pair<SourceLoc, NominalTypeDecl *>, 4>
swift::getDirectlyInheritedNominalTypeDecls(
                        llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                        bool &anyObject) {
  auto typeDecl = decl.dyn_cast<TypeDecl *>();
  auto extDecl = decl.dyn_cast<ExtensionDecl *>();

  // Gather results from all of the inherited types.
  unsigned numInherited = typeDecl ? typeDecl->getInherited().size()
                                   : extDecl->getInherited().size();
  SmallVector<std::pair<SourceLoc, NominalTypeDecl *>, 4> result;
  for (unsigned i : range(numInherited)) {
    getDirectlyInheritedNominalTypeDecls(decl, i, result, anyObject);
  }

  auto *protoDecl = dyn_cast_or_null<ProtocolDecl>(typeDecl);
  if (protoDecl == nullptr)
    return result;

  // FIXME: Refactor SelfBoundsFromWhereClauseRequest to dig out
  // the source location.
  SourceLoc loc = SourceLoc();
  auto selfBounds = getSelfBoundsFromWhereClause(decl);
  anyObject |= selfBounds.anyObject;

  for (auto inheritedNominal : selfBounds.decls)
    result.emplace_back(loc, inheritedNominal);

  return result;
}

void FindLocalVal::checkPattern(const Pattern *Pat, DeclVisibilityKind Reason) {
  switch (Pat->getKind()) {
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(Pat)->getElements())
      checkPattern(field.getPattern(), Reason);
    return;
  case PatternKind::Paren:
  case PatternKind::Typed:
  case PatternKind::Var:
    return checkPattern(Pat->getSemanticsProvidingPattern(), Reason);
  case PatternKind::Named:
    return checkValueDecl(cast<NamedPattern>(Pat)->getDecl(), Reason);
  case PatternKind::EnumElement: {
    auto *OP = cast<EnumElementPattern>(Pat);
    if (OP->hasSubPattern())
      checkPattern(OP->getSubPattern(), Reason);
    return;
  }
  case PatternKind::OptionalSome:
    checkPattern(cast<OptionalSomePattern>(Pat)->getSubPattern(), Reason);
    return;

  case PatternKind::Is: {
    auto *isPat = cast<IsPattern>(Pat);
    if (isPat->hasSubPattern())
      checkPattern(isPat->getSubPattern(), Reason);
    return;
  }

  // Handle non-vars.
  case PatternKind::Bool:
  case PatternKind::Expr:
  case PatternKind::Any:
    return;
  }
}
  
void FindLocalVal::checkParameterList(const ParameterList *params) {
  for (auto param : *params) {
    checkValueDecl(param, DeclVisibilityKind::FunctionParameter);
  }
}

void FindLocalVal::checkGenericParams(GenericParamList *Params) {
  if (!Params)
    return;

  for (auto P : *Params)
    checkValueDecl(P, DeclVisibilityKind::GenericParameter);
}

void FindLocalVal::checkSourceFile(const SourceFile &SF) {
  for (Decl *D : SF.Decls)
    if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D))
      visitBraceStmt(TLCD->getBody(), /*isTopLevel=*/true);
}

void FindLocalVal::checkStmtCondition(const StmtCondition &Cond) {
  SourceLoc start = SourceLoc();
  for (auto entry : Cond) {
    if (start.isInvalid())
      start = entry.getStartLoc();
    if (auto *P = entry.getPatternOrNull()) {
      SourceRange previousConditionsToHere = SourceRange(start, entry.getEndLoc());
      if (!isReferencePointInRange(previousConditionsToHere))
        checkPattern(P, DeclVisibilityKind::LocalVariable);
    }
  }
}

void FindLocalVal::visitIfStmt(IfStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;

  if (!S->getElseStmt() ||
      !isReferencePointInRange(S->getElseStmt()->getSourceRange())) {
    checkStmtCondition(S->getCond());
  }

  visit(S->getThenStmt());
  if (S->getElseStmt())
    visit(S->getElseStmt());
}

void FindLocalVal::visitGuardStmt(GuardStmt *S) {
  if (SM.isBeforeInBuffer(Loc, S->getStartLoc()))
    return;

  // Names in the guard aren't visible until after the body.
  if (!isReferencePointInRange(S->getBody()->getSourceRange()))
    checkStmtCondition(S->getCond());

  visit(S->getBody());
}

void FindLocalVal::visitWhileStmt(WhileStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;

  checkStmtCondition(S->getCond());
  visit(S->getBody());
}
void FindLocalVal::visitRepeatWhileStmt(RepeatWhileStmt *S) {
  visit(S->getBody());
}
void FindLocalVal::visitDoStmt(DoStmt *S) {
  visit(S->getBody());
}

void FindLocalVal::visitForEachStmt(ForEachStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;
  visit(S->getBody());
  if (!isReferencePointInRange(S->getSequence()->getSourceRange()))
    checkPattern(S->getPattern(), DeclVisibilityKind::LocalVariable);
}

void FindLocalVal::visitBraceStmt(BraceStmt *S, bool isTopLevelCode) {
  if (isTopLevelCode) {
    if (SM.isBeforeInBuffer(Loc, S->getStartLoc()))
      return;
  } else {
    if (!isReferencePointInRange(S->getSourceRange()))
      return;
  }

  for (auto elem : S->getElements()) {
    if (auto *S = elem.dyn_cast<Stmt*>())
      visit(S);
  }
  for (auto elem : S->getElements()) {
    if (auto *D = elem.dyn_cast<Decl*>()) {
      if (auto *VD = dyn_cast<ValueDecl>(D))
        checkValueDecl(VD, DeclVisibilityKind::LocalVariable);
    }
  }
}
  
void FindLocalVal::visitSwitchStmt(SwitchStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;
  for (CaseStmt *C : S->getCases()) {
    visit(C);
  }
}

void FindLocalVal::visitCaseStmt(CaseStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;
  // Pattern names aren't visible in the patterns themselves,
  // just in the body or in where guards.
  bool inPatterns = isReferencePointInRange(S->getLabelItemsRange());
  auto items = S->getCaseLabelItems();
  if (inPatterns) {
    for (const auto &CLI : items) {
      auto guard = CLI.getGuardExpr();
      if (guard && isReferencePointInRange(guard->getSourceRange())) {
        checkPattern(CLI.getPattern(), DeclVisibilityKind::LocalVariable);
        break;
      }
    }
  }

  if (!inPatterns && !items.empty()) {
    for (auto *vd : S->getCaseBodyVariablesOrEmptyArray()) {
      checkValueDecl(vd, DeclVisibilityKind::LocalVariable);
    }
  }
  visit(S->getBody());
}

void FindLocalVal::visitDoCatchStmt(DoCatchStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;
  visit(S->getBody());
  visitCatchClauses(S->getCatches());
}
void FindLocalVal::visitCatchClauses(ArrayRef<CatchStmt*> clauses) {
  // TODO: some sort of binary search?
  for (auto clause : clauses) {
    visitCatchStmt(clause);
  }
}
void FindLocalVal::visitCatchStmt(CatchStmt *S) {
  if (!isReferencePointInRange(S->getSourceRange()))
    return;
  // Names in the pattern aren't visible until after the pattern.
  if (!isReferencePointInRange(S->getErrorPattern()->getSourceRange()))
    checkPattern(S->getErrorPattern(), DeclVisibilityKind::LocalVariable);
  visit(S->getBody());
}
