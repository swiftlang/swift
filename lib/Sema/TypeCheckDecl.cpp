//===--- TypeCheckDecl.cpp - Type Checking for Declarations ---------------===//
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
// This file implements semantic analysis for declarations.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

// FIXME: This is gross but temporary.
#include "llvm/Support/CommandLine.h"
llvm::cl::opt<bool> TLDefiniteInit("enable-top-level-definite-init",
                                   llvm::cl::Hidden);


/// \brief Describes the kind of implicit constructor that will be
/// generated.
enum class ImplicitConstructorKind {
  /// \brief The default constructor, which default-initializes each
  /// of the instance variables.
  Default,
  /// \brief The memberwise constructor, which initializes each of
  /// the instance variables from a parameter of the same type and
  /// name.
  Memberwise
};

/// \brief Determine whether the given pattern contains only a single variable
/// that is a property.
static bool isPatternProperty(Pattern *pattern) {
  pattern = pattern->getSemanticsProvidingPattern();
  if (auto named = dyn_cast<NamedPattern>(pattern))
    return named->getDecl()->isProperty();

  return false;
}

/// Determine whether the given declaration can inherit a class.
static bool canInheritClass(Decl *decl) {
  // Classes can inherit from a class.
  if (isa<ClassDecl>(decl))
    return true;

  // Generic type parameters can inherit a class.
  // FIXME: Associated types.
  if (isa<GenericTypeParamDecl>(decl))
    return true;

  // FIXME: Can any typealias declare inheritance from a class?
  return false;
}

/// Retrieve the declared type of a type declaration or extension.
static Type getDeclaredType(Decl *decl) {
  if (auto typeDecl = dyn_cast<TypeDecl>(decl))
    return typeDecl->getDeclaredType();
  return cast<ExtensionDecl>(decl)->getExtendedType();
}

// Add implicit conformances to the given declaration.
static void addImplicitConformances(
              TypeChecker &tc, Decl *decl,
              llvm::SmallSetVector<ProtocolDecl *, 4> &allProtocols) {
  if (auto nominal = dyn_cast<NominalTypeDecl>(decl)) {
    SmallVector<ProtocolDecl *, 2> protocols;
    nominal->getImplicitProtocols(protocols);
    allProtocols.insert(protocols.begin(), protocols.end());
  }
}

/// Check the inheritance clause of a type declaration or extension thereof.
///
/// This routine validates all of the types in the parsed inheritance clause,
/// recording the superclass (if any and if allowed) as well as the protocols
/// to which this type declaration conforms.
static void checkInheritanceClause(TypeChecker &tc, Decl *decl) {
  MutableArrayRef<TypeLoc> inheritedClause;

  // If we already checked the inheritance clause, don't do so again.
  if (auto type = dyn_cast<TypeDecl>(decl)) {
    if (type->checkedInheritanceClause())
      return;

    // This breaks infinite recursion, which will be diagnosed separately.
    type->setCheckedInheritanceClause();
    inheritedClause = type->getInherited();
  } else {
    auto ext = cast<ExtensionDecl>(decl);
    if (ext->checkedInheritanceClause())
      return;

    // This breaks infinite recursion, which will be diagnosed separately.
    ext->setCheckedInheritanceClause();
    inheritedClause = ext->getInherited();
  }

  // Check all of the types listed in the inheritance clause.
  Type superclassTy;
  SourceRange superclassRange;
  llvm::SmallSetVector<ProtocolDecl *, 4> allProtocols;
  llvm::SmallDenseMap<CanType, SourceRange> inheritedTypes;
  addImplicitConformances(tc, decl, allProtocols);
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    auto &inherited = inheritedClause[i];

    // Validate the type.
    if (tc.validateType(inherited)) {
      inherited.setInvalidType(tc.Context);
      continue;
    }

    auto inheritedTy = inherited.getType();

    // Check whether we inherited from the same type twice.
    CanType inheritedCanTy = inheritedTy->getCanonicalType();
    auto knownType = inheritedTypes.find(inheritedCanTy);
    if (knownType != inheritedTypes.end()) {
      SourceLoc afterPriorLoc
        = Lexer::getLocForEndOfToken(tc.Context.SourceMgr,
                                     inheritedClause[i-1].getSourceRange().End);
      SourceLoc afterMyEndLoc
        = Lexer::getLocForEndOfToken(tc.Context.SourceMgr,
                                     inherited.getSourceRange().End);

      tc.diagnose(inherited.getSourceRange().Start,
                  diag::duplicate_inheritance, inheritedTy)
        .fixItRemoveChars(afterPriorLoc, afterMyEndLoc)
        .highlight(knownType->second);
      continue;
    }
    inheritedTypes[inheritedCanTy] = inherited.getSourceRange();

    // If this is a protocol or protocol composition type, record the
    // protocols.
    if (inheritedTy->isExistentialType()) {
      // DynamicLookup cannot be used in a generic constraint.
      if (auto protoTy = inheritedTy->getAs<ProtocolType>()) {
        if (protoTy->getDecl()->isSpecificProtocol(
                                   KnownProtocolKind::DynamicLookup)) {
          tc.diagnose(inheritedClause[i].getSourceRange().Start,
                      diag::dynamic_lookup_conformance);
          continue;
        }
      }

      SmallVector<ProtocolDecl *, 4> protocols;
      inheritedTy->isExistentialType(protocols);
      allProtocols.insert(protocols.begin(), protocols.end());
      continue;
    }

    // If this is a class type, it may be the superclass.
    if (inheritedTy->getClassOrBoundGenericClass()) {
      // First, check if we already had a superclass.
      if (superclassTy) {
        // FIXME: Check for shadowed protocol names, i.e., NSObject?

        // Complain about multiple inheritance.
        // Don't emit a Fix-It here. The user has to think harder about this.
        tc.diagnose(inherited.getSourceRange().Start,
                    diag::multiple_inheritance, superclassTy, inheritedTy)
          .highlight(superclassRange);
        continue;
      }

      // If the declaration we're looking at doesn't allow a superclass,
      // complain.
      //
      // FIXME: Allow type aliases to 'inherit' from classes, as an additional
      // kind of requirement?
      if (!canInheritClass(decl)) {
        tc.diagnose(decl->getLoc(),
                    isa<ExtensionDecl>(decl)
                      ? diag::extension_class_inheritance
                      : diag::non_class_inheritance,
                    getDeclaredType(decl), inheritedTy)
          .highlight(inherited.getSourceRange());
        continue;
      }

      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        SourceLoc afterPriorLoc
          = Lexer::getLocForEndOfToken(
              tc.Context.SourceMgr,
              inheritedClause[i-1].getSourceRange().End);
        SourceLoc afterMyEndLoc
          = Lexer::getLocForEndOfToken(tc.Context.SourceMgr,
                                       inherited.getSourceRange().End);

        tc.diagnose(inherited.getSourceRange().Start,
                    diag::superclass_not_first, inheritedTy)
          .fixItRemoveChars(afterPriorLoc, afterMyEndLoc)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the superclass.
      }

      // Record the superclass.
      superclassTy = inheritedTy;
      superclassRange = inherited.getSourceRange();
      continue;
    }

    // If this is an error type, ignore it.
    if (inheritedTy->is<ErrorType>())
      continue;

    // We can't inherit from a non-class, non-protocol type.
    tc.diagnose(decl->getLoc(),
                canInheritClass(decl)
                  ? diag::inheritance_from_non_protocol_or_class
                  : diag::inheritance_from_non_protocol,
                inheritedTy);
    // FIXME: Note pointing to the declaration 'inheritedTy' references?
  }

  // Record the protocols to which this declaration conforms along with the
  // superclass.
  if (allProtocols.empty() && !superclassTy)
    return;

  auto allProtocolsCopy = tc.Context.AllocateCopy(allProtocols);
  if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    assert(!superclassTy && "Extensions can't add superclasses");
    ext->setProtocols(allProtocolsCopy);
    return;
  }

  auto typeDecl = cast<TypeDecl>(decl);
  typeDecl->setProtocols(allProtocolsCopy);
  if (superclassTy) {
    if (auto classDecl = dyn_cast<ClassDecl>(decl))
      classDecl->setSuperclass(superclassTy);
    else
      cast<AbstractTypeParamDecl>(decl)->setSuperclass(superclassTy);
  }

  // For protocols, generic parameters, and associated types, fill in null
  // conformances.
  if (isa<ProtocolDecl>(decl) || isa<AbstractTypeParamDecl>(decl)) {
     // Set null conformances.
     unsigned conformancesSize
         = sizeof(ProtocolConformance *) * allProtocols.size();
     ProtocolConformance **conformances
         = (ProtocolConformance **)tc.Context.Allocate(
             conformancesSize,
             alignof(ProtocolConformance *));
     memset(conformances, 0, conformancesSize);
     cast<TypeDecl>(decl)->setConformances(
       llvm::makeArrayRef(conformances, allProtocols.size()));
  }
}

/// Retrieve the set of protocols the given protocol inherits.
static ArrayRef<ProtocolDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                          ProtocolDecl *proto,
                                                          ProtocolDecl **scratch) {
  return tc.getDirectConformsTo(proto);
}

/// Retrieve the superclass of the given class.
static ArrayRef<ClassDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                       ClassDecl *classDecl,
                                                       ClassDecl **scratch) {
  checkInheritanceClause(tc, classDecl);

  if (classDecl->hasSuperclass()) {
    *scratch = classDecl->getSuperclass()->getClassOrBoundGenericClass();
    return *scratch;
  }
  return { };
}

// Break the inheritance cycle for a protocol by removing all inherited
// protocols.
//
// FIXME: Just remove the problematic inheritance?
static void breakInheritanceCycle(ProtocolDecl *proto) {
  proto->setProtocols({ });
  proto->setConformances({ });
}

/// Break the inheritance cycle for a protocol by removing its superclass.
static void breakInheritanceCycle(ClassDecl *classDecl) {
  classDecl->setSuperclass(Type());
}

/// Check for circular inheritance.
template<typename T>
static void checkCircularity(TypeChecker &tc, T *decl,
                             Diag<StringRef> circularDiag,
                             Diag<Identifier> declHereDiag,
                             SmallVectorImpl<T *> &path) {
  switch (decl->getCircularityCheck()) {
  case CircularityCheck::Checked:
    return;

  case CircularityCheck::Checking: {
    // We're already checking this protocol, which means we have a cycle.

    // Find the beginning of the cycle within the full path.
    auto cycleStart = path.end()-2;
    while (*cycleStart != decl) {
      assert(cycleStart != path.begin() && "Missing cycle start?");
      --cycleStart;
    }

    // Form the textual path illustrating the cycle.
    llvm::SmallString<128> pathStr;
    for (auto i = cycleStart, iEnd = path.end(); i != iEnd; ++i) {
      if (!pathStr.empty())
        pathStr += " -> ";
      pathStr += ("'" + (*i)->getName().str() + "'").str();
    }
    pathStr += (" -> '" + decl->getName().str() + "'").str();

    // Diagnose the cycle.
    tc.diagnose(decl->getLoc(), circularDiag, pathStr);
    for (auto i = cycleStart + 1, iEnd = path.end(); i != iEnd; ++i) {
      tc.diagnose(*i, declHereDiag, (*i)->getName());
    }

    // Set this declaration as invalid, then break the cycle somehow.
    decl->setInvalid();
    decl->overwriteType(ErrorType::get(tc.Context));
    breakInheritanceCycle(decl);
    break;
  }

  case CircularityCheck::Unchecked: {
    // Walk to the inherited class or protocols.
    path.push_back(decl);
    decl->setCircularityCheck(CircularityCheck::Checking);
    T *scratch = nullptr;
    for (auto inherited : getInheritedForCycleCheck(tc, decl, &scratch)) {
      checkCircularity(tc, inherited, circularDiag, declHereDiag, path);
    }
    decl->setCircularityCheck(CircularityCheck::Checked);
    path.pop_back();
    break;
  }
  }
}

namespace {

class DeclChecker : public DeclVisitor<DeclChecker> {

public:
  TypeChecker &TC;

  // For library-style parsing, we need to make two passes over the global
  // scope.  These booleans indicate whether this is currently the first or
  // second pass over the global scope (or neither, if we're in a context where
  // we only visit each decl once).
  bool IsFirstPass;
  bool IsSecondPass;

  DeclChecker(TypeChecker &TC, bool IsFirstPass, bool IsSecondPass)
      : TC(TC), IsFirstPass(IsFirstPass), IsSecondPass(IsSecondPass) {}

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//

  void validateAttributes(ValueDecl *VD);

  template<typename DeclType>
  void gatherExplicitConformances(DeclType *D, Type T) {
    SmallVector<ProtocolConformance *, 4> conformances;
    for (auto proto : D->getProtocols()) {
      ProtocolConformance *conformance = nullptr;
      // FIXME: Better location info
      if (TC.conformsToProtocol(T, proto, &conformance,
                                D->getStartLoc(), D)) {
        // For nominal types and extensions thereof, record conformance
        // to known protocols.
        if (auto kind = proto->getKnownProtocolKind())
          if (isa<NominalTypeDecl>(D) || isa<ExtensionDecl>(D))
            TC.Context.recordConformance(kind.getValue(), D);
      }
      conformances.push_back(conformance);
    }

    D->setConformances(D->getASTContext().AllocateCopy(conformances));
  }

  void checkExplicitConformance(TypeDecl *D, Type T) {
    gatherExplicitConformances(D, T);
  }

  void checkExplicitConformance(ExtensionDecl *D, Type T) {
    gatherExplicitConformances(D, T);
  }

  /// Create a fresh archetype building.
  ArchetypeBuilder createArchetypeBuilder() {
    return ArchetypeBuilder(
             TC.TU, TC.Diags,
             [&](ProtocolDecl *protocol) -> ArrayRef<ProtocolDecl *> {
               return TC.getDirectConformsTo(protocol);
             },
             [&](AbstractTypeParamDecl *assocType) -> ArrayRef<ProtocolDecl *> {
               checkInheritanceClause(TC, assocType);
               return assocType->getProtocols();
             });
  }

  /// Revert the given dependently-typed TypeLoc to a state where generic
  /// parameters have not yet been resolved.
  void revertDependentTypeLoc(TypeLoc &tl, DeclContext *dc) {
    // Make sure we validate the type again.
    tl.setType(Type(), /*validated=*/false);

    // Walker that reverts dependent identifier types.
    class RevertWalker : public ASTWalker {
      DeclContext *dc;

    public:
      explicit RevertWalker(DeclContext *dc) : dc(dc) { }

      // Skip expressions.
      virtual std::pair<bool, Expr *> walkToExprPre(Expr *expr) {
        return { false, expr };
      }

      // Skip statements.
      virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) {
        return { false, stmt };
      }

      // Skip patterns
      virtual std::pair<bool, Pattern*> walkToPatternPre(Pattern *pattern) {
        return { false, pattern };
      }

      virtual bool walkToTypeReprPost(TypeRepr *repr) {
        auto identType = dyn_cast<IdentTypeRepr>(repr);
        if (!identType)
          return true;

        for (auto &comp : identType->Components) {
          // If it's not a bound type, we're done.
          if (!comp.isBoundType())
            return true;

          // If the bound type isn't dependent, there's nothing to do.
          auto type = comp.getBoundType();
          if (!type->isDependentType())
            return true;

          // Turn a generic parameter type back into a reference to the
          // generic parameter itself.
          if (auto genericParamType
              = dyn_cast<GenericTypeParamType>(type.getPointer())) {
            // FIXME: Assert that it has a decl.
            comp.setValue(genericParamType->getDecl());
          } else {
            // FIXME: Often, we could revert to a decl here.
            comp.revertToContext(dc);
          }
        }

        return true;
      }
    };

    if (tl.isNull())
      return;

    tl.getTypeRepr()->walk(RevertWalker(dc));
  }

  /// Revert the dependently-typed TypeLocs within the given pattern to a
  /// state where generic parameters have not yet been resolved.
  void revertDependentPattern(Pattern *pattern, DeclContext *dc) {
    // Clear out the pattern's type.
    if (pattern->hasType())
      pattern->overwriteType(Type());

    switch (pattern->getKind()) {
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
      // Do nothing for refutable patterns.
      break;

    case PatternKind::Any:
      // Do nothing;
      break;

    case PatternKind::Named: {
      // Clear out the type of the variable.
      auto named = cast<NamedPattern>(pattern);
      if (named->getDecl()->hasType())
        named->getDecl()->overwriteType(Type());
      break;
    }

    case PatternKind::Paren:
      // Recurse into parentheses.
      revertDependentPattern(cast<ParenPattern>(pattern)->getSubPattern(), dc);
      break;

    case PatternKind::Tuple: {
      // Recurse into tuple elements.
      auto tuple = cast<TuplePattern>(pattern);
      for (auto &field : tuple->getFields()) {
        revertDependentPattern(field.getPattern(), dc);
      }
      break;
    }

    case PatternKind::Typed: {
      // Revert the type annotation.
      auto typed = cast<TypedPattern>(pattern);
      revertDependentTypeLoc(typed->getTypeLoc(), dc);

      // Revert the subpattern.
      revertDependentPattern(typed->getSubPattern(), dc);
      break;
    }
    }
  }

  /// Check the given generic parameter list, introduce the generic parameters
  /// and requirements into the archetype builder, but don't assign archetypes
  /// yet.
  void checkGenericParamList(ArchetypeBuilder &builder,
                             GenericParamList *genericParams) {
    assert(genericParams && "Missing generic parameters");
    unsigned Depth = genericParams->getDepth();

    // Assign archetypes to each of the generic parameters.
    unsigned Index = 0;
    for (auto GP : *genericParams) {
      auto TypeParam = GP.getAsTypeParam();

      // Set the depth of this type parameter.
      TypeParam->setDepth(Depth);

      // Check the constraints on the type parameter.
      checkInheritanceClause(TC, TypeParam);

      // Add the generic parameter to the builder.
      builder.addGenericParameter(TypeParam, Index++);

      // Infer requirements from the "inherited" types.
      for (auto &inherited : TypeParam->getInherited()) {
        builder.inferRequirements(inherited.getTypeRepr());
      }
    }

    // Add the requirements clause to the builder, validating the types in
    // the requirements clause along the way.
    for (auto &Req : genericParams->getRequirements()) {
      if (Req.isInvalid())
        continue;

      switch (Req.getKind()) {
      case RequirementKind::Conformance: {
        // Validate the types.
        if (TC.validateType(Req.getSubjectLoc())) {
          Req.setInvalid();
          continue;
        }

        if (TC.validateType(Req.getConstraintLoc())) {
          Req.setInvalid();
          continue;
        }

        // FIXME: Feels too early to perform this check.
        if (!Req.getConstraint()->isExistentialType() &&
            !Req.getConstraint()->getClassOrBoundGenericClass()) {
          TC.diagnose(genericParams->getWhereLoc(),
                      diag::requires_conformance_nonprotocol,
                      Req.getSubjectLoc(), Req.getConstraintLoc());
          Req.getConstraintLoc().setInvalidType(TC.Context);
          Req.setInvalid();
          continue;
        }

        // DynamicLookup cannot be used in a generic constraint.
        if (auto protoTy = Req.getConstraint()->getAs<ProtocolType>()) {
          if (protoTy->getDecl()->isSpecificProtocol(
                                    KnownProtocolKind::DynamicLookup)) {
            TC.diagnose(Req.getConstraintLoc().getSourceRange().Start,
                        diag::dynamic_lookup_conformance);
            continue;
          }
        }
        break;
      }

      case RequirementKind::SameType:
        if (TC.validateType(Req.getFirstTypeLoc())) {
          Req.setInvalid();
          continue;
        }

        if (TC.validateType(Req.getSecondTypeLoc())) {
          Req.setInvalid();
          continue;
        }
        
        break;
      }
      
      if (builder.addRequirement(Req))
        Req.setInvalid();
    }
  }

  /// Finalize the given generic parameter list, assigning archetypes to
  /// the generic parameters.
  void finalizeGenericParamList(ArchetypeBuilder &builder,
                                GenericParamList *genericParams,
                                DeclContext *dc) {
    // Wire up the archetypes.
    builder.assignArchetypes();
    for (auto GP : *genericParams) {
      auto TypeParam = GP.getAsTypeParam();
      TypeParam->setArchetype(builder.getArchetype(TypeParam));
    }
    genericParams->setAllArchetypes(
      TC.Context.AllocateCopy(builder.getAllArchetypes()));

    // Replace the generic parameters with their archetypes throughout the
    // types in the requirements.
    // FIXME: This should not be necessary at this level; it is a transitional
    // step.
    for (auto &Req : genericParams->getRequirements()) {
      if (Req.isInvalid())
        continue;

      switch (Req.getKind()) {
        case RequirementKind::Conformance: {
          revertDependentTypeLoc(Req.getSubjectLoc(), dc);
          if (TC.validateType(Req.getSubjectLoc())) {
            Req.setInvalid();
            continue;
          }

          revertDependentTypeLoc(Req.getConstraintLoc(), dc);
          if (TC.validateType(Req.getConstraintLoc())) {
            Req.setInvalid();
            continue;
          }
          break;
        }

        case RequirementKind::SameType:
          revertDependentTypeLoc(Req.getFirstTypeLoc(), dc);
          if (TC.validateType(Req.getFirstTypeLoc())) {
            Req.setInvalid();
            continue;
          }

          revertDependentTypeLoc(Req.getSecondTypeLoc(), dc);
          if (TC.validateType(Req.getSecondTypeLoc())) {
            Req.setInvalid();
            continue;
          }
          break;
      }
    }
  }

  /// Check the given generic parameter list within the given declaration
  /// context, assigning archetypes to each of the generic parameters.
  ///
  /// This routine simply creates an archetype builder, then calls both
  /// \c checkGenericParamList and \c finalizeGenericParamList.
  void checkGenericParams(GenericParamList *genericParams, DeclContext *dc) {
    ArchetypeBuilder builder = createArchetypeBuilder();
    checkGenericParamList(builder, genericParams);
    finalizeGenericParamList(builder, genericParams, dc);
  }

  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  void visitImportDecl(ImportDecl *ID) {
    // Nothing to do.
  }

  void visitBoundVars(Pattern *P) {
    switch (P->getKind()) {
    // Recur into patterns.
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(P)->getFields())
        visitBoundVars(field.getPattern());
      return;
    case PatternKind::Paren:
      return visitBoundVars(cast<ParenPattern>(P)->getSubPattern());
    case PatternKind::Typed:
      return visitBoundVars(cast<TypedPattern>(P)->getSubPattern());
    case PatternKind::NominalType:
      return visitBoundVars(cast<NominalTypePattern>(P)->getSubPattern());
    case PatternKind::UnionElement: {
      auto *OP = cast<UnionElementPattern>(P);
      if (OP->hasSubPattern())
        visitBoundVars(OP->getSubPattern());
      return;
    }
    case PatternKind::Var:
      return visitBoundVars(cast<VarPattern>(P)->getSubPattern());

    // Handle vars.
    case PatternKind::Named: {
      VarDecl *VD = cast<NamedPattern>(P)->getDecl();

      if (!VD->getType()->isMaterializable()) {
        TC.diagnose(VD->getStartLoc(), diag::var_type_not_materializable,
                    VD->getType());
        VD->overwriteType(ErrorType::get(TC.Context));
        VD->setInvalid();
      }

      validateAttributes(VD);

      // The var requires ObjC interop if it has an [objc] or [iboutlet]
      // attribute or if it's a member of an ObjC class.
      DeclContext *dc = VD->getDeclContext();
      if (dc && dc->getDeclaredTypeInContext()) {
        ClassDecl *classContext = dc->getDeclaredTypeInContext()
          ->getClassOrBoundGenericClass();
        ProtocolDecl *protocolContext = dyn_cast<ProtocolDecl>(dc);
        VD->setIsObjC(VD->getAttrs().isObjC()
                      || (classContext && classContext->isObjC())
                      || (protocolContext && protocolContext->isObjC()));
      }

      return;
    }

    // Handle non-vars.
    case PatternKind::Any:
    case PatternKind::Isa:
    case PatternKind::Expr:
      return;
    }
    llvm_unreachable("bad pattern kind!");
  }

  void setBoundVarsTypeError(Pattern *pattern) {
    switch (pattern->getKind()) {
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(pattern)->getFields())
        setBoundVarsTypeError(field.getPattern());
      return;
    case PatternKind::Paren:
      return setBoundVarsTypeError(cast<ParenPattern>(pattern)->getSubPattern());
    case PatternKind::Typed:
      return setBoundVarsTypeError(cast<TypedPattern>(pattern)->getSubPattern());
    case PatternKind::NominalType:
      return setBoundVarsTypeError(cast<NominalTypePattern>(pattern)
                                     ->getSubPattern());
    case PatternKind::Var:
      return setBoundVarsTypeError(cast<VarPattern>(pattern)->getSubPattern());
    case PatternKind::UnionElement:
      if (auto subpattern = cast<UnionElementPattern>(pattern)->getSubPattern())
        setBoundVarsTypeError(subpattern);
      return;

    // Handle vars.
    case PatternKind::Named: {
      VarDecl *var = cast<NamedPattern>(pattern)->getDecl();
      var->overwriteType(ErrorType::get(TC.Context));
      var->setInvalid();
      return;
    }

    // Handle non-vars.
    case PatternKind::Any:
    case PatternKind::Isa:
    case PatternKind::Expr:
      return;
    }
    llvm_unreachable("bad pattern kind!");
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    bool DelayCheckingPattern =
      TC.TU.Kind != TranslationUnit::Library &&
      TC.TU.Kind != TranslationUnit::SIL &&
      PBD->getDeclContext()->isModuleContext();
    if (IsSecondPass && !DelayCheckingPattern) {
      if (PBD->getInit() && PBD->getPattern()->hasType()) {
        Expr *Init = PBD->getInit();
        Type DestTy = PBD->getPattern()->getType();
        if (TC.typeCheckExpression(Init, PBD->getDeclContext(), DestTy,
                                   /*discardedExpr=*/false)) {
          if (DestTy)
            TC.diagnose(PBD, diag::while_converting_var_init,
                        DestTy);
        } else {
          PBD->setInit(Init);
        }
      }
      return;
    }

    // If there is no initializer and we are not in a type context,
    // create a default initializer.
    if (!PBD->getInit() && !IsFirstPass &&
        isa<TypedPattern>(PBD->getPattern()) &&
        !PBD->getDeclContext()->isTypeContext()) {
      // Type-check the pattern.
      if (TC.typeCheckPattern(PBD->getPattern(),
                              PBD->getDeclContext(),
                              /*allowUnknownTypes*/false)) {
        setBoundVarsTypeError(PBD->getPattern());
        return;
      }

      Type ty = PBD->getPattern()->getType();
      Expr *initializer = nullptr;
      if (isPatternProperty(PBD->getPattern())) {
        // Properties don't have initializers.
      } else if (!isa<TopLevelCodeDecl>(PBD->getDeclContext()) ||
                 TLDefiniteInit) {
        // If we are using the new definite initialization rules, we don't
        // default initialize local variables, only globals.
      } else {
        if (!TC.isDefaultInitializable(ty, &initializer)) {
          // FIXME: Better diagnostics here.
          TC.diagnose(PBD, diag::decl_no_default_init, ty);
        } else {
          if (TC.typeCheckExpression(initializer, PBD->getDeclContext(), ty,
                                     /*discardedExpr=*/false)) {
            TC.diagnose(PBD, diag::while_converting_var_init, ty);
            return;
          }
          
          PBD->setInit(initializer);
        }
      }
    } else if (PBD->getInit() && !IsFirstPass) {
      Type DestTy;
      if (isa<TypedPattern>(PBD->getPattern())) {
        if (TC.typeCheckPattern(PBD->getPattern(),
                                PBD->getDeclContext(),
                                /*allowUnknownTypes*/false)) {
          setBoundVarsTypeError(PBD->getPattern());
          return;
        }
        DestTy = PBD->getPattern()->getType();
      }
      Expr *Init = PBD->getInit();
      if (TC.typeCheckExpression(Init, PBD->getDeclContext(), DestTy,
                                 /*discardedExpr=*/false)) {
        if (DestTy)
          TC.diagnose(PBD, diag::while_converting_var_init,
                      DestTy);
        else
          setBoundVarsTypeError(PBD->getPattern());
        return;
      }
      if (!DestTy) {
        Expr *newInit = TC.coerceToMaterializable(Init);
        if (newInit) Init = newInit;
      }
      PBD->setInit(Init);
      if (!DestTy) {
        if (TC.coerceToType(PBD->getPattern(),
                            PBD->getDeclContext(),
                            Init->getType())) {
          setBoundVarsTypeError(PBD->getPattern());
          return;
        }
      }
    } else if (!IsFirstPass || !DelayCheckingPattern) {
      if (TC.typeCheckPattern(PBD->getPattern(),
                              PBD->getDeclContext(),
                              /*allowUnknownTypes*/false)) {
        setBoundVarsTypeError(PBD->getPattern());
        return;
      }
    }

    visitBoundVars(PBD->getPattern());

  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    if (IsSecondPass)
      return;

    assert(SD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent subscripts outside of types!");

    bool isInvalid = TC.validateType(SD->getElementTypeLoc());
    isInvalid |= TC.typeCheckPattern(SD->getIndices(),
                                     SD->getDeclContext(),
                                     /*allowUnknownTypes*/false);

    if (isInvalid) {
      SD->overwriteType(ErrorType::get(TC.Context));
      SD->setInvalid();
    } else {
      SD->setType(FunctionType::get(SD->getIndices()->getType(),
                                    SD->getElementType(), TC.Context));
    }
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    if (!IsSecondPass) {
      if (TC.validateType(TAD->getUnderlyingTypeLoc())) {
        TAD->setInvalid();
        TAD->overwriteType(ErrorType::get(TC.Context));
        TAD->getUnderlyingTypeLoc().setType(ErrorType::get(TC.Context));
      }

      if (!isa<ProtocolDecl>(TAD->getDeclContext()))
        checkInheritanceClause(TC, TAD);
    }

    if (!IsFirstPass)
      checkExplicitConformance(TAD, TAD->getDeclaredType());
  }

  void visitUnionDecl(UnionDecl *UD) {
    if (!IsSecondPass) {
      TC.validateTypeDecl(UD);
      validateAttributes(UD);

      checkInheritanceClause(TC, UD);
    }

    for (Decl *member : UD->getMembers())
      visit(member);

    if (!IsFirstPass)
      checkExplicitConformance(UD, UD->getDeclaredTypeInContext());
  }

  void visitStructDecl(StructDecl *SD) {

    if (!IsSecondPass) {
      TC.validateTypeDecl(SD);
      validateAttributes(SD);
      checkInheritanceClause(TC, SD);
    }

    // Visit each of the members.
    for (Decl *Member : SD->getMembers()) {
      visit(Member);
    }

    if (!IsSecondPass) {
      TC.addImplicitConstructors(SD);
    }

    if (!IsFirstPass) {
      checkExplicitConformance(SD, SD->getDeclaredTypeInContext());
    }
  }

  void checkObjCConformance(ProtocolDecl *protocol,
                            ProtocolConformance *conformance) {
    if (!conformance)
      return;
    if (protocol->isObjC())
      for (auto &mapping : conformance->getWitnesses())
        mapping.second.getDecl()->setIsObjC(true);
    for (auto &inherited : conformance->getInheritedConformances())
      checkObjCConformance(inherited.first, inherited.second);
  }

  /// Mark class members needed to conform to ObjC protocols as requiring ObjC
  /// interop.
  void checkObjCConformances(ArrayRef<ProtocolDecl*> protocols,
                             ArrayRef<ProtocolConformance*> conformances) {
    assert(protocols.size() == conformances.size() &&
           "protocol conformance mismatch");

    for (unsigned i = 0, size = protocols.size(); i < size; ++i)
      checkObjCConformance(protocols[i], conformances[i]);
  }

  void visitClassDecl(ClassDecl *CD) {
    if (!IsSecondPass) {
      TC.validateTypeDecl(CD);
      validateAttributes(CD);
      checkInheritanceClause(TC, CD);

      {
        // Check for circular inheritance.
        SmallVector<ClassDecl *, 8> path;
        checkCircularity(TC, CD, diag::circular_class_inheritance,
                         diag::class_here, path);
      }

      ClassDecl *superclassDecl = CD->hasSuperclass()
        ? CD->getSuperclass()->getClassOrBoundGenericClass()
        : nullptr;

      CD->setIsObjC(CD->getAttrs().isObjC()
                    || (superclassDecl && superclassDecl->isObjC()));
    }

    for (Decl *Member : CD->getMembers())
      visit(Member);

    if (!IsFirstPass) {
      checkExplicitConformance(CD, CD->getDeclaredTypeInContext());
      checkObjCConformances(CD->getProtocols(), CD->getConformances());
    }
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    if (IsSecondPass) {
      return;
    }

    TC.validateTypeDecl(PD);
    checkInheritanceClause(TC, PD);

    {
      // Check for circular inheritance within the protocol.
      SmallVector<ProtocolDecl *, 8> path;
      checkCircularity(TC, PD, diag::circular_protocol_def,
                       diag::protocol_here, path);
    }

    // If the protocol is [objc], it may only refine other [objc] protocols.
    // FIXME: Revisit this restriction.
    if (PD->getAttrs().isObjC()) {
      bool isObjC = true;

      SmallVector<ProtocolDecl*, 2> inheritedProtocols;
      for (auto inherited : PD->getInherited()) {
        if (!inherited.getType()->isExistentialType(inheritedProtocols))
          continue;

        for (auto proto : inheritedProtocols) {
          if (!proto->getAttrs().isObjC()) {
            TC.diagnose(PD->getLoc(),
                        diag::objc_protocol_inherits_non_objc_protocol,
                        PD->getDeclaredType(), proto->getDeclaredType());
            TC.diagnose(proto->getLoc(),
                        diag::protocol_here,
                        proto->getName());
            isObjC = false;
          }
        }

        inheritedProtocols.clear();
      }

      PD->setIsObjC(isObjC);
    }

    // Fix the 'Self' associated type.
    AssociatedTypeDecl *selfDecl = nullptr;
    for (auto Member : PD->getMembers()) {
      if (auto AssocType = dyn_cast<AssociatedTypeDecl>(Member)) {
        checkInheritanceClause(TC, AssocType);

        if (AssocType->isSelf()) {
          selfDecl = AssocType;
          break;
        }
      }
    }
    assert(selfDecl && "no This decl?");

    // Build archetypes for this protocol.
    ArchetypeBuilder builder = createArchetypeBuilder();
    builder.addGenericParameter(selfDecl, 0);
    builder.addImplicitConformance(selfDecl, PD);
    builder.assignArchetypes();

    // Set the underlying type of each of the associated types to the
    // appropriate archetype.
    ArchetypeType *selfArchetype = builder.getArchetype(selfDecl);
    for (auto member : PD->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        TypeLoc underlyingTy;
        if (assocType == selfDecl)
          assocType->setArchetype(selfArchetype);
        else
          assocType->setArchetype(
            selfArchetype->getNestedType(assocType->getName()));
      }
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);

    validateAttributes(PD);
  }

  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  bool semaFuncParamPatterns(DeclContext *dc,
                             ArrayRef<Pattern*> paramPatterns) {
    bool badType = false;
    for (Pattern *P : paramPatterns) {
      if (P->hasType())
        continue;
      if (TC.typeCheckPattern(P, dc, false)) {
        badType = true;
        continue;
      }
    }
    return badType;
  }

  /// \brief Validate and consume the attributes that are applicable to the
  /// AnyFunctionType.
  ///
  /// Currently, we only allow 'noreturn' to be applied on a FuncDecl.
  AnyFunctionType::ExtInfo
  validateAndConsumeFunctionTypeAttributes(FuncDecl *FD,
                                           bool consumeAttributes) {
    DeclAttributes &Attrs = FD->getMutableAttrs();
    auto Info = AnyFunctionType::ExtInfo();

    if (Attrs.hasCC()) {
      TC.diagnose(FD->getStartLoc(), diag::invalid_decl_attribute, "cc");
      Attrs.cc = {};
    }

    if (Attrs.isThin()) {
      TC.diagnose(FD->getStartLoc(), diag::invalid_decl_attribute,
                  "thin");
      Attrs.Thin = false;
    }

    // 'noreturn' is allowed on a function declaration.
    Info = Info.withIsNoReturn(Attrs.isNoReturn());
    if (consumeAttributes) {
      Attrs.NoReturn = false;
    }

    if (Attrs.isAutoClosure()) {
      TC.diagnose(FD->getStartLoc(), diag::invalid_decl_attribute,
                  "auto_closure");
      Attrs.AutoClosure = false;
    }

    if (Attrs.isObjCBlock()) {
      TC.diagnose(FD->getStartLoc(), diag::invalid_decl_attribute,
                  "objc_block");
      Attrs.ObjCBlock = false;
    }

    return Info;
  }

  void semaFuncDecl(FuncDecl *FD, bool consumeAttributes) {
    FuncExpr *FE = FD->getFuncExpr();

    if (FE->getType())
      return;

    bool badType = false;
    if (!FE->getBodyResultTypeLoc().isNull()) {
      if (TC.validateType(FE->getBodyResultTypeLoc())) {
        badType = true;
      }
    }

    badType =
        badType || semaFuncParamPatterns(FE, FD->getArgParamPatterns());
    badType =
        badType || semaFuncParamPatterns(FE, FD->getBodyParamPatterns());

    if (badType) {
      FE->setType(ErrorType::get(TC.Context));
      FD->setInvalid();
      return;
    }

    Type funcTy = FE->getBodyResultTypeLoc().getType();
    if (!funcTy) {
      funcTy = TupleType::getEmpty(TC.Context);
    }
    auto bodyResultType = funcTy;

    // FIXME: it would be nice to have comments explaining what this is all about.
    auto patterns = FE->getDecl()->getArgParamPatterns();
    bool isInstanceFunc = false;
    GenericParamList *genericParams = nullptr;
    GenericParamList *outerGenericParams = nullptr;
    if (FuncDecl *FD = FE->getDecl()) {
      isInstanceFunc = (bool)FD->computeSelfType(&outerGenericParams);
      genericParams = FD->getGenericParams();
    }

    for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
      Type argTy = patterns[e - i - 1]->getType();

      GenericParamList *params = nullptr;
      if (e - i - 1 == isInstanceFunc && genericParams) {
        params = genericParams;
      } else if (e - i - 1 == 0 && outerGenericParams) {
        params = outerGenericParams;
      }

      // Validate and consume the function type attributes.
      auto Info = validateAndConsumeFunctionTypeAttributes(FD,
                                                           consumeAttributes);
      if (params) {
        funcTy = PolymorphicFunctionType::get(argTy, funcTy,
                                              params,
                                              Info,
                                              TC.Context);
      } else {
        funcTy = FunctionType::get(argTy, funcTy, Info, TC.Context);
      }

    }
    FE->setType(funcTy);
    FE->setBodyResultType(bodyResultType);
  }

  /// Bind the given function declaration, which declares an operator, to
  /// the corresponding operator declaration.
  void bindFuncDeclToOperator(FuncDecl *FD) {
    OperatorDecl *op = nullptr;
    auto &TU = TC.TU;
    if (FD->isUnaryOperator()) {
      if (FD->getAttrs().isPrefix()) {
        if (auto maybeOp = TU.lookupPrefixOperator(FD->getName(), FD->getLoc()))
          op = *maybeOp;
        else
          return;
      } else if (FD->getAttrs().isPostfix()) {
        if (auto maybeOp = TU.lookupPostfixOperator(FD->getName(),FD->getLoc()))
          op = *maybeOp;
        else
          return;
      } else {
        auto prefixOp = TU.lookupPrefixOperator(FD->getName(), FD->getLoc());
        auto postfixOp = TU.lookupPostfixOperator(FD->getName(), FD->getLoc());

        // If we found both prefix and postfix, or neither prefix nor postfix,
        // complain. We can't fix this situation.
        if (static_cast<bool>(prefixOp) == static_cast<bool>(postfixOp)) {
          TC.diagnose(FD, diag::declared_unary_op_without_attribute);

          // If we found both, point at them.
          if (prefixOp) {
            SourceLoc insertionLoc = FD->getLoc();

            TC.diagnose(*prefixOp, diag::unary_operator_declaration_here,false)
              .fixItInsert(insertionLoc, "[prefix] ");
            TC.diagnose(*postfixOp, diag::unary_operator_declaration_here, true)
              .fixItInsert(insertionLoc, "[postfix] ");
          } else {
            // FIXME: Introduce a Fix-It that adds the operator declaration?
          }

          // FIXME: Errors could cascade here, because name lookup for this
          // operator won't find this declaration.
          return;
        }

        // We found only one operator declaration, so we know whether this
        // should be a prefix or a postfix operator.

        // Fix the AST and determine the insertion text.
        SourceLoc insertionLoc = FD->getLoc();
        const char *insertionText;
        if (postfixOp) {
          insertionText = "[postfix] ";
          op = *postfixOp;
          FD->getMutableAttrs().ExplicitPostfix = true;
        } else {
          insertionText = "[prefix] ";
          op = *prefixOp;
          FD->getMutableAttrs().ExplicitPrefix = true;
        }

        // Emit diagnostic with the Fix-It.
        TC.diagnose(FD, diag::unary_op_missing_prepos_attribute,
                    static_cast<bool>(postfixOp))
          .fixItInsert(insertionLoc, insertionText);
        TC.diagnose(op, diag::unary_operator_declaration_here,
                    static_cast<bool>(postfixOp));
      }
    } else if (FD->isBinaryOperator()) {
      if (auto maybeOp = TU.lookupInfixOperator(FD->getName(), FD->getLoc()))
        op = *maybeOp;
      else {
        // FIXME: Add Fix-It introducing an operator declaration?
        TC.diagnose(FD, diag::declared_operator_without_operator_decl);
        return;
      }
    } else {
      TC.diagnose(FD, diag::invalid_arg_count_for_operator);
      return;
    }

    assert(op && "Should have computed operator above");
    FD->setOperatorDecl(op);
  }

  void visitFuncDecl(FuncDecl *FD) {
    if (!IsFirstPass && FD->getBody())
      TC.definedFunctions.push_back(FD);

    if (IsSecondPass)
      return;

    // Bind operator functions to the corresponding operator declaration.
    if (FD->isOperator())
      bindFuncDeclToOperator(FD);

    FuncExpr *body = FD->getFuncExpr();

    // Before anything else, set up the 'self' argument correctly.
    GenericParamList *outerGenericParams = nullptr;
    if (Type selfType = FD->computeSelfType(&outerGenericParams)) {
      FD->getImplicitSelfDecl()->setType(selfType);

      TypedPattern *selfPattern =
          cast<TypedPattern>(FD->getArgParamPatterns()[0]);
      if (selfPattern->hasType()) {
        assert(selfPattern->getType().getPointer() == selfType.getPointer());
      } else {
        selfPattern->setType(selfType);
        cast<NamedPattern>(selfPattern->getSubPattern())->setType(selfType);
      }
    }

    Optional<ArchetypeBuilder> builder;
    if (auto gp = FD->getGenericParams()) {
      gp->setOuterParameters(outerGenericParams);
      builder.emplace(createArchetypeBuilder());
      checkGenericParamList(*builder, gp);
    }

    semaFuncDecl(FD, /*consumeAttributes=*/!builder);

    // If we have generic parameters, create archetypes now.
    if (builder) {
      // Infer requirements from the parameters of the function.
      for (auto pattern : FD->getArgParamPatterns()) {
        builder->inferRequirements(pattern);
      }

      // Infer requirements from the result type.
      if (auto resultType = body->getBodyResultTypeLoc().getTypeRepr())
        builder->inferRequirements(resultType);

      // Assign archetypes.
      finalizeGenericParamList(*builder, FD->getGenericParams(), body);

      // Go through and revert all of the dependent types we computed.

      // Revert the result type.
      if (!body->getBodyResultTypeLoc().isNull()) {
        revertDependentTypeLoc(body->getBodyResultTypeLoc(), body);
      }

      // Revert the argument patterns.
      ArrayRef<Pattern *> argPatterns = FD->getArgParamPatterns();
      if (FD->getDeclContext()->isTypeContext())
        argPatterns = argPatterns.slice(1);
      for (auto argPattern : argPatterns) {
        revertDependentPattern(argPattern, body);
      }

      // Revert the body patterns.
      ArrayRef<Pattern *> bodyPatterns = FD->getBodyParamPatterns();
      if (FD->getDeclContext()->isTypeContext())
        bodyPatterns = bodyPatterns.slice(1);
      for (auto bodyPattern : bodyPatterns) {
        revertDependentPattern(bodyPattern, body);
      }

      // Clear out the types.
      body->revertType();

      // Type check the parameters and return type again, now with archetypes.
      semaFuncDecl(FD, /*consumeAttributes=*/true);

      // The second type check should have created a non-dependent type.
      assert(!body->getType()->isDependentType());
    }

    FD->setType(body->getType());

    validateAttributes(FD);

    // A method is ObjC-compatible if it's explicitly [objc], a member of an
    // ObjC-compatible class, or an accessor for an ObjC property.
    DeclContext *dc = FD->getDeclContext();
    if (dc && dc->getDeclaredTypeInContext()) {
      ClassDecl *classContext = dc->getDeclaredTypeInContext()
        ->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext = dyn_cast<ProtocolDecl>(dc);
      bool isObjC = FD->getAttrs().isObjC()
        || (classContext && classContext->isObjC())
        || (protocolContext && protocolContext->isObjC());
      if (!isObjC && FD->isGetterOrSetter()) {
        // If the property decl is an instance property, its accessors will
        // be instance methods and the above condition will mark them ObjC.
        // The only additional condition we need to check is if the var decl
        // had an [objc] or [iboutlet] property. We don't use prop->isObjC()
        // because the property accessors may be visited before the VarDecl and
        // prop->isObjC() may not yet be set by typechecking.
        ValueDecl *prop = cast<ValueDecl>(FD->getGetterOrSetterDecl());
        isObjC = prop->getAttrs().isObjC() || prop->getAttrs().isIBOutlet();
      }

      FD->setIsObjC(isObjC);
    }
  }

  void visitUnionElementDecl(UnionElementDecl *ED) {
    if (IsSecondPass)
      return;

    UnionDecl *UD = ED->getParentUnion();
    Type ElemTy = UD->getDeclaredTypeInContext();

    if (!ED->getArgumentTypeLoc().isNull())
      if (TC.validateType(ED->getArgumentTypeLoc())) {
        ED->overwriteType(ErrorType::get(TC.Context));
        ED->setInvalid();
        return;
      }
    if (!ED->getResultTypeLoc().isNull())
      if (TC.validateType(ED->getResultTypeLoc())) {
        ED->overwriteType(ErrorType::get(TC.Context));
        ED->setInvalid();
        return;
      }

    // If we have a simple element, just set the type.
    if (ED->getArgumentType().isNull()) {
      Type argTy = MetaTypeType::get(ElemTy, TC.Context);
      Type fnTy;
      if (auto gp = UD->getGenericParamsOfContext())
        fnTy = PolymorphicFunctionType::get(argTy, ElemTy, gp, TC.Context);
      else
        fnTy = FunctionType::get(argTy, ElemTy, TC.Context);
      ED->setType(fnTy);
      return;
    }

    Type fnTy = FunctionType::get(ED->getArgumentType(), ElemTy, TC.Context);
    if (auto gp = UD->getGenericParamsOfContext())
      fnTy = PolymorphicFunctionType::get(MetaTypeType::get(ElemTy, TC.Context),
                                          fnTy, gp, TC.Context);
    else
      fnTy = FunctionType::get(MetaTypeType::get(ElemTy, TC.Context), fnTy,
                               TC.Context);
    ED->setType(fnTy);

    // Require the carried type to be materializable.
    if (!ED->getArgumentType()->isMaterializable()) {
      TC.diagnose(ED->getLoc(), diag::union_element_not_materializable);
      ED->overwriteType(ErrorType::get(TC.Context));
      ED->setInvalid();
    }
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    if (ED->isInvalid()) {
      // Mark children as invalid.
      // FIXME: This is awful.
      for (auto member : ED->getMembers()) {
        member->setInvalid();
        if (ValueDecl *VD = dyn_cast<ValueDecl>(member))
          VD->overwriteType(ErrorType::get(TC.Context));
      }
      return;
    }

    if (!IsSecondPass) {
      CanType ExtendedTy = ED->getExtendedType()->getCanonicalType();

      // FIXME: we should require generic parameter clauses here
      if (auto unbound = dyn_cast<UnboundGenericType>(ExtendedTy)) {
        auto boundType = unbound->getDecl()->getDeclaredTypeInContext();
        ED->getExtendedTypeLoc() = TypeLoc::withoutLoc(boundType);
        ExtendedTy = boundType->getCanonicalType();
      }

      if (!isa<UnionType>(ExtendedTy) &&
          !isa<StructType>(ExtendedTy) &&
          !isa<ClassType>(ExtendedTy) &&
          !isa<BoundGenericUnionType>(ExtendedTy) &&
          !isa<BoundGenericStructType>(ExtendedTy) &&
          !isa<BoundGenericClassType>(ExtendedTy) &&
          !isa<ErrorType>(ExtendedTy)) {
        TC.diagnose(ED->getStartLoc(), diag::non_nominal_extension,
                    isa<ProtocolType>(ExtendedTy), ExtendedTy);
        // FIXME: It would be nice to point out where we found the named type
        // declaration, if any.
        ED->setInvalid();
      }

      // Add this extension to the list of extensions for the extended type.
      if (auto nominal = ExtendedTy->getAnyNominal()) {
        nominal->addExtension(ED);
      }

      checkInheritanceClause(TC, ED);
    }

    for (Decl *Member : ED->getMembers())
      visit(Member);

    if (!IsFirstPass) {
      checkExplicitConformance(ED, ED->getExtendedType());
      checkObjCConformances(ED->getProtocols(), ED->getConformances());
    }
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    if (CD->isInvalid()) {
      CD->overwriteType(ErrorType::get(TC.Context));
      return;
    }

    if (!IsFirstPass) {
      if (CD->getBody())
        TC.definedFunctions.push_back(CD);
    }

    if (IsSecondPass) {
      return;
    }

    assert(CD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent constructors outside of types!");

    GenericParamList *outerGenericParams = nullptr;
    Type SelfTy = CD->computeSelfType(&outerGenericParams);
    TC.validateTypeSimple(SelfTy);
    CD->getImplicitSelfDecl()->setType(SelfTy);

    Optional<ArchetypeBuilder> builder;
    if (auto gp = CD->getGenericParams()) {
      // Write up generic parameters and check the generic parameter list.
      gp->setOuterParameters(outerGenericParams);
      ArchetypeBuilder builder = createArchetypeBuilder();
      checkGenericParamList(builder, gp);

      // Type check the constructor parameters.
      if (TC.typeCheckPattern(CD->getArguments(),
                              CD,
                              /*allowUnknownTypes*/false)) {
        CD->overwriteType(ErrorType::get(TC.Context));
        CD->setInvalid();
      }

      // Infer requirements from the parameters of the constructor.
      builder.inferRequirements(CD->getArguments());

      // Assign archetypes.
      finalizeGenericParamList(builder, gp, CD);

      // Reverse the constructor parameter pattern; it will be checked
      // again, with archetypes, below.
      revertDependentPattern(CD->getArguments(), CD);
    }

    // Type check the constructor parameters.
    if (TC.typeCheckPattern(CD->getArguments(),
                            CD,
                            /*allowUnknownTypes*/false)) {
      CD->overwriteType(ErrorType::get(TC.Context));
      CD->setInvalid();
    } else {
      Type FnTy;
      Type AllocFnTy;
      Type InitFnTy;
      if (GenericParamList *innerGenericParams = CD->getGenericParams()) {
        innerGenericParams->setOuterParameters(outerGenericParams);
        FnTy = PolymorphicFunctionType::get(CD->getArguments()->getType(),
                                            SelfTy, innerGenericParams,
                                            TC.Context);
      } else
        FnTy = FunctionType::get(CD->getArguments()->getType(),
                                 SelfTy, TC.Context);
      Type SelfMetaTy = MetaTypeType::get(SelfTy, TC.Context);
      if (outerGenericParams) {
        AllocFnTy = PolymorphicFunctionType::get(SelfMetaTy, FnTy,
                                                outerGenericParams, TC.Context);
        InitFnTy = PolymorphicFunctionType::get(SelfTy, FnTy,
                                                outerGenericParams, TC.Context);
      } else {
        AllocFnTy = FunctionType::get(SelfMetaTy, FnTy, TC.Context);
        InitFnTy = FunctionType::get(SelfTy, FnTy, TC.Context);
      }
      CD->setType(AllocFnTy);
      CD->setInitializerType(InitFnTy);
    }

    validateAttributes(CD);
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    if (!IsFirstPass) {
      if (DD->getBody())
        TC.definedFunctions.push_back(DD);
    }

    if (IsSecondPass) {
      return;
    }

    assert(DD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent destructors outside of types!");

    GenericParamList *outerGenericParams = nullptr;
    Type SelfTy = DD->computeSelfType(&outerGenericParams);
    TC.validateTypeSimple(SelfTy);
    Type FnTy;
    if (outerGenericParams)
      FnTy = PolymorphicFunctionType::get(SelfTy,
                                          TupleType::getEmpty(TC.Context),
                                          outerGenericParams, TC.Context);
    else
      FnTy = FunctionType::get(SelfTy, TupleType::getEmpty(TC.Context),
                               TC.Context);

    DD->setType(FnTy);
    DD->getImplicitSelfDecl()->setType(SelfTy);

    validateAttributes(DD);
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D, bool isFirstPass) {
  bool isSecondPass = !isFirstPass && D->getDeclContext()->isModuleContext();
  DeclChecker(*this, isFirstPass, isSecondPass).visit(D);
}

void TypeChecker::validateTypeDecl(TypeDecl *D) {
  // Type aliases may not have an underlying type yet.
  if (auto typeAlias = dyn_cast<TypeAliasDecl>(D)) {
    if (typeAlias->getUnderlyingTypeLoc().getTypeRepr())
      validateType(typeAlias->getUnderlyingTypeLoc());
    return;
  }

  // Nominal declarations may not have a type yet.
  if (auto nominal = dyn_cast<NominalTypeDecl>(D)) {
    if (nominal->hasType())
      return;

    // Check generic parameters, if needed.
    if (auto gp = nominal->getGenericParams()) {
      gp->setOuterParameters(
        nominal->getDeclContext()->getGenericParamsOfContext());
      DeclChecker(*this, false, false).checkGenericParams(gp, nominal);
    }

    // Compute the declared type.
    nominal->computeType();

    // Now that we have archetypes for our generic parameters (including
    // generic parameters from outer scopes), we can canonicalize our type.
    validateTypeSimple(nominal->getDeclaredTypeInContext());

    return;
  }
}

ArrayRef<ProtocolDecl *>
TypeChecker::getDirectConformsTo(NominalTypeDecl *nominal) {
  checkInheritanceClause(*this, nominal);
  return nominal->getProtocols();
}

ArrayRef<ProtocolDecl *>
TypeChecker::getDirectConformsTo(ExtensionDecl *ext) {
  checkInheritanceClause(*this, ext);
  return ext->getProtocols();
}

/// \brief Create an implicit struct constructor.
///
/// \param structDecl The struct for which a constructor will be created.
/// \param ICK The kind of implicit constructor to create.
///
/// \returns The newly-created constructor, which has already been type-checked
/// (but has not been added to the containing struct).
static ConstructorDecl *createImplicitConstructor(TypeChecker &tc,
                                                  StructDecl *structDecl,
                                                  ImplicitConstructorKind ICK) {
  ASTContext &context = tc.Context;
  // Determine the parameter type of the implicit constructor.
  SmallVector<TuplePatternElt, 8> patternElts;
  SmallVector<VarDecl *, 8> allArgs;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    for (auto member : structDecl->getMembers()) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var)
        continue;

      // Properties are computed, not initialized.
      if (var->isProperty())
        continue;

      auto varType = tc.getTypeOfRValue(var);

      // Create the parameter.
      auto *arg = new (context) VarDecl(SourceLoc(),
                                        var->getName(),
                                        varType, structDecl);
      allArgs.push_back(arg);
      Pattern *pattern = new (context) NamedPattern(arg);
      TypeLoc tyLoc = TypeLoc::withoutLoc(varType);
      pattern = new (context) TypedPattern(pattern, tyLoc);
      patternElts.push_back(TuplePatternElt(pattern));
    }
  }

  // Crate the onstructor.
  auto constructorID = context.getIdentifier("constructor");
  VarDecl *selfDecl
    = new (context) VarDecl(SourceLoc(),
                            context.getIdentifier("self"),
                            Type(), structDecl);
  ConstructorDecl *ctor
    = new (context) ConstructorDecl(constructorID, structDecl->getLoc(),
                                    nullptr, selfDecl, nullptr, structDecl);
  selfDecl->setDeclContext(ctor);
  for (auto var : allArgs) {
    var->setDeclContext(ctor);
  }

  // Set its arguments.
  auto pattern = TuplePattern::create(context, structDecl->getLoc(),
                                      patternElts, structDecl->getLoc());
  ctor->setArguments(pattern);

  // Mark implicit.
  ctor->setImplicit();

  // Type-check the constructor declaration.
  tc.typeCheckDecl(ctor, /*isFirstPass=*/true);

  // If the struct in which this constructor is being added was imported,
  // add it as an external definition.
  if (structDecl->hasClangNode()) {
    tc.Context.ExternalDefinitions.insert(ctor);
  }

  return ctor;
}

void TypeChecker::addImplicitConstructors(StructDecl *structDecl) {
  // Check whether there is a user-declared constructor or,
  // failing that, an instance variable.
  bool FoundConstructor = false;
  bool FoundInstanceVar = false;
  for (auto member : structDecl->getMembers()) {
    if (isa<ConstructorDecl>(member)) {
      FoundConstructor = true;
      break;
    }

    if (auto var = dyn_cast<VarDecl>(member)) {
      if (!var->isProperty())
        FoundInstanceVar = true;
    }
  }

  // If we didn't find such a constructor, add the implicit one(s).
  if (!FoundConstructor) {
    // Copy the list of members, so we can add to it.
    // FIXME: Painfully inefficient to do the copy here.
    SmallVector<Decl *, 4> members(structDecl->getMembers().begin(),
                                   structDecl->getMembers().end());

    // Create the implicit memberwise constructor.
    auto ctor = createImplicitConstructor(*this, structDecl,
                                          ImplicitConstructorKind::Memberwise);
    members.push_back(ctor);

    // Set the members of the struct.
    structDecl->setMembers(Context.AllocateCopy(members),
                           structDecl->getBraces());

    // If we found any instance variables, the default constructor will be
    // different than the memberwise constructor. Whether this
    // constructor will actually be defined depends on whether all of
    // the instance variables can be default-initialized, which we
    // don't know yet. This will be determined lazily.
    if (FoundInstanceVar) {
      assert(!structsNeedingImplicitDefaultConstructor.count(structDecl));
      structsNeedingImplicitDefaultConstructor.insert(structDecl);
      structsWithImplicitDefaultConstructor.push_back(structDecl);
    }
  }
}

bool TypeChecker::isDefaultInitializable(Type ty, Expr **initializer) {
  CanType canTy = ty->getCanonicalType();
  switch (canTy->getKind()) {
  case TypeKind::Archetype:
  case TypeKind::BoundGenericStruct:
  case TypeKind::BoundGenericUnion:
  case TypeKind::Union:
  case TypeKind::Struct:
    // Break to look for constructors.
    break;

  case TypeKind::Array:
    // Arrays are default-initializable if their element types are.
    // FIXME: We don't implement this rule yet, so just fail.
    return false;

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage:
    llvm_unreachable("should not be directly asking whether a [weak] or "
                     "[unowned] type is default-initializable");

  case TypeKind::BoundGenericClass:
  case TypeKind::Class:
    // Classes are default-initializable (with 0).
    // FIXME: This may not be what we want in the long term.
    if (initializer) {
      *initializer = new (Context) ZeroValueExpr(ty);
    }
    return true;

  case TypeKind::Protocol:
  case TypeKind::ProtocolComposition:
    // Existentials are not default-initializable.
    return false;

  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinVector:
    // Built-in types are default-initializable.
    if (initializer) {
      *initializer = new (Context) ZeroValueExpr(ty);
    }
    return true;

  case TypeKind::Tuple: {
    // Check whether all fields either have an initializer or have
    // default-initializable types.
    SmallVector<Expr *, 4> eltInits;
    SmallVector<Identifier, 4> eltNames;
    for (auto &elt : ty->castTo<TupleType>()->getFields()) {
      assert(!elt.hasInit() && "Initializers can't appear here");

      // Check whether the element is default-initializable.
      Expr *eltInit = nullptr;
      if (!isDefaultInitializable(elt.getType(),
                                  initializer? &eltInit : nullptr))
        return false;

      // If we need to produce an initializer, add this element.
      if (initializer) {
        assert(eltInit && "Missing initializer?");
        eltInits.push_back(eltInit);
        eltNames.push_back(elt.getName());
      }
    }

    // If we need to build an initializer, build a TupleExpr or use the
    // sole initializer (if all others are unnamed).
    if (initializer) {
      if (eltInits.size() == 1 && eltNames[0].empty())
        *initializer = eltInits[0];
      else
        *initializer
          = new (Context) TupleExpr(SourceLoc(),
                                    Context.AllocateCopy(eltInits),
                                    Context.AllocateCopy(eltNames).data(),
                                    SourceLoc(),
                                    /*hasTrailingClosure=*/false);
    }
    return true;
  }

  case TypeKind::GenericTypeParam:
  case TypeKind::DependentMember:
    llvm_unreachable("Should never ask about dependent types");

  case TypeKind::Function:
  case TypeKind::LValue:
  case TypeKind::PolymorphicFunction:
  case TypeKind::MetaType:
  case TypeKind::Module:
      return false;

  // Sugar types.
#define TYPE(Id, Parent)
#define SUGARED_TYPE(Id, Parent) case TypeKind::Id:
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("Not using the canonical type?");

#define TYPE(Id, Parent)
#define UNCHECKED_TYPE(Id, Parent) case TypeKind::Id:
#include "swift/AST/TypeNodes.def"
    // Error cases.
    return false;
  }

  // We need to look for a default constructor.
  auto ctors = lookupConstructors(ty);
  if (!ctors)
    return false;

  // Check whether we have a constructor that can be called with an empty
  // tuple.
  bool foundDefaultConstructor = false;
  for (auto member : ctors) {
    // Dig out the parameter tuple for this constructor.
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (!ctor || ctor->isInvalid())
      continue;

    auto paramTuple = ctor->getArgumentType()->getAs<TupleType>();
    if (!paramTuple)
      continue;

    // Check whether any of the tuple elements are missing an initializer.
    bool missingInit = false;
    for (auto &elt : paramTuple->getFields()) {
      if (elt.hasInit())
        continue;

      missingInit = true;
      break;
    }
    if (missingInit)
      continue;

    // We found a constructor that can be invoked with an empty tuple.
    if (foundDefaultConstructor) {
      // We found two constructors that can be invoked with an empty tuple.
      return false;
    }

    foundDefaultConstructor = true;
  }

  if (!foundDefaultConstructor || !initializer)
    return foundDefaultConstructor;

  // We found a default constructor. Construct the initializer expression.
  // FIXME: As an optimization, we could build a fully type-checked AST here.
  Expr *arg = new (Context) TupleExpr(SourceLoc(), { }, nullptr, SourceLoc(),
                                      /*hasTrailingClosure=*/false);
  Expr *metatype = new (Context) MetatypeExpr(nullptr, SourceLoc(),
                                              MetaTypeType::get(ty, Context));
  *initializer = new (Context) CallExpr(metatype, arg);

  return true;
}

void TypeChecker::defineDefaultConstructor(StructDecl *structDecl) {
  PrettyStackTraceDecl stackTrace("defining default constructor for",
                                  structDecl);

  // Erase this from the set of structs that need an implicit default
  // constructor.
  assert(structsNeedingImplicitDefaultConstructor.count(structDecl));
  structsNeedingImplicitDefaultConstructor.erase(structDecl);

  // Verify that all of the instance variables of this struct have default
  // constructors.
  for (auto member : structDecl->getMembers()) {
    // We only care about pattern bindings.
    auto patternBind = dyn_cast<PatternBindingDecl>(member);
    if (!patternBind)
      continue;

    // If the pattern has an initializer, we don't need any default
    // initialization for its variables.
    if (patternBind->getInit())
      continue;

    // Find the variables in the pattern. They'll each need to be
    // default-initialized.
    SmallVector<VarDecl *, 4> variables;
    patternBind->getPattern()->collectVariables(variables);

    for (auto var : variables) {
      if (var->isProperty() || var->isInvalid())
        continue;

      // If this variable is not default-initializable, we're done: we can't
      // add the default constructor because it will be ill-formed.
      if (!isDefaultInitializable(getTypeOfRValue(var), nullptr))
        return;
    }
  }

  // Create the default constructor.
  auto ctor = createImplicitConstructor(
                *this, structDecl, ImplicitConstructorKind::Default);

  // Copy the list of members, so we can add to it.
  // FIXME: Painfully inefficient to do the copy here.
  SmallVector<Decl *, 4> members(structDecl->getMembers().begin(),
                                 structDecl->getMembers().end());

  // Create the implicit memberwise constructor.
  members.push_back(ctor);

  // Set the members of the struct.
  structDecl->setMembers(Context.AllocateCopy(members),
                         structDecl->getBraces());

  // Create an empty body for the default constructor. The type-check of the
  // constructor body will introduce default initializations of the members.
  ctor->setBody(BraceStmt::create(Context, SourceLoc(), { }, SourceLoc()));

  // Add this to the list of implicitly-defined functions.
  implicitlyDefinedFunctions.push_back(ctor);
}

void TypeChecker::definePendingImplicitDecls() {
  // Default any implicit default constructors.
  for (auto structDecl : structsWithImplicitDefaultConstructor) {
    if (structsNeedingImplicitDefaultConstructor.count(structDecl))
      defineDefaultConstructor(structDecl);
  }
}

/// validateAttributes - Check that the func/var declaration attributes are ok.
void DeclChecker::validateAttributes(ValueDecl *VD) {
  const DeclAttributes &Attrs = VD->getAttrs();
  Type Ty = VD->getType();

  // Get the number of lexical arguments, for semantic checks below.
  int NumArguments = -1;
  FuncDecl *FDOrNull = dyn_cast<FuncDecl>(VD);
  if (FDOrNull) {
    if (AnyFunctionType *FT = Ty->getAs<AnyFunctionType>()) {
      if (FDOrNull->getDeclContext()->isTypeContext() && FDOrNull->isStatic())
        FT = FT->getResult()->castTo<AnyFunctionType>();
      if (TupleType *TT = FT->getInput()->getAs<TupleType>())
        NumArguments = TT->getFields().size();
    }
  }

  bool isOperator = VD->isOperator();

  // Operators must be declared with 'func', not 'var'.
  if (isOperator) {
    if (!FDOrNull) {
      TC.diagnose(VD->getLoc(), diag::operator_not_func);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // The unary prefix operator '&' is reserved and cannot be overloaded.
    if (FDOrNull->isUnaryOperator() && VD->getName().str() == "&"
        && !Attrs.isPostfix()) {
      TC.diagnose(VD->getStartLoc(), diag::custom_operator_addressof);
      return;
    }
  }

  auto isInClassContext = [](ValueDecl *vd) {
    return bool(vd->getDeclContext()->getDeclaredTypeOfContext()
                  ->getClassOrBoundGenericClass());
  };

  if (Attrs.isObjC()) {
    // Only classes, class protocols, instance properties, and methods can be
    // ObjC.
    Optional<Diag<>> error;
    if (isa<ClassDecl>(VD)) {
      /* ok */
    } else if (isa<FuncDecl>(VD) && isInClassContext(VD)) {
      if (isOperator)
        error = diag::invalid_objc_decl;
    } else if (isa<VarDecl>(VD) && isInClassContext(VD)) {
      /* ok */
    } else if (auto *protocol = dyn_cast<ProtocolDecl>(VD)) {
      if (!protocol->requiresClass())
        error = diag::objc_protocol_not_class_protocol;
    } else {
      error = diag::invalid_objc_decl;
    }

    if (error) {
      TC.diagnose(VD->getStartLoc(), *error);
      VD->getMutableAttrs().ObjC = false;
      return;
    }
  }

  // Ownership attributes (weak, unowned).
  if (Attrs.hasOwnership()) {
    // Diagnostics expect:
    //   0 - unowned
    //   1 - weak
    assert(unsigned(Attrs.isWeak()) + unsigned(Attrs.isUnowned()) == 1);
    unsigned ownershipKind = (unsigned) Attrs.getOwnership();

    // Only 'var' declarations can have ownership.
    // TODO: captures, consts, etc.
    if (!isa<VarDecl>(VD)) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_ownership_decl,
                  ownershipKind);
      VD->getMutableAttrs().clearOwnership();
      return;
    }

    // Type of declaration must be a reference type.
    if (!VD->getType()->allowsOwnership()) {
      // If we have an opaque type, suggest the possibility of adding
      // a class bound.
      if (VD->getType()->isExistentialType() ||
          VD->getType()->getAs<ArchetypeType>()) {
        TC.diagnose(VD->getStartLoc(), diag::invalid_ownership_opaque_type,
                    ownershipKind, VD->getType());
      } else {
        TC.diagnose(VD->getStartLoc(), diag::invalid_ownership_type,
                    ownershipKind, VD->getType());
      }
      VD->getMutableAttrs().clearOwnership();
      return;
    }

    // Change the type to the appropriate reference storage type.
    VD->overwriteType(ReferenceStorageType::get(VD->getType(),
                                                Attrs.getOwnership(),
                                                TC.Context));
  }

  if (Attrs.isIBOutlet()) {
    // Only instance properties can be IBOutlets.
    // FIXME: This could do some type validation as well (all IBOutlets refer
    // to objects).
    if (!(isa<VarDecl>(VD) && isInClassContext(VD))) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_iboutlet);
      VD->getMutableAttrs().IBOutlet = false;
      return;
    }
  }

  if (Attrs.isIBAction()) {
    // Only instance methods returning () can be IBActions.
    const FuncDecl *FD = dyn_cast<FuncDecl>(VD);
    if (!FD || !isa<ClassDecl>(VD->getDeclContext()) || FD->isStatic() ||
        FD->isGetterOrSetter()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_ibaction_decl);
      VD->getMutableAttrs().IBAction = false;
      return;
    }

    // IBActions instance methods must have type Class -> (...) -> ().
    // FIXME: This could do some argument type validation as well (only certain
    // method signatures are allowed for IBActions).
    Type CurriedTy = VD->getType()->castTo<AnyFunctionType>()->getResult();
    Type ResultTy = CurriedTy->castTo<AnyFunctionType>()->getResult();
    if (!ResultTy->isEqual(TupleType::getEmpty(TC.Context))) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_ibaction_result, ResultTy);
      VD->getMutableAttrs().IBAction = false;
      return;
    }
  }

  if (Attrs.isInfix()) {
    // Only operator functions can be infix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::infix_not_an_operator);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only binary operators can be infix.
    if (!FDOrNull || !FDOrNull->isBinaryOperator()) {
      TC.diagnose(Attrs.LSquareLoc, diag::invalid_infix_input);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isPostfix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::postfix_not_an_operator);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only unary operators can be postfix.
    if (!FDOrNull || !FDOrNull->isUnaryOperator()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_postfix_input);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isPrefix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(VD->getStartLoc(), diag::prefix_not_an_operator);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only unary operators can be postfix.
    if (!FDOrNull || !FDOrNull->isUnaryOperator()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_prefix_input);
      VD->getMutableAttrs().ExplicitPostfix = false;
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isAssignment()) {
    // Only function declarations can be assignments.
    if (!isa<FuncDecl>(VD) || !VD->isOperator()) {
      TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute,"assignment");
      VD->getMutableAttrs().Assignment = false;
    } else if (NumArguments < 1) {
      TC.diagnose(VD->getStartLoc(), diag::assignment_without_byref);
      VD->getMutableAttrs().Assignment = false;
    } else {
      auto FT = VD->getType()->castTo<AnyFunctionType>();
      Type ParamType = FT->getInput();
      TupleType *ParamTT = ParamType->getAs<TupleType>();
      if (ParamTT)
        ParamType = ParamTT->getElementType(0);

      if (!ParamType->is<LValueType>()) {
        TC.diagnose(VD->getStartLoc(), diag::assignment_without_byref);
        VD->getMutableAttrs().Assignment = false;
      }
    }
  }

  if (Attrs.isConversion()) {
    // Only instance members with no non-defaulted parameters can be
    // conversions.
    if (!isa<FuncDecl>(VD) || !VD->isInstanceMember()) {
      TC.diagnose(VD->getStartLoc(), diag::conversion_not_instance_method,
                  VD->getName());
      VD->getMutableAttrs().Conversion = false;
    } else if (!VD->getType()->is<ErrorType>()) {
      AnyFunctionType *BoundMethodTy
        = VD->getType()->castTo<AnyFunctionType>()->getResult()
            ->castTo<AnyFunctionType>();

      bool AcceptsEmptyParamList = false;
      Type InputTy = BoundMethodTy->getInput();
      if (const TupleType *Tuple = InputTy->getAs<TupleType>()) {
        bool AllDefaulted = true;
        for (auto Elt : Tuple->getFields()) {
          if (!Elt.hasInit()) {
            AllDefaulted = false;
            break;
          }
        }

        AcceptsEmptyParamList = AllDefaulted;
      }

      if (!AcceptsEmptyParamList) {
        TC.diagnose(VD->getStartLoc(), diag::conversion_params,
                    VD->getName());
        VD->getMutableAttrs().Conversion = false;
      }
    }
  }

  if (Attrs.isTransparent()) {
    // Only functions can be 'transparent'.
    auto *FD = dyn_cast<FuncDecl>(VD);
    if (!FD) {
      TC.diagnose(VD->getStartLoc(), diag::transparent_not_function);
      VD->getMutableAttrs().Transparent = false;
    } else if (FD->getGenericParams()) {
      // We don't yet support transparent on generic functions.
      TC.diagnose(VD->getStartLoc(), diag::transparent_generic_not_supported);
      VD->getMutableAttrs().Transparent = false;
    }
  }

  if (Attrs.isByref()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "byref");
    VD->getMutableAttrs().Byref = false;
  }

  if (Attrs.isAutoClosure()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "auto_closure");
    VD->getMutableAttrs().AutoClosure = false;
  }

  if (Attrs.isExported()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "exported");
    VD->getMutableAttrs().Exported = false;
  }

  if (Attrs.isObjCBlock()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "objc_block");
    VD->getMutableAttrs().ObjCBlock = false;
  }

  // Only protocols can have the [class_protocol] attribute.
  if (Attrs.isClassProtocol() && !isa<ProtocolDecl>(VD)) {
    TC.diagnose(VD->getStartLoc(), diag::class_protocol_not_protocol);
    VD->getMutableAttrs().ClassProtocol = false;
  }

  if (Attrs.hasCC()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "cc");
    VD->getMutableAttrs().cc = {};
  }

  if (Attrs.isThin()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "thin");
    VD->getMutableAttrs().Thin = false;
  }

  if (Attrs.isNoReturn()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "noreturn");
    VD->getMutableAttrs().NoReturn = false;
  }

  if (Attrs.isLocalStorage()) {
    TC.diagnose(VD->getStartLoc(), diag::invalid_decl_attribute, "local_storage");
    VD->getMutableAttrs().LocalStorage = false;
  }
}
