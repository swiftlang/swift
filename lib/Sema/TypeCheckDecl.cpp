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
#include "swift/AST/Expr.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

namespace {

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

/// Used during enum raw value checking to identify duplicate raw values.
/// Character, string, float, and integer literals are all keyed by value.
/// Float and integer literals are additionally keyed by numeric equivalence.
struct RawValueKey {
  enum class Kind : uint8_t { String, Char, Float, Int, Tombstone, Empty } kind;
  
  // FIXME: doesn't accommodate >64-bit or signed raw integer or float values.
  union {
    StringRef stringValue;
    uint32_t charValue;
    uint64_t intValue;
    double floatValue;
  };
  
  explicit RawValueKey(LiteralExpr *expr) {
    switch (expr->getKind()) {
    case ExprKind::IntegerLiteral:
      kind = Kind::Int;
      intValue = cast<IntegerLiteralExpr>(expr)->getValue().getZExtValue();
      return;
    case ExprKind::FloatLiteral: {
      double v = cast<FloatLiteralExpr>(expr)->getValue().convertToDouble();
      // If the value losslessly converts to int, key it as an int.
      if (v <= (double)UINT64_MAX && round(v) == v) {
        kind = Kind::Int;
        intValue = (uint64_t)v;
      } else {
        kind = Kind::Float;
        floatValue = v;
      }
      return;
    }
    case ExprKind::CharacterLiteral:
      kind = Kind::Char;
      charValue = cast<CharacterLiteralExpr>(expr)->getValue();
      return;
    case ExprKind::StringLiteral:
      kind = Kind::String;
      stringValue = cast<StringLiteralExpr>(expr)->getValue();
      return;
    default:
      llvm_unreachable("not a valid literal expr for raw value");
    }
  }
  
  explicit RawValueKey(Kind k) : kind(k) {
    assert((k == Kind::Tombstone || k == Kind::Empty)
           && "this ctor is only for creating DenseMap special values");
  }
};
  
/// Used during enum raw value checking to identify the source of a raw value,
/// which may have been derived by auto-incrementing, for diagnostic purposes.
struct RawValueSource {
  /// The decl that has the raw value.
  EnumElementDecl *sourceElt;
  /// If the sourceDecl didn't explicitly name a raw value, this is the most
  /// recent preceding decl with an explicit raw value. This is used to
  /// diagnose 'autoincrementing from' messages.
  EnumElementDecl *lastExplicitValueElt;
};

} // end anonymous namespace

namespace llvm {

template<>
class DenseMapInfo<RawValueKey> {
public:
  static RawValueKey getEmptyKey() {
    return RawValueKey(RawValueKey::Kind::Empty);
  }
  static RawValueKey getTombstoneKey() {
    return RawValueKey(RawValueKey::Kind::Tombstone);
  }
  static unsigned getHashValue(RawValueKey k) {
    switch (k.kind) {
    case RawValueKey::Kind::Float:
      // Hash as bits. We want to treat distinct but IEEE-equal values as not
      // equal.
    case RawValueKey::Kind::Int:
      return DenseMapInfo<uint64_t>::getHashValue(k.intValue);
      return DenseMapInfo<uint64_t>::getHashValue(k.intValue);
    case RawValueKey::Kind::Char:
      return DenseMapInfo<uint32_t>::getHashValue(k.charValue);
    case RawValueKey::Kind::String:
      return llvm::HashString(k.stringValue);
    case RawValueKey::Kind::Empty:
    case RawValueKey::Kind::Tombstone:
      return 0;
    }
  }
  static bool isEqual(RawValueKey a, RawValueKey b) {
    if (a.kind != b.kind)
      return false;
    switch (a.kind) {
    case RawValueKey::Kind::Float:
      // Hash as bits. We want to treat distinct but IEEE-equal values as not
      // equal.
    case RawValueKey::Kind::Int:
      return a.intValue == b.intValue;
    case RawValueKey::Kind::Char:
      return a.charValue == b.charValue;
    case RawValueKey::Kind::String:
      return a.stringValue.equals(b.stringValue);
    case RawValueKey::Kind::Empty:
    case RawValueKey::Kind::Tombstone:
      return true;
    }
  }
};
  
} // end llvm namespace

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

/// Check that the func/var declaration attributes are ok.
static void validateAttributes(TypeChecker &TC, ValueDecl *VD);

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
    
    // If this is an enum inheritance clause, check for a raw type.
    if (isa<EnumDecl>(decl)) {
      // Check if we already had a raw type.
      if (superclassTy) {
        tc.diagnose(inherited.getSourceRange().Start,
                    diag::multiple_enum_raw_types, superclassTy, inheritedTy)
          .highlight(superclassRange);
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
                    diag::raw_type_not_first, inheritedTy)
          .fixItRemoveChars(afterPriorLoc, afterMyEndLoc)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the raw type.
      }

      // Record the raw type.
      superclassTy = inheritedTy;
      superclassRange = inherited.getSourceRange();
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
    else if (auto enumDecl = dyn_cast<EnumDecl>(decl))
      enumDecl->setRawType(superclassTy);
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

/// Retrieve the raw type of the given enum.
static ArrayRef<EnumDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                      EnumDecl *enumDecl,
                                                      EnumDecl **scratch) {
  checkInheritanceClause(tc, enumDecl);
  
  if (enumDecl->hasRawType()) {
    *scratch = enumDecl->getRawType()->getEnumOrBoundGenericEnum();
    return *scratch ? ArrayRef<EnumDecl*>(*scratch) : ArrayRef<EnumDecl*>{};
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

/// Break the inheritance cycle for a class by removing its superclass.
static void breakInheritanceCycle(ClassDecl *classDecl) {
  classDecl->setSuperclass(Type());
}

/// Break the inheritance cycle for an enum by removing its raw type.
static void breakInheritanceCycle(EnumDecl *enumDecl) {
  enumDecl->setRawType(Type());
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

static void setBoundVarsTypeError(Pattern *pattern, ASTContext &ctx) {
  switch (pattern->getKind()) {
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(pattern)->getFields())
      setBoundVarsTypeError(field.getPattern(), ctx);
    return;
  case PatternKind::Paren:
    return setBoundVarsTypeError(cast<ParenPattern>(pattern)->getSubPattern(),
                                 ctx);
  case PatternKind::Typed:
    return setBoundVarsTypeError(cast<TypedPattern>(pattern)->getSubPattern(),
                                 ctx);
  case PatternKind::NominalType:
    return setBoundVarsTypeError(cast<NominalTypePattern>(pattern)
                                   ->getSubPattern(),
                                 ctx);
  case PatternKind::Var:
    return setBoundVarsTypeError(cast<VarPattern>(pattern)->getSubPattern(),
                                 ctx);
  case PatternKind::EnumElement:
    if (auto subpattern = cast<EnumElementPattern>(pattern)->getSubPattern())
      setBoundVarsTypeError(subpattern, ctx);
    return;

  // Handle vars.
  case PatternKind::Named: {
    // Don't change the type of a variable that we've been able to
    // compute a type for.
    VarDecl *var = cast<NamedPattern>(pattern)->getDecl();
    if (var->hasType()) {
      if (var->getType()->is<ErrorType>())
        var->setInvalid();
    } else {
      var->setType(ErrorType::get(ctx));
      var->setInvalid();
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

/// Create a fresh archetype building.
static ArchetypeBuilder createArchetypeBuilder(TypeChecker &TC) {
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
      std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
        return { false, expr };
      }

      // Skip statements.
      std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
        return { false, stmt };
      }

      // Skip patterns
      std::pair<bool, Pattern*> walkToPatternPre(Pattern *pattern) override {
        return { false, pattern };
      }

      bool walkToTypeReprPost(TypeRepr *repr) override {
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
    ArchetypeBuilder builder = createArchetypeBuilder(TC);
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
    case PatternKind::EnumElement: {
      auto *OP = cast<EnumElementPattern>(P);
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

      validateAttributes(TC, VD);

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
    if (!IsFirstPass && !PBD->getInit() &&
        isa<TypedPattern>(PBD->getPattern()) &&
        !PBD->getDeclContext()->isTypeContext()) {
      // Type-check the pattern.
      if (TC.typeCheckPattern(PBD->getPattern(),
                              PBD->getDeclContext(),
                              /*allowUnknownTypes*/false)) {
        setBoundVarsTypeError(PBD->getPattern(), TC.Context);
        return;
      }
    } else if (!IsFirstPass && PBD->getInit()) {
      if (TC.typeCheckBinding(PBD)) {
        setBoundVarsTypeError(PBD->getPattern(), TC.Context);
        return;
      }
    } else if (!IsFirstPass || !DelayCheckingPattern) {
      if (TC.typeCheckPattern(PBD->getPattern(),
                              PBD->getDeclContext(),
                              /*allowUnknownTypes*/false)) {
        setBoundVarsTypeError(PBD->getPattern(), TC.Context);
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

    // A subscript is ObjC-compatible if it's explicitly [objc], or a
    // member of an ObjC-compatible class or property.
    DeclContext *dc = SD->getDeclContext();
    if (dc && dc->getDeclaredTypeInContext()) {
      ClassDecl *classContext = dc->getDeclaredTypeInContext()
        ->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext = dyn_cast<ProtocolDecl>(dc);
      bool isObjC = (classContext && classContext->isObjC())
                  || (protocolContext && protocolContext->isObjC())
                  || SD->getAttrs().ObjC;
      if (isObjC && SD->getObjCSubscriptKind() != ObjCSubscriptKind::None) {
        SD->setIsObjC(true);
      }
    }

    validateAttributes(TC, SD);
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
  
  // Given the raw value literal expression for an enum case, produces the
  // auto-incremented raw value for the subsequent case, or returns null if
  // the value is not auto-incrementable.
  LiteralExpr *getAutoIncrementedLiteralExpr(Type rawTy,
                                             EnumElementDecl *forElt,
                                             LiteralExpr *prevValue) {
    // If there was no previous value, start from zero.
    if (!prevValue) {
      // The raw type must be integer literal convertible for this to work.
      if (!TC.conformsToProtocol(rawTy,
                 TC.getProtocol(forElt->getLoc(),
                                KnownProtocolKind::IntegerLiteralConvertible)))
      {
        TC.diagnose(forElt->getLoc(),
                    diag::enum_non_integer_convertible_raw_type_no_value);
        return nullptr;
      }
      
      return new (TC.Context) IntegerLiteralExpr("0", SourceLoc(),
                                                 /*Implicit=*/true);
    }
    
    if (auto intLit = dyn_cast<IntegerLiteralExpr>(prevValue)) {
      APInt nextVal = intLit->getValue() + 1;
      llvm::SmallString<10> nextValStr;
      nextVal.toStringSigned(nextValStr);
      return new (TC.Context)
        IntegerLiteralExpr(TC.Context.AllocateCopy(StringRef(nextValStr)),
                           SourceLoc(), /*Implicit=*/true);
    }
    
    TC.diagnose(forElt->getLoc(),
                diag::enum_non_integer_raw_value_auto_increment);
    return nullptr;
  }
  
  void visitEnumDecl(EnumDecl *UD) {
    if (!IsSecondPass) {
      TC.validateTypeDecl(UD);

      {
        // Check for circular inheritance of the raw type.
        SmallVector<EnumDecl *, 8> path;
        checkCircularity(TC, UD, diag::circular_enum_inheritance,
                         diag::enum_here, path);
      }
    }

    for (Decl *member : UD->getMembers())
      visit(member);

    if (!IsFirstPass) {
      checkExplicitConformance(UD, UD->getDeclaredTypeInContext());
      
      // If we have a raw type, check it and the cases' raw values.
      if (auto rawTy = UD->getRawType()) {
        // Check that the raw type is convertible from one of the primitive
        // literal protocols.
        bool literalConvertible = false;
        for (auto literalProtocol : {
                                KnownProtocolKind::IntegerLiteralConvertible,
                                KnownProtocolKind::StringLiteralConvertible,
                                KnownProtocolKind::FloatLiteralConvertible,
                                KnownProtocolKind::CharacterLiteralConvertible})
        {
          if (TC.conformsToProtocol(rawTy,
                              TC.getProtocol(UD->getLoc(), literalProtocol))) {
            literalConvertible = true;
            break;
          }
        }
        
        if (!literalConvertible) {
          TC.diagnose(UD->getInherited()[0].getSourceRange().Start,
                      diag::raw_type_not_literal_convertible,
                      rawTy);
        }
        
        // Check the raw values of the cases.
        LiteralExpr *prevValue = nullptr;
        EnumElementDecl *lastExplicitValueElt = nullptr;
        // Keep a map we can use to check for duplicate case values.
        llvm::DenseMap<RawValueKey, RawValueSource> uniqueRawValues;
        
        for (auto elt : UD->getAllElements()) {
          // We don't yet support raw values on payload cases.
          if (elt->hasArgumentType()) {
            TC.diagnose(elt->getLoc(),
                        diag::enum_with_raw_type_case_with_argument);
            TC.diagnose(UD->getInherited()[0].getSourceRange().Start,
                        diag::enum_raw_type_here, rawTy);
          }
          
          // If the union element has no explicit raw value, try to
          // autoincrement from the previous value, or start from zero if this
          // is the first element.
          if (!elt->hasRawValueExpr()) {
            auto nextValue = getAutoIncrementedLiteralExpr(rawTy,elt,prevValue);
            if (!nextValue) {
              break;
            }
            elt->setRawValueExpr(nextValue);
            Expr *typeChecked = nextValue;
            if (!TC.typeCheckExpression(typeChecked, UD, rawTy, false))
              elt->setTypeCheckedRawValueExpr(typeChecked);
          } else {
            lastExplicitValueElt = elt;
          }
          prevValue = elt->getRawValueExpr();
          assert(prevValue && "continued without setting raw value of enum case");
          
          // Check that the raw value is unique.
          RawValueKey key(elt->getRawValueExpr());
          auto found = uniqueRawValues.find(key);
          if (found != uniqueRawValues.end()) {
            SourceLoc diagLoc = elt->getRawValueExpr()->isImplicit()
              ? elt->getLoc() : elt->getRawValueExpr()->getLoc();
            TC.diagnose(diagLoc, diag::enum_raw_value_not_unique);
            assert(lastExplicitValueElt &&
                   "should not be able to have non-unique raw values when "
                   "relying on autoincrement");
            if (lastExplicitValueElt != elt)
              TC.diagnose(lastExplicitValueElt->getRawValueExpr()->getLoc(),
                          diag::enum_raw_value_incrementing_from_here);
            
            auto foundElt = found->second.sourceElt;
            diagLoc = foundElt->getRawValueExpr()->isImplicit()
              ? foundElt->getLoc() : foundElt->getRawValueExpr()->getLoc();
            TC.diagnose(diagLoc, diag::enum_raw_value_used_here);
            if (foundElt != found->second.lastExplicitValueElt) {
              if (found->second.lastExplicitValueElt)
                TC.diagnose(found->second.lastExplicitValueElt
                              ->getRawValueExpr()->getLoc(),
                            diag::enum_raw_value_incrementing_from_here);
              else
                TC.diagnose(UD->getAllElements().front()->getLoc(),
                            diag::enum_raw_value_incrementing_from_zero);
            }
          } else {
            uniqueRawValues.insert({RawValueKey(elt->getRawValueExpr()),
                                    RawValueSource{elt, lastExplicitValueElt}});
          }
        }
      }
    }
  }

  void visitStructDecl(StructDecl *SD) {
    if (!IsSecondPass)
      TC.validateTypeDecl(SD);

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

      {
        // Check for circular inheritance.
        SmallVector<ClassDecl *, 8> path;
        checkCircularity(TC, CD, diag::circular_class_inheritance,
                         diag::class_here, path);
      }
    }

    for (Decl *Member : CD->getMembers())
      visit(Member);

    if (!IsSecondPass) {
      TC.addImplicitConstructors(CD);
      TC.addImplicitDestructor(CD);
    }
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

    {
      // Check for circular inheritance within the protocol.
      SmallVector<ProtocolDecl *, 8> path;
      checkCircularity(TC, PD, diag::circular_protocol_def,
                       diag::protocol_here, path);
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);
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
    if (FD->hasType())
      return;

    bool badType = false;
    if (!FD->getBodyResultTypeLoc().isNull()) {
      if (TC.validateType(FD->getBodyResultTypeLoc())) {
        badType = true;
      }
    }

    badType =
        badType || semaFuncParamPatterns(FD, FD->getArgParamPatterns());
    badType =
        badType || semaFuncParamPatterns(FD, FD->getBodyParamPatterns());

    if (badType) {
      FD->setType(ErrorType::get(TC.Context));
      FD->setInvalid();
      return;
    }

    Type funcTy = FD->getBodyResultTypeLoc().getType();
    if (!funcTy) {
      funcTy = TupleType::getEmpty(TC.Context);
    }
    auto bodyResultType = funcTy;

    // FIXME: it would be nice to have comments explaining what this is all about.
    GenericParamList *genericParams = FD->getGenericParams();
    GenericParamList *outerGenericParams = nullptr;
    auto patterns = FD->getArgParamPatterns();
    bool isInstanceFunc = (bool)FD->computeSelfType(&outerGenericParams);

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
    FD->setType(funcTy);
    FD->setBodyResultType(bodyResultType);
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

    if (IsSecondPass || FD->hasType())
      return;

    // Bind operator functions to the corresponding operator declaration.
    if (FD->isOperator())
      bindFuncDeclToOperator(FD);

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
      builder.emplace(createArchetypeBuilder(TC));
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
      if (auto resultType = FD->getBodyResultTypeLoc().getTypeRepr())
        builder->inferRequirements(resultType);

      // Assign archetypes.
      finalizeGenericParamList(*builder, FD->getGenericParams(), FD);

      // Go through and revert all of the dependent types we computed.

      // Revert the result type.
      if (!FD->getBodyResultTypeLoc().isNull()) {
        revertDependentTypeLoc(FD->getBodyResultTypeLoc(), FD);
      }

      // Revert the argument patterns.
      ArrayRef<Pattern *> argPatterns = FD->getArgParamPatterns();
      if (FD->getDeclContext()->isTypeContext())
        argPatterns = argPatterns.slice(1);
      for (auto argPattern : argPatterns) {
        revertDependentPattern(argPattern, FD);
      }

      // Revert the body patterns.
      ArrayRef<Pattern *> bodyPatterns = FD->getBodyParamPatterns();
      if (FD->getDeclContext()->isTypeContext())
        bodyPatterns = bodyPatterns.slice(1);
      for (auto bodyPattern : bodyPatterns) {
        revertDependentPattern(bodyPattern, FD);
      }

      // Clear out the types.
      FD->revertType();

      // Type check the parameters and return type again, now with archetypes.
      semaFuncDecl(FD, /*consumeAttributes=*/true);

      // The second type check should have created a non-dependent type.
      assert(!FD->getType()->isDependentType());
    }

    validateAttributes(TC, FD);

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
        isObjC = prop->isObjC() || prop->getAttrs().isIBOutlet();
      }

      FD->setIsObjC(isObjC);
    }
  }

  void visitEnumElementDecl(EnumElementDecl *ED) {
    if (IsSecondPass)
      return;

    EnumDecl *UD = ED->getParentEnum();
    Type ElemTy = UD->getDeclaredTypeInContext();

    if (!ED->getArgumentTypeLoc().isNull())
      if (TC.validateType(ED->getArgumentTypeLoc())) {
        ED->overwriteType(ErrorType::get(TC.Context));
        ED->setInvalid();
        return;
      }
    if (!ED->getResultTypeLoc().isNull()) {
      if (TC.validateType(ED->getResultTypeLoc())) {
        ED->overwriteType(ErrorType::get(TC.Context));
        ED->setInvalid();
        return;
      }
      if (ED->getResultType()->getEnumOrBoundGenericEnum() != UD)
        TC.diagnose(ED->getLoc(), diag::invalid_enum_case_result_type);
      ElemTy = ED->getResultType();
    }

    // Check the raw value, if we have one.
    if (auto *rawValue = ED->getRawValueExpr()) {
      auto rawTy = UD->getRawType();
      if (!rawTy) {
        TC.diagnose(rawValue->getLoc(), diag::enum_raw_value_without_raw_type);
        // Recover by setting the raw type as this element's type.
      }
      Expr *typeCheckedExpr = rawValue;
      if (!TC.typeCheckExpression(typeCheckedExpr, UD, rawTy, false))
        ED->setTypeCheckedRawValueExpr(typeCheckedExpr);
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
      TC.diagnose(ED->getLoc(), diag::enum_element_not_materializable);
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

      if (!isa<EnumType>(ExtendedTy) &&
          !isa<StructType>(ExtendedTy) &&
          !isa<ClassType>(ExtendedTy) &&
          !isa<BoundGenericEnumType>(ExtendedTy) &&
          !isa<BoundGenericStructType>(ExtendedTy) &&
          !isa<BoundGenericClassType>(ExtendedTy) &&
          !isa<ErrorType>(ExtendedTy)) {
        TC.diagnose(ED->getStartLoc(), diag::non_nominal_extension,
                    isa<ProtocolType>(ExtendedTy), ExtendedTy);
        // FIXME: It would be nice to point out where we found the named type
        // declaration, if any.
        ED->setInvalid();
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

    if (IsSecondPass || CD->hasType()) {
      return;
    }

    assert(CD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent constructors outside of types!");

    GenericParamList *outerGenericParams = nullptr;
    Type SelfTy = CD->computeSelfType(&outerGenericParams);
    CD->getImplicitSelfDecl()->setType(SelfTy);

    Optional<ArchetypeBuilder> builder;
    if (auto gp = CD->getGenericParams()) {
      // Write up generic parameters and check the generic parameter list.
      gp->setOuterParameters(outerGenericParams);
      ArchetypeBuilder builder = createArchetypeBuilder(TC);
      checkGenericParamList(builder, gp);

      // Type check the constructor parameters.
      if (TC.typeCheckPattern(CD->getArgParams(),
                              CD,
                              /*allowUnknownTypes*/false)) {
        CD->overwriteType(ErrorType::get(TC.Context));
        CD->setInvalid();
      }

      // Infer requirements from the parameters of the constructor.
      builder.inferRequirements(CD->getArgParams());

      // Assign archetypes.
      finalizeGenericParamList(builder, gp, CD);

      // Reverse the constructor argument parameter pattern; it will be checked
      // again, with archetypes, below.
      revertDependentPattern(CD->getArgParams(), CD);
    }

    // Type check the constructor parameters.
    if (TC.typeCheckPattern(CD->getArgParams(),
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
        FnTy = PolymorphicFunctionType::get(CD->getArgParams()->getType(),
                                            SelfTy, innerGenericParams,
                                            TC.Context);
      } else
        FnTy = FunctionType::get(CD->getArgParams()->getType(),
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

      // Type check the constructor body parameters.
      if (TC.typeCheckPattern(CD->getBodyParams(),
                              CD,
                              /*allowUnknownTypes*/false)) {
        CD->overwriteType(ErrorType::get(TC.Context));
        CD->setInvalid();
      }
    }

    // A method is ObjC-compatible if it's explicitly [objc], a member of an
    // ObjC-compatible class, or an accessor for an ObjC property.
    DeclContext *dc = CD->getDeclContext();
    if (dc && dc->getDeclaredTypeInContext()) {
      ClassDecl *classContext = dc->getDeclaredTypeInContext()
                                  ->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext = dyn_cast<ProtocolDecl>(dc);
      bool isObjC = CD->getAttrs().isObjC()
        || (classContext && classContext->isObjC())
        || (protocolContext && protocolContext->isObjC());
      CD->setIsObjC(isObjC);
    }

    validateAttributes(TC, CD);
  }

  void visitDestructorDecl(DestructorDecl *DD) {
    if (DD->isInvalid()) {
      DD->overwriteType(ErrorType::get(TC.Context));
      return;
    }

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

    validateAttributes(TC, DD);
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D, bool isFirstPass) {
  PrettyStackTraceDecl StackTrace("type-checking", D);
  bool isSecondPass = !isFirstPass && D->getDeclContext()->isModuleContext();
  DeclChecker(*this, isFirstPass, isSecondPass).visit(D);
}

void TypeChecker::validateTypeDecl(ValueDecl *D, bool resolveTypeParams) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("not a value decl");

  case DeclKind::TypeAlias: {
    // Type aliases may not have an underlying type yet.
    auto typeAlias = cast<TypeAliasDecl>(D);
    if (typeAlias->getUnderlyingTypeLoc().getTypeRepr())
      validateType(typeAlias->getUnderlyingTypeLoc());
    break;
  }

  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType: {
    auto typeParam = cast<AbstractTypeParamDecl>(D);
    if (!resolveTypeParams || typeParam->getArchetype())
      break;
    
    // FIXME: Avoid full check in these cases?
    DeclContext *DC = typeParam->getDeclContext();
    switch (DC->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::TopLevelCodeDecl:
      llvm_unreachable("cannot have type params");

    case DeclContextKind::NominalTypeDecl:
      typeCheckDecl(cast<NominalTypeDecl>(DC), true);
      break;

    case DeclContextKind::ExtensionDecl:
      llvm_unreachable("not yet implemented");
    
    case DeclContextKind::AbstractClosureExpr:
      llvm_unreachable("cannot have type params");

    case DeclContextKind::AbstractFunctionDecl:
      if (auto nominal = dyn_cast<NominalTypeDecl>(DC->getParent()))
        typeCheckDecl(nominal, true);
      else if (auto extension = dyn_cast<ExtensionDecl>(DC->getParent()))
        typeCheckDecl(extension, true);
      typeCheckDecl(cast<AbstractFunctionDecl>(DC), true);
      break;
    }
    break;
  }
  
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class: {
    auto nominal = cast<NominalTypeDecl>(D);
    if (nominal->hasType())
      return;

    // Check generic parameters, if needed.
    if (auto gp = nominal->getGenericParams()) {
      gp->setOuterParameters(
        nominal->getDeclContext()->getGenericParamsOfContext());
      // FIXME: Should not have to create a DeclChecker.
      DeclChecker(*this, false, false).checkGenericParams(gp, nominal);
    }

    // Compute the declared type.
    nominal->computeType();

    if (auto SD = dyn_cast<StructDecl>(D))
      addImplicitConstructors(SD);

    validateAttributes(*this, D);
    checkInheritanceClause(*this, D);

    // Mark a class as [objc]. This must happen before checking its members.
    if (auto CD = dyn_cast<ClassDecl>(nominal)) {
      ClassDecl *superclassDecl = nullptr;
      if (CD->hasSuperclass())
        superclassDecl = CD->getSuperclass()->getClassOrBoundGenericClass();

      CD->setIsObjC(CD->getAttrs().isObjC() ||
                    (superclassDecl && superclassDecl->isObjC()));
    }

    // FIXME: Don't validate members so eagerly.
    if (isa<StructDecl>(nominal) || isa<ClassDecl>(nominal)) {
      for (Decl *member : nominal->getMembers())
        if (auto VD = dyn_cast<ValueDecl>(member))
          validateTypeDecl(VD, true);
    }
    break;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    if (proto->hasType())
      return;
    proto->computeType();

    checkInheritanceClause(*this, D);
    validateAttributes(*this, D);

    // Fix the 'Self' associated type.
    AssociatedTypeDecl *selfDecl = nullptr;
    for (auto member : proto->getMembers()) {
      if (auto AssocType = dyn_cast<AssociatedTypeDecl>(member)) {
        checkInheritanceClause(*this, AssocType);

        if (AssocType->isSelf()) {
          selfDecl = AssocType;
          break;
        }
      }
    }
    assert(selfDecl && "no Self decl?");

    // Build archetypes for this protocol.
    ArchetypeBuilder builder = createArchetypeBuilder(*this);
    builder.addGenericParameter(selfDecl, 0);
    builder.addImplicitConformance(selfDecl, proto);
    builder.assignArchetypes();

    // Set the underlying type of each of the associated types to the
    // appropriate archetype.
    ArchetypeType *selfArchetype = builder.getArchetype(selfDecl);
    for (auto member : proto->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        TypeLoc underlyingTy;
        ArchetypeType *archetype = selfArchetype;
        if (assocType != selfDecl)
          archetype = selfArchetype->getNestedType(assocType->getName());
        assocType->setArchetype(archetype);
      }
    }

    // If the protocol is [objc], it may only refine other [objc] protocols.
    // FIXME: Revisit this restriction.
    if (proto->getAttrs().isObjC()) {
      bool isObjC = true;

      SmallVector<ProtocolDecl*, 2> inheritedProtocols;
      for (auto directInherited : proto->getInherited()) {
        if (!directInherited.getType()->isExistentialType(inheritedProtocols))
          continue;

        for (auto inherited : inheritedProtocols) {
          if (!inherited->getAttrs().isObjC()) {
            diagnose(proto->getLoc(),
                     diag::objc_protocol_inherits_non_objc_protocol,
                     proto->getDeclaredType(), inherited->getDeclaredType());
            diagnose(inherited->getLoc(), diag::protocol_here,
                     inherited->getName());
            isObjC = false;
          }
        }

        inheritedProtocols.clear();
      }

      proto->setIsObjC(isObjC);
    }
    break;
  }
      
  case DeclKind::Var: {
    if (D->hasType())
      return;
    if (PatternBindingDecl *PBD = cast<VarDecl>(D)->getParentPattern()) {
      if (typeCheckPattern(PBD->getPattern(), PBD->getDeclContext(),
                           /*allowUnknownTypes*/false)) {
        setBoundVarsTypeError(PBD->getPattern(), Context);
        return;
      }
    } else {
      // FIXME: This case is hit when code completion occurs in a function
      // parameter list. Previous parameters are definitely in scope, but
      // we don't really know how to type-check them.
      assert(isa<AbstractFunctionDecl>(D->getDeclContext()));
      D->setType(ErrorType::get(Context));
    }
    break;
  }
      
  case DeclKind::Func:
  case DeclKind::Subscript:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::EnumElement:
    if (D->hasType())
      return;
    typeCheckDecl(D, true);
    break;
  }

  assert(D->hasType());  
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

/// \brief Create an implicit struct or class constructor.
///
/// \param decl The struct or class for which a constructor will be created.
/// \param ICK The kind of implicit constructor to create.
///
/// \returns The newly-created constructor, which has already been type-checked
/// (but has not been added to the containing struct or class).
static ConstructorDecl *createImplicitConstructor(TypeChecker &tc,
                                                  NominalTypeDecl *decl,
                                                  ImplicitConstructorKind ICK) {
  ASTContext &context = tc.Context;
  // Determine the parameter type of the implicit constructor.
  SmallVector<TuplePatternElt, 8> patternElts;
  SmallVector<VarDecl *, 8> allArgs;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    assert(isa<StructDecl>(decl) && "Only struct have memberwise constructor");

    for (auto member : decl->getMembers()) {
      auto var = dyn_cast<VarDecl>(member);
      if (!var)
        continue;

      // Properties are computed, not initialized.
      if (var->isProperty())
        continue;
      tc.validateTypeDecl(var);

      auto varType = tc.getTypeOfRValue(var);

      // Create the parameter.
      auto *arg = new (context) VarDecl(SourceLoc(),
                                        var->getName(),
                                        varType, decl);
      allArgs.push_back(arg);
      Pattern *pattern = new (context) NamedPattern(arg);
      TypeLoc tyLoc = TypeLoc::withoutLoc(varType);
      pattern = new (context) TypedPattern(pattern, tyLoc);
      patternElts.push_back(TuplePatternElt(pattern));
    }
  }

  // Create the constructor.
  auto constructorID = context.getIdentifier("init");
  VarDecl *selfDecl
    = new (context) VarDecl(SourceLoc(),
                            context.getIdentifier("self"),
                            Type(), decl);
  ConstructorDecl *ctor
    = new (context) ConstructorDecl(constructorID, decl->getLoc(),
                                    nullptr, nullptr, selfDecl, nullptr,
                                    decl);
  selfDecl->setDeclContext(ctor);
  for (auto var : allArgs) {
    var->setDeclContext(ctor);
  }

  // Set its arguments.
  auto pattern = TuplePattern::create(context, decl->getLoc(),
                                      patternElts, decl->getLoc());
  ctor->setArgParams(pattern);
  ctor->setBodyParams(pattern);

  // Mark implicit.
  ctor->setImplicit();

  // Type-check the constructor declaration.
  tc.typeCheckDecl(ctor, /*isFirstPass=*/true);

  // If the struct in which this constructor is being added was imported,
  // add it as an external definition.
  if (decl->hasClangNode()) {
    tc.Context.ExternalDefinitions.insert(ctor);
  }

  return ctor;
}

void TypeChecker::addImplicitConstructors(NominalTypeDecl *decl) {
  assert(isa<StructDecl>(decl) || isa<ClassDecl>(decl));

  // Check whether there is a user-declared constructor or an instance
  // variable.
  bool FoundConstructor = false;
  bool FoundInstanceVar = false;
  for (auto member : decl->getMembers()) {
    if (isa<ConstructorDecl>(member)) {
      FoundConstructor = true;
      break;
    }

    if (auto var = dyn_cast<VarDecl>(member)) {
      if (!var->isProperty())
        FoundInstanceVar = true;
    }
  }

  // If we found a constructor, don't add any implicit constructors.
  if (FoundConstructor)
    return;

  // If we didn't find such a constructor, add the implicit one(s).

  // For a struct, we add a memberwise constructor.
  if (isa<StructDecl>(decl)) {
    // Copy the list of members, so we can add to it.
    // FIXME: Painfully inefficient to do the copy here.
    SmallVector<Decl *, 4> members(decl->getMembers().begin(),
                                   decl->getMembers().end());

    // Create the implicit memberwise constructor.
    auto ctor = createImplicitConstructor(*this, decl,
                                          ImplicitConstructorKind::Memberwise);
    members.push_back(ctor);

    // Set the members of the struct.
    decl->setMembers(Context.AllocateCopy(members), decl->getBraces());

    // If we didn't find any instance variables, the default
    // constructor will be the same as the memberwise constructor, so
    // we're done.
    if (!FoundInstanceVar)
      return;
  }

  // Note that we need a default constructor.
  assert(!typesNeedingImplicitDefaultConstructor.count(decl));
  typesNeedingImplicitDefaultConstructor.insert(decl);
  typesWithImplicitDefaultConstructor.push_back(decl);
}

void TypeChecker::addImplicitDestructor(ClassDecl *CD) {
  bool FoundDestructor = false;
  for (auto Member : CD->getMembers()) {
    if (isa<DestructorDecl>(Member)) {
      FoundDestructor = true;
      break;
    }
  }

  if (FoundDestructor)
    return;

  VarDecl *SelfDecl = new (Context)
      VarDecl(SourceLoc(), Context.getIdentifier("self"), Type(), CD);
  DestructorDecl *DD =
      new (Context) DestructorDecl(Context.getIdentifier("destructor"),
                                   CD->getLoc(), SelfDecl, CD);
  SelfDecl->setDeclContext(DD);

  DD->setImplicit();

  // Type-check the constructor declaration.
  typeCheckDecl(DD, /*isFirstPass=*/true);

  // Copy the list of members, so we can add to it.
  // FIXME: Painfully inefficient to do the copy here.
  SmallVector<Decl *, 4> Members(CD->getMembers().begin(),
                                 CD->getMembers().end());
  Members.push_back(DD);

  // Create an empty body for the destructor.
  DD->setBody(BraceStmt::create(Context, CD->getLoc(), { }, CD->getLoc()));

  CD->setMembers(Context.AllocateCopy(Members), CD->getBraces());

  // Add this to the list of implicitly-defined functions.
  implicitlyDefinedFunctions.push_back(DD);
}

bool TypeChecker::isDefaultInitializable(Type ty, Expr **initializer, 
                                         bool useConstructor) {
  CanType canTy = ty->getCanonicalType();
  switch (canTy->getKind()) {
  case TypeKind::Archetype:
  case TypeKind::BoundGenericStruct:
  case TypeKind::BoundGenericEnum:
  case TypeKind::Enum:
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
    // If we were asked to use a constructor, do so below.
    if (useConstructor)
      break;

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
                                    /*hasTrailingClosure=*/false,
                                    /*Implicit=*/true);
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
                                      /*hasTrailingClosure=*/false,
                                      /*Implicit=*/true);
  Expr *metatype = new (Context) MetatypeExpr(nullptr, SourceLoc(),
                                              MetaTypeType::get(ty, Context));
  *initializer = new (Context) CallExpr(metatype, arg, /*Implicit=*/true);

  return true;
}

void TypeChecker::defineDefaultConstructor(NominalTypeDecl *decl) {
  PrettyStackTraceDecl stackTrace("defining default constructor for",
                                  decl);

  // Erase this from the set of types that need an implicit default
  // constructor.
  assert(typesNeedingImplicitDefaultConstructor.count(decl));
  typesNeedingImplicitDefaultConstructor.erase(decl);

  // Verify that all of the instance variables of this type have default
  // constructors.
  for (auto member : decl->getMembers()) {
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

  // For a class, check whether the superclass (if it exists) is
  // default-initializable.
  if (isa<ClassDecl>(decl)) {
    if (auto superTy = getSuperClassOf(decl->getDeclaredTypeInContext())) {
      if (!isDefaultInitializable(superTy, nullptr, /*useConstructor=*/true))
        return;
    }
  }

  // Create the default constructor.
  auto ctor = createImplicitConstructor(
                *this, decl, ImplicitConstructorKind::Default);

  // Copy the list of members, so we can add to it.
  // FIXME: Painfully inefficient to do the copy here.
  SmallVector<Decl *, 4> members(decl->getMembers().begin(),
                                 decl->getMembers().end());

  // Add the constructor.
  members.push_back(ctor);

  // Set the members of the type.
  decl->setMembers(Context.AllocateCopy(members), decl->getBraces());

  // Create an empty body for the default constructor. The type-check of the
  // constructor body will introduce default initializations of the members.
  ctor->setBody(BraceStmt::create(Context, SourceLoc(), { }, SourceLoc()));

  // Add this to the list of implicitly-defined functions.
  implicitlyDefinedFunctions.push_back(ctor);
}

void TypeChecker::definePendingImplicitDecls() {
  // Default any implicit default constructors.
  for (unsigned i = 0; i != typesWithImplicitDefaultConstructor.size(); ++i) {
    auto decl = typesWithImplicitDefaultConstructor[i];
    if (typesNeedingImplicitDefaultConstructor.count(decl))
      defineDefaultConstructor(decl);
  }
}

static void validateAttributes(TypeChecker &TC, ValueDecl *VD) {
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
    // Only classes, class protocols, instance properties, methods,
    // constructors, and subscripts can be ObjC.
    Optional<Diag<>> error;
    if (isa<ClassDecl>(VD)) {
      /* ok */
    } else if (isa<FuncDecl>(VD) && isInClassContext(VD)) {
      if (isOperator)
        error = diag::invalid_objc_decl;
    } else if (isa<ConstructorDecl>(VD) && isInClassContext(VD)) {
      /* ok */
    } else if (isa<SubscriptDecl>(VD) && isInClassContext(VD) &&
               cast<SubscriptDecl>(VD)->getObjCSubscriptKind() 
                 != ObjCSubscriptKind::None) {
      /* ok */
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

    Type type = VD->getType();

    // A [weak] variable must have type R? for some ownership-capable type R.
    if (Attrs.isWeak()) {
      Type objType = type->getOptionalObjectType(TC.Context);

      // Use this special diagnostic if it's actually a reference type
      // but just isn't Optional.
      if (!objType && type->allowsOwnership()) {
        TC.diagnose(VD->getStartLoc(), diag::invalid_weak_ownership_not_optional,
                    OptionalType::get(type, TC.Context));
        VD->getMutableAttrs().clearOwnership();
        return;
      } else if (objType) {
        type = objType;
      }
    }

    if (!type->allowsOwnership()) {
      // If we have an opaque type, suggest the possibility of adding
      // a class bound.
      if (type->isExistentialType() || type->getAs<ArchetypeType>()) {
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
    VD->overwriteType(ReferenceStorageType::get(type,
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
    // Only abstract functions can be 'transparent'.
    auto *AFD = dyn_cast<AbstractFunctionDecl>(VD);

    if (!AFD) {
      TC.diagnose(VD->getStartLoc(), diag::transparent_not_valid);
      VD->getMutableAttrs().Transparent = false;

    // FIXME: We currently do not support transparent generics.
    } else if (AFD->getGenericParams()) {
      // We don't yet support transparent on generic functions.
      TC.diagnose(VD->getStartLoc(), diag::transparent_generic_not_supported);
      VD->getMutableAttrs().Transparent = false;

    // Protocol method declarations cannot be transparent.
    } else if (isa<ProtocolDecl>(AFD->getParent())) {
      TC.diagnose(VD->getStartLoc(),
                  diag::transparent_in_protocols_not_supported);
      VD->getMutableAttrs().Transparent = false;

    // Class methods cannot be transparent.
    } else if (isa<ClassDecl>(AFD->getParent())) {
      TC.diagnose(VD->getStartLoc(),
                  diag::transparent_in_classes_not_supported);
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
