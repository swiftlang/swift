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

#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "GenericTypeResolver.h"
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
#include "swift/AST/NameLookup.h"
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
  enum class Kind : uint8_t {
    String, UnicodeScalar, Float, Int, Tombstone, Empty
  } kind;
  
  // FIXME: doesn't accommodate >64-bit or signed raw integer or float values.
  union {
    StringRef stringValue;
    uint32_t charValue;
    int64_t intValue;
    double floatValue;
  };
  
  explicit RawValueKey(LiteralExpr *expr) {
    switch (expr->getKind()) {
    case ExprKind::IntegerLiteral:
      kind = Kind::Int;
      intValue = cast<IntegerLiteralExpr>(expr)->getValue().getSExtValue();
      return;
    case ExprKind::FloatLiteral: {
      double v = cast<FloatLiteralExpr>(expr)->getValue().convertToDouble();
      // If the value losslessly converts to int, key it as an int.
      if (v <= (double)INT64_MAX && round(v) == v) {
        kind = Kind::Int;
        intValue = (int64_t)v;
      } else {
        kind = Kind::Float;
        floatValue = v;
      }
      return;
    }
    case ExprKind::CharacterLiteral:
      kind = Kind::UnicodeScalar;
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
      return DenseMapInfo<int64_t>::getHashValue(k.intValue);
    case RawValueKey::Kind::UnicodeScalar:
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
    case RawValueKey::Kind::UnicodeScalar:
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
  if (isa<GenericTypeParamDecl>(decl))
    return true;

  // Associated types can inherit a class.
  if (isa<AssociatedTypeDecl>(decl))
    return true;

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

/// Check that the declaration attributes are ok.
static void validateAttributes(TypeChecker &TC, Decl *VD);

/// Check the inheritance clause of a type declaration or extension thereof.
///
/// This routine validates all of the types in the parsed inheritance clause,
/// recording the superclass (if any and if allowed) as well as the protocols
/// to which this type declaration conforms.
void TypeChecker::checkInheritanceClause(Decl *decl, DeclContext *DC,
                                         GenericTypeResolver *resolver) {
  if (!DC) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(decl))
      DC = nominal;
    else
      DC = decl->getDeclContext();
  }

  // Establish a default generic type resolver.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

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
  addImplicitConformances(*this, decl, allProtocols);
  for (unsigned i = 0, n = inheritedClause.size(); i != n; ++i) {
    auto &inherited = inheritedClause[i];

    // Validate the type.
    if (validateType(inherited, DC, None, resolver)) {
      inherited.setInvalidType(Context);
      continue;
    }

    auto inheritedTy = inherited.getType();

    // If this is an error type, ignore it.
    if (inheritedTy->is<ErrorType>())
      continue;

    // Retrieve the interface type for this inherited type.
    if (DC->isGenericContext() && DC->isTypeContext()) {
      inheritedTy = getInterfaceTypeFromInternalType(DC, inheritedTy);
    }

    // Check whether we inherited from the same type twice.
    CanType inheritedCanTy = inheritedTy->getCanonicalType();
    auto knownType = inheritedTypes.find(inheritedCanTy);
    if (knownType != inheritedTypes.end()) {
      SourceLoc afterPriorLoc
        = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                     inheritedClause[i-1].getSourceRange().End);
      SourceLoc afterMyEndLoc
        = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                     inherited.getSourceRange().End);

      diagnose(inherited.getSourceRange().Start,
               diag::duplicate_inheritance, inheritedTy)
        .fixItRemoveChars(afterPriorLoc, afterMyEndLoc)
        .highlight(knownType->second);
      inherited.setInvalidType(Context);
      continue;
    }
    inheritedTypes[inheritedCanTy] = inherited.getSourceRange();

    // If this is a protocol or protocol composition type, record the
    // protocols.
    if (inheritedTy->isExistentialType()) {
      SmallVector<ProtocolDecl *, 4> protocols;
      inheritedTy->isExistentialType(protocols);

      // AnyObject cannot be used in a type's inheritance clause.
      if (isa<NominalTypeDecl>(decl) && !decl->isImplicit()) {
        bool hasAnyObject = false;
        for (auto proto : protocols) {
          if (proto->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
            hasAnyObject = true;
            break;
          }
        }
        if (hasAnyObject) {
          diagnose(inheritedClause[i].getSourceRange().Start,
                   diag::dynamic_lookup_conformance);
          inherited.setInvalidType(Context);
          continue;
        }
      }

      allProtocols.insert(protocols.begin(), protocols.end());
      continue;
    }
    
    // If this is an enum inheritance clause, check for a raw type.
    if (isa<EnumDecl>(decl)) {
      // Check if we already had a raw type.
      if (superclassTy) {
        diagnose(inherited.getSourceRange().Start,
                 diag::multiple_enum_raw_types, superclassTy, inheritedTy)
          .highlight(superclassRange);
        inherited.setInvalidType(Context);
        continue;
      }
      
      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        SourceLoc afterPriorLoc
          = Lexer::getLocForEndOfToken(
              Context.SourceMgr,
              inheritedClause[i-1].getSourceRange().End);
        SourceLoc afterMyEndLoc
          = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                       inherited.getSourceRange().End);

        diagnose(inherited.getSourceRange().Start,
                 diag::raw_type_not_first, inheritedTy)
          .fixItRemoveChars(afterPriorLoc, afterMyEndLoc)
          .fixItInsert(inheritedClause[0].getSourceRange().Start,
                       inheritedTy.getString() + ", ");

        // Fall through to record the raw type.
      }

      // Record the raw type.
      superclassTy = inheritedTy;
      superclassRange = inherited.getSourceRange();
      
      // Add the RawRepresentable conformance implied by the raw type.
      allProtocols.insert(getProtocol(decl->getLoc(),
                                      KnownProtocolKind::RawRepresentable));
      continue;
    }

    // If this is a class type, it may be the superclass.
    if (inheritedTy->getClassOrBoundGenericClass()) {
      // First, check if we already had a superclass.
      if (superclassTy) {
        // FIXME: Check for shadowed protocol names, i.e., NSObject?

        // Complain about multiple inheritance.
        // Don't emit a Fix-It here. The user has to think harder about this.
        diagnose(inherited.getSourceRange().Start,
                 diag::multiple_inheritance, superclassTy, inheritedTy)
          .highlight(superclassRange);
        inherited.setInvalidType(Context);
        continue;
      }

      // If the declaration we're looking at doesn't allow a superclass,
      // complain.
      if (!canInheritClass(decl)) {
        diagnose(decl->getLoc(),
                 isa<ExtensionDecl>(decl)
                   ? diag::extension_class_inheritance
                   : diag::non_class_inheritance,
                 getDeclaredType(decl), inheritedTy)
          .highlight(inherited.getSourceRange());
        inherited.setInvalidType(Context);
        continue;
      }

      // If this is not the first entry in the inheritance clause, complain.
      if (i > 0) {
        SourceLoc afterPriorLoc
          = Lexer::getLocForEndOfToken(
              Context.SourceMgr,
              inheritedClause[i-1].getSourceRange().End);
        SourceLoc afterMyEndLoc
          = Lexer::getLocForEndOfToken(Context.SourceMgr,
                                       inherited.getSourceRange().End);

        diagnose(inherited.getSourceRange().Start,
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

    // We can't inherit from a non-class, non-protocol type.
    diagnose(decl->getLoc(),
             canInheritClass(decl)
               ? diag::inheritance_from_non_protocol_or_class
               : diag::inheritance_from_non_protocol,
             inheritedTy);
    // FIXME: Note pointing to the declaration 'inheritedTy' references?
    inherited.setInvalidType(Context);
  }

  // Record the protocols to which this declaration conforms along with the
  // superclass.
  auto allProtocolsCopy = Context.AllocateCopy(allProtocols);
  if (auto ext = dyn_cast<ExtensionDecl>(decl)) {
    assert(!superclassTy && "Extensions can't add superclasses");
    ext->setProtocols(allProtocolsCopy);
    return;
  }

  auto typeDecl = cast<TypeDecl>(decl);

  // FIXME: If we already set the protocols, bail out. We'd rather not have
  // to check this.
  if (typeDecl->isProtocolsValid())
    return;

  typeDecl->setProtocols(allProtocolsCopy);
  if (superclassTy) {
    if (auto classDecl = dyn_cast<ClassDecl>(decl))
      classDecl->setSuperclass(superclassTy);
    else if (auto enumDecl = dyn_cast<EnumDecl>(decl))
      enumDecl->setRawType(superclassTy);
    else
      cast<AbstractTypeParamDecl>(decl)->setSuperclass(superclassTy);
  }

  // For protocol decls, fill in null conformances.
  // FIXME: This shouldn't really be necessary, but for now the conformances
  // array is supposed to have a 1-to-1 mapping with the protocols array.
  if (auto proto = dyn_cast<ProtocolDecl>(decl)) {
    auto nulls = Context.Allocate<ProtocolConformance *>(allProtocols.size());
    proto->setConformances(nulls);
  }
}

/// Retrieve the set of protocols the given protocol inherits.
static ArrayRef<ProtocolDecl *>
getInheritedForCycleCheck(TypeChecker &tc,
                          ProtocolDecl *proto,
                          ProtocolDecl **scratch) {
  return tc.getDirectConformsTo(proto);
}

/// Retrieve the superclass of the given class.
static ArrayRef<ClassDecl *> getInheritedForCycleCheck(TypeChecker &tc,
                                                       ClassDecl *classDecl,
                                                       ClassDecl **scratch) {
  tc.checkInheritanceClause(classDecl);

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
  tc.checkInheritanceClause(enumDecl);
  
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

/// Set each bound variable in the pattern to have an error type.
static void setBoundVarsTypeError(Pattern *pattern, ASTContext &ctx) {
  pattern->forEachVariable([&](VarDecl *var) {
    // Don't change the type of a variable that we've been able to
    // compute a type for.
    if (var->hasType()) {
      if (var->getType()->is<ErrorType>())
        var->setInvalid();
    } else {
      var->setType(ErrorType::get(ctx));
      var->setInvalid();
    }
  });
}

/// Create a fresh archetype builder.
/// FIXME: Duplicated with TypeCheckGeneric.cpp; this one should go away.
ArchetypeBuilder TypeChecker::createArchetypeBuilder(Module *mod) {
  return ArchetypeBuilder(
           *mod, Diags,
           [=](ProtocolDecl *protocol) -> ArrayRef<ProtocolDecl *> {
             return getDirectConformsTo(protocol);
           },
           [=](AbstractTypeParamDecl *assocType) -> 
                 std::pair<Type, ArrayRef<ProtocolDecl *>> {
             checkInheritanceClause(assocType);
             return std::make_pair(assocType->getSuperclass(),
                                   assocType->getProtocols());
           },
           [=](Module &M, Type T, ProtocolDecl *Protocol)
           -> ProtocolConformance* {
             ProtocolConformance *c;
             if (conformsToProtocol(T, Protocol, &M, &c))
               return c;
             return nullptr;
           });
}

static void revertDependentTypeLoc(TypeLoc &tl) {
  // If there's no type representation, there's nothing to revert.
  if (!tl.getTypeRepr())
    return;

  // Don't revert an error type; we've already complained.
  if (tl.wasValidated() && tl.isError())
    return;

  // Make sure we validate the type again.
  tl.setType(Type(), /*validated=*/false);

  // Walker that reverts dependent identifier types.
  class RevertWalker : public ASTWalker {
  public:
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

      for (auto &comp : identType->getComponentRange()) {
        // If it's not a bound type, we're done.
        if (!comp->isBoundType())
          return true;

        // If the bound type isn't dependent, there's nothing to do.
        auto type = comp->getBoundType();
        if (!type->isDependentType())
          return true;

        // Turn a generic parameter type back into a reference to the
        // generic parameter itself.
        if (auto genericParamType
            = dyn_cast<GenericTypeParamType>(type.getPointer())) {
          assert(genericParamType->getDecl() && "Missing type parameter decl");
          comp->setValue(genericParamType->getDecl());
        } else {
          comp->revert();
        }
      }

      return true;
    }
  };

  if (tl.isNull())
    return;

  tl.getTypeRepr()->walk(RevertWalker());
}

static void revertDependentPattern(Pattern *pattern) {
  // Clear out the pattern's type.
  if (pattern->hasType()) {
    // If the type of the pattern was in error, we're done.
    if (pattern->getType()->is<ErrorType>())
      return;

    pattern->overwriteType(Type());
  }

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
    if (named->getDecl()->hasType() &&
        !named->getDecl()->isInvalid())
      named->getDecl()->overwriteType(Type());
    break;
  }

  case PatternKind::Paren:
    // Recurse into parentheses patterns.
    revertDependentPattern(cast<ParenPattern>(pattern)->getSubPattern());
    break;
      
  case PatternKind::Var:
    // Recurse into var patterns.
    revertDependentPattern(cast<VarPattern>(pattern)->getSubPattern());
    break;

  case PatternKind::Tuple: {
    // Recurse into tuple elements.
    auto tuple = cast<TuplePattern>(pattern);
    for (auto &field : tuple->getFields()) {
      revertDependentPattern(field.getPattern());
    }
    break;
  }

  case PatternKind::Typed: {
    // Revert the type annotation.
    auto typed = cast<TypedPattern>(pattern);
    revertDependentTypeLoc(typed->getTypeLoc());

    // Revert the subpattern.
    revertDependentPattern(typed->getSubPattern());
    break;
  }
  }
}

/// Check the given generic parameter list, introduce the generic parameters
/// and requirements into the archetype builder, but don't assign archetypes
/// yet.
static void checkGenericParamList(ArchetypeBuilder &builder,
                                  GenericParamList *genericParams,
                                  TypeChecker &TC, DeclContext *DC) {
  assert(genericParams && "Missing generic parameters");
  unsigned Depth = genericParams->getDepth();

  // Assign archetypes to each of the generic parameters.
  unsigned Index = 0;
  for (auto GP : *genericParams) {
    auto TypeParam = GP.getAsTypeParam();

    // Set the depth of this type parameter.
    TypeParam->setDepth(Depth);

    // Check the constraints on the type parameter.
    TC.checkInheritanceClause(TypeParam, DC);

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
      if (TC.validateType(Req.getSubjectLoc(), DC)) {
        Req.setInvalid();
        continue;
      }

      if (TC.validateType(Req.getConstraintLoc(), DC)) {
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
      
      break;
    }

    case RequirementKind::SameType:
      if (TC.validateType(Req.getFirstTypeLoc(), DC)) {
        Req.setInvalid();
        continue;
      }

      if (TC.validateType(Req.getSecondTypeLoc(), DC)) {
        Req.setInvalid();
        continue;
      }
      
      break;

    case RequirementKind::WitnessMarker:
      llvm_unreachable("value witness markers in syntactic requirement?");
    }
    
    if (builder.addRequirement(Req))
      Req.setInvalid();
  }
}

/// Revert the dependent types within the given generic parameter list.
void TypeChecker::revertGenericParamList(GenericParamList *genericParams,
                                         DeclContext *dc) {
  // Revert the inherited clause of the generic parameter list.
  for (auto param : *genericParams) {
    auto typeParam = param.getAsTypeParam();

    typeParam->setCheckedInheritanceClause(false);
    for (auto &inherited : typeParam->getInherited())
      revertDependentTypeLoc(inherited);
  }

  // Revert the requirements of the generic parameter list.
  for (auto &req : genericParams->getRequirements()) {
    if (req.isInvalid())
      continue;

    switch (req.getKind()) {
    case RequirementKind::Conformance: {
      revertDependentTypeLoc(req.getSubjectLoc());
      revertDependentTypeLoc(req.getConstraintLoc());
      break;
    }

    case RequirementKind::SameType:
      revertDependentTypeLoc(req.getFirstTypeLoc());
      revertDependentTypeLoc(req.getSecondTypeLoc());
      break;

    case RequirementKind::WitnessMarker:
      llvm_unreachable("value witness markers in syntactic requirement?");
    }
  }
}

/// Finalize the given generic parameter list, assigning archetypes to
/// the generic parameters.
static void finalizeGenericParamList(ArchetypeBuilder &builder,
                                     GenericParamList *genericParams,
                                     DeclContext *dc,
                                     TypeChecker &TC) {
  // Wire up the archetypes.
  builder.assignArchetypes();
  for (auto GP : *genericParams) {
    auto TypeParam = GP.getAsTypeParam();
    TypeParam->setArchetype(builder.getArchetype(TypeParam));

    TC.checkInheritanceClause(TypeParam);
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
      revertDependentTypeLoc(Req.getSubjectLoc());
      if (TC.validateType(Req.getSubjectLoc(), dc)) {
        Req.setInvalid();
        continue;
      }

      revertDependentTypeLoc(Req.getConstraintLoc());
      if (TC.validateType(Req.getConstraintLoc(), dc)) {
        Req.setInvalid();
        continue;
      }
      break;
    }

    case RequirementKind::SameType:
      revertDependentTypeLoc(Req.getFirstTypeLoc());
      if (TC.validateType(Req.getFirstTypeLoc(), dc)) {
        Req.setInvalid();
        continue;
      }

      revertDependentTypeLoc(Req.getSecondTypeLoc());
      if (TC.validateType(Req.getSecondTypeLoc(), dc)) {
        Req.setInvalid();
        continue;
      }
      break;

    case RequirementKind::WitnessMarker:
      llvm_unreachable("value witness markers in syntactic requirement?");
    }
  }
}

/// Expose TypeChecker's handling of GenericParamList to SIL parsing.
bool TypeChecker::handleSILGenericParams(ArchetypeBuilder *builder, TypeLoc &T,
                                         DeclContext *DC) {
  if (T.wasValidated() || !T.getType().isNull())
    return false;

  if (auto fnType = dyn_cast<FunctionTypeRepr>(T.getTypeRepr())) {
    if (auto gp = fnType->getGenericParams()) {
      // What should the DeclContext be for the following callsites?
      checkGenericParamList(*builder, gp, *this, DC);
      finalizeGenericParamList(*builder, gp, DC, *this);
    }
  }
  return false;
}

void TypeChecker::revertGenericFuncSignature(AbstractFunctionDecl *func) {
  // Revert the result type.
  if (auto fn = dyn_cast<FuncDecl>(func)) {
    if (!fn->getBodyResultTypeLoc().isNull()) {
      revertDependentTypeLoc(fn->getBodyResultTypeLoc());
    }
  }

  // Revert the body patterns.
  ArrayRef<Pattern *> bodyPatterns = func->getBodyParamPatterns();
  for (auto bodyPattern : bodyPatterns) {
    revertDependentPattern(bodyPattern);
  }

  // Revert the generic parameter list.
  if (func->getGenericParams())
    revertGenericParamList(func->getGenericParams(), func);

  // Clear out the types.
  if (auto fn = dyn_cast<FuncDecl>(func))
    fn->revertType();
  else
    func->overwriteType(Type());
}

/// Check whether the given type representation will be
/// default-initializable.
static bool isDefaultInitializable(TypeRepr *typeRepr) {
  // Look through most attributes.
  if (auto attributed = dyn_cast<AttributedTypeRepr>(typeRepr)) {
    // Weak ownership implies optionality.
    if (attributed->getAttrs().getOwnership() == Ownership::Weak)
      return true;
    
    return isDefaultInitializable(attributed->getTypeRepr());
  }

  // Look through named types.
  if (auto named = dyn_cast<NamedTypeRepr>(typeRepr))
    return isDefaultInitializable(named->getTypeRepr());
  
  // Optional types are default-initializable.
  if (isa<OptionalTypeRepr>(typeRepr))
    return true;

  // Tuple types are default-initializable if all of their element
  // types are.
  if (auto tuple = dyn_cast<TupleTypeRepr>(typeRepr)) {
    // ... but not variadic ones.
    if (tuple->hasEllipsis())
      return false;

    for (auto elt : tuple->getElements()) {
      if (!isDefaultInitializable(elt))
        return false;
    }

    return true;
  }

  // Not default initializable.
  return false;
}

/// Determine whether the given pattern binding declaration either has
/// or will have a default initializer, without performing any type
/// checking on it.
static bool isDefaultInitializable(PatternBindingDecl *pbd) {
  // If it has an initializer, this is trivially true.
  if (pbd->hasInit())
    return true;

  // A 'let' variable is not default-initializable, because it cannot change.
  bool isLet = false;
  pbd->getPattern()->forEachVariable([&](VarDecl *var) {
      if (var->isLet())
        isLet = true;
    });
  if (isLet)
    return false;

  // If it is an IBOutlet, it is trivially true.
  if (auto var = pbd->getSingleVar()) {
    if (var->getAttrs().hasAttribute<IBOutletAttr>())
      return true;
  }

  // If the pattern is typed with optionals, it is true.
  if (auto typedPattern = dyn_cast<TypedPattern>(pbd->getPattern())) {
    if (auto typeRepr = typedPattern->getTypeLoc().getTypeRepr()) {
      return isDefaultInitializable(typeRepr);
    }
  }

  return false;
}

/// Build a default initializer for the given type.
static Expr *buildDefaultInitializer(TypeChecker &tc, Type type) {
  // Default-initialize optional types and weak values to 'nil'.
  if (type->getAnyOptionalObjectType() || type->is<WeakStorageType>()) {
    auto nilDecl = tc.Context.getNilDecl();
    return new (tc.Context) DeclRefExpr(nilDecl, SourceLoc(), /*implicit=*/true,
                                        /*direct access=*/false,
                                        nilDecl->getType());
  }

  // Build tuple literals for tuple types.
  if (auto tupleType = type->getAs<TupleType>()) {
    SmallVector<Expr *, 2> inits;
    for (const auto &elt : tupleType->getFields()) {
      if (elt.isVararg())
        return nullptr;

      auto eltInit = buildDefaultInitializer(tc, elt.getType());
      if (!eltInit)
        return nullptr;

      inits.push_back(eltInit);
    }

    return TupleExpr::createImplicit(tc.Context, inits, { });
  }

  // We don't default-initialize anything else.
  return nullptr;
}

/// Check whether \c current is a declaration.
static void checkRedeclaration(TypeChecker &tc, ValueDecl *current) {
  // If we've already checked this declaration, don't do it again.
  if (current->alreadyCheckedRedeclaration())
    return;

  // Make sure we don't do this checking again.
  current->setCheckedRedeclaration(true);

  // Ignore invalid declarations.
  if (current->isInvalid())
    return;

  // If this declaration isn't from a source file, don't check it.
  // FIXME: Should restrict this to the source file we care about.
  DeclContext *currentDC = current->getDeclContext();
  SourceFile *currentFile = currentDC->getParentSourceFile();
  if (!currentFile || currentDC->isLocalContext())
    return;

  // Find other potential definitions.
  SmallVector<ValueDecl *, 4> otherDefinitionsVec;
  ArrayRef<ValueDecl *> otherDefinitions;
  if (currentDC->isTypeContext()) {
    // Look within a type context.
    if (auto nominal = currentDC->getDeclaredTypeOfContext()->getAnyNominal()) {
      otherDefinitions = nominal->lookupDirect(current->getBaseName());
    }
  } else {
    // Look within a module context.
    currentDC->getParentModule()->lookupValue({ }, current->getBaseName(),
                                              NLKind::QualifiedLookup,
                                              otherDefinitionsVec);
    otherDefinitions = otherDefinitionsVec;
  }

  // Compare this signature against the signature of other
  // declarations with the same name.
  OverloadSignature currentSig = current->getOverloadSignature();
  Module *currentModule = current->getModuleContext();
  for (auto other : otherDefinitions) {
    // Skip invalid declarations and ourselves.
    if (current == other || other->isInvalid())
      continue;

    // Skip declarations in other modules.
    if (currentModule != other->getModuleContext())
      continue;

    // Don't compare methods vs. non-methods (which only happens with
    // operators).
    if (currentDC->isTypeContext() != other->getDeclContext()->isTypeContext())
      continue;

    // Validate the declaration.
    tc.validateDecl(other);
    if (other->isInvalid())
      continue;

    // If there is a conflict, complain.
    if (conflicting(currentSig, other->getOverloadSignature())) {
      // If the two declarations occur in the same source file, make sure
      // we get the diagnostic ordering to be sensible.
      if (auto otherFile = other->getDeclContext()->getParentSourceFile()) {
        if (currentFile == otherFile &&
            current->getLoc().isValid() &&
            other->getLoc().isValid() &&
            tc.Context.SourceMgr.isBeforeInBuffer(current->getLoc(),
                                                  other->getLoc())) {
          std::swap(current, other);
        }
      }

      tc.diagnose(current, diag::invalid_redecl, current->getFullName());
      tc.diagnose(other, diag::invalid_redecl_prev, other->getFullName());

      current->setInvalid();
      if (current->hasType())
        current->overwriteType(ErrorType::get(tc.Context));
      break;
    }
  }
}

/// Validate the given pattern binding declaration.
static void validatePatternBindingDecl(TypeChecker &tc,
                                       PatternBindingDecl *binding) {
  // If the pattern already has a type, we're done.
  if (binding->getPattern()->hasType())
    return;

  // Validate 'static'/'class' on properties in extensions.
  auto StaticSpelling = binding->getStaticSpelling();
  if (StaticSpelling != StaticSpellingKind::None &&
      binding->getDeclContext()->isExtensionContext()) {
    if (Type T = binding->getDeclContext()->getDeclaredTypeInContext()) {
      if (auto NTD = T->getAnyNominal()) {
        if (isa<ClassDecl>(NTD) || isa<ProtocolDecl>(NTD)) {
          if (StaticSpelling == StaticSpellingKind::KeywordStatic) {
            tc.diagnose(binding, diag::static_var_in_class)
                .fixItReplace(binding->getStaticLoc(), "class");
            tc.diagnose(NTD, diag::extended_type_declared_here);
          }
        } else if (StaticSpelling == StaticSpellingKind::KeywordClass) {
          tc.diagnose(binding, diag::class_var_in_struct)
              .fixItReplace(binding->getStaticLoc(), "static");
          tc.diagnose(NTD, diag::extended_type_declared_here);
        }
      }
    }
  }

  // Check the pattern.
  // If we have an initializer, we can also have unknown types.
  TypeResolutionOptions options;
  if (binding->getInit()) {
    options |= TR_AllowUnspecifiedTypes;
    options |= TR_AllowUnboundGenerics;
  }
  if (tc.typeCheckPattern(binding->getPattern(),
                          binding->getDeclContext(),
                          options)) {
    setBoundVarsTypeError(binding->getPattern(), tc.Context);
    binding->setInvalid();
    binding->getPattern()->setType(ErrorType::get(tc.Context));
    return;
  }

  // If we have a type but no initializer on an @IBOutlet, check
  // whether the type is default-initializable. If so, do it.
  if (binding->getPattern()->hasType() && binding->hasStorage() &&
      !binding->hasInit() &&
      !binding->getPattern()->getType()->is<ErrorType>()) {

    // If we have a type-adjusting attribute, apply it now.
    if (auto var = binding->getSingleVar()) {
      if (var->getAttrs().hasAttribute<IBOutletAttr>())
        tc.checkIBOutlet(var);

      // FIXME: Ugly hack to get the pattern type to reflect the
      // updated variable type. Not cool.
      binding->getPattern()->setType(var->getType());

      if (var->getAttrs().hasOwnership())
        tc.checkOwnershipAttr(var, var->getAttrs().getOwnership());
    }

    // Make sure we don't have a 'let'.
    bool isLet = false;
    binding->getPattern()->forEachVariable([&](VarDecl *var) {
      if (var->isLet())
        isLet = true;
    });

    if (!isLet) {
      auto type = binding->getPattern()->getType();
      if (auto defaultInit = buildDefaultInitializer(tc, type)) {
        binding->setInit(defaultInit, /*checked=*/false);
      }
    }
  }

  // If the pattern didn't get a type, it's because we ran into some
  // unknown types along the way. We'll need to check the initializer.
  if (!binding->getPattern()->hasType()) {
    if (tc.typeCheckBinding(binding)) {
      setBoundVarsTypeError(binding->getPattern(), tc.Context);
      binding->setInvalid();
      binding->getPattern()->setType(ErrorType::get(tc.Context));
      return;
    }
  }

  // If we have any type-adjusting attributes, apply them here.
  if (binding->getPattern()->hasType()) {
    if (auto var = binding->getSingleVar()) {
      if (var->getAttrs().hasAttribute<IBOutletAttr>())
        tc.checkIBOutlet(var);
      if (var->getAttrs().hasOwnership())
        tc.checkOwnershipAttr(var, var->getAttrs().getOwnership());
    }
  }

  // If we're in a generic type context, provide interface types for all of
  // the variables.
  auto dc = binding->getDeclContext();
  if (dc->isGenericContext() && dc->isTypeContext()) {
    binding->getPattern()->forEachVariable([&](VarDecl *var) {
      var->setInterfaceType(
        tc.getInterfaceTypeFromInternalType(dc, var->getType()));
    });
  }

  // For now, we only support 'type' variables in specific contexts.
  if (binding->isStatic()) {
    // Selector for unimplemented_type_var message.
    enum : unsigned {
      Misc,
      GenericTypes,
      Classes,
      Protocols,
    };
      
    auto unimplementedStatic = [&](unsigned diagSel) {
      auto staticLoc = binding->getStaticLoc();
      tc.diagnose(staticLoc, diag::unimplemented_type_var, diagSel)
        .highlight(SourceRange(staticLoc));
    };

    assert(dc->isTypeContext());
    // The parser only accepts 'type' variables in type contexts, so
    // we're either in a nominal type context or an extension.
    NominalTypeDecl *nominal;
    if (auto extension = dyn_cast<ExtensionDecl>(dc)) {
      nominal = extension->getExtendedType()->getAnyNominal();
      assert(nominal);
    } else {
      nominal = cast<NominalTypeDecl>(dc);
    }

    // Type variables in a protocol context are just a kind of
    // requirement we don't know how to work with yet.
    if (isa<ProtocolDecl>(nominal)) {
      unimplementedStatic(Protocols);

    // Non-stored properties are fine in any other context.
    } else if (!binding->hasStorage()) {
      // do nothing

    // Stored type variables in a generic context need to logically
    // occur once per instantiation, which we don't yet handle.
    } else if (dc->isGenericContext()) {
      unimplementedStatic(GenericTypes);

    // Stored type variables in a class context need to be created
    // once per subclass, which we don't yet handle.
    } else if (isa<ClassDecl>(nominal)) {
      unimplementedStatic(Classes);
    }
  }
}

/// \brief Build an implicit 'self' parameter for the specified DeclContext.
static Pattern *buildImplicitSelfParameter(SourceLoc Loc, DeclContext *DC) {
  ASTContext &Ctx = DC->getASTContext();
  auto *SelfDecl = new (Ctx) ParamDecl(/*IsLet*/ true, Loc, Identifier(),
                                       Loc, Ctx.Id_self, Type(), DC);
  SelfDecl->setImplicit();
  Pattern *P = new (Ctx) NamedPattern(SelfDecl, /*Implicit=*/true);
  return new (Ctx) TypedPattern(P, TypeLoc());
}

static Pattern *buildSetterValueArgumentPattern(VarDecl *VD,
                                                VarDecl **ValueDecl,
                                                TypeChecker &TC) {
  auto &Context = VD->getASTContext();
  auto *Arg = new (Context) ParamDecl(/*IsLet*/true,
                                      SourceLoc(), Identifier(),
                                      VD->getLoc(),
                                      Context.getIdentifier("value"),
                                      Type(), VD->getDeclContext());
  *ValueDecl = Arg;
  Arg->setImplicit();

  auto VDTy = TC.getTypeOfRValue(VD, /*want interface type*/false);

  Pattern *ValuePattern
    = new (Context) TypedPattern(new (Context) NamedPattern(Arg),
                                 TypeLoc::withoutLoc(VDTy));
  ValuePattern->setImplicit();
  
  TuplePatternElt ValueElt(ValuePattern);
  Pattern *ValueParamsPattern
    = TuplePattern::create(Context, VD->getLoc(), ValueElt, VD->getLoc());
  ValueParamsPattern->setImplicit();
  return ValueParamsPattern;
}

static FuncDecl *createGetterPrototype(VarDecl *VD, TypeChecker &TC) {
  auto &Context = VD->getASTContext();
  SourceLoc Loc = VD->getLoc();

  // Create the parameter list for the getter.
  Pattern *GetterParams[] = {
    // The implicit 'self' argument.
    buildImplicitSelfParameter(Loc, VD->getDeclContext()),
    
    // Add a no-parameters clause.
    TuplePattern::create(VD->getASTContext(), Loc, ArrayRef<TuplePatternElt>(),
                         Loc, /*hasVararg=*/false, SourceLoc(),
                         /*Implicit=*/true)
  };
  
  SourceLoc StaticLoc = VD->isStatic() ? VD->getLoc() : SourceLoc();

  auto Ty = TC.getTypeOfRValue(VD, /*want interface type*/false);

  auto Get = FuncDecl::create(
      Context, StaticLoc, StaticSpellingKind::None, Loc, Identifier(), Loc,
      /*GenericParams=*/nullptr, Type(), GetterParams,
      TypeLoc::withoutLoc(Ty), VD->getDeclContext());
  Get->setImplicit();
  return Get;
}

static FuncDecl *createSetterPrototype(VarDecl *VD, VarDecl *&ValueDecl,
                                       TypeChecker &TC) {
  auto &Context = VD->getASTContext();
  SourceLoc Loc = VD->getLoc();

  // Create the parameter list for the setter.
  Pattern *SetterParams[] = {
    // The implicit 'self' argument.
    buildImplicitSelfParameter(Loc, VD->getDeclContext()),
    
    // Add a "(value : T)" pattern.
    buildSetterValueArgumentPattern(VD, &ValueDecl, TC)
  };
  Type SetterRetTy = TupleType::getEmpty(Context);
  auto *Set = FuncDecl::create(
      Context, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None, Loc,
      Identifier(), Loc, /*generic=*/nullptr, Type(), SetterParams,
      TypeLoc::withoutLoc(SetterRetTy), VD->getDeclContext());
  Set->setImplicit();
  
  // Setters default to mutating.
  Set->setMutating();
  
  return Set;
}


static void convertStoredVarInProtocolToComputed(VarDecl *VD, TypeChecker &TC) {
  auto *Get = createGetterPrototype(VD, TC);
  
  // Okay, we have both the getter and setter.  Set them in VD.
  VD->makeComputed(VD->getLoc(), Get, nullptr, VD->getLoc());
  
  // We've added some members to our containing class, add them to the members
  // list.
  ProtocolDecl *PD = cast<ProtocolDecl>(VD->getDeclContext());
  PD->addMember(Get);
}


/// Load the value of VD.  If VD is an @override of another value, we call the
/// superclass getter.  Otherwise, we do a direct load of the value.
static Expr *createPropertyLoadOrCallSuperclassGetter(VarDecl *VD,
                                                      VarDecl *SelfDecl) {
  auto &Ctx = VD->getASTContext();
  if (!SelfDecl) {
    // Non-member observing accessors just directly load the variable.
    // Create (return (decl_ref_expr(VD)))
    return new (Ctx) DeclRefExpr(VD, SourceLoc(),/*implicit*/true,
                                 /*direct ivar*/true);
  }
  
  if (!VD->getOverriddenDecl()) {
    // For a non-overridden getter, just access the storage.  Create:
    // (return (member_ref_expr(decl_ref_expr(self), VD)))
    auto *DRE = new (Ctx) DeclRefExpr(SelfDecl, SourceLoc(),/*implicit*/true);
    return new (Ctx) MemberRefExpr(DRE, SourceLoc(), VD, SourceLoc(),
                                   /*implicit*/true,/*direct ivar*/true);
  }

  // For an override, chain up to super.  Create:
  // (unresolved_dot_expr field 'declname'
  //   (super_ref_expr))
  auto *SRE = new (Ctx) SuperRefExpr(SelfDecl, SourceLoc(), /*implicit*/true);
  return new (Ctx) UnresolvedDotExpr(SRE, SourceLoc(), VD->getName(),
                                     SourceLoc(), /*implicit*/true);
}


/// Look up the NSCopying protocol from the Foundation module, if present.
/// Otherwise return null.
static ProtocolDecl *getNSCopyingProtocol(TypeChecker &TC,
                                          DeclContext *DC) {

  // Perform standard value name lookup.
  UnqualifiedLookup Lookup(DeclName(TC.Context.getIdentifier("NSCopying")),
                           DC, &TC, SourceLoc());

  if (!Lookup.isSuccess() || Lookup.Results.size() != 1 ||
      !Lookup.Results[0].hasValueDecl())
    return nullptr;

  return dyn_cast<ProtocolDecl>(Lookup.Results[0].getValueDecl());
}



/// Synthesize the code to store 'Val' to 'VD', given that VD has an @NSCopying
/// attribute on it.  We know that VD is a stored property in a class, so we
/// just need to generate something like "self.property = val.copyWithZone(nil)"
/// here.  This does some type checking to validate that the call will succeed.
static Expr *synthesizeCopyWithZoneCall(Expr *Val, VarDecl *VD,
                                        TypeChecker &TC) {
  auto &Ctx = TC.Context;

  // We support @NSCopying on class types (which conform to NSCopying),
  // protocols which conform, and option types thereof.
  Type UnderlyingType = TC.getTypeOfRValue(VD, /*want interface type*/false);

  bool isOptional = false;
  if (Type optionalEltTy = UnderlyingType->getAnyOptionalObjectType()) {
    UnderlyingType = optionalEltTy;
    isOptional = true;
  }

  // The element type must conform to NSCopying.  If not, emit an error and just
  // recovery by synthesizing without the copy call.
  auto *CopyingProto = getNSCopyingProtocol(TC, VD->getDeclContext());
  if (!CopyingProto || !TC.conformsToProtocol(UnderlyingType, CopyingProto,
                                              VD->getDeclContext())) {
    TC.diagnose(VD->getLoc(), diag::nscopying_doesnt_conform);
    return Val;
  }

  // If we have an optional type, we have to "?" the incoming value to only
  // evaluate the subexpression if the incoming value is non-null.
  if (isOptional)
    Val = new (Ctx) BindOptionalExpr(Val, SourceLoc(), 0);

  // Generate:
  // (force_value_expr type='<null>'
  //   (call_expr type='<null>'
  //     (unresolved_dot_expr type='<null>' field 'copyWithZone'
  //       "Val")
  //     (paren_expr type='<null>'
  //       (unresolved_decl_ref_expr type='<null>' name=nil specialized=no))))
  auto UDE = new (Ctx) UnresolvedDotExpr(Val, SourceLoc(),
                                         Ctx.getIdentifier("copyWithZone"),
                                         SourceLoc(), /*implicit*/true);
  Expr *Nil = new (Ctx) UnresolvedDeclRefExpr(Ctx.getIdentifier("nil"),
                                             DeclRefKind::Ordinary,
                                             SourceLoc());
  Nil = new (Ctx) ParenExpr(SourceLoc(), Nil, SourceLoc(), false);

  //- (id)copyWithZone:(NSZone *)zone;
  Expr *Call = new (Ctx) CallExpr(UDE, Nil, /*implicit*/true);

  TypeLoc ResultTy;
  ResultTy.setType(VD->getType(), true);

  Call = new (Ctx) ConditionalCheckedCastExpr(Call, SourceLoc(),
                                           TypeLoc::withoutLoc(UnderlyingType));
  Call->setImplicit();

  // If we're working with non-optional types, we use ! to force downcast.
  if (!isOptional)
    return new (Ctx) ForceValueExpr(Call, SourceLoc());

  // If we're working with optional types, we use OptionalEvaluationExpr to
  // evaluate the "?".
  return new (Ctx) OptionalEvaluationExpr(Call);
}

/// Store 'Val' to 'VD'.  If VD is an @override of another value, we call the
/// superclass setter.  Otherwise, we do a direct store of the value.
static void createPropertyStoreOrCallSuperclassSetter(Expr *Val, VarDecl *VD,
                                                      VarDecl *SelfDecl,
                                            SmallVectorImpl<ASTNode> &Result,
                                                      TypeChecker &TC) {
  auto &Ctx = VD->getASTContext();

  // If this property is @NSCopying, then we store the result of a copyWithZone
  // call on the value, not the value itself.
  if (VD->getAttrs().hasAttribute<NSCopyingAttr>())
    Val = synthesizeCopyWithZoneCall(Val, VD, TC);

  // Create:
  //   (assign (decl_ref_expr(VD)), decl_ref_expr(value))
  // or:
  //   (assign (member_ref_expr(decl_ref_expr(self), VD)), decl_ref_expr(value))
  Expr *Dest;
  if (!SelfDecl) {
    Dest = new (Ctx) DeclRefExpr(VD, SourceLoc(),/*implicit*/true,
                                 /*direct ivar*/true);
  } else if (VD->getOverriddenDecl() == nullptr) {
    auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, SourceLoc(), /*imp*/true);
    Dest = new (Ctx) MemberRefExpr(SelfDRE, SourceLoc(), VD, SourceLoc(),
                                   /*implicit*/true, /*direct ivar*/true);
  } else {
    auto *SRE = new (Ctx) SuperRefExpr(SelfDecl, SourceLoc(), /*implicit*/true);
    Dest = new (Ctx) UnresolvedDotExpr(SRE, SourceLoc(), VD->getName(),
                                       SourceLoc(), /*implicit*/true);
  }
  Result.push_back(new (Ctx) AssignExpr(Dest, SourceLoc(), Val, true));
}


/// Synthesize the body of a trivial getter.  For a non-member vardecl or one
/// which is not an override of a base class property, it performs a a direct
/// storage load.  For an override of a base member property, it chains up to
/// super.
///
static void synthesizeTrivialGetter(FuncDecl *Get, VarDecl *VD) {
  auto &Ctx = VD->getASTContext();
  VarDecl *SelfDecl = Get->getImplicitSelfDecl();
  
  Expr *GetResult = createPropertyLoadOrCallSuperclassGetter(VD, SelfDecl);
  ASTNode Return = new (Ctx) ReturnStmt(SourceLoc(), GetResult,
                                        /*implicit*/true);

  SourceLoc Loc = VD->getLoc();
  Get->setBody(BraceStmt::create(Ctx, Loc, Return, Loc));

  // Mark it transparent, there is no user benefit to this actually existing, we
  // just want it for abstraction purposes (i.e., to make access to the variable
  // uniform and to be able to put the getter in a vtable).
  Get->getMutableAttrs().setAttr(AK_transparent, Loc);

  // If the var is marked @final, then so is the getter.
  if (VD->isFinal())
    Get->getMutableAttrs().add(new (Ctx) FinalAttr(/*IsImplicit=*/true));
}

/// Given a "Stored" property that needs to be converted to
/// StoredWithTrivialAccessors, create the trivial getter and setter, and switch
/// the storage kind.
static void addAccessorsToStoredVar(VarDecl *VD, TypeChecker &TC) {
  assert(VD->getStorageKind() == VarDecl::Stored && "Isn't a stored vardecl");
  auto &Context = VD->getASTContext();
  SourceLoc Loc = VD->getLoc();
  
  auto *Get = createGetterPrototype(VD, TC);
  synthesizeTrivialGetter(Get, VD);

  FuncDecl *Set = nullptr;
  if (!VD->isLet()) {
    // Okay, the getter is set up, create the setter next.
    VarDecl *ValueDecl = nullptr;

    Set = createSetterPrototype(VD, ValueDecl, TC);

    VarDecl *SelfDecl = Set->getImplicitSelfDecl();

    auto *ValueDRE = new (Context) DeclRefExpr(ValueDecl, SourceLoc(), true);
    SmallVector<ASTNode, 1> SetterBody;
    createPropertyStoreOrCallSuperclassSetter(ValueDRE, VD, SelfDecl,
                                              SetterBody, TC);
    Set->setBody(BraceStmt::create(Context, Loc, SetterBody, Loc));

    // Mark it transparent, there is no user benefit to this actually existing.
    Set->getMutableAttrs().setAttr(AK_transparent, Loc);

    if (VD->isFinal())
      Set->getMutableAttrs().add(new (Context) FinalAttr(/*IsImplicit=*/true));
  }
  
  // Okay, we have both the getter and setter.  Set them in VD.
  VD->makeStoredWithTrivialAccessors(Get, Set);
  
  // We've added some members to our containing type, add them to the
  // members list.
  if (auto ext = dyn_cast<ExtensionDecl>(VD->getDeclContext())) {
    ext->addMember(Get);
    if (Set) ext->addMember(Set);
    return;
  }

  auto nominal = cast<NominalTypeDecl>(VD->getDeclContext());
  nominal->addMember(Get);
  if (Set) nominal->addMember(Set);
}



/// The specified VarDecl with "Stored" StorageKind was just found to satisfy
/// a protocol property requirement.  Convert it to
/// "StoredWithTrivialAccessors" storage by sythesizing accessors for the
/// variable, enabling the witness table to use those accessors.
void TypeChecker::synthesizeWitnessAccessorsForStoredVar(VarDecl *VD) {
  addAccessorsToStoredVar(VD, *this);

  // Type check the body of the getter and setter.
  validateDecl(VD->getGetter(), true);
  definedFunctions.push_back(VD->getGetter());

  if (auto *setter = VD->getSetter()) {
    validateDecl(setter, true);
    definedFunctions.push_back(setter);
  }
}


/// Given a VarDecl with a willSet: and/or didSet: specifier, synthesize the
/// (trivial) getter and the setter, which calls these.
static void synthesizeObservingAccessors(VarDecl *VD, TypeChecker &TC) {
  assert(VD->getStorageKind() == VarDecl::Observing);
  assert(VD->getGetter() && VD->getSetter() &&
         !VD->getGetter()->hasBody() && !VD->getSetter()->hasBody() &&
         "willSet/didSet var already has a getter or setter");
  
  auto &Ctx = VD->getASTContext();
  SourceLoc Loc = VD->getLoc();
  
  // The getter is always trivial: just perform a (direct!) load of storage, or
  // a call of a superclass getter if this is an override.
  auto *Get = VD->getGetter();
  synthesizeTrivialGetter(Get, VD);

  // Okay, the getter is done, create the setter now.  Start by finding the
  // decls for 'self' and 'value'.
  auto *Set = VD->getSetter();
  auto *SelfDecl = Set->getImplicitSelfDecl();
  VarDecl *ValueDecl = nullptr;
  Set->getBodyParamPatterns().back()->forEachVariable([&](VarDecl *VD) {
    assert(!ValueDecl && "Already found 'value'?");
    ValueDecl = VD;
  });

  // The setter loads the oldValue, invokes willSet with the incoming value,
  // does a direct store, then invokes didSet with the oldValue.
  SmallVector<ASTNode, 6> SetterBody;

  // If there is a didSet, it will take the old value.  Load it into a temporary
  // 'let' so we have it for later.
  // TODO: check the body of didSet to only do this load (which may call the
  // superclass getter) if didSet takes an argument.
  VarDecl *OldValue = nullptr;
  if (VD->getDidSetFunc()) {
    Expr *OldValueExpr
      = createPropertyLoadOrCallSuperclassGetter(VD, SelfDecl);
    
    OldValue = new (Ctx) ParamDecl(/*isLet*/ true,
                                   SourceLoc(), Identifier(),
                                   SourceLoc(), Ctx.getIdentifier("tmp"),
                                   Type(), Set);
    OldValue->setImplicit();
    auto *tmpPattern = new (Ctx) NamedPattern(OldValue, /*implicit*/ true);
    auto tmpPBD = new (Ctx) PatternBindingDecl(SourceLoc(),
                                               StaticSpellingKind::None,
                                               SourceLoc(),
                                               tmpPattern, OldValueExpr,
                                               /*conditional*/ false, Set);
    tmpPBD->setImplicit();
    SetterBody.push_back(tmpPBD);
    SetterBody.push_back(OldValue);
  }
  
  // Create:
  //   (call_expr (dot_syntax_call_expr (decl_ref_expr(willSet)),
  //                                    (decl_ref_expr(self))),
  //              (declrefexpr(value)))
  // or:
  //   (call_expr (decl_ref_expr(willSet)), (declrefexpr(value)))
  if (auto willSet = VD->getWillSetFunc()) {
    Expr *Callee = new (Ctx) DeclRefExpr(willSet, SourceLoc(), /*imp*/true);
    auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, SourceLoc(), /*imp*/true);
    if (SelfDecl) {
      auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, SourceLoc(), /*imp*/true);
      Callee = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);
    }
    SetterBody.push_back(new (Ctx) CallExpr(Callee, ValueDRE, true));

    // Make sure the didSet/willSet accessors are marked @final.
    if (!willSet->isFinal() && willSet->getExtensionType() &&
        willSet->getExtensionType()->getClassOrBoundGenericClass())
      willSet->getMutableAttrs().add(new (Ctx) FinalAttr(/*IsImplicit=*/true));
  }
  
  // Create an assignment into the storage or call to superclass setter.
  auto *ValueDRE = new (Ctx) DeclRefExpr(ValueDecl, SourceLoc(), true);
  createPropertyStoreOrCallSuperclassSetter(ValueDRE, VD, SelfDecl, SetterBody,
                                            TC);

  // Create:
  //   (call_expr (dot_syntax_call_expr (decl_ref_expr(didSet)),
  //                                    (decl_ref_expr(self))),
  //              (decl_ref_expr(tmp)))
  // or:
  //   (call_expr (decl_ref_expr(didSet)), (decl_ref_expr(tmp)))
  if (auto didSet = VD->getDidSetFunc()) {
    auto *OldValueExpr = new (Ctx) DeclRefExpr(OldValue, SourceLoc(),
                                               /*impl*/true);
    Expr *Callee = new (Ctx) DeclRefExpr(didSet, SourceLoc(), /*imp*/true);
    if (SelfDecl) {
      auto *SelfDRE = new (Ctx) DeclRefExpr(SelfDecl, SourceLoc(), /*imp*/true);
      Callee = new (Ctx) DotSyntaxCallExpr(Callee, SourceLoc(), SelfDRE);
    }
    SetterBody.push_back(new (Ctx) CallExpr(Callee, OldValueExpr, true));

    // Make sure the didSet/willSet accessors are marked @final.
    if (!didSet->isFinal() && didSet->getExtensionType() &&
        didSet->getExtensionType()->getClassOrBoundGenericClass())
      didSet->getMutableAttrs().add(new (Ctx) FinalAttr(/*IsImplicit=*/true));
  }

  Set->setBody(BraceStmt::create(Ctx, Loc, SetterBody, Loc));
}

namespace {

/// The kind of designated initializer to synthesize.
enum class designatedInitKind {
  /// A stub initializer, which is not visible to name lookup and
  /// merely aborts at runtime.
  Stub,

  /// An initializer that simply chains to the corresponding
  /// superclass initializer.
  Chaining
};

}

/// Create a new initializer that overrides the given designated
/// initializer.
///
/// \param classDecl The subclass in which the new initializer will
/// be declared.
///
/// \param superclassCtor The superclass initializer for which this
/// routine will create an override.
///
/// \param kind The kind of initializer to synthesize.
///
/// \returns the newly-created initializer that overrides \p
/// superclassCtor.
static ConstructorDecl *
createdesignatedInitOverride(TypeChecker &tc,
                            ClassDecl *classDecl,
                            ConstructorDecl *superclassCtor,
                            designatedInitKind kind);

/// Configure the implicit 'self' parameter of a function, setting its type,
/// pattern, etc.
///
/// \param func The function whose 'self' is being configured.
/// \param outerGenericParams The generic parameters from the outer scope.
///
/// \returns the type of 'self'.
static Type configureImplicitSelf(AbstractFunctionDecl *func,
                                  GenericParamList *&outerGenericParams) {
  outerGenericParams = nullptr;

  auto selfDecl = func->getImplicitSelfDecl();

  // Compute the type of self.
  Type selfTy = func->computeSelfType(&outerGenericParams);
  assert(selfDecl && selfTy && "Not a method");

  // 'self' is 'let' for reference types (i.e., classes) or when 'self' is
  // neither inout.
  selfDecl->setLet(!selfTy->is<InOutType>());
  selfDecl->setType(selfTy);

  auto bodyPattern = cast<TypedPattern>(func->getBodyParamPatterns()[0]);
  if (!bodyPattern->getTypeLoc().getTypeRepr())
    bodyPattern->getTypeLoc() = TypeLoc::withoutLoc(selfTy);

  return selfTy;
}

/// Compute the allocating and initializing constructor types for
/// the given constructor.
static void configureConstructorType(ConstructorDecl *ctor,
                                     GenericParamList *outerGenericParams,
                                     Type selfType,
                                     Type argType) {
  Type fnType;
  Type allocFnType;
  Type initFnType;
  Type resultType = selfType->getInOutObjectType();

  // Use the argument names in the argument type.
  argType = argType->getRelabeledType(ctor->getASTContext(), 
                                      ctor->getFullName().getArgumentNames());

  if (GenericParamList *innerGenericParams = ctor->getGenericParams()) {
    innerGenericParams->setOuterParameters(outerGenericParams);
    fnType = PolymorphicFunctionType::get(argType, resultType,
                                          innerGenericParams);
  } else {
    fnType = FunctionType::get(argType, resultType);
  }
  Type selfMetaType = MetatypeType::get(resultType);
  if (outerGenericParams) {
    allocFnType = PolymorphicFunctionType::get(selfMetaType, fnType,
                                               outerGenericParams);
    initFnType = PolymorphicFunctionType::get(selfType, fnType,
                                              outerGenericParams);
  } else {
    allocFnType = FunctionType::get(selfMetaType, fnType);
    initFnType = FunctionType::get(selfType, fnType);
  }
  ctor->setType(allocFnType);
  ctor->setInitializerType(initFnType);
}

namespace {

class DeclChecker : public DeclVisitor<DeclChecker> {
public:
  TypeChecker &TC;

  // For library-style parsing, we need to make two passes over the global
  // scope.  These booleans indicate whether this is currently the first or
  // second pass over the global scope (or neither, if we're in a context where
  // we only visit each decl once).
  unsigned IsFirstPass : 1;
  unsigned IsSecondPass : 1;

  DeclChecker(TypeChecker &TC, bool IsFirstPass, bool IsSecondPass)
      : TC(TC), IsFirstPass(IsFirstPass), IsSecondPass(IsSecondPass) {}

  void visit(Decl *decl) {
    DeclVisitor<DeclChecker>::visit(decl);

    if (auto valueDecl = dyn_cast<ValueDecl>(decl)) {
      checkRedeclaration(TC, valueDecl);
    }
  }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//

  template<typename DeclType>
  void checkExplicitConformance(DeclType *D, Type T) {
    SmallVector<ProtocolConformance *, 4> conformances;
    // Don't force delayed protocols to be created if they haven't already been
    // resolved.
    for (auto proto : D->getProtocols(false)) {
      ProtocolConformance *conformance = nullptr;
      // FIXME: Better location info
      if (TC.conformsToProtocol(T, proto, D, &conformance,
                                D->getStartLoc(), D)) {
        // For nominal types and extensions thereof, record conformance
        // to known protocols.
        if (auto kind = proto->getKnownProtocolKind())
            TC.Context.recordConformance(kind.getValue(), D);
      }
      conformances.push_back(conformance);
    }

    D->setConformances(D->getASTContext().AllocateCopy(conformances));
  }
  
  /// Check runtime functions responsible for implicit bridging of Objective-C
  /// types.
  void checkObjCBridgingFunctions(Module *mod,
                              StringRef bridgedTypeName,
                              StringRef forwardConversion,
                              StringRef reverseConversion) {
    assert(mod);
    Module::AccessPathTy unscopedAccess = {};
    SmallVector<ValueDecl *, 4> results;
    
    mod->lookupValue(unscopedAccess, mod->Ctx.getIdentifier(bridgedTypeName),
                     NLKind::QualifiedLookup, results);
    mod->lookupValue(unscopedAccess, mod->Ctx.getIdentifier(forwardConversion),
                     NLKind::QualifiedLookup, results);
    mod->lookupValue(unscopedAccess, mod->Ctx.getIdentifier(reverseConversion),
                     NLKind::QualifiedLookup, results);
    
    for (auto D : results)
      TC.validateDecl(D);
  }
  
  void checkBridgedFunctions() {
    if (TC.HasCheckedBridgeFunctions)
      return;
    
    TC.HasCheckedBridgeFunctions = true;
    
    #define BRIDGE_TYPE(BRIDGED_MOD, BRIDGED_TYPE, _, NATIVE_TYPE, OPT) \
    if (Module *module = TC.Context.LoadedModules.lookup(#BRIDGED_MOD)) {\
      checkObjCBridgingFunctions(module, #BRIDGED_TYPE, \
      "_convert" #BRIDGED_TYPE "To" #NATIVE_TYPE, \
      "_convert" #NATIVE_TYPE "To" #BRIDGED_TYPE); \
    }
    #include "swift/SIL/BridgedTypes.def"
    
    if (Module *module = TC.Context.LoadedModules.lookup("Swift")) {
      checkObjCBridgingFunctions(module, "ObjCMutablePointer",
                                 "_convertUnsafePointerToObjCMutablePointer",
                                 "_convertObjCMutablePointerToUnsafePointer");
      checkObjCBridgingFunctions(module, "CMutablePointer",
                                 "_convertUnsafePointerToCMutablePointer",
                                 "_convertCMutablePointerToUnsafePointer");
      checkObjCBridgingFunctions(module, "CConstPointer",
                                 "_convertUnsafePointerToCConstPointer",
                                 "_convertCConstPointerToUnsafePointer");
    }
  }
  
  void markAsObjC(ValueDecl *D, bool isObjC) {
    D->setIsObjC(isObjC);
    
    if (isObjC)
      checkBridgedFunctions();
  }

  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//
  
  void visitImportDecl(ImportDecl *ID) {
    TC.checkDeclAttributesEarly(ID);
    TC.checkDeclAttributes(ID);
  }

  void visitBoundVariable(VarDecl *VD) {
    if (!VD->getType()->isMaterializable()) {
      TC.diagnose(VD->getStartLoc(), diag::var_type_not_materializable,
                  VD->getType());
      VD->overwriteType(ErrorType::get(TC.Context));
      VD->setInvalid();
    }

    validateAttributes(TC, VD);
    TC.checkDeclAttributesEarly(VD);

    // The instance var requires ObjC interop if it has an @objc or @iboutlet
    // attribute or if it's a member of an ObjC class or protocol.
    Type ContextTy = VD->getDeclContext()->getDeclaredTypeInContext();
    if (ContextTy && !VD->isStatic()) {
      ClassDecl *classContext = ContextTy->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext =
          dyn_cast<ProtocolDecl>(VD->getDeclContext());
      bool isMemberOfObjCProtocol =
          protocolContext && protocolContext->isObjC();
      ObjCReason reason = ObjCReason::DontDiagnose;
      if (VD->getAttrs().hasAttribute<ObjCAttr>())
        reason = ObjCReason::ExplicitlyObjC;
      else if (VD->getAttrs().hasAttribute<IBOutletAttr>())
        reason = ObjCReason::ExplicitlyIBOutlet;
      else if (VD->getAttrs().hasAttribute<NSManagedAttr>())
        reason = ObjCReason::ExplicitlyNSManaged;
      else if (isMemberOfObjCProtocol)
        reason = ObjCReason::MemberOfObjCProtocol;

      bool isObjC = (reason != ObjCReason::DontDiagnose) ||
                    (classContext && classContext->isObjC());
      if (isObjC && !TC.isRepresentableInObjC(VD, reason))
        isObjC = false;
      
      markAsObjC(VD, isObjC);
    }

    if (IsFirstPass && !checkOverrides(VD)) {
      // If a property has an override attribute but does not override
      // anything, complain.
      if (auto *OA = VD->getMutableAttrs().getAttribute<OverrideAttr>()) {
        if (!VD->getOverriddenDecl()) {
          TC.diagnose(VD, diag::property_does_not_override)
              .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    // In a protocol context, variables written as "var x : Int" are really
    // computed properties with just a getter.  Create the getter decl now.
    if (isa<ProtocolDecl>(VD->getDeclContext()) &&
        VD->getStorageKind() == VarDecl::Stored &&
        !VD->isLet()) {
      TC.diagnose(VD->getLoc(), diag::protocol_property_must_be_computed);
      
      convertStoredVarInProtocolToComputed(VD, TC);

      // Type check the getter declaration.
      TC.typeCheckDecl(VD->getGetter(), true);
      TC.typeCheckDecl(VD->getGetter(), false);
    }

    // Is this a class member?
    bool isClassMember = false;
    if (auto ctx = VD->getDeclContext()->getDeclaredTypeOfContext())
      isClassMember = ctx->getClassOrBoundGenericClass();

    // Checking for @NSManaged.
    if (auto attr = VD->getMutableAttrs().getAttribute<NSManagedAttr>()) {
      if (VD->isStatic() || !isClassMember) {
        TC.diagnose(attr->getLocation(), diag::attr_NSManaged_not_property)
          .fixItRemove(attr->getRange());
        attr->setInvalid();
      } else {
        // @NSManaged properties must be written as stored.
        switch (VD->getStorageKind()) {
        case AbstractStorageDecl::Stored:
          // FIXME: Convert to computed.
          break;

        case AbstractStorageDecl::StoredWithTrivialAccessors:
          llvm_unreachable("Already created accessors?");

        case AbstractStorageDecl::Computed:
        case AbstractStorageDecl::Observing:
          TC.diagnose(attr->getLocation(), diag::attr_NSManaged_not_stored,
                      VD->getStorageKind() == AbstractStorageDecl::Observing);
          attr->setInvalid();
          break;
        }
      }
    }

    // If this is a non-final stored property in a class, then synthesize getter
    // and setter accessors and change its storage kind.  This allows it to be
    // overriden and provide objc entrypoints if needed.
    if (VD->getStorageKind() == VarDecl::Stored && !VD->isStatic()) {
      // Variables in SIL mode don't get auto-synthesized getters.
      bool isInSILMode = false;
      if (auto sourceFile = VD->getDeclContext()->getParentSourceFile())
        isInSILMode = sourceFile->Kind == SourceFileKind::SIL;

      if (isClassMember && !isInSILMode) {
        addAccessorsToStoredVar(VD, TC);

        // Type check the body of the getter and setter.
        TC.typeCheckDecl(VD->getGetter(), true);
        TC.typeCheckDecl(VD->getGetter(), false);
        if (VD->getSetter()) {
          TC.typeCheckDecl(VD->getSetter(), true);
          TC.typeCheckDecl(VD->getSetter(), false);
        }
      }
    }

    // If this is a willSet/didSet property, synthesize the getter and setter
    // decl.
    if (VD->getStorageKind() == VarDecl::Observing &&
        !VD->getGetter()->getBody()) {
      synthesizeObservingAccessors(VD, TC);

      // Type check the body of the getter and setter.
      TC.typeCheckDecl(VD->getGetter(), true);
      TC.typeCheckDecl(VD->getSetter(), true);
    }

    // If this variable is marked @final and has a getter or setter, mark the
    // getter and setter as final as well.
    if (VD->isFinal()) {
      if (VD->getGetter() && !VD->getGetter()->isFinal())
        VD->getGetter()->getMutableAttrs().add(
            new (TC.Context) FinalAttr(/*IsImplicit=*/true));
      if (VD->getSetter() && !VD->getSetter()->isFinal())
        VD->getSetter()->getMutableAttrs().add(
            new (TC.Context) FinalAttr(/*IsImplicit=*/true));
    }
    TC.checkDeclAttributes(VD);
  }


  void visitBoundVars(Pattern *P) {
    P->forEachVariable([&] (VarDecl *VD) { this->visitBoundVariable(VD); });
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    validatePatternBindingDecl(TC, PBD);
    if (PBD->isInvalid())
      return;

    if (!IsFirstPass) {
      if (PBD->getInit() && !PBD->wasInitChecked()) {
        if (TC.typeCheckBinding(PBD)) {
          PBD->setInvalid();
          if (!PBD->getPattern()->hasType()) {
            PBD->getPattern()->setType(ErrorType::get(TC.Context));
            setBoundVarsTypeError(PBD->getPattern(), TC.Context);
            return;
          }
        }
      }
    }

    TC.checkDeclAttributesEarly(PBD);

    bool isInSILMode = false;
    if (auto sourceFile = PBD->getDeclContext()->getParentSourceFile())
      isInSILMode = sourceFile->Kind == SourceFileKind::SIL;

    // If this is a declaration with an initializer, reject code if
    // uninitialized vars are not allowed.
    if (!PBD->hasInit() && !isInSILMode) {
      PBD->getPattern()->forEachVariable([&](VarDecl *var) {
        // If the variable has no storage, it never needs an initializer.
        if (!var->hasStorage())
          return;

        auto *varDC = var->getDeclContext();

        // Let declarations require an initializer, unless they are a property
        // (in which case they get set during the init method of the enclosing
        // type).
        if (var->isLet() && !varDC->isTypeContext()) {
          TC.diagnose(var->getLoc(), diag::let_requires_initializer);
          PBD->setInvalid();
          var->setInvalid();
          var->overwriteType(ErrorType::get(TC.Context));
          return;
        }
        
        // Non-member observing properties need an initializer.
        if (var->getStorageKind() == VarDecl::Observing &&
            !var->getDeclContext()->isTypeContext()) {
          TC.diagnose(var->getLoc(), diag::observingprop_requires_initializer);
          PBD->setInvalid();
          var->setInvalid();
          var->overwriteType(ErrorType::get(TC.Context));
          return;
        }

        // Static/class declarations require an initializer unless in a
        // protocol.
        if (var->isStatic() && !isa<ProtocolDecl>(varDC)) {
          TC.diagnose(var->getLoc(), diag::static_requires_initializer,
                      var->getCorrectStaticSpelling());
          PBD->setInvalid();
          var->setInvalid();
          var->overwriteType(ErrorType::get(TC.Context));
          return;
        }

        // Global variables require an initializer (except in top level code).
        if (varDC->isModuleScopeContext() &&
            !varDC->getParentSourceFile()->isScriptMode()) {
          TC.diagnose(var->getLoc(), diag::global_requires_initializer);
          PBD->setInvalid();
          var->setInvalid();
          var->overwriteType(ErrorType::get(TC.Context));
          return;
        }
      });
    }

    if (!IsSecondPass)
      visitBoundVars(PBD->getPattern());
    TC.checkDeclAttributes(PBD);
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    if (IsSecondPass || SD->hasType())
      return;

    assert(SD->getDeclContext()->isTypeContext() &&
           "Decl parsing must prevent subscripts outside of types!");

    TC.checkDeclAttributesEarly(SD);

    auto dc = SD->getDeclContext();
    bool isInvalid = TC.validateType(SD->getElementTypeLoc(), dc);
    isInvalid |= TC.typeCheckPattern(SD->getIndices(), dc, None);

    if (isInvalid) {
      SD->overwriteType(ErrorType::get(TC.Context));
      SD->setInvalid();
    } else {
      SD->setType(FunctionType::get(SD->getIndices()->getType(),
                                    SD->getElementType()));

      // If we're in a generic context, set the interface type.
      if (dc->isGenericContext()) {
        auto indicesTy = TC.getInterfaceTypeFromInternalType(
                           dc, SD->getIndices()->getType());
        auto elementTy = TC.getInterfaceTypeFromInternalType(
                           dc, SD->getElementType());
        SD->setInterfaceType(FunctionType::get(indicesTy, elementTy));
      }
    }

    validateAttributes(TC, SD);

    // If this variable is marked @final and has a getter or setter, mark the
    // getter and setter as final as well.
    if (SD->isFinal()) {
      if (SD->getGetter() && !SD->getGetter()->isFinal())
        SD->getGetter()->getMutableAttrs().add(
            new (TC.Context) FinalAttr(/*IsImplicit=*/true));
      if (SD->getSetter() && !SD->getSetter()->isFinal())
        SD->getSetter()->getMutableAttrs().add(
            new (TC.Context) FinalAttr(/*IsImplicit=*/true));
    }

    // A subscript is ObjC-compatible if it's explicitly @objc, or a
    // member of an ObjC-compatible class or protocol.
    if (dc->getDeclaredTypeInContext()) {
      ClassDecl *classContext = dc->getDeclaredTypeInContext()
        ->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext = dyn_cast<ProtocolDecl>(dc);
      bool isMemberOfObjCProtocol =
          protocolContext && protocolContext->isObjC();
      ObjCReason reason = ObjCReason::DontDiagnose;
      if (SD->getAttrs().hasAttribute<ObjCAttr>())
        reason = ObjCReason::ExplicitlyObjC;
      else if (isMemberOfObjCProtocol)
        reason = ObjCReason::MemberOfObjCProtocol;
      bool isObjC = (reason != ObjCReason::DontDiagnose) ||
                    (classContext && classContext->isObjC());
      if (isObjC && !TC.isRepresentableInObjC(SD, reason))
        isObjC = false;
      
      markAsObjC(SD, isObjC);
    }

    // Make sure the getter and setter have valid types, since they will be
    // used by SILGen for any accesses to this subscript.
    if (auto getter = SD->getGetter())
      TC.validateDecl(getter);
    if (auto setter = SD->getSetter())
      TC.validateDecl(setter);

    if (!checkOverrides(SD)) {
      // If a subscript has an override attribute but does not override
      // anything, complain.
      if (auto *OA = SD->getMutableAttrs().getAttribute<OverrideAttr>()) {
        if (!SD->getOverriddenDecl()) {
          TC.diagnose(SD, diag::subscript_does_not_override)
              .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }

    TC.checkDeclAttributes(SD);
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    TC.checkDeclAttributesEarly(TAD);
    if (!IsSecondPass) {
      if (TC.validateType(TAD->getUnderlyingTypeLoc(), TAD->getDeclContext())) {
        TAD->setInvalid();
        TAD->overwriteType(ErrorType::get(TC.Context));
        TAD->getUnderlyingTypeLoc().setType(ErrorType::get(TC.Context));
      } else if (TAD->getDeclContext()->isGenericContext()) {
        TAD->setInterfaceType(
          TC.getInterfaceTypeFromInternalType(TAD->getDeclContext(),
                                              TAD->getType()));
      }

      // We create TypeAliasTypes with invalid underlying types, so we
      // need to propagate recursive properties now.
      if (TAD->hasUnderlyingType())
        TAD->getAliasType()->setRecursiveProperties(
                         TAD->getUnderlyingType()->getRecursiveProperties());

      if (!isa<ProtocolDecl>(TAD->getDeclContext()))
        TC.checkInheritanceClause(TAD);
    }
    TC.checkDeclAttributes(TAD);
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *assocType) {
    TC.checkDeclAttributesEarly(assocType);
    // Check the default definition, if there is one.
    TypeLoc &defaultDefinition = assocType->getDefaultDefinitionLoc();
    if (!defaultDefinition.isNull() &&
        TC.validateType(defaultDefinition, assocType->getDeclContext())) {
      defaultDefinition.setInvalidType(TC.Context);
    }
    TC.checkDeclAttributes(assocType);
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
      ProtocolDecl *ilcProto =
        TC.getProtocol(forElt->getLoc(),
                       KnownProtocolKind::IntegerLiteralConvertible);
      if (!TC.conformsToProtocol(rawTy, ilcProto, forElt->getDeclContext())) {
        TC.diagnose(forElt->getLoc(),
                    diag::enum_non_integer_convertible_raw_type_no_value);
        return nullptr;
      }
      
      return new (TC.Context) IntegerLiteralExpr("0", SourceLoc(),
                                                 /*Implicit=*/true);
    }
    
    if (auto intLit = dyn_cast<IntegerLiteralExpr>(prevValue)) {
      APInt nextVal = intLit->getValue() + 1;
      bool negative = nextVal.slt(0);
      if (negative)
        nextVal = -nextVal;
      
      llvm::SmallString<10> nextValStr;
      nextVal.toStringSigned(nextValStr);
      auto expr = new (TC.Context)
        IntegerLiteralExpr(TC.Context.AllocateCopy(StringRef(nextValStr)),
                           SourceLoc(), /*Implicit=*/true);
      if (negative)
        expr->setNegative(SourceLoc());
      
      return expr;
    }
    
    TC.diagnose(forElt->getLoc(),
                diag::enum_non_integer_raw_value_auto_increment);
    return nullptr;
  }
  
  void visitEnumDecl(EnumDecl *ED) {
    // This enum declaration is technically a parse error, so do not type
    // check.
    if (isa<ProtocolDecl>(ED->getParent()))
      return;

    TC.checkDeclAttributesEarly(ED);

    if (!IsSecondPass) {
      TC.validateDecl(ED);

      TC.ValidatedTypes.remove(ED);

      {
        // Check for circular inheritance of the raw type.
        SmallVector<EnumDecl *, 8> path;
        checkCircularity(TC, ED, diag::circular_enum_inheritance,
                         diag::enum_here, path);
      }
      {
        // Check for duplicate enum members.
        llvm::DenseMap<Identifier, EnumElementDecl *> Elements;
        for (auto *EED : ED->getAllElements()) {
          auto Res = Elements.insert({ EED->getName(), EED });
          if (!Res.second) {
            EED->overwriteType(ErrorType::get(TC.Context));
            EED->setInvalid();
            if (auto *RawValueExpr = EED->getRawValueExpr())
              RawValueExpr->setType(ErrorType::get(TC.Context));

            auto PreviousEED = Res.first->second;
            TC.diagnose(EED->getLoc(), diag::duplicate_enum_element);
            TC.diagnose(PreviousEED->getLoc(),
                        diag::previous_decldef, true, EED->getName());
          }
        }
      }
    }

    Type rawTy;
    if (!IsFirstPass) {
      if (ED->hasRawType()) {
        rawTy = ArchetypeBuilder::mapTypeIntoContext(ED, ED->getRawType());

        // Check that the raw type is convertible from one of the primitive
        // literal protocols.
        bool literalConvertible = false;
        for (auto literalProtoKind : {
                 KnownProtocolKind::CharacterLiteralConvertible,
                 KnownProtocolKind::ExtendedGraphemeClusterLiteralConvertible,
                 KnownProtocolKind::FloatLiteralConvertible,
                 KnownProtocolKind::IntegerLiteralConvertible,
                 KnownProtocolKind::StringLiteralConvertible})
        {
          ProtocolDecl *literalProto =
            TC.getProtocol(ED->getLoc(), literalProtoKind);
          if (TC.conformsToProtocol(rawTy, literalProto, ED->getDeclContext())){
            literalConvertible = true;
            break;
          }
        }
        
        if (!literalConvertible) {
          TC.diagnose(ED->getInherited()[0].getSourceRange().Start,
                      diag::raw_type_not_literal_convertible,
                      rawTy);
          ED->getInherited()[0].setInvalidType(TC.Context);
        }
        
        // We need at least one case to have a raw value.
        if (ED->getAllElements().empty())
          TC.diagnose(ED->getInherited()[0].getSourceRange().Start,
                      diag::empty_enum_raw_type);
      }

      checkExplicitConformance(ED, ED->getDeclaredTypeInContext());
    }

    for (Decl *member : ED->getMembers())
      visit(member);
    for (Decl *global : ED->getDerivedGlobalDecls())
      visit(global);

    if (!IsFirstPass) {
      if (rawTy) {
        // Check the raw values of the cases.
        LiteralExpr *prevValue = nullptr;
        EnumElementDecl *lastExplicitValueElt = nullptr;
        // Keep a map we can use to check for duplicate case values.
        llvm::DenseMap<RawValueKey, RawValueSource> uniqueRawValues;

        auto rawTy = ArchetypeBuilder::mapTypeIntoContext(ED, ED->getRawType());

        for (auto elt : ED->getAllElements()) {
          if (elt->isInvalid())
            continue;

          // We don't yet support raw values on payload cases.
          if (elt->hasArgumentType()) {
            TC.diagnose(elt->getLoc(),
                        diag::enum_with_raw_type_case_with_argument);
            TC.diagnose(ED->getInherited()[0].getSourceRange().Start,
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
            if (!TC.typeCheckExpression(typeChecked, ED, rawTy, false))
              elt->setTypeCheckedRawValueExpr(typeChecked);
          } else {
            lastExplicitValueElt = elt;
          }
          prevValue = elt->getRawValueExpr();
          assert(prevValue &&
                 "continued without setting raw value of enum case");
          
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
                TC.diagnose(ED->getAllElements().front()->getLoc(),
                            diag::enum_raw_value_incrementing_from_zero);
            }
          } else {
            uniqueRawValues.insert({RawValueKey(elt->getRawValueExpr()),
                                    RawValueSource{elt, lastExplicitValueElt}});
          }
        }
      }
    }
    TC.checkDeclAttributes(ED);
  }

  void visitStructDecl(StructDecl *SD) {
    // This struct declaration is technically a parse error, so do not type
    // check.
    if (isa<ProtocolDecl>(SD->getParent()))
      return;

    TC.checkDeclAttributesEarly(SD);

    if (!IsSecondPass) {
      TC.validateDecl(SD);
      TC.ValidatedTypes.remove(SD);
    }

    if (!IsSecondPass)
      TC.addImplicitConstructors(SD);

    // Visit each of the members.
    for (Decl *Member : SD->getMembers())
      visit(Member);
    for (Decl *global : SD->getDerivedGlobalDecls())
      visit(global);

    if (!IsFirstPass) {
      checkExplicitConformance(SD, SD->getDeclaredTypeInContext());
    }
    TC.checkDeclAttributes(SD);
  }

  void checkObjCConformance(ProtocolDecl *protocol,
                            ProtocolConformance *conformance) {
    // FIXME: Put the invalid-conformance check below?
    if (!conformance || conformance->isInvalid())
      return;
    if (protocol->isObjC()) {
      conformance->forEachValueWitness(&TC,
                                       [&](ValueDecl *req,
                                           ConcreteDeclRef witness) {
        if (req->isObjC() && witness)
          markAsObjC(witness.getDecl(), true);
      });
    }

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

  /// Check that all stored properties have in-class initializers.
  void checkRequiredInClassInits(ClassDecl *cd) {
    ClassDecl *source = nullptr;
    for (auto member : cd->getMembers()) {
      auto pbd = dyn_cast<PatternBindingDecl>(member);
      if (!pbd)
        continue;

      if (pbd->isStatic() || !pbd->hasStorage() || 
          isDefaultInitializable(pbd) || pbd->isInvalid())
        continue;

      // The variables in this pattern have not been
      // initialized. Diagnose the lack of initial value.
      pbd->setInvalid();
      SmallVector<VarDecl *, 4> vars;
      pbd->getPattern()->collectVariables(vars);
      switch (vars.size()) {
      case 0:
        llvm_unreachable("should have been marked invalid");

      case 1:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_1,
                    vars[0]->getName());
        break;

      case 2:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_2,
                    vars[0]->getName(), vars[1]->getName());
        break;

      case 3:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_3plus,
                    vars[0]->getName(), vars[1]->getName(), vars[2]->getName(),
                    false);
        break;

      default:
        TC.diagnose(pbd->getLoc(), diag::missing_in_class_init_3plus,
                    vars[0]->getName(), vars[1]->getName(), vars[2]->getName(),
                    true);
        break;
      }

      // Figure out where this requirement came from.
      if (!source) {
        source = cd;
        while (true) {
          // If this class had the 'requires_stored_property_inits'
          // attribute, diagnose here.
          if (source->getAttrs().requiresStoredPropertyInits())
            break;

          // If the superclass doesn't require in-class initial
          // values, the requirement was introduced at this point, so
          // stop here.
          auto superclass = cast<ClassDecl>(
                              source->getSuperclass()->getAnyNominal());
          if (!superclass->requiresStoredPropertyInits())
            break;

          // Keep looking.
          source = superclass;
        }
      }

      // Add a note describing why we need an initializer.
      TC.diagnose(source, diag::requires_stored_property_inits_here,
                  source->getDeclaredType(), cd == source);
    }
  }

  void visitClassDecl(ClassDecl *CD) {
    // This class declaration is technically a parse error, so do not type
    // check.
    if (isa<ProtocolDecl>(CD->getParent()))
      return;

    TC.checkDeclAttributesEarly(CD);

    if (!IsSecondPass) {
      TC.validateDecl(CD);

      TC.ValidatedTypes.remove(CD);

      {
        // Check for circular inheritance.
        SmallVector<ClassDecl *, 8> path;
        checkCircularity(TC, CD, diag::circular_class_inheritance,
                         diag::class_here, path);
      }
    }

    if (!IsFirstPass) {
      TC.addImplicitConstructors(CD);
    }
    TC.addImplicitDestructor(CD);

    // Mark all members of @final classes as @final.
    if (CD->isFinal())
      for (Decl *Member : CD->getMembers())
        if (isa<FuncDecl>(Member) || isa<VarDecl>(Member) ||
            isa<SubscriptDecl>(Member))
          Member->getMutableAttrs().add(new (TC.Context) FinalAttr(true));

    for (Decl *Member : CD->getMembers())
      visit(Member);
    for (Decl *global : CD->getDerivedGlobalDecls())
      visit(global);

    // If this class requires all of its stored properties to have
    // in-class initializers, diagnose this now.
    if (CD->requiresStoredPropertyInits())
      checkRequiredInClassInits(CD);

    if (!IsFirstPass) {
      // Check that we don't inherit from a final class.
      if (auto superclassTy = CD->getSuperclass()) {
        ClassDecl *Super = superclassTy->getClassOrBoundGenericClass();
        if (Super->isFinal()) {
          TC.diagnose(CD, diag::inheritance_from_final_class,
                      Super->getName());
          return;
        }
      }

      // Check for inconsistencies between the initializers of our
      // superclass and our own initializers.
      if (auto superclassTy = CD->getSuperclass()) {
        // Verify that if the super class is generic, the derived class is as
        // well.
        if (superclassTy->getAs<BoundGenericClassType>() &&
            !CD->getDeclaredTypeInContext()->getAs<BoundGenericClassType>())
          TC.diagnose(CD, diag::non_generic_class_with_generic_superclass);
              
        // Look for any required constructors or designated initializers in the
        // subclass that have not been overridden or otherwise provided.
        // Collect the set of initializers we override in superclass.
        llvm::SmallPtrSet<ConstructorDecl *, 4> overriddenCtors;
        for (auto member : CD->getMembers()) {
          auto ctor = dyn_cast<ConstructorDecl>(member);
          if (!ctor)
            continue;

          if (auto overridden = ctor->getOverriddenDecl())
            overriddenCtors.insert(overridden);
        }
        
        bool diagnosed = false;
        for (auto superclassMember : TC.lookupConstructors(superclassTy, CD)) {
          // We only care about required or designated initializers.
          auto superclassCtor = cast<ConstructorDecl>(superclassMember);
          if (!superclassCtor->isRequired() && 
              !superclassCtor->isDesignatedInit())
            continue;

          // Skip invalid superclass initializers.
          if (superclassCtor->isInvalid())
            continue;

          // If we have an override for this constructor, it's okay.
          if (overriddenCtors.count(superclassCtor) > 0)
            continue;

          // If the superclass constructor is a convenience initializer
          // that is inherited into the current class, it's okay.
          if (superclassCtor->isInheritable() &&
              CD->inheritsSuperclassInitializers(&TC)) {
            assert(superclassCtor->isRequired());
            continue;
          }

          // Diagnose a missing override of a required initializer.
          if (superclassCtor->isRequired()) {
            // Complain that we don't have an overriding constructor.
            if (!diagnosed) {
              TC.diagnose(CD, diag::required_incomplete_implementation,
                          CD->getDeclaredInterfaceType());
              diagnosed = true;
            }

            // FIXME: Using the type here is awful. We want to use the selector
            // name and provide a nice Fix-It with that declaration.
            TC.diagnose(superclassCtor,
                        diag::required_initializer_not_overridden,
                        superclassCtor->getArgumentType());
            continue;
          }

          // A designated initializer has not been overridden. 

          // Skip this designated initializer if it's in an extension.
          // FIXME: We shouldn't allow this.
          if (isa<ExtensionDecl>(superclassCtor->getDeclContext()))
            continue;

          // Create an override for it.
          if (auto ctor = createdesignatedInitOverride(
                            TC, CD, superclassCtor,
                            designatedInitKind::Stub)) {
            assert(ctor->getOverriddenDecl() == superclassCtor && 
                   "Not an override?");
            CD->addMember(ctor);
            visit(ctor);
          }
        }
      }
    }
    if (!IsFirstPass) {
      checkExplicitConformance(CD, CD->getDeclaredTypeInContext());
      checkObjCConformances(CD->getProtocols(), CD->getConformances());
    }

    TC.checkDeclAttributes(CD);
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    // This protocol declaration is technically a parse error, so do not type
    // check.
    if (isa<ProtocolDecl>(PD->getParent()))
      return;

    TC.checkDeclAttributesEarly(PD);

    if (IsSecondPass) {
      return;
    }

    TC.validateDecl(PD);

    {
      // Check for circular inheritance within the protocol.
      SmallVector<ProtocolDecl *, 8> path;
      checkCircularity(TC, PD, diag::circular_protocol_def,
                       diag::protocol_here, path);
    }

    // Check the members.
    for (auto Member : PD->getMembers())
      visit(Member);

    TC.checkDeclAttributes(PD);
  }

  void visitVarDecl(VarDecl *VD) {
    // Delay type-checking on VarDecls until we see the corresponding
    // PatternBindingDecl.
  }

  bool semaFuncParamPatterns(AbstractFunctionDecl *fd,
                             GenericTypeResolver *resolver = nullptr) {
    // Type check the body patterns.
    bool badType = false;
    auto bodyPatterns = fd->getBodyParamPatterns();
    for (unsigned i = 0, e = bodyPatterns.size(); i != e; ++i) {
      auto *bodyPat = bodyPatterns[i];

      if (bodyPat->hasType())
        continue;

      if (TC.typeCheckPattern(bodyPat, fd, TR_ImmediateFunctionInput, resolver))
        badType = true;
    }

    return badType;
  }

  /// \brief Validate and apply the attributes that are applicable to the
  /// AnyFunctionType.
  ///
  /// Currently, we only allow 'noreturn' to be applied on a FuncDecl.
  AnyFunctionType::ExtInfo
  validateAndApplyFunctionTypeAttributes(FuncDecl *FD) {
    auto Info = AnyFunctionType::ExtInfo();

    // 'noreturn' is allowed on a function declaration.
    Info = Info.withIsNoReturn(FD->getAttrs().hasAttribute<NoReturnAttr>());

    return Info;
  }

  void semaFuncDecl(FuncDecl *FD, GenericTypeResolver *resolver) {
    if (FD->hasType())
      return;

    bool badType = false;
    if (!FD->getBodyResultTypeLoc().isNull()) {
      if (TC.validateType(FD->getBodyResultTypeLoc(), FD->getDeclContext(),
                          TR_FunctionResult, resolver)) {
        badType = true;
      }
    }

    if (!badType)
      badType = semaFuncParamPatterns(FD, resolver);

    // Checking the function parameter patterns might (recursively)
    // end up setting the type.
    if (FD->hasType())
      return;

    if (badType) {
      FD->setType(ErrorType::get(TC.Context));
      FD->setInvalid();
      return;
    }


    // Reject things like "func f(Int)" if it has a body, since this will
    // implicitly name the argument 'f'.  Instead, suggest that the user write
    // this as "func f(_: Int)".
    if (FD->hasBody() && FD->getBodyParamPatterns().size() == 1) {
      Pattern *BodyPattern = FD->getBodyParamPatterns()[0];
      
      // Look through single-entry tuple elements, which can exist when there
      // are default values.
      if (auto *TP = dyn_cast<TuplePattern>(BodyPattern))
        if (TP->getNumFields() == 1 && !TP->hasVararg())
          BodyPattern =TP->getFields()[0].getPattern();
      // Look through typedpatterns and parens.
      BodyPattern = BodyPattern->getSemanticsProvidingPattern();
      
      if (auto *NP = dyn_cast<NamedPattern>(BodyPattern))
        if (NP->getDecl()->getName() == FD->getName() && NP->isImplicit()) {
          TC.diagnose(BodyPattern->getLoc(), diag::implied_name_no_argument)
            .fixItInsert(BodyPattern->getLoc(), "_: ");
          // Mark the decl as invalid to avoid inscrutable downstream errors.
          NP->getDecl()->setInvalid();
          NP->getDecl()->overwriteType(ErrorType::get(TC.Context));
        }
    }
    
    Type funcTy = FD->getBodyResultTypeLoc().getType();
    if (!funcTy) {
      funcTy = TupleType::getEmpty(TC.Context);
    }
    auto bodyResultType = funcTy;

    // Form the function type by building the curried function type
    // from the back to the front, "prepending" each of the parameter
    // patterns.
    GenericParamList *genericParams = FD->getGenericParams();
    GenericParamList *outerGenericParams = nullptr;
    auto patterns = FD->getBodyParamPatterns();
    bool hasSelf = FD->getDeclContext()->isTypeContext();
    if (hasSelf)
      outerGenericParams = FD->getDeclContext()->getGenericParamsOfContext();

    for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
      Type argTy = patterns[e - i - 1]->getType();

      // Determine the appropriate generic parameters at this level.
      GenericParamList *params = nullptr;
      if (e - i - 1 == hasSelf && genericParams) {
        params = genericParams;
      } else if (e - i - 1 == 0 && outerGenericParams) {
        params = outerGenericParams;
      }
      
      // If we have a compound name, relabel the argument type for the primary
      // argument list.
      if (e - i - 1 == hasSelf) {
        if (auto name = FD->getFullName()) {
          argTy = argTy->getRelabeledType(TC.Context, name.getArgumentNames());
        }
      }

      // Validate and consume the function type attributes.
      auto Info = validateAndApplyFunctionTypeAttributes(FD);
      if (params) {
        funcTy = PolymorphicFunctionType::get(argTy, funcTy, params, Info);
      } else {
        funcTy = FunctionType::get(argTy, funcTy, Info);
      }

    }
    FD->setType(funcTy);
    FD->setBodyResultType(bodyResultType);

    // For a non-generic method that returns dynamic Self, we need to
    // provide an interface type where the 'self' argument is the
    // nominal type.
    if (FD->hasDynamicSelf() && !genericParams && !outerGenericParams) {
      auto fnType = FD->getType()->castTo<FunctionType>();
      auto inputType = fnType->getInput().transform([&](Type type) -> Type {
        if (type->is<DynamicSelfType>())
          return FD->getExtensionType();
        return type;
      });
      FD->setInterfaceType(FunctionType::get(inputType, fnType->getResult(),
                                             fnType->getExtInfo()));
    }
  }

  /// Bind the given function declaration, which declares an operator, to
  /// the corresponding operator declaration.
  void bindFuncDeclToOperator(FuncDecl *FD) {
    OperatorDecl *op = nullptr;
    auto operatorName = FD->getFullName().getBaseName();
    SourceFile &SF = *FD->getDeclContext()->getParentSourceFile();
    if (FD->isUnaryOperator()) {
      if (FD->getAttrs().isPrefix()) {
        op = SF.lookupPrefixOperator(operatorName, FD->getLoc());
      } else if (FD->getAttrs().isPostfix()) {
        // Postfix '!' is reserved.
        if (operatorName.str().equals("!")) {
          TC.diagnose(FD->getLoc(), diag::custom_operator_postfix_exclaim);
          return;
        }

        op = SF.lookupPostfixOperator(operatorName,FD->getLoc());
      } else {
        auto prefixOp = SF.lookupPrefixOperator(operatorName, FD->getLoc());
        auto postfixOp = SF.lookupPostfixOperator(operatorName, FD->getLoc());

        // If we found both prefix and postfix, or neither prefix nor postfix,
        // complain. We can't fix this situation.
        if (static_cast<bool>(prefixOp) == static_cast<bool>(postfixOp)) {
          TC.diagnose(FD, diag::declared_unary_op_without_attribute);

          // If we found both, point at them.
          if (prefixOp) {
            SourceLoc insertionLoc = FD->getLoc();

            TC.diagnose(prefixOp, diag::unary_operator_declaration_here,false)
              .fixItInsert(insertionLoc, "@prefix ");
            TC.diagnose(postfixOp, diag::unary_operator_declaration_here, true)
              .fixItInsert(insertionLoc, "@postfix ");
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
        SourceLoc insertionLoc = FD->getFuncLoc();
        const char *insertionText;
        if (postfixOp) {
          insertionText = "@postfix ";
          op = postfixOp;
          FD->getMutableAttrs().setAttr(AK_postfix, SourceLoc());
        } else {
          insertionText = "@prefix ";
          op = prefixOp;
          FD->getMutableAttrs().setAttr(AK_prefix, SourceLoc());
        }

        // Emit diagnostic with the Fix-It.
        TC.diagnose(insertionLoc, diag::unary_op_missing_prepos_attribute,
                    static_cast<bool>(postfixOp))
          .fixItInsert(insertionLoc, insertionText);
        TC.diagnose(op, diag::unary_operator_declaration_here,
                    static_cast<bool>(postfixOp));
      }
    } else if (FD->isBinaryOperator()) {
      op = SF.lookupInfixOperator(operatorName, FD->getLoc());
    } else {
      TC.diagnose(FD, diag::invalid_arg_count_for_operator);
      return;
    }

    if (!op) {
      // FIXME: Add Fix-It introducing an operator declaration?
      TC.diagnose(FD, diag::declared_operator_without_operator_decl);
      return;
    }

    FD->setOperatorDecl(op);
  }

  /// Determine whether the given declaration requires a definition.
  ///
  /// Only valid for declarations that can have definitions, i.e.,
  /// functions, initializers, etc.
  static bool requiresDefinition(Decl *decl) {
    // Invalid, implicit, and Clang-imported declarations never
    // require a definition.
    if (decl->isInvalid() || decl->isImplicit() || decl->hasClangNode())
      return false;

    // Functions can have an asmname attribute.
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (func->getAttrs().hasAttribute<AsmnameAttr>())
        return false;
    }

    // Declarations in SIL don't require definitions.
    if (auto sourceFile = decl->getDeclContext()->getParentSourceFile()) {
      if (sourceFile->Kind == SourceFileKind::SIL)
        return false;
    }

    // Everything else requires a definition.
    return true;
  }

  /// Check for methods that return 'DynamicResult'.
  bool checkDynamicSelfReturn(FuncDecl *func) {
    // Check whether we have a specified result type.
    auto typeRepr = func->getBodyResultTypeLoc().getTypeRepr();
    if (!typeRepr)
      return false;
      
    return checkDynamicSelfReturn(func, typeRepr, 0);
  }

  bool checkDynamicSelfReturn(FuncDecl *func, TypeRepr *typeRepr,
                              unsigned optionalDepth) {
    // Look through parentheses.
    if (auto parenRepr = dyn_cast<TupleTypeRepr>(typeRepr)) {
      if (!parenRepr->isParenType()) return false;
      return checkDynamicSelfReturn(func, parenRepr->getElements()[0],
                                    optionalDepth);
    }

    // Look through attributes.
    if (auto attrRepr = dyn_cast<AttributedTypeRepr>(typeRepr)) {
      // Only allow @unchecked.
      TypeAttributes attrs = attrRepr->getAttrs();
      attrs.clearAttribute(TAK_unchecked);
      if (!attrs.empty()) return false;
      return checkDynamicSelfReturn(func, attrRepr->getTypeRepr(),
                                    optionalDepth);
    }

    // Look through optional types.
    if (auto attrRepr = dyn_cast<OptionalTypeRepr>(typeRepr)) {
      // But only one level.
      if (optionalDepth != 0) return false;
      return checkDynamicSelfReturn(func, attrRepr->getBase(),
                                    optionalDepth + 1);
    }

    // Check whether we have a simple identifier type.
    auto simpleRepr = dyn_cast<SimpleIdentTypeRepr>(typeRepr);
    if (!simpleRepr)
      return false;

    // Check whether it is 'Self'.
    if (simpleRepr->getIdentifier() != TC.Context.Id_Self)
      return false;

    // Dynamic 'Self' is only permitted on methods.
    auto dc = func->getDeclContext();
    if (!dc->isTypeContext()) {
      TC.diagnose(simpleRepr->getIdLoc(), diag::dynamic_self_non_method,
                  dc->isLocalContext());
      simpleRepr->setValue(ErrorType::get(TC.Context));
      return true;
    }

    auto containerTy = dc->getDeclaredTypeOfContext();
    if (containerTy->is<ErrorType>())
      return true;

    // 'Self' is only a dynamic self on class methods.
    auto nominal = containerTy->getAnyNominal();
    assert(nominal && "Non-nominal container for method type?");
    if (!isa<ClassDecl>(nominal) && !isa<ProtocolDecl>(nominal)) {
      int which;
      if (isa<StructDecl>(nominal))
        which = 0;
      else if (isa<EnumDecl>(nominal))
        which = 1;
      else
        llvm_unreachable("Unknown nominal type");
      TC.diagnose(simpleRepr->getIdLoc(), diag::dynamic_self_struct_enum,
                  which, nominal->getName())
        .fixItReplace(simpleRepr->getIdLoc(), nominal->getName().str());
      simpleRepr->setValue(ErrorType::get(TC.Context));
      return true;
    }

    // Note that the function has a dynamic Self return type and set
    // the return type component to the dynamic self type.
    func->setDynamicSelf(true);
    auto dynamicSelfType = func->getDynamicSelf();
    simpleRepr->setValue(dynamicSelfType);
    return false;
  }

  void visitFuncDecl(FuncDecl *FD) {
    if (!IsFirstPass) {
      if (FD->hasBody()) {
        // Record the body.
        TC.definedFunctions.push_back(FD);
      } else if (requiresDefinition(FD)) {
        // Complain if we should have a body.
        TC.diagnose(FD->getLoc(), diag::func_decl_without_brace);
      }
    }

    TC.checkDeclAttributesEarly(FD);

    if (IsSecondPass || FD->hasType())
      return;

    // Bind operator functions to the corresponding operator declaration.
    if (FD->isOperator())
      bindFuncDeclToOperator(FD);

    // Validate 'static'/'class' on functions in extensions.
    auto StaticSpelling = FD->getStaticSpelling();
    if (StaticSpelling != StaticSpellingKind::None &&
        FD->getDeclContext()->isExtensionContext()) {
      if (Type T = FD->getDeclContext()->getDeclaredTypeInContext()) {
        if (auto NTD = T->getAnyNominal()) {
          if (isa<ClassDecl>(NTD) || isa<ProtocolDecl>(NTD)) {
            if (StaticSpelling == StaticSpellingKind::KeywordStatic) {
              TC.diagnose(FD, diag::static_func_in_class)
                  .fixItReplace(FD->getStaticLoc(), "class");
              TC.diagnose(NTD, diag::extended_type_declared_here);
            }
          } else if (StaticSpelling == StaticSpellingKind::KeywordClass) {
            TC.diagnose(FD, diag::class_func_in_struct)
                .fixItReplace(FD->getStaticLoc(), "static");
            TC.diagnose(NTD, diag::extended_type_declared_here);
          }
        }
      }
    }

    // Validate the mutating attribute if present, and install it into the bit
    // on funcdecl (instead of just being in DeclAttrs).
    Optional<bool> MutatingAttr = FD->getAttrs().getMutating();
    if (MutatingAttr) {
      if (!FD->getDeclContext()->isTypeContext())
        TC.diagnose(FD->getAttrs().getLoc(AK_mutating),
                    diag::mutating_invalid_global_scope);
      else if (FD->getDeclContext()->getDeclaredTypeInContext()
                 ->hasReferenceSemantics())
        TC.diagnose(FD->getAttrs().getLoc(AK_mutating),
                    diag::mutating_invalid_classes);
      else
        FD->setMutating(MutatingAttr.getValue());
    }

    bool isInvalid = false;

    // Check whether the return type is dynamic 'Self'.
    if (checkDynamicSelfReturn(FD))
      isInvalid = true;

    // Before anything else, set up the 'self' argument correctly if present.
    GenericParamList *outerGenericParams = nullptr;
    if (FD->getDeclContext()->isTypeContext())
      configureImplicitSelf(FD, outerGenericParams);

    // If we have generic parameters, check the generic signature now.
    if (auto gp = FD->getGenericParams()) {
      gp->setOuterParameters(outerGenericParams);

      if (TC.validateGenericFuncSignature(FD))
        isInvalid = true;
      else {
        // Create a fresh archetype builder.
        ArchetypeBuilder builder =
          TC.createArchetypeBuilder(FD->getModuleContext());
        checkGenericParamList(builder, gp, TC, FD->getDeclContext());

        // Infer requirements from parameter patterns.
        for (auto pattern : FD->getBodyParamPatterns()) {
          builder.inferRequirements(pattern);
        }

        // Infer requirements from the result type.
        if (!FD->getBodyResultTypeLoc().isNull()) {
          builder.inferRequirements(FD->getBodyResultTypeLoc().getTypeRepr());
        }

        // Revert all of the types within the signature of the function.
        TC.revertGenericFuncSignature(FD);

        finalizeGenericParamList(builder, FD->getGenericParams(), FD, TC);
      }
    } else if (outerGenericParams) {
      if (TC.validateGenericFuncSignature(FD))
        isInvalid = true;
      else {
        // Revert all of the types within the signature of the function.
        TC.revertGenericFuncSignature(FD);
      }
    }

    // Type check the parameters and return type again, now with archetypes.
    GenericTypeToArchetypeResolver resolver;
    semaFuncDecl(FD, &resolver);

    if (FD->isInvalid())
      return;

    // This type check should have created a non-dependent type.
    assert(!FD->getType()->isDependentType());

    validateAttributes(TC, FD);

    // A method is ObjC-compatible if it's explicitly @objc, a member of an
    // ObjC-compatible class, or an accessor for an ObjC property.
    Type ContextTy = FD->getDeclContext()->getDeclaredTypeInContext();
    if (ContextTy) {
      ClassDecl *classContext = ContextTy->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext =
          dyn_cast<ProtocolDecl>(FD->getDeclContext());
      bool isMemberOfObjCProtocol =
          protocolContext && protocolContext->isObjC();
      ObjCReason reason = ObjCReason::DontDiagnose;
      if (FD->getAttrs().hasAttribute<ObjCAttr>())
        reason = ObjCReason::ExplicitlyObjC;
      else if (isMemberOfObjCProtocol)
        reason = ObjCReason::MemberOfObjCProtocol;
      bool isObjC = (reason != ObjCReason::DontDiagnose) ||
                    (classContext && classContext->isObjC());
      
      if (protocolContext && FD->isGetterOrSetter()) {
        // Don't complain about accessors in protocols.  We will emit a
        // diagnostic about the property itself.
        reason = ObjCReason::DontDiagnose;
      }
      if (!isObjC && FD->isGetterOrSetter()) {
        // If the property decl is an instance property, its accessors will
        // be instance methods and the above condition will mark them ObjC.
        // The only additional condition we need to check is if the var decl
        // had an @objc or @iboutlet property.

        ValueDecl *prop = cast<ValueDecl>(FD->getAccessorStorageDecl());
        // Validate the subscript or property because it might not be type
        // checked yet.
        if (isa<SubscriptDecl>(prop))
          TC.validateDecl(prop);
        else if (auto pat = cast<VarDecl>(prop)->getParentPattern())
          validatePatternBindingDecl(TC, pat);

        isObjC = prop->isObjC() ||
                 prop->getAttrs().hasAttribute<IBOutletAttr>();
      }

      if (isObjC &&
          (FD->isInvalid() || !TC.isRepresentableInObjC(FD, reason)))
        isObjC = false;
      markAsObjC(FD, isObjC);
    }
    
    if (!checkOverrides(FD)) {
      // If a method has an 'override' keyword but does not override anything,
      // complain.
      if (auto *OA = FD->getMutableAttrs().getAttribute<OverrideAttr>()) {
        if (!FD->getOverriddenDecl()) {
          TC.diagnose(FD, diag::method_does_not_override)
              .highlight(OA->getLocation());
          OA->setInvalid();
        }
      }
    }
    
    TC.checkDeclAttributes(FD);
  }

  /// Adjust the type of the given declaration to appear as if it were
  /// in the given subclass of its actual declared class.
  Type adjustSuperclassMemberDeclType(ValueDecl *decl, Type subclass) {
    ClassDecl *superclassDecl =
      decl->getDeclContext()->getDeclaredTypeInContext()
        ->getClassOrBoundGenericClass();
    auto superclass = subclass;
    while (superclass->getClassOrBoundGenericClass() != superclassDecl)
      superclass = TC.getSuperClassOf(superclass);
    auto type = TC.substMemberTypeWithBase(decl->getModuleContext(),
                                           decl->getInterfaceType(), decl, 
                                           superclass);
    if (auto func = dyn_cast<FuncDecl>(decl)) {
      if (func->hasDynamicSelf()) {
        type = type.transform([subclass](Type type) -> Type {
            if (type->is<DynamicSelfType>())
              return subclass;
            return type;
        });
      }
    } else if (isa<ConstructorDecl>(decl)) {
      type = type->replaceCovariantResultType(subclass, /*uncurryLevel=*/2);
    }

    return type;
  }

  /// Perform basic checking to determine whether a declaration can override a
  /// declaration in a superclass.
  static bool areOverrideCompatibleSimple(ValueDecl *decl,
                                          ValueDecl *parentDecl) {
    if (auto func = dyn_cast<FuncDecl>(decl)) {
      // Specific checking for methods.
      auto parentFunc = cast<FuncDecl>(parentDecl);
      if (func->isStatic() != parentFunc->isStatic())
        return false;
    } else if (auto var = dyn_cast<VarDecl>(decl)) {
      auto parentVar = cast<VarDecl>(parentDecl);
      if (var->isStatic() != parentVar->isStatic())
        return false;
    }

    return true;
  }

  /// Determine which method or subscript this method or subscript overrides
  /// (if any).
  ///
  /// \returns true if an error occurred.
  bool checkOverrides(ValueDecl *decl) {
    if (decl->isInvalid() || decl->getOverriddenDecl())
      return false;

    auto owningTy = decl->getDeclContext()->getDeclaredInterfaceType();
    if (!owningTy)
      return false;

    auto classDecl = owningTy->getClassOrBoundGenericClass();
    if (!classDecl)
      return false;

    Type superclass = classDecl->getSuperclass();
    if (!superclass)
      return false;

    // Ignore accessor methods (e.g. getters and setters), they will be handled
    // when their storage decl is processed.
    if (auto *fd = dyn_cast<FuncDecl>(decl))
      if (fd->isAccessor())
        return false;
    
    auto method = dyn_cast<AbstractFunctionDecl>(decl);
    auto abstractStorage = dyn_cast<AbstractStorageDecl>(decl);
    assert((method || abstractStorage) && "Not a method or abstractStorage?");

    // Figure out the type of the declaration that we're using for comparisons.
    auto declTy = decl->getInterfaceType()->getUnlabeledType(TC.Context);
    auto uncurriedDeclTy = declTy;
    if (method) {
      declTy = declTy->getWithoutNoReturn(2);
      uncurriedDeclTy = declTy->castTo<AnyFunctionType>()->getResult();
    }

    // If the method is an Objective-C method, compute its selector.
    Optional<ObjCSelector> methodSelector;
    ObjCSubscriptKind subscriptKind = ObjCSubscriptKind::None;

    if (decl->isObjC()) {
      if (method)
        methodSelector = method->getObjCSelector();
      else if (auto *subscript = dyn_cast<SubscriptDecl>(abstractStorage))
        subscriptKind = subscript->getObjCSubscriptKind();
    }

    // Look for members with the same name and matching types as this
    // one.
    auto superclassMetaTy = MetatypeType::get(superclass);
    bool retried = false;
    DeclName name = decl->getFullName();

  retry:
    LookupResult members = TC.lookupMember(superclassMetaTy, name,
                                           decl->getDeclContext(),
                                           /*allowDynamicLookup=*/false);

    typedef std::tuple<ValueDecl *, bool, Type> MatchType;
    SmallVector<MatchType, 2> matches;
    bool hadExactMatch = false;

    for (auto member : members) {
      if (member->isInvalid())
        continue;

      if (member->getKind() != decl->getKind())
        continue;

      auto parentDecl = cast<ValueDecl>(member);

      // Check whether there are any obvious reasons why the two given
      // declarations do not have an overriding relationship.
      if (!areOverrideCompatibleSimple(decl, parentDecl))
        continue;

      auto parentMethod = dyn_cast<AbstractFunctionDecl>(parentDecl);
      auto parentStorage = dyn_cast<AbstractStorageDecl>(parentDecl);
      assert(parentMethod || parentStorage);

      // If both are Objective-C, then match based on selectors or subscript
      // kind and check the types separately.
      bool objCMatch = false;
      if (decl->isObjC() && parentDecl->isObjC()) {
        if (method) {
          // If the selectors don't match, it's not an override.
          if (*methodSelector != parentMethod->getObjCSelector())
            continue;

          objCMatch = true;
        } else if (auto *parentSubscript =
                     dyn_cast<SubscriptDecl>(parentStorage)) {
          // If the subscript kinds don't match, it's not an override.
          if (subscriptKind != parentSubscript->getObjCSubscriptKind())
            continue;

          objCMatch = true;
        }

        // Properties don't need anything here since they are always checked by
        // name.
      }

      // Check whether the types are identical.
      // FIXME: It's wrong to use the uncurried types here for methods.
      auto parentDeclTy = adjustSuperclassMemberDeclType(parentDecl, owningTy)
                            ->getUnlabeledType(TC.Context);
      auto uncurriedParentDeclTy = parentDeclTy;
      if (method) {
        parentDeclTy = parentDeclTy->getWithoutNoReturn(2);
        uncurriedParentDeclTy = parentDeclTy->castTo<AnyFunctionType>()
                                  ->getResult();
      }

      if (uncurriedDeclTy->isEqual(uncurriedParentDeclTy)) {
        matches.push_back({parentDecl, true, uncurriedParentDeclTy});
        hadExactMatch = true;
        continue;
      }
      
      // If this is a property, we accept the match and then reject it below if
      // the types don't line up, since you can't overload properties based on
      // types.
      if (isa<VarDecl>(parentDecl)) {
        matches.push_back({parentDecl, false, uncurriedParentDeclTy});
        continue;
      }

      // Failing that, check for subtyping.
      if (uncurriedDeclTy->canOverride(uncurriedParentDeclTy,
                                       parentDecl->isObjC(), &TC)) {
        // If the Objective-C selectors match, always call it exact.
        matches.push_back({parentDecl, objCMatch, uncurriedParentDeclTy});
        hadExactMatch |= objCMatch;
        continue;
      }

      // Not a match. If we had an Objective-C match, this is a serious problem.
      if (objCMatch) {
        if (method) {
          TC.diagnose(decl, diag::override_objc_type_mismatch_method,
                      *methodSelector, uncurriedDeclTy);
        } else {
          TC.diagnose(decl, diag::override_objc_type_mismatch_subscript,
                      static_cast<unsigned>(subscriptKind), uncurriedDeclTy);
        }
        TC.diagnose(parentDecl, diag::overridden_here_with_type,
                    uncurriedParentDeclTy);
        return true;
      }
    }

    // If we have no matches.
    if (matches.empty()) {
      // If we already re-tried, or if the user didn't indicate that this is
      // an override, or we don't know what else to look for, try again.
      if (retried || name.isSimpleName() ||
          name.getArgumentNames().size() == 0 ||
          !decl->getAttrs().has(DAK_override))
        return false;

      // Try looking again, this time using just the base name, so that we'll
      // catch
      retried = true;
      name = name.getBaseName();
      goto retry;
    }

    // If we had an exact match, throw away any non-exact matches.
    if (hadExactMatch)
      matches.erase(std::remove_if(matches.begin(), matches.end(),
                                   [&](MatchType &match) {
                                     return !std::get<1>(match);
                                   }), matches.end());

    // If we have a single match (exact or not), take it.
    if (matches.size() == 1) {
      auto matchDecl = std::get<0>(matches[0]);
      auto matchType = std::get<2>(matches[0]);

      // If the name of our match differs from the name we were looking for,
      // complain.
      if (decl->getFullName() != matchDecl->getFullName()) {
        auto diag = TC.diagnose(decl, diag::override_argument_name_mismatch,
                                isa<ConstructorDecl>(decl),
                                decl->getFullName(),
                                matchDecl->getFullName());
        TC.fixAbstractFunctionNames(diag, cast<AbstractFunctionDecl>(decl),
                                    matchDecl->getFullName());
      }

      // If this is an exact type match, we're successful!
      if (uncurriedDeclTy->isEqual(matchType)) {
        // Nothing to do.
        
      } else if (auto subscript =
                   dyn_cast_or_null<SubscriptDecl>(abstractStorage)) {
        // Otherwise, if this is a subscript, validate that covariance is ok.
        // If the parent is non-mutable, it's okay to be covariant.
        auto parentSubscript = cast<SubscriptDecl>(matchDecl);
        if (parentSubscript->getSetter()) {
          TC.diagnose(subscript, diag::override_mutable_covariant_subscript,
                      uncurriedDeclTy, matchType);
          TC.diagnose(matchDecl, diag::subscript_override_here);
          return true;
        }
      } else if (auto property = dyn_cast_or_null<VarDecl>(abstractStorage)) {
        auto propertyTy = property->getInterfaceType();
        auto parentPropertyTy = adjustSuperclassMemberDeclType(matchDecl,
                                                               superclass);
        
        if (!propertyTy->canOverride(parentPropertyTy, false, &TC)) {
          TC.diagnose(property, diag::override_property_type_mismatch,
                      property->getName(), propertyTy, parentPropertyTy);
          TC.diagnose(matchDecl, diag::property_override_here);
          return true;
        }
        
        // Differing only in Optional vs. UncheckedOptional is fine.
        bool IsSilentDifference = false;
        if (auto propertyTyNoOptional = propertyTy->getAnyOptionalObjectType())
          if (auto parentPropertyTyNoOptional =
              parentPropertyTy->getAnyOptionalObjectType())
            if (propertyTyNoOptional->isEqual(parentPropertyTyNoOptional))
              IsSilentDifference = true;
        
        // The overridden property must not be mutable.
        if (cast<AbstractStorageDecl>(matchDecl)->getSetter() &&
            !IsSilentDifference) {
          TC.diagnose(property, diag::override_mutable_covariant_property,
                      property->getName(), parentPropertyTy, propertyTy);
          TC.diagnose(matchDecl, diag::property_override_here);
          return true;
        }
      }

      return recordOverride(decl, matchDecl);
    }

    // We override more than one declaration. Complain.
    TC.diagnose(decl,
                retried ? diag::override_multiple_decls_arg_mismatch
                        : diag::override_multiple_decls_base,
                decl->getFullName());
    for (auto match : matches) {
      auto matchDecl = std::get<0>(match);
      if (retried) {
        auto diag = TC.diagnose(matchDecl, diag::overridden_near_match_here,
                                isa<ConstructorDecl>(matchDecl),
                                matchDecl->getFullName());
        TC.fixAbstractFunctionNames(diag, cast<AbstractFunctionDecl>(decl),
                                    matchDecl->getFullName());
        continue;
      }

      TC.diagnose(std::get<0>(match), diag::overridden_here);
    }
    return true;
  }

  /// Attribute visitor that checks how the given attribute should be
  /// considered when overriding a declaration.
  class AttributeOverrideChecker
          : public AttributeVisitor<AttributeOverrideChecker> {
    TypeChecker &TC;
    ValueDecl *Base;
    ValueDecl *Override;

  public:
    AttributeOverrideChecker(TypeChecker &tc, ValueDecl *base,
                             ValueDecl *override)
      : TC(tc), Base(base), Override(override) { }

    /// Deleting this ensures that all attributes are covered by the visitor
    /// below.
    void visitDeclAttribute(DeclAttribute *A) = delete;

#define UNINTERESTING_ATTR(CLASS)                                              \
    void visit##CLASS##Attr(CLASS##Attr *) {}

    UNINTERESTING_ATTR(IBAction)
    UNINTERESTING_ATTR(IBDesignable)
    UNINTERESTING_ATTR(IBInspectable)
    UNINTERESTING_ATTR(IBOutlet)
    UNINTERESTING_ATTR(Asmname)
    UNINTERESTING_ATTR(Assignment)
    UNINTERESTING_ATTR(ClassProtocol)
    UNINTERESTING_ATTR(Exported)
    UNINTERESTING_ATTR(Override)
    UNINTERESTING_ATTR(RawDocComment)
    UNINTERESTING_ATTR(Required)
    UNINTERESTING_ATTR(NSCopying)
    UNINTERESTING_ATTR(NSManaged)

#undef UNINTERESTING_ATTR

    void visitAvailabilityAttr(AvailabilityAttr *attr) {
      // FIXME: Check that this declaration is at least as available as the
      // one it overrides.
    }

    void visitFinalAttr(FinalAttr *attr) {
      // If this is an accessor, don't complain if we would have
      // complained about the storage declaration.
      if (auto func = dyn_cast<FuncDecl>(Override)) {
        if (auto storageDecl = func->getAccessorStorageDecl()) {
          if (storageDecl->getOverriddenDecl() &&
              storageDecl->getOverriddenDecl()->isFinal())
            return;
        }
      }

      // FIXME: Customize message to the kind of thing.
      TC.diagnose(Override, diag::override_final, 
                  Override->getDescriptiveKind());
      TC.diagnose(Base, diag::overridden_here);
    }

    void visitNoReturnAttr(NoReturnAttr *attr) {
      // Disallow overriding a @noreturn function with a returning one.
      if (Base->getAttrs().hasAttribute<NoReturnAttr>() &&
          !Override->getAttrs().hasAttribute<NoReturnAttr>()) {
        TC.diagnose(Override, diag::override_noreturn_with_return);
        TC.diagnose(Base, diag::overridden_here);
      }
    }

    void visitObjCAttr(ObjCAttr *attr) {
      // If the attribute on the base does not have a name, there's nothing
      // to check.
      if (!attr->hasName())
        return;

      // If the overriding declaration already has an @objc attribute, check
      // whether the names are consistent.
      auto name = *attr->getName();
      if (auto overrideAttr = Override->getAttrs().getAttribute<ObjCAttr>()) {
        if (overrideAttr->hasName()) {
          auto overrideName =  *overrideAttr->getName();

          // If the names (and kind) match, we're done.
          if (overrideName == name) {
            return;
          }

          // The names don't match, which indicates that this is a Swift
          // override that is not going to be reflected in Objective-C.
          llvm::SmallString<64> baseScratch, overrideScratch;
          TC.diagnose(overrideAttr->AtLoc, diag::objc_override_name_mismatch,
                      overrideName, name);
          TC.diagnose(Base, diag::overridden_here);
        }

        // Set the name on the attribute.
        const_cast<ObjCAttr *>(overrideAttr)->setName(name);
        return;
      }

      // Copy the name from the base declaration to the overriding
      // declaration.
      Override->getMutableAttrs().add(attr->clone(TC.Context));
      return;
    }
  };

  /// Record that the \c overriding declarations overrides the
  /// \c overridden declaration.
  ///
  /// \returns true if an error occurred.
  bool recordOverride(ValueDecl *override, ValueDecl *base) {
    // Check property and subscript overriding.
    if (auto *baseASD = dyn_cast<AbstractStorageDecl>(base)) {
      auto *overrideASD = cast<AbstractStorageDecl>(override);

      // Make sure that the overriding property doesn't have storage.
      if (overrideASD->hasStorage() &&
          overrideASD->getStorageKind() != VarDecl::Observing) {
        TC.diagnose(overrideASD, diag::override_with_stored_property,
                    overrideASD->getName());
        TC.diagnose(baseASD, diag::property_override_here);
        return true;
      }

      // Make sure that an observing property isn't observing something
      // read-only.  Observing properties look at change, read-only properties
      // have nothing to observe!
      if (overrideASD->getStorageKind() == VarDecl::Observing &&
          !baseASD->isSettable(baseASD->getDeclContext())) {
        TC.diagnose(overrideASD, diag::observing_readonly_property,
                    overrideASD->getName());
        TC.diagnose(baseASD, diag::property_override_here);
        return true;
      }

      // Make sure we're not overriding a settable property with a non-settable
      // one.  The only reasonable semantics for this would be to inherit the
      // setter but override the getter, and that would be surprising at best.
      if (baseASD->isSettable(baseASD->getDeclContext()) &&
          !override->isSettable(override->getDeclContext())) {
        TC.diagnose(overrideASD, diag::override_mutable_with_readonly_property,
                    overrideASD->getName());
        TC.diagnose(baseASD, diag::property_override_here);
        return true;
      }
    }
    
    // Non-Objective-C declarations in extensions cannot override or
    // be overridden.
    if ((base->getDeclContext()->isExtensionContext() ||
         override->getDeclContext()->isExtensionContext()) &&
        !base->isObjC()) {
      TC.diagnose(override, diag::override_decl_extension,
                  !override->getDeclContext()->isExtensionContext());
      TC.diagnose(base, diag::overridden_here);
      return true;
    }
    
    // If the overriding declaration does not have the @override
    // attribute on it, complain.
    if (!override->getAttrs().hasAttribute<OverrideAttr>() &&
        !isa<ConstructorDecl>(override)) {
      // FIXME: rdar://16320042 - For properties, we don't have a useful
      // location for the 'var' token.  Instead of emitting a bogus fixit, only
      // emit the fixit for 'func's.
      if (!isa<VarDecl>(override))
        TC.diagnose(override, diag::missing_override)
            .fixItInsert(override->getStartLoc(), "override ");
      else
        TC.diagnose(override, diag::missing_override);
      TC.diagnose(base, diag::overridden_here);
      override->getMutableAttrs().add(
          new (TC.Context) OverrideAttr(SourceLoc()));
    }

    // FIXME: Possibly should extend to more availability checking.
    if (base->getAttrs().isUnavailable()) {
      TC.diagnose(override, diag::override_unavailable, override->getName());
    }

    /// Check attributes associated with the base; some may need to merged with
    /// or checked against attributes in the overriding declaration.
    AttributeOverrideChecker attrChecker(TC, base, override);
    for (auto attr : base->getMutableAttrs()) {
      attrChecker.visit(attr);
    }

    if (auto overridingFunc = dyn_cast<FuncDecl>(override)) {
      overridingFunc->setOverriddenDecl(cast<FuncDecl>(base));
    } else if (auto overridingCtor = dyn_cast<ConstructorDecl>(override)) {
      overridingCtor->setOverriddenDecl(cast<ConstructorDecl>(base));
    } else if (auto overridingASD = dyn_cast<AbstractStorageDecl>(override)) {
      auto *baseASD = cast<AbstractStorageDecl>(base);
      overridingASD->setOverriddenDecl(baseASD);
      
      // If there is a getter and/or setter, set their overrides as well.  This
      // is not done for observing accessors, since they are never dynamicly
      // dispatched (they are a local implementation detail of a property).
      if (baseASD->getGetter() && overridingASD->getGetter()) {
        auto overridingGetter = overridingASD->getGetter();
        if (!overridingGetter->getAttrs().hasAttribute<OverrideAttr>()) {
          // FIXME: Egregious hack to set 'override'.
          auto loc = overridingASD->getOverrideLoc();
          overridingGetter->getMutableAttrs().add(
              new (TC.Context) OverrideAttr(loc));
        }
          
        recordOverride(overridingGetter, baseASD->getGetter());
      }
      if (baseASD->getSetter() && overridingASD->getSetter()) {
        auto overridingSetter = overridingASD->getSetter();
        if (!overridingSetter->getAttrs().hasAttribute<OverrideAttr>()) {
          // FIXME: Egregious hack to set 'override'.
          auto loc = overridingASD->getOverrideLoc();
          overridingSetter->getMutableAttrs().add(
              new (TC.Context) OverrideAttr(loc));
        }
        recordOverride(overridingASD->getSetter(), baseASD->getSetter());
      }
      
    } else {
      llvm_unreachable("Unexpected decl");
    }
    
    return false;
  }

  /// Compute the interface type of the given enum element.
  void computeEnumElementInterfaceType(EnumElementDecl *elt) {
    auto enumDecl = cast<EnumDecl>(elt->getDeclContext());
    assert(enumDecl->isGenericContext() && "Not a generic enum");

    // Build the generic function type.
    auto funcTy = elt->getType()->castTo<AnyFunctionType>();
    auto inputTy = TC.getInterfaceTypeFromInternalType(enumDecl,
                                                       funcTy->getInput());
    auto resultTy = TC.getInterfaceTypeFromInternalType(enumDecl,
                                                        funcTy->getResult());
    auto interfaceTy
      = GenericFunctionType::get(enumDecl->getGenericSignature(),
                                 inputTy, resultTy, funcTy->getExtInfo());

    // Record the interface type.
    elt->setInterfaceType(interfaceTy);
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    if (IsSecondPass || EED->hasType())
      return;

    TC.checkDeclAttributesEarly(EED);

    EnumDecl *ED = EED->getParentEnum();
    Type ElemTy = ED->getDeclaredTypeInContext();
    
    // Only attempt to validate the argument type or raw value if the element
    // is not currenly being validated.
    if (EED->getRecursiveness() == ElementRecursiveness::NotRecursive) {
      EED->setRecursiveness(ElementRecursiveness::PotentiallyRecursive);
      
      validateAttributes(TC, EED);
      
      if (!EED->getArgumentTypeLoc().isNull())
        if (TC.validateType(EED->getArgumentTypeLoc(), EED->getDeclContext())) {
          EED->overwriteType(ErrorType::get(TC.Context));
          EED->setInvalid();
          return;
        }
      
      // Check the raw value, if we have one.
      if (auto *rawValue = EED->getRawValueExpr()) {
        
        Type rawTy;
        if (ED->hasRawType()) {
          rawTy = ArchetypeBuilder::mapTypeIntoContext(ED, ED->getRawType());
        } else {
          TC.diagnose(rawValue->getLoc(), diag::enum_raw_value_without_raw_type);
          // Recover by setting the raw type as this element's type.
        }
        Expr *typeCheckedExpr = rawValue;
        if (!TC.typeCheckExpression(typeCheckedExpr, ED, rawTy, false))
          EED->setTypeCheckedRawValueExpr(typeCheckedExpr);
      }
    } else if (EED->getRecursiveness() ==
                ElementRecursiveness::PotentiallyRecursive) {
      EED->setRecursiveness(ElementRecursiveness::Recursive);
    }
    
    // If the element was not already marked as recursive by a re-entrant call,
    // we can be sure it's not recursive.
    if (EED->getRecursiveness() == ElementRecursiveness::PotentiallyRecursive) {
      EED->setRecursiveness(ElementRecursiveness::NotRecursive);
    }

    // If we have a simple element, just set the type.
    if (EED->getArgumentType().isNull()) {
      Type argTy = MetatypeType::get(ElemTy);
      Type fnTy;
      if (auto gp = ED->getGenericParamsOfContext())
        fnTy = PolymorphicFunctionType::get(argTy, ElemTy, gp);
      else
        fnTy = FunctionType::get(argTy, ElemTy);
      EED->setType(fnTy);
      if (EED->getDeclContext()->isGenericContext())
        computeEnumElementInterfaceType(EED);
      return;
    }

    Type fnTy = FunctionType::get(EED->getArgumentType(), ElemTy);
    if (auto gp = ED->getGenericParamsOfContext())
      fnTy = PolymorphicFunctionType::get(MetatypeType::get(ElemTy),
                                          fnTy, gp);
    else
      fnTy = FunctionType::get(MetatypeType::get(ElemTy), fnTy);
    EED->setType(fnTy);

    if (EED->getDeclContext()->isGenericContext())
      computeEnumElementInterfaceType(EED);

    // Require the carried type to be materializable.
    if (!EED->getArgumentType()->isMaterializable()) {
      TC.diagnose(EED->getLoc(), diag::enum_element_not_materializable);
      EED->overwriteType(ErrorType::get(TC.Context));
      EED->setInvalid();
    }
    TC.checkDeclAttributes(EED);
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

    TC.checkDeclAttributesEarly(ED);

    if (!IsSecondPass) {
      CanType ExtendedTy = DeclContext::getExtendedType(ED);

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

      TC.checkInheritanceClause(ED);
      if (auto nominal = ExtendedTy->getAnyNominal())
        TC.validateDecl(nominal);

      validateAttributes(TC, ED);
    }

    for (Decl *Member : ED->getMembers())
      visit(Member);

    if (!IsFirstPass) {
      checkExplicitConformance(ED, ED->getExtendedType());
      checkObjCConformances(ED->getProtocols(), ED->getConformances());
    }
    TC.checkDeclAttributes(ED);
 }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
    // See swift::performTypeChecking for TopLevelCodeDecl handling.
    llvm_unreachable("TopLevelCodeDecls are handled elsewhere");
  }
  
  void visitIfConfigDecl(IfConfigDecl *ICD) {
    // The active members of the #if block will be type checked along with
    // their enclosing declaration.
    TC.checkDeclAttributesEarly(ICD);
    TC.checkDeclAttributes(ICD);
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    if (CD->isInvalid()) {
      CD->overwriteType(ErrorType::get(TC.Context));
      return;
    }

    if (!IsFirstPass) {
      if (CD->getBody()) {
        TC.definedFunctions.push_back(CD);
      } else if (requiresDefinition(CD)) {
        // Complain if we should have a body.
        TC.diagnose(CD->getLoc(), diag::missing_initializer_def);
      }
    }

    if (IsSecondPass || CD->hasType()) {
      return;
    }

    TC.checkDeclAttributesEarly(CD);

    assert(CD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent constructors outside of types!");
    assert(!CD->getAttrs().hasMutating() && "Cannot parse this");

    // convenience initializers are only allowed on classes and in
    // extensions thereof.
    if (CD->isConvenienceInit()) {
      if (auto extType = CD->getExtensionType()) {
        if (!extType->getClassOrBoundGenericClass() &&
            !extType->is<ErrorType>()) {
          // FIXME: Add a Fix-It here, which requires source-location
          // information within the AST for '->' and 'Self'.
          TC.diagnose(CD->getLoc(), diag::nonclass_convenience_init,
                      extType);
          CD->setInitKind(CtorInitializerKind::Designated);
        }
      }
    } else if (auto extType = CD->getExtensionType()) {
      // A designated initializer for a class must be written within the class
      // itself.
      if (extType->getClassOrBoundGenericClass() &&
          isa<ExtensionDecl>(CD->getDeclContext())) {
        SourceLoc fixItLoc = CD->getBodyParamPatterns().back()->getEndLoc();
        fixItLoc = Lexer::getLocForEndOfToken(TC.Context.SourceMgr, fixItLoc);
        TC.diagnose(CD->getLoc(), diag::designated_init_in_extension, extType)
          .fixItInsert(fixItLoc, " -> Self"); 
        CD->setInitKind(CtorInitializerKind::Convenience);
      }
    }

    GenericParamList *outerGenericParams;
    Type SelfTy = configureImplicitSelf(CD, outerGenericParams);

    Optional<ArchetypeBuilder> builder;
    if (auto gp = CD->getGenericParams()) {
      // Write up generic parameters and check the generic parameter list.
      gp->setOuterParameters(outerGenericParams);

      if (TC.validateGenericFuncSignature(CD)) {
        CD->overwriteType(ErrorType::get(TC.Context));
        CD->setInvalid();
      } else {
        ArchetypeBuilder builder =
          TC.createArchetypeBuilder(CD->getModuleContext());
        checkGenericParamList(builder, gp, TC, CD->getDeclContext());

        // Type check the constructor parameters.
        if (semaFuncParamPatterns(CD)) {
          CD->overwriteType(ErrorType::get(TC.Context));
          CD->setInvalid();
        }

        // Infer requirements from the parameters of the constructor.
        builder.inferRequirements(CD->getBodyParamPatterns()[1]);

        // Revert the constructor signature so it can be type-checked with
        // archetypes below.
        TC.revertGenericFuncSignature(CD);

        // Assign archetypes.
        finalizeGenericParamList(builder, gp, CD, TC);
      }
    } else if (outerGenericParams) {
      if (TC.validateGenericFuncSignature(CD)) {
        CD->overwriteType(ErrorType::get(TC.Context));
        CD->setInvalid();
      } else {
        // Revert all of the types within the signature of the constructor.
        TC.revertGenericFuncSignature(CD);
      }
    }

    // Type check the constructor parameters.
    if (CD->isInvalid() || semaFuncParamPatterns(CD)) {
      CD->overwriteType(ErrorType::get(TC.Context));
      CD->setInvalid();
    } else {
      configureConstructorType(CD, outerGenericParams, SelfTy, 
                               CD->getBodyParamPatterns()[1]->getType());
    }

    validateAttributes(TC, CD);

    // An initializer is ObjC-compatible if it's explicitly @objc or a member
    // of an ObjC-compatible class.
    Type ContextTy = CD->getDeclContext()->getDeclaredTypeInContext();
    if (ContextTy) {
      ClassDecl *classContext = ContextTy->getClassOrBoundGenericClass();
      ProtocolDecl *protocolContext =
          dyn_cast<ProtocolDecl>(CD->getDeclContext());
      bool isMemberOfObjCProtocol =
          protocolContext && protocolContext->isObjC();
      ObjCReason reason = ObjCReason::DontDiagnose;
      if (CD->getAttrs().hasAttribute<ObjCAttr>())
        reason = ObjCReason::ExplicitlyObjC;
      else if (isMemberOfObjCProtocol)
        reason = ObjCReason::MemberOfObjCProtocol;
      bool isObjC = (reason != ObjCReason::DontDiagnose) ||
                    (classContext && classContext->isObjC());
      if (isObjC &&
          (CD->isInvalid() || !TC.isRepresentableInObjC(CD, reason)))
        isObjC = false;
      markAsObjC(CD, isObjC);
    }

    // Check whether this constructor overrides a constructor in its base class.
    // This only makes sense when the overridden constructor is required.
    checkOverrides(CD);

    // Determine whether this constructor is required.
    if (!CD->getAttrs().hasAttribute<RequiredAttr>()) {
      if (CD->getOverriddenDecl() && CD->getOverriddenDecl()->isRequired())
        CD->getMutableAttrs().add(
            new (TC.Context) RequiredAttr(/*IsImplicit=*/true));
    }

    TC.checkDeclAttributes(CD);
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

    if (IsSecondPass || DD->hasType()) {
      return;
    }

    TC.checkDeclAttributesEarly(DD);

    assert(DD->getDeclContext()->isTypeContext()
           && "Decl parsing must prevent destructors outside of types!");

    GenericParamList *outerGenericParams;
    Type SelfTy = configureImplicitSelf(DD, outerGenericParams);

    if (outerGenericParams)
      TC.validateGenericFuncSignature(DD);

    if (semaFuncParamPatterns(DD)) {
      DD->overwriteType(ErrorType::get(TC.Context));
      DD->setInvalid();
    }

    Type FnTy;
    if (outerGenericParams)
      FnTy = PolymorphicFunctionType::get(SelfTy,
                                          TupleType::getEmpty(TC.Context),
                                          outerGenericParams);
    else
      FnTy = FunctionType::get(SelfTy, TupleType::getEmpty(TC.Context));

    DD->setType(FnTy);

    // Destructors are always @objc, because their Objective-C entry point is
    // -dealloc.
    markAsObjC(DD, true);

    validateAttributes(TC, DD);
    TC.checkDeclAttributes(DD);
  }
};
}; // end anonymous namespace.


void TypeChecker::typeCheckDecl(Decl *D, bool isFirstPass) {
  PrettyStackTraceDecl StackTrace("type-checking", D);
  bool isSecondPass =
    !isFirstPass && D->getDeclContext()->isModuleScopeContext();
  DeclChecker(*this, isFirstPass, isSecondPass).visit(D);
}

void TypeChecker::validateDecl(ValueDecl *D, bool resolveTypeParams) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::IfConfig:
    llvm_unreachable("not a value decl");

  case DeclKind::TypeAlias: {
    // Type aliases may not have an underlying type yet.
    auto typeAlias = cast<TypeAliasDecl>(D);
    if (typeAlias->getUnderlyingTypeLoc().getTypeRepr() &&
        !typeAlias->getUnderlyingTypeLoc().wasValidated())
      typeCheckDecl(typeAlias, true);
    
    break;
  }

  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType: {
    auto typeParam = cast<AbstractTypeParamDecl>(D);
    if (!resolveTypeParams || typeParam->getArchetype()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(typeParam)) {
        DeclChecker(*this, false, false).visitAssociatedTypeDecl(assocType);
      }

      break;
    }
    
    // FIXME: Avoid full check in these cases?
    DeclContext *DC = typeParam->getDeclContext();
    switch (DC->getContextKind()) {
    case DeclContextKind::Module:
    case DeclContextKind::FileUnit:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::Initializer:
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
    for (auto ext : nominal->getExtensions())
      checkInheritanceClause(ext);

    if (nominal->hasType())
      return;

    // Check generic parameters, if needed.
    if (auto gp = nominal->getGenericParams()) {
      gp->setOuterParameters(
        nominal->getDeclContext()->getGenericParamsOfContext());

      // Validate the generic type parameters.
      validateGenericTypeSignature(nominal);

      revertGenericParamList(nominal->getGenericParams(), nominal);

      ArchetypeBuilder builder =
        createArchetypeBuilder(nominal->getModuleContext());
      checkGenericParamList(builder, gp, *this, nominal->getDeclContext());
      finalizeGenericParamList(builder, gp, nominal, *this);
    }

    // Compute the declared type.
    nominal->computeType();

    validateAttributes(*this, D);
    checkInheritanceClause(D);

    // Mark a class as @objc. This must happen before checking its members.
    if (auto CD = dyn_cast<ClassDecl>(nominal)) {
      ClassDecl *superclassDecl = nullptr;
      if (CD->hasSuperclass())
        superclassDecl = CD->getSuperclass()->getClassOrBoundGenericClass();

      CD->setIsObjC(CD->getAttrs().hasAttribute<ObjCAttr>() ||
                    (superclassDecl && superclassDecl->isObjC()));

      // Determine whether we require in-class initializers.
      if (CD->getAttrs().requiresStoredPropertyInits() ||
          (superclassDecl && superclassDecl->requiresStoredPropertyInits()))
        CD->setRequiresStoredPropertyInits(true);
    }

    ValidatedTypes.insert(nominal);
    break;
  }

  case DeclKind::Protocol: {
    auto proto = cast<ProtocolDecl>(D);
    if (proto->hasType())
      return;
    proto->computeType();

    // Validate the generic type parameters.
    validateGenericTypeSignature(proto);

    revertGenericParamList(proto->getGenericParams(), proto);

    ArchetypeBuilder builder =
      createArchetypeBuilder(proto->getModuleContext());
    checkGenericParamList(builder, proto->getGenericParams(), *this,
                          proto->getDeclContext());
    finalizeGenericParamList(builder, proto->getGenericParams(), proto, *this);

    checkInheritanceClause(D);
    validateAttributes(*this, D);

    // Set the underlying type of each of the associated types to the
    // appropriate archetype.
    auto selfDecl = proto->getSelf();
    ArchetypeType *selfArchetype = builder.getArchetype(selfDecl);
    for (auto member : proto->getMembers()) {
      if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
        TypeLoc underlyingTy;
        ArchetypeType *archetype = selfArchetype;
        archetype = selfArchetype->getNestedType(assocType->getName())
          .dyn_cast<ArchetypeType*>();
        if (!archetype)
          return;
        assocType->setArchetype(archetype);
      }
    }

    // If the protocol is @objc, it may only refine other @objc protocols.
    // FIXME: Revisit this restriction.
    if (proto->getAttrs().hasAttribute<ObjCAttr>()) {
      bool isObjC = true;

      for (auto inherited : proto->getProtocols()) {
        if (!inherited->isObjC()) {
          diagnose(proto->getLoc(),
                   diag::objc_protocol_inherits_non_objc_protocol,
                   proto->getDeclaredType(), inherited->getDeclaredType());
          diagnose(inherited->getLoc(), diag::protocol_here,
                   inherited->getName());
          isObjC = false;
        }
      }

      proto->setIsObjC(isObjC);
    }
    break;
  }
      
  case DeclKind::Var:
  case DeclKind::Param: {
    if (D->hasType())
      return;

    auto VD = cast<VarDecl>(D);

    // Make sure the getter and setter have valid types, since they will be
    // used by SILGen for any accesses to this variable.
    if (auto getter = VD->getGetter())
      validateDecl(getter);
    if (auto setter = VD->getSetter())
      validateDecl(setter);

    if (PatternBindingDecl *PBD = VD->getParentPattern()) {
      validatePatternBindingDecl(*this, PBD);
      if (PBD->isInvalid() || !PBD->getPattern()->hasType()) {
        PBD->getPattern()->setType(ErrorType::get(Context));
        setBoundVarsTypeError(PBD->getPattern(), Context);
        return;
      }
    } else if (VD->isImplicit() &&
               (VD->getName() == Context.Id_self)) {
      // If the variable declaration is for a 'self' parameter, it may be
      // because the self variable was reverted whilst validating the function
      // signature.  In that case, reset the type.
      if (isa<NominalTypeDecl>(VD->getDeclContext()->getParent())) {
        if (auto funcDeclContext =
                dyn_cast<AbstractFunctionDecl>(VD->getDeclContext())) {
          GenericParamList *outerGenericParams = nullptr;
          configureImplicitSelf(funcDeclContext, outerGenericParams);
        }
      } else {
        D->setType(ErrorType::get(Context));
      }      
    } else {
      // FIXME: This case is hit when code completion occurs in a function
      // parameter list. Previous parameters are definitely in scope, but
      // we don't really know how to type-check them.
      assert(isa<AbstractFunctionDecl>(D->getDeclContext()) ||
             isa<TopLevelCodeDecl>(D->getDeclContext()));
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
  checkInheritanceClause(nominal);
  return nominal->getProtocols();
}

ArrayRef<ProtocolDecl *>
TypeChecker::getDirectConformsTo(ExtensionDecl *ext) {
  checkInheritanceClause(ext);
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
  SourceLoc Loc = decl->getLoc();
  // Determine the parameter type of the implicit constructor.
  SmallVector<TuplePatternElt, 8> patternElts;
  SmallVector<Identifier, 8> argNames;
  if (ICK == ImplicitConstructorKind::Memberwise) {
    assert(isa<StructDecl>(decl) && "Only struct have memberwise constructor");

    // Computed and static properties are not initialized.
    for (auto var : decl->getStoredProperties()) {
      tc.validateDecl(var);

      auto varType = tc.getTypeOfRValue(var);

      // Create the parameter.
      auto *arg = new (context) ParamDecl(/*IsLet*/true, Loc, var->getName(),
                                          Loc, var->getName(), varType, decl);
      argNames.push_back(var->getName());
      Pattern *pattern = new (context) NamedPattern(arg);
      TypeLoc tyLoc = TypeLoc::withoutLoc(varType);
      pattern = new (context) TypedPattern(pattern, tyLoc);
      patternElts.push_back(TuplePatternElt(pattern));
    }
  }

  auto pattern = TuplePattern::create(context, Loc, patternElts, Loc);

  // Create the constructor.
  DeclName name(context, context.Id_init, argNames);
  Pattern *selfPat = buildImplicitSelfParameter(Loc, decl);
  auto *ctor = new (context) ConstructorDecl(name, Loc, selfPat, pattern,
                                             nullptr, decl);

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

/// Create an expression that references the variables in the given
/// pattern for, e.g., forwarding of these variables to another
/// function with the same signature.
static Expr *forwardArguments(TypeChecker &tc, ClassDecl *classDecl,
                              ConstructorDecl *toDecl,
                              Pattern *bodyPattern,
                              ArrayRef<Identifier> argumentNames) {
  switch (bodyPattern->getKind()) {
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    return nullptr;
    
  case PatternKind::Paren:
    if (auto subExpr = forwardArguments(
                         tc, classDecl, toDecl,
                         cast<ParenPattern>(bodyPattern)->getSubPattern(),
                         { })) {
      return new (tc.Context) ParenExpr(SourceLoc(), subExpr, SourceLoc(),
                                        /*hasTrailingClosure=*/false);
    }

    return nullptr;

  case PatternKind::Tuple: {
    auto bodyTuple = cast<TuplePattern>(bodyPattern);
    SmallVector<Expr *, 4> values;

    // FIXME: Can't forward varargs yet.
    if (bodyTuple->hasVararg()) {
      tc.diagnose(classDecl->getLoc(),
                  diag::unsupported_synthesize_init_variadic,
                  classDecl->getDeclaredType());
      tc.diagnose(toDecl, diag::variadic_superclass_init_here);
      return nullptr;
    }

    for (unsigned i = 0, n = bodyTuple->getNumFields(); i != n; ++i) {
      // Forward the value.
      auto subExpr = forwardArguments(tc, classDecl, toDecl,
                                      bodyTuple->getFields()[i].getPattern(),
                                      { });
      if (!subExpr)
        return nullptr;
      values.push_back(subExpr);
      
      // Dig out the name.
      auto subPattern = bodyTuple->getFields()[i].getPattern();
      do {
        if (auto typed = dyn_cast<TypedPattern>(subPattern)) {
          subPattern = typed->getSubPattern();
          continue;
        }

        if (auto paren = dyn_cast<ParenPattern>(subPattern)) {
          subPattern = paren->getSubPattern();
          continue;
        }

        break;
      } while (true);
    }

    if (values.size() == 1 && 
        (argumentNames.empty() || argumentNames[0].empty()))
      return new (tc.Context) ParenExpr(SourceLoc(), values[0], SourceLoc(),
                                        /*hasTrailingClosure=*/false);

    return TupleExpr::createImplicit(tc.Context, values, argumentNames);
  }

  case PatternKind::Any:
  case PatternKind::Named: {
    auto decl = cast<NamedPattern>(bodyPattern)->getDecl();
    Expr *declRef = new (tc.Context) DeclRefExpr(decl, SourceLoc(),
                                                 /*Implicit=*/true);
    if (decl->getType()->is<InOutType>()) {
      declRef = new (tc.Context) InOutExpr(SourceLoc(), declRef,
                                               Type(), /*isImplicit=*/true);
    }
    return declRef;
  }

  case PatternKind::Typed:
    return forwardArguments(tc, classDecl, toDecl,
                            cast<TypedPattern>(bodyPattern)->getSubPattern(),
                            argumentNames);

  case PatternKind::Var:
    return forwardArguments(tc, classDecl, toDecl,
                            cast<VarPattern>(bodyPattern)->getSubPattern(),
                            argumentNames);

  }
}

/// Create a stub body that emits a fatal error message.
static void createStubBody(TypeChecker &tc, ConstructorDecl *ctor) {
  auto unimplementedInitDecl = tc.Context.getUnimplementedInitializerDecl(&tc);
  auto classDecl = ctor->getExtensionType()->getClassOrBoundGenericClass();
  if (!unimplementedInitDecl) {
    tc.diagnose(classDecl->getLoc(), diag::missing_unimplemented_init_runtime);
    return;
  }

  // Create a call to Swift._unimplemented_initializer
  auto loc = classDecl->getLoc();
  Expr *fn = new (tc.Context) DeclRefExpr(unimplementedInitDecl, loc,
                                          /*Implicit=*/true);

  llvm::SmallString<64> buffer;
  StringRef fullClassName = tc.Context.AllocateCopy(
                              (classDecl->getModuleContext()->Name.str() + "." +
                               classDecl->getName().str()).toStringRef(buffer));

  Expr *className = new (tc.Context) StringLiteralExpr(fullClassName, loc);
  className = new (tc.Context) ParenExpr(loc, className, loc, false);
  Expr *call = new (tc.Context) CallExpr(fn, className, /*Implicit=*/true);
  ctor->setBody(BraceStmt::create(tc.Context, SourceLoc(),
                                  ASTNode(call),
                                  SourceLoc(),
                                  /*Implicit=*/true));

  // Note that this is a stub implementation/
  ctor->setStubImplementation(true);
}

static ConstructorDecl *
createdesignatedInitOverride(TypeChecker &tc,
                            ClassDecl *classDecl,
                            ConstructorDecl *superclassCtor,
                            designatedInitKind kind) {
  // Determine the initializer parameters.
  Type superInitType = superclassCtor->getInitializerInterfaceType();
  if (superInitType->is<GenericFunctionType>() ||
      classDecl->getGenericParamsOfContext()) {
    // FIXME: Handle generic initializers as well.
    return nullptr;
  }

  auto &ctx = tc.Context;

  // Create the 'self' declaration and patterns.
  auto *selfDecl = new (ctx) ParamDecl(/*IsLet*/ true,
                                       SourceLoc(), Identifier(),
                                       SourceLoc(), ctx.Id_self,
                                       Type(), classDecl);
  selfDecl->setImplicit();
  Pattern *selfBodyPattern 
    = new (ctx) NamedPattern(selfDecl, /*Implicit=*/true);
  selfBodyPattern = new (ctx) TypedPattern(selfBodyPattern, TypeLoc());

  // Create the initializer parameter patterns.
  OptionSet<Pattern::CloneFlags> options = Pattern::Implicit;
  options |= Pattern::Inherited;
  Pattern *bodyParamPatterns
    = superclassCtor->getBodyParamPatterns()[1]->clone(
        ctx, options | Pattern::AlwaysNamed);

  // Fix up the default arguments in the type to refer to inherited default
  // arguments.
  // FIXME: If we weren't cloning the type along with the pattern, this would be
  // a lot more direct.
  Type argType = bodyParamPatterns->getType();

  // Local function that maps default arguments to inherited default arguments.
  std::function<Type(Type)> inheritDefaultArgs = [&](Type type) -> Type {
    auto tuple = type->getAs<TupleType>();
    if (!tuple)
      return type;

    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    unsigned index = 0;
    for (const auto &elt : tuple->getFields()) {
      Type eltTy = elt.getType().transform(inheritDefaultArgs);
      if (!eltTy)
        return Type();

      // If nothing has changd, just keep going.
      if (!anyChanged && eltTy.getPointer() == elt.getType().getPointer() &&
          (elt.getDefaultArgKind() == DefaultArgumentKind::None ||
           elt.getDefaultArgKind() == DefaultArgumentKind::Inherited)) {
        ++index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!anyChanged) {
        // Copy all of the previous elements.
        for (unsigned i = 0; i != index; ++i) {
          const TupleTypeElt &FromElt =tuple->getFields()[i];
          elements.push_back(TupleTypeElt(FromElt.getType(), FromElt.getName(),
                                          FromElt.getDefaultArgKind(),
                                          FromElt.isVararg()));
        }

        anyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      auto defaultArgKind = elt.getDefaultArgKind();
      if (defaultArgKind != DefaultArgumentKind::None)
        defaultArgKind = DefaultArgumentKind::Inherited;
      elements.push_back(TupleTypeElt(eltTy, elt.getName(), defaultArgKind,
                                      elt.isVararg()));
      ++index;
    }

    if (!anyChanged)
      return type;

    return TupleType::get(elements, ctx);
  };

  argType = argType.transform(inheritDefaultArgs);
  bodyParamPatterns->setType(argType);

  // Create the initializer declaration.
  auto ctor = new (ctx) ConstructorDecl(superclassCtor->getFullName(), 
                                        SourceLoc(),
                                        selfBodyPattern, bodyParamPatterns,
                                        nullptr, classDecl);
  ctor->setImplicit();

  // Configure 'self'.
  GenericParamList *outerGenericParams = nullptr;
  Type selfType = configureImplicitSelf(ctor, outerGenericParams);
  selfBodyPattern->setType(selfType);
  cast<TypedPattern>(selfBodyPattern)->getSubPattern()->setType(selfType);

  // Set the type of the initializer.
  configureConstructorType(ctor, outerGenericParams, selfType, 
                           bodyParamPatterns->getType());
  if (superclassCtor->isObjC()) {
    ctor->setIsObjC(true);

    // Inherit the @objc name from the superclass initializer, if it
    // has one.
    if (auto objcAttr = superclassCtor->getAttrs().getAttribute<ObjCAttr>()) {
      if (objcAttr->hasName())
        ctor->getMutableAttrs().add(objcAttr->clone(ctx));
    }

  }

  // Wire up the overides.
  DeclChecker(tc, false, false).checkOverrides(ctor);

  if (kind == designatedInitKind::Stub) {
    // Make this a stub implementation.
    createStubBody(tc, ctor);
    return ctor;
  }

  // Form the body of a chaining designated initializer.
  assert(kind == designatedInitKind::Chaining);

  // Reference to super.init.
  Expr *superRef = new (ctx) SuperRefExpr(selfDecl, SourceLoc(),
                                          /*Implicit=*/true);
  Expr *ctorRef  = new (ctx) UnresolvedConstructorExpr(superRef,
                                                       SourceLoc(),
                                                       SourceLoc(),
                                                       /*Implicit=*/true);

  Expr *ctorArgs = forwardArguments(tc, classDecl, superclassCtor,
                                    ctor->getBodyParamPatterns()[1],
                                    ctor->getFullName().getArgumentNames());
  if (!ctorArgs) {
    // FIXME: We should be able to assert that this never happens,
    // but there are currently holes when dealing with vararg
    // initializers and _ parameters. Fail somewhat gracefully by
    // generating a stub here.
    createStubBody(tc, ctor);
    return ctor;
  }

  Expr *superCall = new (ctx) CallExpr(ctorRef, ctorArgs, /*Implicit=*/true);
  superCall = new (ctx) RebindSelfInConstructorExpr(superCall, selfDecl);
  ctor->setBody(BraceStmt::create(tc.Context, SourceLoc(),
                                  ASTNode(superCall),
                                  SourceLoc(),
                                  /*Implicit=*/true));

  return ctor;
}

/// Build a default initializer string for the given pattern.
///
/// This string is suitable for display in diagnostics.
static Optional<std::string> buildDefaultInitializerString(TypeChecker &tc,
                                                           DeclContext *dc,
                                                           Pattern *pattern) {
  switch (pattern->getKind()) {
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#define PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
    return Nothing;
  case PatternKind::Any:
    return Nothing;

  case PatternKind::Named: {
    if (!pattern->hasType())
      return Nothing;

    // Special-case the various types we might see here.
    auto type = pattern->getType();

    // For literal-convertible types, form the corresponding literal.
#define CHECK_LITERAL_PROTOCOL(Kind, String)                            \
    if (auto proto = tc.getProtocol(SourceLoc(), KnownProtocolKind::Kind)) { \
      if (tc.conformsToProtocol(type, proto, dc))                       \
        return String;                                                  \
    }
    CHECK_LITERAL_PROTOCOL(ArrayLiteralConvertible, "[]")
    CHECK_LITERAL_PROTOCOL(DictionaryLiteralConvertible, "[]")
    CHECK_LITERAL_PROTOCOL(ExtendedGraphemeClusterLiteralConvertible, "\"\"")
    CHECK_LITERAL_PROTOCOL(FloatLiteralConvertible, "0.0")
    CHECK_LITERAL_PROTOCOL(IntegerLiteralConvertible, "0")
    CHECK_LITERAL_PROTOCOL(StringLiteralConvertible, "\"\"")
#undef CHECK_LITERAL_PROTOCOL

    // For optional types, use 'nil'.
    if (type->getAnyOptionalObjectType())
      return "nil";

    return Nothing;
  }

  case PatternKind::Paren: {
    if (auto sub = buildDefaultInitializerString(
                     tc, dc, cast<ParenPattern>(pattern)->getSubPattern())) {
      return "(" + *sub + ")";
    }

    return Nothing;
  }

  case PatternKind::Tuple: {
    std::string result = "(";
    bool first = true;
    for (auto elt : cast<TuplePattern>(pattern)->getFields()) {
      if (auto sub = buildDefaultInitializerString(tc, dc, elt.getPattern())) {
        if (first) {
          first = false;
        } else {
          result += ", ";
        }

        result += *sub;
      } else {
        return Nothing;
      }
    }
    result += ")";
    return result;
  }

  case PatternKind::Typed:
    return buildDefaultInitializerString(
             tc, dc, cast<TypedPattern>(pattern)->getSubPattern());

  case PatternKind::Var:
    return buildDefaultInitializerString(
             tc, dc, cast<VarPattern>(pattern)->getSubPattern());
  }
}

/// Diagnose a class that does not have any initializers.
static void diagnoseClassWithoutInitializers(TypeChecker &tc,
                                             ClassDecl *classDecl) {
  tc.diagnose(classDecl, diag::class_without_init,
              classDecl->getDeclaredType());

  SourceLoc lastLoc;
  for (auto member : classDecl->getMembers()) {
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd)
      continue;

    if (pbd->isStatic() || !pbd->hasStorage() || isDefaultInitializable(pbd) ||
        pbd->isInvalid())
      continue;

    // FIXME: When we parse "var a, b: Int" we create multiple
    // PatternBindingDecls, which is convenience elsewhere but
    // unfortunate here, where it causes us to emit multiple
    // initializers.
    if (pbd->getLoc() == lastLoc)
      continue;

    lastLoc = pbd->getLoc();
    SmallVector<VarDecl *, 4> vars;
    pbd->getPattern()->collectVariables(vars);
    Optional<InFlightDiagnostic> diag;
    switch (vars.size()) {
    case 0:
      break;

    case 1: {
      diag.emplace(tc.diagnose(vars[0]->getLoc(), diag::note_no_in_class_init_1,
                               vars[0]->getName()));
      break;
    }

    case 2:
      diag.emplace(tc.diagnose(pbd->getLoc(), diag::note_no_in_class_init_2,
                               vars[0]->getName(), vars[1]->getName()));
      break;

    case 3:
      diag.emplace(tc.diagnose(pbd->getLoc(), diag::note_no_in_class_init_3plus,
                               vars[0]->getName(), vars[1]->getName(), 
                               vars[2]->getName(), false));
      break;

    default:
      diag.emplace(tc.diagnose(pbd->getLoc(), diag::note_no_in_class_init_3plus,
                               vars[0]->getName(), vars[1]->getName(), 
                               vars[2]->getName(), true));
      break;
    }

    if (diag) {
      if (auto defaultValueSuggestion
                 = buildDefaultInitializerString(tc, classDecl, 
                                                 pbd->getPattern())) {
        SourceLoc afterLoc = Lexer::getLocForEndOfToken(tc.Context.SourceMgr,
                                                        pbd->getEndLoc());
        diag->fixItInsert(afterLoc, " = " + *defaultValueSuggestion);
      }
    }
  }
}

void TypeChecker::addImplicitConstructors(NominalTypeDecl *decl) {
  // We can only synthesize implicit constructors for classes and structs.
  if (!isa<ClassDecl>(decl) && !isa<StructDecl>(decl))
    return;

  // If we already added implicit initializers, we're done.
  if (decl->addedImplicitInitializers())
    return;

  // Check whether there is a user-declared constructor or an instance
  // variable.
  bool FoundInstanceVar = false;
  bool FoundUninitializedVars = false;
  bool FoundDesignatedInit = false;
  decl->setAddedImplicitInitializers();
  for (auto member : decl->getMembers()) {
    if (auto ctor = dyn_cast<ConstructorDecl>(member)) {
      if (ctor->isDesignatedInit()) {
        FoundDesignatedInit = true;
        break;
      }
      continue;
    }

    if (auto var = dyn_cast<VarDecl>(member)) {
      if (var->hasStorage() && !var->isStatic())
        FoundInstanceVar = true;
      continue;
    }

    if (auto pbd = dyn_cast<PatternBindingDecl>(member)) {
      if (pbd->hasStorage() && !pbd->isStatic() && !isDefaultInitializable(pbd))
        FoundUninitializedVars = true;
      continue;
    }
  }

  // If we found a designated initializer, don't add any implicit
  // initializers.
  if (FoundDesignatedInit)
    return;

  if (isa<StructDecl>(decl)) {
    // For a struct, we add a memberwise constructor.

    // Create the implicit memberwise constructor.
    auto ctor = createImplicitConstructor(*this, decl,
                                          ImplicitConstructorKind::Memberwise);
    decl->addMember(ctor);

    // If we found a stored property, add a default constructor.
    if (FoundInstanceVar && !FoundUninitializedVars)
      defineDefaultConstructor(decl);

    return;
  }
 
  // For a class with a superclass, automatically define overrides
  // for all of the superclass's designated initializers.
  // FIXME: Currently skipping generic classes.
  auto classDecl = cast<ClassDecl>(decl);
  if (classDecl->hasSuperclass() && !classDecl->isGenericContext() &&
      !classDecl->getSuperclass()->isSpecialized()) {
    // We can't define these overrides if we have any uninitialized
    // stored properties.
    if (FoundUninitializedVars) {
      diagnoseClassWithoutInitializers(*this, classDecl);
      return;
    }

    auto superclassTy = classDecl->getSuperclass();
    for (auto member : lookupConstructors(superclassTy, classDecl)) {
      auto superclassCtor = dyn_cast<ConstructorDecl>(member);
      if (!superclassCtor || !superclassCtor->isDesignatedInit()
          || superclassCtor->isRequired() || superclassCtor->isInvalid())
        continue;

      // We have a designated initializer. Create an override of it.
      if (auto ctor = createdesignatedInitOverride(
                        *this, classDecl, superclassCtor,
                        designatedInitKind::Chaining)) {
        classDecl->addMember(ctor);
      }
    }

    return;
  }

  // For a class with no superclass, automatically define a default
  // constructor.

  // ... unless there are uninitialized stored properties.
  if (FoundUninitializedVars) {
    diagnoseClassWithoutInitializers(*this, classDecl);
    return;
  }

  defineDefaultConstructor(decl);
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

  Pattern *selfPat = buildImplicitSelfParameter(CD->getLoc(), CD);

  auto *DD = new (Context) DestructorDecl(Context.Id_deinit, CD->getLoc(),
                                          selfPat, CD);

  DD->setImplicit();

  // Type-check the constructor declaration.
  typeCheckDecl(DD, /*isFirstPass=*/true);

  // Create an empty body for the destructor.
  DD->setBody(BraceStmt::create(Context, CD->getLoc(), { }, CD->getLoc()));
  CD->addMember(DD);
}

void TypeChecker::addImplicitStructConformances(StructDecl *SD) {
  // Type-check the protocol conformances of the struct decl to instantiate its
  // derived conformances.
  DeclChecker(*this, false, false)
    .checkExplicitConformance(SD, SD->getDeclaredTypeInContext());
}

void TypeChecker::addImplicitEnumConformances(EnumDecl *ED) {
  // Type-check the raw values of the enum.
  for (auto elt : ED->getAllElements()) {
    assert(elt->hasRawValueExpr());
    Expr *typeChecked = elt->getRawValueExpr();
    Type rawTy = ArchetypeBuilder::mapTypeIntoContext(ED, ED->getRawType());
    bool error = typeCheckExpression(typeChecked, ED, rawTy, false);
    assert(!error); (void)error;
    elt->setTypeCheckedRawValueExpr(typeChecked);
  }
  
  // Type-check the protocol conformances of the enum decl to instantiate its
  // derived conformances.
  DeclChecker(*this, false, false)
    .checkExplicitConformance(ED, ED->getDeclaredTypeInContext());
}

void TypeChecker::defineDefaultConstructor(NominalTypeDecl *decl) {
  PrettyStackTraceDecl stackTrace("defining default constructor for",
                                  decl);

  // Clang-imported types should never get a default constructor, just a
  // memberwise one.
  if (decl->hasClangNode())
    return;

  // Verify that all of the instance variables of this type have default
  // constructors.
  for (auto member : decl->getMembers()) {
    // We only care about pattern bindings, and if the pattern has an
    // initializer, it can get a default initializer.
    auto patternBind = dyn_cast<PatternBindingDecl>(member);
    if (!patternBind || patternBind->getInit())
      continue;

    bool CantBuildInitializer = false;

    // Find the variables in the pattern. They'll each need to be
    // default-initialized.
    patternBind->getPattern()->forEachVariable([&](VarDecl *VD) {
      if (!VD->isStatic() && VD->hasStorage() && !VD->isInvalid())
        CantBuildInitializer = true;
    });

    // If there is a stored ivar without an initializer, we can't generate a
    // default initializer for this.
    if (CantBuildInitializer)
      return;
  }

  // For a class, check whether the superclass (if it exists) is
  // default-initializable.
  if (isa<ClassDecl>(decl)) {
    // We need to look for a default constructor.
    if (auto superTy = getSuperClassOf(decl->getDeclaredTypeInContext())) {
      // If there are no default ctors for our supertype, we can't do anything.
      auto ctors = lookupConstructors(superTy, decl);
      if (!ctors)
        return;

      // Check whether we have a constructor that can be called with an empty
      // tuple.
      bool foundDefaultConstructor = false;
      for (auto member : ctors) {
        // Dig out the parameter tuple for this constructor.
        auto ctor = dyn_cast<ConstructorDecl>(member);
        if (!ctor || ctor->isInvalid())
          continue;

        auto paramTuple = ctor->getArgumentType()->getAs<TupleType>();
        if (!paramTuple) {
          // A designated initializer other than a default initializer
          // means we can't call super.init().
          if (ctor->isDesignatedInit())
            return;

          continue;
        }

        // Check whether any of the tuple elements are missing an initializer.
        bool missingInit = false;
        for (auto &elt : paramTuple->getFields()) {
          if (elt.hasInit())
            continue;

          missingInit = true;
          break;
        }
        if (missingInit) {
          // A designated initializer other than a default initializer
          // means we can't call super.init().
          if (ctor->isDesignatedInit())
            return;

          continue;
        }

        // We found a constructor that can be invoked with an empty tuple.
        if (foundDefaultConstructor) {
          // We found two constructors that can be invoked with an empty tuple.
          foundDefaultConstructor = false;
          break;
        }

        foundDefaultConstructor = true;
      }

      // If our superclass isn't default constructible, we aren't either.
      if (!foundDefaultConstructor) return;
    }
  }

  // Create the default constructor.
  auto ctor = createImplicitConstructor(
                *this, decl, ImplicitConstructorKind::Default);

  // Add the constructor.
  decl->addMember(ctor);

  // Create an empty body for the default constructor. The type-check of the
  // constructor body will introduce default initializations of the members.
  ctor->setBody(BraceStmt::create(Context, SourceLoc(), { }, SourceLoc()));
}

static bool isDeclOfOperator(const Decl *D) {
  if (const ValueDecl *ValD = dyn_cast<ValueDecl>(D))
    return ValD->isOperator();
  return false;
}

static void validateAttributes(TypeChecker &TC, Decl *D) {
  const DeclAttributes &Attrs = D->getAttrs();
  auto *FDOrNull = dyn_cast<FuncDecl>(D);

  // Determine if VD is an operator declaration.
  bool isOperator = isDeclOfOperator(D);

  // Operators must be declared with 'func', not 'var'.
  if (isOperator) {
    if (!FDOrNull) {
      TC.diagnose(D->getLoc(), diag::operator_not_func);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // The unary prefix operator '&' is reserved and cannot be overloaded.
    if (FDOrNull->isUnaryOperator() && FDOrNull->getName().str() == "&"
        && !Attrs.isPostfix()) {
      TC.diagnose(D->getStartLoc(), diag::custom_operator_addressof);
      return;
    }
  }

  auto isInClassOrProtocolContext = [](Decl *vd) {
   Type ContextTy = vd->getDeclContext()->getDeclaredTypeInContext();
    if (!ContextTy)
      return false;
    return bool(ContextTy->getClassOrBoundGenericClass()) ||
           ContextTy->is<ProtocolType>();
  };

  if (auto objcAttr = Attrs.getAttribute<ObjCAttr>()) {
    // Only classes, class protocols, instance properties, methods,
    // constructors, and subscripts can be ObjC.
    Optional<Diag<>> error;
    if (isa<ClassDecl>(D)) {
      if (!D->getDeclContext()->isModuleScopeContext())
        error = diag::objc_class_not_top_level;
    } else if (isa<FuncDecl>(D) && isInClassOrProtocolContext(D)) {
      auto func = cast<FuncDecl>(D);
      if (isOperator)
        error = diag::invalid_objc_decl;
      else if (func->isGetterOrSetter()) {
        auto storage = func->getAccessorStorageDecl();
        if (!storage->isObjC()) {
          error = func->isGetter()
                    ? (isa<VarDecl>(storage) 
                         ? diag::objc_getter_for_nonobjc_property
                         : diag::objc_getter_for_nonobjc_subscript)
                    : (isa<VarDecl>(storage)
                         ? diag::objc_setter_for_nonobjc_property
                         : diag::objc_setter_for_nonobjc_subscript);
        }
      } else if (func->isAccessor()) {
        error= diag::objc_observing_accessor;
      }
    } else if (isa<ConstructorDecl>(D) && isInClassOrProtocolContext(D)) {
      /* ok */
    } else if (isa<DestructorDecl>(D)) {
      /* ok */
    } else if (isa<SubscriptDecl>(D) && isInClassOrProtocolContext(D)) {
      /* ok */
    } else if (auto *VD = dyn_cast<VarDecl>(D)) {
      if (!isInClassOrProtocolContext(VD))
        error = diag::invalid_objc_decl;
      else if (VD->isStatic())
        error = diag::objc_invalid_on_static_var;
      /* Otherwise it is an instance variable -- ok */
    } else if (isa<ProtocolDecl>(D)) {
      /* ok */
    } else {
      error = diag::invalid_objc_decl;
    }

    if (error) {
      TC.diagnose(D->getStartLoc(), *error);
      const_cast<ObjCAttr *>(objcAttr)->setInvalid();
      return;
    }

    // If there is a name, check whether the kind of name is
    // appropriate.
    if (auto objcName = objcAttr->getName()) {
      if (isa<ClassDecl>(D) || isa<ProtocolDecl>(D) || isa<VarDecl>(D)) {
        // Protocols, classes, and properties can only have nullary
        // names. Complain and recover by chopping off everything
        // after the first name.
        if (objcName->getNumArgs() > 0) {
          int which = isa<ClassDecl>(D)? 0 
                    : isa<ProtocolDecl>(D)? 1
                    : 2;
          SourceLoc firstNameLoc = objcAttr->getNameLocs().front();
          SourceLoc afterFirstNameLoc = 
            Lexer::getLocForEndOfToken(TC.Context.SourceMgr, firstNameLoc);
          TC.diagnose(firstNameLoc, diag::objc_name_req_nullary, which)
            .fixItRemoveChars(afterFirstNameLoc, objcAttr->getRParenLoc());
          const_cast<ObjCAttr *>(objcAttr)->setName(
            ObjCSelector(TC.Context, 0, objcName->getSelectorPieces()[0]));
        }
      } else if (isa<SubscriptDecl>(D)) {
      // Subscripts can never have names.
        TC.diagnose(objcAttr->getLParenLoc(), diag::objc_name_subscript);
        const_cast<ObjCAttr *>(objcAttr)->clearName();
      } else {
        // We have a function. Make sure that the number of parameters
        // matches the "number of colons" in the name.
        auto func = cast<AbstractFunctionDecl>(D);
        auto bodyPattern = func->getBodyParamPatterns()[1];
        unsigned numParameters;
        if (auto tuple = dyn_cast<TuplePattern>(bodyPattern))
          numParameters = tuple->getNumFields() - tuple->hasVararg();
        else
          numParameters = 1;

        unsigned numArgumentNames = objcName->getNumArgs();
        if (numArgumentNames != numParameters) {
          TC.diagnose(objcAttr->getNameLocs().front(), 
                      diag::objc_name_func_mismatch,
                      isa<FuncDecl>(func), 
                      numArgumentNames, 
                      numArgumentNames != 1,
                      numParameters,
                      numParameters != 1);
          D->getMutableAttrs().add(
            ObjCAttr::createUnnamed(TC.Context,
                                    objcAttr->AtLoc,
                                    objcAttr->Range.Start));
          D->getMutableAttrs().removeAttribute(objcAttr);
        }
      }
    }
  }

  // Ownership attributes (weak, unowned(safe), unowned(unsafe)).
  if (Attrs.hasOwnership()) {
    assert(unsigned(Attrs.isWeak()) +
           unsigned(Attrs.isUnowned()) +
           unsigned(Attrs.isUnmanaged()) == 1 &&
           "multiple ownership attributes present?");

    // Only 'var' declarations can have ownership.
    // TODO: captures, consts, etc.
    VarDecl *var = dyn_cast<VarDecl>(D);
    if (!var) {
      TC.diagnose(D->getStartLoc(), diag::invalid_ownership_decl,
                  (unsigned) Attrs.getOwnership());
      D->getMutableAttrs().clearOwnership();
      return;
    }

    TC.checkOwnershipAttr(var, Attrs.getOwnership());
  }

  if (Attrs.isInfix()) {
    // Only operator functions can be infix.
    if (!isOperator) {
      TC.diagnose(D->getStartLoc(), diag::infix_not_an_operator);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only binary operators can be infix.
    if (!FDOrNull || !FDOrNull->isBinaryOperator()) {
      TC.diagnose(Attrs.AtLoc, diag::invalid_infix_input);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isPostfix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(Attrs.getLoc(AK_postfix), diag::postfix_not_an_operator);
      D->getMutableAttrs().clearAttribute(AK_postfix);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only unary operators can be postfix.
    if (!FDOrNull || !FDOrNull->isUnaryOperator()) {
      TC.diagnose(Attrs.getLoc(AK_postfix), diag::invalid_postfix_input);
      D->getMutableAttrs().clearAttribute(AK_postfix);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isPrefix()) {
    // Only operator functions can be postfix.
    if (!isOperator) {
      TC.diagnose(Attrs.getLoc(AK_prefix), diag::prefix_not_an_operator);
      D->getMutableAttrs().clearAttribute(AK_prefix);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }

    // Only unary operators can be postfix.
    if (!FDOrNull || !FDOrNull->isUnaryOperator()) {
      TC.diagnose(Attrs.getLoc(AK_prefix), diag::invalid_prefix_input);
      D->getMutableAttrs().clearAttribute(AK_prefix);
      // FIXME: Set the 'isError' bit on the decl.
      return;
    }
  }

  if (Attrs.isConversion()) {
    // Only instance members with no non-defaulted parameters can be
    // conversions.
    FuncDecl *FD = dyn_cast<FuncDecl>(D);
    if (!FD) {
      TC.diagnose(Attrs.getLoc(AK_conversion), diag::conversion_not_function);
      D->getMutableAttrs().clearAttribute(AK_conversion);
    } else if (!FD->isInstanceMember()) {
      TC.diagnose(Attrs.getLoc(AK_conversion),
                  diag::conversion_not_instance_method, FD->getName());
      D->getMutableAttrs().clearAttribute(AK_conversion);
    } else if (!FD->getType()->is<ErrorType>()) {
      AnyFunctionType *BoundMethodTy
        = FD->getType()->castTo<AnyFunctionType>()->getResult()
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
        TC.diagnose(Attrs.getLoc(AK_conversion), diag::conversion_params,
                    FD->getName());
        D->getMutableAttrs().clearAttribute(AK_conversion);
      }
    }
  }

  if (Attrs.isTransparent()) {
    auto *AFD = dyn_cast<AbstractFunctionDecl>(D);
    auto *ED = dyn_cast<ExtensionDecl>(D);
    auto *VD = dyn_cast<VarDecl>(D);

    if ((!AFD && !ED & !VD) || (VD && VD->hasStorage())) {
      TC.diagnose(Attrs.getLoc(AK_transparent), diag::transparent_not_valid);
      D->getMutableAttrs().clearAttribute(AK_transparent);

    // Only Struct and Enum extensions can be transparent.
    } else if (ED) {
      CanType ExtendedTy = DeclContext::getExtendedType(ED);
      if (!isa<StructType>(ExtendedTy) && !isa<EnumType>(ExtendedTy)) {
        TC.diagnose(Attrs.getLoc(AK_transparent),
                    diag::transparent_on_invalid_extension);
        D->getMutableAttrs().clearAttribute(AK_transparent);
      }
    } else if (isa<FuncDecl>(D) && cast<FuncDecl>(D)->isAccessor() &&
               D->isImplicit()) {
      // @transparent is always ok on implicitly generated accessors: they can
      // be dispatched (even in classes) when the references are within the
      // class themself.
    } else {
      assert(AFD || VD);
      DeclContext *Ctx = AFD ? AFD->getParent() : VD->getDeclContext();
      // Protocol declarations cannot be transparent.
      if (isa<ProtocolDecl>(Ctx)) {
        TC.diagnose(Attrs.getLoc(AK_transparent),
                    diag::transparent_in_protocols_not_supported);
        D->getMutableAttrs().clearAttribute(AK_transparent);
      // Class declarations cannot be transparent.
      } else if (isa<ClassDecl>(Ctx)) {
        TC.diagnose(Attrs.getLoc(AK_transparent),
                    diag::transparent_in_classes_not_supported);
        D->getMutableAttrs().clearAttribute(AK_transparent);
      }
    }
  }

  // The requires_stored_property_inits attribute only applies to
  // classes.
  if (Attrs.requiresStoredPropertyInits() && !isa<ClassDecl>(D)) {
    TC.diagnose(Attrs.getLoc(AK_requires_stored_property_inits),
                diag::requires_stored_property_inits_nonclass);
    D->getMutableAttrs().clearAttribute(AK_requires_stored_property_inits);
  }

  // Only protocol members can be @optional.
  if (Attrs.isOptional()) {
    if (!isa<ProtocolDecl>(D->getDeclContext())) {
      TC.diagnose(Attrs.getLoc(AK_optional),
                  diag::optional_attribute_non_protocol);
      D->getMutableAttrs().clearAttribute(AK_optional);
    } else if (!cast<ProtocolDecl>(D->getDeclContext())->isObjC()) {
      TC.diagnose(Attrs.getLoc(AK_optional),
                  diag::optional_attribute_non_objc_protocol);
      D->getMutableAttrs().clearAttribute(AK_optional);
    } else if (isa<ConstructorDecl>(D)) {
      TC.diagnose(Attrs.getLoc(AK_optional),
                  diag::optional_attribute_initializer);
      D->getMutableAttrs().clearAttribute(AK_optional);
    }
  }

  // Only protocols that are @objc can have "unavailable" methods.
  if (auto AvAttr = Attrs.getUnavailable()) {
    if (auto PD = dyn_cast<ProtocolDecl>(D->getDeclContext())) {
      if (!PD->isObjC()) {
        TC.diagnose(AvAttr->getLocation(),
                    diag::unavailable_method_non_objc_protocol);
        D->getMutableAttrs().removeAttribute(AvAttr);
      }
    }
  }
}

bool TypeChecker::typeCheckConditionalPatternBinding(PatternBindingDecl *PBD,
                                                     DeclContext *dc) {
  validatePatternBindingDecl(*this, PBD);
  if (PBD->isInvalid())
    return true;
  
  assert(PBD->getInit() && "conditional pattern binding should always have init");
  if (!PBD->wasInitChecked()) {
    if (typeCheckBinding(PBD)) {
      PBD->setInvalid();
      if (!PBD->getPattern()->hasType()) {
        PBD->getPattern()->setType(ErrorType::get(Context));
        setBoundVarsTypeError(PBD->getPattern(), Context);
        return true;
      }
    }
  }
  
  DeclChecker(*this, false, false).visitBoundVars(PBD->getPattern());
  return false;
}

/// Fix the names in the given function to match those in the given target
/// name by adding Fix-Its to the provided in-flight diagnostic.
void TypeChecker::fixAbstractFunctionNames(InFlightDiagnostic &diag,
                                           AbstractFunctionDecl *func,
                                           DeclName targetName) {
  auto name = func->getFullName();
  
  // Fix the name of the function itself.
  if (name.getBaseName() != targetName.getBaseName()) {
    diag.fixItReplace(func->getLoc(), targetName.getBaseName().str());
  }
  
  // Fix the argument names that need fixing.
  assert(name.getArgumentNames().size()
           == targetName.getArgumentNames().size());
  auto pattern
    = func->getBodyParamPatterns()[func->getDeclContext()->isTypeContext()];
  auto tuplePattern = dyn_cast<TuplePattern>(
                        pattern->getSemanticsProvidingPattern());
  for (unsigned i = 0, n = name.getArgumentNames().size(); i != n; ++i) {
    auto origArg = name.getArgumentNames()[i];
    auto targetArg = targetName.getArgumentNames()[i];
    
    if (origArg == targetArg)
      continue;
    
    // Find the location to update or insert.
    SourceLoc loc;
    bool needColon = false;
    if (tuplePattern) {
      auto origPattern = tuplePattern->getFields()[i].getPattern();
      if (auto param = cast_or_null<ParamDecl>(origPattern->getSingleVar())) {
        // The parameter has an explicitly-specified API name, and it's wrong.
        if (param->getArgumentNameLoc() != param->getLoc() &&
            param->getArgumentNameLoc().isValid()) {
          // ... but the internal parameter name was right. Just zap the
          // incorrect explicit specialization.
          if (param->getName() == targetArg) {
            diag.fixItRemoveChars(param->getArgumentNameLoc(),
                                  param->getLoc());
            continue;
          }
          
          // Fix the API name.
          StringRef targetArgStr = targetArg.empty()? "_" : targetArg.str();
          diag.fixItReplace(param->getArgumentNameLoc(), targetArgStr);
          continue;
        }
        
        // The parameter did not specify a separate API name. Insert one.
        if (targetArg.empty())
          diag.fixItInsert(param->getLoc(), "_ ");
        else {
          llvm::SmallString<8> targetArgStr;
          targetArgStr += targetArg.str();
          targetArgStr += ' ';
          diag.fixItInsert(param->getLoc(), targetArgStr);
        }
        continue;
      }
      
      if (auto any = dyn_cast<AnyPattern>(
                       origPattern->getSemanticsProvidingPattern())) {
        if (any->isImplicit()) {
          needColon = true;
          loc = origPattern->getLoc();
        } else {
          needColon = false;
          loc = any->getLoc();
        }
      } else {
        loc = origPattern->getLoc();
        needColon = true;
      }
    } else if (auto paren = dyn_cast<ParenPattern>(pattern)) {
      loc = paren->getSubPattern()->getLoc();
      needColon = true;

      // FIXME: Representation doesn't let us fix this easily.
      if (targetArg.empty())
        continue;

    } else {
      loc = pattern->getLoc();
      needColon = true;
    }
    
    assert(!targetArg.empty() && "Must have a name here");
    llvm::SmallString<8> replacement;
    replacement += targetArg.str();
    if (needColon)
      replacement += ": ";
    
    diag.fixItInsert(loc, replacement);
  }
  
  // FIXME: Update the AST accordingly.
}
