//===--- TypeChecker.h - Type Checking Class --------------------*- C++ -*-===//
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
//
//  This file defines the TypeChecking class.
//
//===----------------------------------------------------------------------===//

#ifndef TYPECHECKING_H
#define TYPECHECKING_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Availability.h"
#include "swift/AST/AvailabilityScope.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Config.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/CompletionContextFinder.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <functional>

namespace swift {

class Decl;
class DeclAttribute;
class DiagnosticEngine;
class ExportContext;
class NominalTypeDecl;
class NormalProtocolConformance;
class RootProtocolConformance;
class TypeResolutionOptions;
class TypoCorrectionResults;
class ExprPattern;
enum class TypeResolutionStage : uint8_t;
enum class ExportabilityReason : unsigned;

namespace constraints {
  enum class ConstraintKind : char;
  class ConstraintSystem;
  class Solution;
  class SyntacticElementTarget;
  class SolutionResult;
}

/// Special-case type checking semantics for certain declarations.
enum class DeclTypeCheckingSemantics {
  /// A normal declaration.
  Normal,
  
  /// The type(of:) declaration, which performs a "dynamic type" operation,
  /// with different behavior for existential and non-existential arguments.
  TypeOf,
  
  /// The withoutActuallyEscaping(_:do:) declaration, which makes a nonescaping
  /// closure temporarily escapable.
  WithoutActuallyEscaping,

  /// The _openExistential(_:do:) declaration, which extracts the value inside
  /// an existential and passes it as a value of its own dynamic type.
  OpenExistential,
};

/// An individual result of a name lookup for a type.
struct LookupTypeResultEntry {
  TypeDecl *Member;
  Type MemberType;
  /// The associated type that the Member/MemberType were inferred for, but only
  /// if inference happened when creating this entry.
  AssociatedTypeDecl *InferredAssociatedType;
};

/// The result of name lookup for types.
class LookupTypeResult {
  /// The set of results found.
  SmallVector<LookupTypeResultEntry, 4> Results;

public:
  using iterator = SmallVectorImpl<LookupTypeResultEntry>::iterator;
  iterator begin() { return Results.begin(); }
  iterator end() { return Results.end(); }
  unsigned size() const { return Results.size(); }

  LookupTypeResultEntry operator[](unsigned index) const {
    return Results[index];
  }

  LookupTypeResultEntry front() const { return Results.front(); }
  LookupTypeResultEntry back() const { return Results.back(); }

  /// Add a result to the set of results.
  void addResult(LookupTypeResultEntry result) { Results.push_back(result); }

  /// Determine whether this result set is ambiguous.
  bool isAmbiguous() const {
    return Results.size() > 1;
  }

  /// Determine whether the result set is nonempty.
  explicit operator bool() const {
    return !Results.empty();
  }
};

/// Flags that can be used to control type checking.
enum class TypeCheckExprFlags {
  /// Whether we know that the result of the expression is discarded.  This
  /// disables constraints forcing an lvalue result to be loadable.
  IsDiscarded = 0x01,

  /// If set, this expression isn't embedded in a larger expression or
  /// statement. This should only be used for syntactic restrictions, and should
  /// not affect type checking itself.
  IsExprStmt = 0x02,

  /// Don't expand macros.
  DisableMacroExpansions = 0x04,

  /// If set, typeCheckExpression will avoid invalidating the AST if
  /// type-checking fails. Do not add new uses of this.
  AvoidInvalidatingAST = 0x08,
};

using TypeCheckExprOptions = OptionSet<TypeCheckExprFlags>;

inline TypeCheckExprOptions operator|(TypeCheckExprFlags flag1,
                                      TypeCheckExprFlags flag2) {
  return TypeCheckExprOptions(flag1) | flag2;
}

/// Flags that can be used to control name lookup.
enum class NameLookupFlags {
  /// Whether to ignore access control for this lookup, allowing inaccessible
  /// results to be returned.
  IgnoreAccessControl = 1 << 0,
  /// Whether to include results from outside the innermost scope that has a
  /// result.
  IncludeOuterResults = 1 << 1,
  // Whether to include results that are marked @inlinable or @usableFromInline.
  IncludeUsableFromInline = 1 << 2,
  /// This lookup should exclude any names introduced by macro expansions.
  ExcludeMacroExpansions = 1 << 3,
  /// Whether to include members that would otherwise be filtered out because
  /// they come from a module that has not been imported.
  IgnoreMissingImports = 1 << 4,
};

/// A set of options that control name lookup.
using NameLookupOptions = OptionSet<NameLookupFlags>;

inline NameLookupOptions operator|(NameLookupFlags flag1,
                                   NameLookupFlags flag2) {
  return NameLookupOptions(flag1) | flag2;
}

/// Default options for member name lookup.
const NameLookupOptions defaultMemberLookupOptions;

/// Default options for member name lookup in the constraint solver.
/// Overloads which come from modules that have not been imported should be
/// deprioritized by ranking and diagnosed by MiscDiagnostics, so we allow
/// them to be found in constraint solver lookups to improve diagnostics
/// overall.
const NameLookupOptions defaultConstraintSolverMemberLookupOptions(
    NameLookupFlags::IgnoreMissingImports);

/// Default options for member type lookup.
const NameLookupOptions defaultMemberTypeLookupOptions;

/// Default options for unqualified name lookup.
const NameLookupOptions defaultUnqualifiedLookupOptions;

/// Describes the result of comparing two entities, of which one may be better
/// or worse than the other, or they are unordered.
enum class Comparison {
  /// Neither entity is better than the other.
  Unordered,
  /// The first entity is better than the second.
  Better,
  /// The first entity is worse than the second.
  Worse
};

/// A conditional conformance that implied some other requirements. That is, \c
/// ConformingType conforming to \c Protocol may have required additional
/// requirements to be satisfied.
///
/// This is designed to be used in a stack of such requirements, which can be
/// formatted with \c diagnoseConformanceStack.
struct ParentConditionalConformance {
  Type ConformingType;
  ProtocolDecl *Protocol;

  /// Format the stack \c conformances as a series of notes that trace a path of
  /// conditional conformances that lead to some other failing requirement (that
  /// is not in \c conformances).
  ///
  /// The end of \c conformances is the active end of the stack, i.e. \c
  /// conformances[0] is a conditional conformance that requires \c
  /// conformances[1], etc.
  static void
  diagnoseConformanceStack(DiagnosticEngine &diags, SourceLoc location,
                           ArrayRef<ParentConditionalConformance> conformances);
};

class CheckGenericArgumentsResult {
public:
  struct RequirementFailureInfo {
    /// The failed requirement.
    Requirement Req;

    /// The failed requirement with substitutions applied.
    Requirement SubstReq;

    /// The chain of conditional conformances that leads to the failed
    /// requirement \c Req. Accordingly, \c Req is a conditional requirement of
    /// the last conformance in the chain (if any).
    SmallVector<ParentConditionalConformance, 2> ReqPath;
  };

private:
  CheckRequirementsResult Kind;
  std::optional<RequirementFailureInfo> ReqFailureInfo;

  CheckGenericArgumentsResult(
      CheckRequirementsResult Kind,
      std::optional<RequirementFailureInfo> ReqFailureInfo)
      : Kind(Kind), ReqFailureInfo(ReqFailureInfo) {}

public:
  static CheckGenericArgumentsResult createSuccess() {
    return CheckGenericArgumentsResult(CheckRequirementsResult::Success,
                                       std::nullopt);
  }

  static CheckGenericArgumentsResult createSubstitutionFailure() {
    return CheckGenericArgumentsResult(
        CheckRequirementsResult::SubstitutionFailure, std::nullopt);
  }

  static CheckGenericArgumentsResult createRequirementFailure(
      Requirement Req, Requirement SubstReq,
      SmallVector<ParentConditionalConformance, 2> ReqPath) {
    return CheckGenericArgumentsResult(
        CheckRequirementsResult::RequirementFailure,
        RequirementFailureInfo{Req, SubstReq, ReqPath});
  }

  const RequirementFailureInfo &getRequirementFailureInfo() const {
    assert(Kind == CheckRequirementsResult::RequirementFailure);

    return ReqFailureInfo.value();
  }

  CheckRequirementsResult getKind() const { return Kind; }
};

/// Describes the kind of checked cast operation being performed.
enum class CheckedCastContextKind {
  /// None: we're just establishing how to perform the checked cast. This
  /// is useful when we don't care to produce any diagnostics.
  None,
  /// A forced cast, with "as!".
  ForcedCast,
  /// A conditional cast, with "as?".
  ConditionalCast,
  /// An "is" expression.
  IsExpr,
  /// An "is" pattern.
  IsPattern,
  /// An enum-element pattern.
  EnumElementPattern,
  /// Coerce to checked cast. Used when we verify if it is possible to
  /// suggest to convert a coercion to a checked cast.
  Coercion,
};

namespace TypeChecker {
// DANGER: callers must verify that elementType satisfies the requirements of
// the Wrapped generic parameter, as this function will not do so!
Type getOptionalType(SourceLoc loc, Type elementType);

/// Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression.  Context is the DeclContext used
/// for the lookup.
Expr *resolveDeclRefExpr(UnresolvedDeclRefExpr *UDRE, DeclContext *Context);

/// Check for invalid existential types in the given declaration.
void checkExistentialTypes(Decl *decl);

/// Check for invalid existential types in the given statement.
void checkExistentialTypes(ASTContext &ctx, Stmt *stmt, DeclContext *DC);

/// Check for invalid existential types in the underlying type of
/// the given type alias.
void checkExistentialTypes(ASTContext &ctx, TypeAliasDecl *typeAlias);

/// Check for invalid existential types in the given generic requirement
/// list.
void checkExistentialTypes(ASTContext &ctx,
                           TrailingWhereClause *whereClause);

/// Check for invalid existential types in the given generic requirement
/// list.
void checkExistentialTypes(ASTContext &ctx,
                           GenericParamList *genericParams);

/// Substitute the given base type into the type of the given nested type,
/// producing the effective type that the nested type will have.
///
/// \param member The member whose type projection is being computed.
/// \param baseTy The base type that will be substituted for the 'Self' of the
/// member.
/// \param useArchetypes Whether to use context archetypes for outer generic
/// parameters if the class is nested inside a generic function.
Type substMemberTypeWithBase(TypeDecl *member, Type baseTy,
                             bool useArchetypes = true);

/// Determine whether this is a "pass-through" typealias, which has the
/// same type parameters as the nominal type it references and specializes
/// the underlying nominal type with exactly those type parameters.
/// For example, the following typealias \c GX is a pass-through typealias:
///
/// \code
/// struct X<T, U> { }
/// typealias GX<A, B> = X<A, B>
/// \endcode
///
/// whereas \c GX2 and \c GX3 are not pass-through because \c GX2 has
/// different type parameters and \c GX3 doesn't pass its type parameters
/// directly through.
///
/// \code
/// typealias GX2<A> = X<A, A>
/// typealias GX3<A, B> = X<B, A>
/// \endcode
bool isPassThroughTypealias(TypeAliasDecl *typealias, NominalTypeDecl *nominal);

/// Determine whether one type is a subtype of another.
///
/// \param t1 The potential subtype.
/// \param t2 The potential supertype.
/// \param dc The context of the check.
///
/// \returns true if \c t1 is a subtype of \c t2.
bool isSubtypeOf(Type t1, Type t2, DeclContext *dc);

/// Determine whether one type is implicitly convertible to another.
///
/// \param t1 The potential source type of the conversion.
///
/// \param t2 The potential destination type of the conversion.
///
/// \param dc The context of the conversion.
///
/// \param unwrappedIUO If non-null, will be set to indicate whether the
/// conversion force-unwrapped an implicitly-unwrapped optional.
///
/// \returns true if \c t1 can be implicitly converted to \c t2.
bool isConvertibleTo(Type t1, Type t2, DeclContext *dc,
                     bool *unwrappedIUO = nullptr);

/// Determine whether one type is explicitly convertible to another,
/// i.e. using an 'as' expression.
///
/// \param t1 The potential source type of the conversion.
///
/// \param t2 The potential destination type of the conversion.
///
/// \param dc The context of the conversion.
///
/// \returns true if \c t1 can be explicitly converted to \c t2.
bool isExplicitlyConvertibleTo(Type t1, Type t2, DeclContext *dc);

/// Determine whether one type is bridged to another type.
///
/// \param t1 The potential source type of the conversion.
///
/// \param t2 The potential destination type of the conversion.
///
/// \param dc The context of the conversion.
///
/// \param unwrappedIUO If non-null, will be set to indicate whether the
/// conversion force-unwrapped an implicitly-unwrapped optional.
///
/// \returns true if \c t1 can be explicitly converted to \c t2.
bool isObjCBridgedTo(Type t1, Type t2, DeclContext *dc,
                     bool *unwrappedIUO = nullptr);

/// Return true if performing a checked cast from one type to another
/// with the "as!" operator could possibly succeed.
///
/// \param t1 The potential source type of the cast.
///
/// \param t2 The potential destination type of the cast.
///
/// \param dc The context of the cast.
///
/// \returns true if a checked cast from \c t1 to \c t2 may succeed, and
/// false if it will certainly fail, e.g. because the types are unrelated.
bool checkedCastMaySucceed(Type t1, Type t2, DeclContext *dc);

/// Determine whether a constraint of the given kind can be satisfied
/// by the two types.
///
/// \param t1 The first type of the constraint.
///
/// \param t2 The second type of the constraint.
///
/// \param openArchetypes If true, archetypes are replaced with type
/// variables, and the result can be interpreted as whether or not the
/// two types can possibly equal at runtime.
///
/// \param dc The context of the conversion.
///
/// \param unwrappedIUO   If non-null, will be set to \c true if the coercion
/// or bridge operation force-unwraps an implicitly-unwrapped optional.
///
/// \returns true if \c t1 and \c t2 satisfy the constraint.
bool typesSatisfyConstraint(Type t1, Type t2, bool openArchetypes,
                            constraints::ConstraintKind kind, DeclContext *dc,
                            bool *unwrappedIUO = nullptr);

/// If the inputs to an apply expression use a consistent "sugar" type
/// (that is, a typealias or shorthand syntax) equivalent to the result type
/// of the function, set the result type of the expression to that sugar type.
Expr *substituteInputSugarTypeForResult(ApplyExpr *E);

/// Type check a \c StmtConditionElement.
/// Sets \p isFalsable to \c true if the condition might evaluate to \c false,
/// otherwise leaves \p isFalsable untouched.
/// \returns \c true if there was an error type checking, \c false otherwise.
bool typeCheckStmtConditionElement(StmtConditionElement &elt, bool &isFalsable,
                                   DeclContext *dc);

/// Returns the unique decl ref identified by the expr according to the
/// requirements of the \c #_hasSymbol() condition type.
ConcreteDeclRef getReferencedDeclForHasSymbolCondition(Expr *E);

void typeCheckASTNode(ASTNode &node, DeclContext *DC,
                      bool LeaveBodyUnchecked = false);

/// Try to apply the result builder transform of the given builder type
/// to the body of the function.
///
/// \returns \c None if the builder transformation cannot be applied at all,
/// e.g., because of a \c return statement. Otherwise, returns either the
/// fully type-checked body of the function (on success) or a \c nullptr
/// value if an error occurred while type checking the transformed body.
std::optional<BraceStmt *> applyResultBuilderBodyTransform(FuncDecl *func,
                                                           Type builderType);

bool typeCheckTapBody(TapExpr *expr, DeclContext *DC);

Type typeCheckParameterDefault(Expr *&defaultValue, DeclContext *DC,
                               Type paramType, bool isAutoClosure,
                               bool atCallerSide);

void typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD);

void typeCheckDecl(Decl *D);

void addImplicitDynamicAttribute(Decl *D);
void checkDeclAttributes(Decl *D);
void checkClosureAttributes(ClosureExpr *closure);
void checkParameterList(ParameterList *params, DeclContext *owner);

void diagnoseDuplicateBoundVars(Pattern *pattern);

void diagnoseDuplicateCaptureVars(CaptureListExpr *expr);

Type checkReferenceOwnershipAttr(VarDecl *D, Type interfaceType,
                                 ReferenceOwnershipAttr *attr);

/// Checks if the given witness can structurally match the requirement, ie.
/// whether it has a matching kind like static vs. non-static, matching decl,
/// etc. It does not check if the types of the witness is able to satisfy the
/// requirement.
/// If there is a matching failure, returns a `false`, otherwise return `true`.
bool witnessStructureMatches(ValueDecl *req, const ValueDecl *witness);

/// Infer default value witnesses for all requirements in the given protocol.
void inferDefaultWitnesses(ProtocolDecl *proto);

/// For a generic requirement in a protocol, make sure that the requirement
/// set didn't add any requirements to Self or its associated types.
void checkProtocolSelfRequirements(ValueDecl *decl);

/// All generic parameters of a generic function must be referenced in the
/// declaration's type, otherwise we have no way to infer them.
void checkReferencedGenericParams(GenericContext *dc);

/// Ensure we don't re-declare any generic parameters in the current scope,
/// or shadow a generic parameter from an outer scope.
void checkShadowedGenericParams(GenericContext *dc);

/// Diagnose a requirement failure.
///
/// \param errorLoc The location at which an error shall be emitted.
/// \param noteLoc The location at which any notes shall be emitted.
/// \param targetTy The type whose generic arguments caused the requirement
/// failure.
/// \param genericParams The generic parameters that were substituted.
/// \param substitutions The substitutions that caused the requirement failure.
void diagnoseRequirementFailure(
    const CheckGenericArgumentsResult::RequirementFailureInfo &reqFailureInfo,
    SourceLoc errorLoc, SourceLoc noteLoc, Type targetTy,
    ArrayRef<GenericTypeParamType *> genericParams,
    TypeSubstitutionFn substitutions);

/// Check the given generic parameter substitutions against the given
/// requirements and report on any requirement failures in detail for
/// diagnostic needs.
CheckGenericArgumentsResult
checkGenericArgumentsForDiagnostics(ArrayRef<Requirement> requirements,
                                    TypeSubstitutionFn substitutions);

/// Checks whether the generic requirements imposed on the nested type
/// declaration \p decl (if present) are in agreement with the substitutions
/// that are needed to spell it as a member of the given parent type
/// \p parentTy.
///
/// For example, given
/// \code
/// struct S<X> {}
/// extension S where X == Bool {
///   struct Inner {}
/// }
/// \endcode
/// \c Inner cannot be referenced on \c S<Int>, because its contextual
/// requirement \c X \c == \c Bool is not satisfied by the substitution
/// \c [X \c = \c Int].
///
/// Similarly, \c typealias \c Y below is a viable type witness in the
/// conformance of \c S to \c P, because its contextual requirement
/// \c Self.X \c == \c Bool is satisfied by the substitution
/// \c [Self \c = \c S].
/// \code
/// protocol P {
///   associatedtype X
///   associatedtype Y
/// }
/// extension P where X == Bool {
///   typealias Y = Bool
/// }
///
/// struct S: P {
///   typealias X = Bool
/// }
/// \endcode
///
/// \param module The module to use for conformance lookup.
/// \param contextSig The generic signature that should be used to map
/// \p parentTy into context. We pass a generic signature to secure on-demand
/// computation of the associated generic environment.
///
/// \returns \c true on success.
bool checkContextualRequirements(GenericTypeDecl *decl, Type parentTy,
                                 SourceLoc loc, GenericSignature contextSig);

/// Add any implicitly-defined constructors required for the given
/// struct, class or actor.
void addImplicitConstructors(NominalTypeDecl *typeDecl);

/// Fold the given sequence expression into an (unchecked) expression
/// tree.
Expr *foldSequence(SequenceExpr *expr, DeclContext *dc);

/// Type check the given expression.
///
/// \param expr The expression to type-check, which will be modified in
/// place.
///
/// \param contextualInfo The type that the expression is being converted to,
/// or null if the expression is standalone. When convertType is specified, this indicates
/// what the conversion is doing.  This allows diagnostics generation to
/// produce more specific and helpful error messages when the conversion fails
/// to be possible.
///
/// \param options Options that control how type checking is performed.
///
/// \returns The type of the top-level expression, or Type() if an
///          error occurred.
Type typeCheckExpression(Expr *&expr, DeclContext *dc,
                         constraints::ContextualTypeInfo contextualInfo = {},
                         TypeCheckExprOptions options = TypeCheckExprOptions());

std::optional<constraints::SyntacticElementTarget>
typeCheckExpression(constraints::SyntacticElementTarget &target,
                    TypeCheckExprOptions options = TypeCheckExprOptions(),
                    DiagnosticTransaction *diagnosticTransaction);

std::optional<constraints::SyntacticElementTarget>
typeCheckTarget(constraints::SyntacticElementTarget &target,
                TypeCheckExprOptions options = TypeCheckExprOptions(),
                DiagnosticTransaction *diagnosticTransaction);

/// Remove any solutions from the provided vector that require more fixes than
/// the best score or don't contain a type for the code completion token.
void filterSolutionsForCodeCompletion(
    SmallVectorImpl<constraints::Solution> &solutions,
    CompletionContextFinder &contextAnalyzer);

/// Type check the given expression and provide results back to code completion
/// via specified callback.
///
/// This method is designed to be used for code completion which means that
/// it doesn't mutate given expression, even if there is a single valid
/// solution, and constraint solver is allowed to produce partially correct
/// solutions. Such solutions can have any number of holes in them.
///
/// \returns `true` if target was applicable and it was possible to infer
/// types for code completion, `false` otherwise.
bool typeCheckForCodeCompletion(
    constraints::SyntacticElementTarget &target, bool needsPrecheck,
    llvm::function_ref<void(const constraints::Solution &)> callback);

/// Check the key-path expression.
///
/// Returns the type of the last component of the key-path.
std::optional<Type> checkObjCKeyPathExpr(DeclContext *dc, KeyPathExpr *expr,
                                         bool requireResultType = false);

/// Type check whether the given type declaration includes members of
/// unsupported recursive value types.
///
/// \param decl The declaration to be type-checked. This process will not
/// modify the declaration.
void checkDeclCircularity(NominalTypeDecl *decl);

/// Type check whether an extension matches its Objective-C interface, if it
/// has one.
///
/// \param D The declaration to check.
void checkObjCImplementation(Decl *D);

/// Type check whether the given switch statement exhaustively covers
/// its domain.
///
/// \param stmt The switch statement to be type-checked.  No modification of
/// the statement occurs.
/// \param DC The decl context containing \p stmt.
/// \param limitChecking The checking process relies on the switch statement
/// being well-formed.  If it is not, pass true to this flag to run a limited
/// form of analysis.
void checkSwitchExhaustiveness(const SwitchStmt *stmt, const DeclContext *DC,
                               bool limitChecking);

/// Type check the given expression as a condition, which converts
/// it to a logic value.
///
/// \param expr The expression to type-check, which will be modified in place
/// to return a logic value (builtin i1).
///
/// \returns true if an error occurred, false otherwise.
bool typeCheckCondition(Expr *&expr, DeclContext *dc);

/// Determine the semantics of a checked cast operation.
///
/// \param fromType       The source type of the cast.
/// \param toType         The destination type of the cast.
/// \param contextKind    The cast context in which this is being typechecked.
/// \param dc             The context of the cast.
///
/// \returns a CheckedCastKind indicating the semantics of the cast. If the
/// cast is invalid, Unresolved is returned. If the cast represents an implicit
/// conversion, Coercion is returned.
CheckedCastKind typeCheckCheckedCast(Type fromType, Type toType,
                                     CheckedCastContextKind contextKind,
                                     DeclContext *dc);

/// Find the Objective-C class that bridges between a value of the given
/// dynamic type and the given value type.
///
/// \param dc The declaration context from which we will look for
/// bridging.
///
/// \param dynamicType A dynamic type from which we are bridging. Class and
/// Objective-C protocol types can be used for bridging.
///
/// \param valueType The value type being queried, e.g., String.
///
/// \returns the Objective-C class type that represents the value
/// type as an Objective-C class, e.g., \c NSString represents \c
/// String, or a null type if there is no such type or if the
/// dynamic type isn't something we can start from.
Type getDynamicBridgedThroughObjCClass(DeclContext *dc, Type dynamicType,
                                       Type valueType);

/// Resolve ambiguous pattern/expr productions inside a pattern using
/// name lookup information. Must be done before type-checking the pattern.
Pattern *resolvePattern(Pattern *P, DeclContext *dc, bool isStmtCondition);

/// Type check the given pattern.
///
/// \returns the type of the pattern, which may be an error type if an
/// unrecoverable error occurred. If the options permit it, the type may
/// involve \c UnresolvedType (for patterns with no type information) and
/// unbound generic types.
Type typeCheckPattern(ContextualPattern pattern);

/// Attempt to simplify an ExprPattern into a BoolPattern or
/// OptionalSomePattern. Returns \c nullptr if the pattern could not be
/// simplified.
NullablePtr<Pattern> trySimplifyExprPattern(ExprPattern *EP, Type patternTy);

/// Coerce a pattern to the given type.
///
/// \param pattern The contextual pattern.
/// \param type the type to coerce the pattern to.
/// \param options Options that control the coercion.
/// \param tryRewritePattern A function that attempts to externally rewrite
/// the given pattern. This is used by the constraint system to take over
/// rewriting for ExprPatterns.
///
/// \returns the coerced pattern, or nullptr if the coercion failed.
Pattern *coercePatternToType(
    ContextualPattern pattern, Type type, TypeResolutionOptions options,
    llvm::function_ref<std::optional<Pattern *>(Pattern *, Type)>
        tryRewritePattern = [](Pattern *, Type) { return std::nullopt; });

bool typeCheckExprPattern(ExprPattern *EP, DeclContext *DC, Type type);

/// Coerce the specified parameter list of a ClosureExpr to the specified
/// contextual type.
void coerceParameterListToType(ParameterList *P, AnyFunctionType *FN);

/// Type-check an initialized variable pattern declaration.
bool typeCheckBinding(Pattern *&P, Expr *&Init, DeclContext *DC,
                      Type patternType,
                      PatternBindingDecl *PBD = nullptr,
                      unsigned patternNumber = 0,
                      TypeCheckExprOptions options = {});
bool typeCheckPatternBinding(PatternBindingDecl *PBD, unsigned patternNumber,
                             Type patternType = Type(),
                             TypeCheckExprOptions options = {});

/// Type-check a for-each loop's pattern binding, sequence, and where clause
/// together.
///
/// \returns true if a failure occurred.
bool typeCheckForEachPreamble(DeclContext *dc, ForEachStmt *stmt,
                              GenericEnvironment *packElementEnv);

/// Compute the set of captures for the given closure.
void computeCaptures(AbstractClosureExpr *ACE);

/// Check for invalid captures from stored property initializers.
void checkPatternBindingCaptures(IterableDeclContext *DC);

/// Change the context of closures in the given initializer
/// expression to the given context.
void contextualizeInitializer(Initializer *DC, Expr *init);
void contextualizeCallSideDefaultArgument(DeclContext *DC, Expr *init);
void contextualizeTopLevelCode(TopLevelCodeDecl *TLCD);

/// Retrieve the default type for the given protocol.
///
/// Some protocols, particularly those that correspond to literals, have
/// default types associated with them. This routine retrieves that default
/// type.
///
/// \returns the default type, or null if there is no default type for
/// this protocol.
Type getDefaultType(ProtocolDecl *protocol, DeclContext *dc);

/// Coerce the given expression to materializable type, if it
/// isn't already.
Expr *coerceToRValue(
    ASTContext &Context, Expr *expr,
    llvm::function_ref<Type(Expr *)> getType =
        [](Expr *expr) { return expr->getType(); },
    llvm::function_ref<void(Expr *, Type)> setType =
        [](Expr *expr, Type type) { expr->setType(type); });

/// Add implicit load expression to given AST, this is sometimes
/// more complicated than simplify wrapping given root in newly created
/// `LoadExpr`, because `ForceValueExpr` and `ParenExpr` supposed to appear
/// only at certain positions in AST.
Expr *addImplicitLoadExpr(
    ASTContext &Context, Expr *expr,
    std::function<Type(Expr *)> getType = [](Expr *E) { return E->getType(); },
    std::function<void(Expr *, Type)> setType =
        [](Expr *E, Type type) { E->setType(type); });

/// Determine whether the given type contains the given protocol.
///
/// \returns the conformance, if \c T conforms to the protocol \c Proto, or
/// an empty optional.
ProtocolConformanceRef containsProtocol(Type T, ProtocolDecl *Proto,
                                        bool allowMissing=false);

/// Check whether the type conforms to a given known protocol.
bool conformsToKnownProtocol(Type type, KnownProtocolKind protocol,
                             bool allowMissing = true);

/// This is similar to \c conformsToProtocol, but returns \c true for cases where
/// the type \p T could be dynamically cast to \p Proto protocol, such as a non-final
/// class where a subclass conforms to \p Proto.
///
/// \returns True if \p T conforms to the protocol \p Proto, false otherwise.
bool couldDynamicallyConformToProtocol(Type T, ProtocolDecl *Proto);

/// Check all of the conformances in the given context.
void checkConformancesInContext(IterableDeclContext *idc);

/// Check that the type of the given property conforms to NSCopying.
ProtocolConformanceRef checkConformanceToNSCopying(VarDecl *var);

/// \name Name lookup
///
/// Routines that perform name lookup.
///
/// @{

/// Perform unqualified name lookup at the given source location
/// within a particular declaration context.
///
/// \param dc The declaration context in which to perform name lookup.
/// \param name The name of the entity to look for.
/// \param loc The source location at which name lookup occurs.
/// \param options Options that control name lookup.
LookupResult lookupUnqualified(
    DeclContext *dc, DeclNameRef name, SourceLoc loc,
    NameLookupOptions options = defaultUnqualifiedLookupOptions);

/// Perform unqualified type lookup at the given source location
/// within a particular declaration context.
///
/// \param dc The declaration context in which to perform name lookup.
/// \param name The name of the entity to look for.
/// \param loc The source location at which name lookup occurs.
/// \param options Options that control name lookup.
LookupResult lookupUnqualifiedType(
    DeclContext *dc, DeclNameRef name, SourceLoc loc,
    NameLookupOptions options = defaultUnqualifiedLookupOptions);

/// Lookup a member in the given type.
///
/// \param dc The context that needs the member.
/// \param type The type in which we will look for a member.
/// \param name The name of the member to look for.
/// \param options Options that control name lookup.
///
/// \returns The result of name lookup.
LookupResult
lookupMember(DeclContext *dc, Type type, DeclNameRef name,
             SourceLoc loc = SourceLoc(),
             NameLookupOptions options = defaultMemberLookupOptions);

/// Look up a member type within the given type.
///
/// This routine looks for member types with the given name within the
/// given type.
///
/// \param dc The context that needs the member.
/// \param type The type in which we will look for a member type.
/// \param name The name of the member to look for.
/// \param options Options that control name lookup.
///
/// \returns The result of name lookup.
LookupTypeResult
lookupMemberType(DeclContext *dc, Type type, DeclNameRef name,
                 SourceLoc loc = SourceLoc(),
                 NameLookupOptions options = defaultMemberTypeLookupOptions);

/// Given an expression that's known to be an infix operator,
/// look up its precedence group.
PrecedenceGroupDecl *
lookupPrecedenceGroupForInfixOperator(DeclContext *dc, Expr *op, bool diagnose);

PrecedenceGroupLookupResult
lookupPrecedenceGroup(DeclContext *dc, Identifier name, SourceLoc nameLoc);

enum class UnsupportedMemberTypeAccessKind : uint8_t {
  None,
  TypeAliasOfUnboundGeneric,
  TypeAliasOfExistential,
  AssociatedTypeOfUnboundGeneric,
  AssociatedTypeOfExistential,
  NominalTypeOfUnboundGeneric
};

/// Check whether the given declaration can be written as a
/// member of the given base type.
UnsupportedMemberTypeAccessKind
isUnsupportedMemberTypeAccess(Type type, TypeDecl *typeDecl,
                              bool hasUnboundOpener,
                              bool isExtensionBinding = false);

/// @}

/// \name Overload resolution
///
/// Routines that perform overload resolution or provide diagnostics related
/// to overload resolution.
/// @{

/// Compare two declarations to determine whether one is more specialized
/// than the other.
///
/// A declaration is more specialized than another declaration if its type
/// is a subtype of the other declaration's type (ignoring the 'self'
/// parameter of function declarations) and if
Comparison compareDeclarations(DeclContext *dc, ValueDecl *decl1,
                               ValueDecl *decl2);

/// Checks whether the first decl is a refinement of the second
/// decl, meaning that the second decl can always be used in place
/// of the first one and the expression will still type check.
bool isDeclRefinementOf(ValueDecl *declA, ValueDecl *declB);

/// Build a reference to a declaration, where name lookup returned
/// the given set of declarations.
Expr *buildRefExpr(ArrayRef<ValueDecl *> Decls, DeclContext *UseDC,
                   DeclNameLoc NameLoc, bool Implicit,
                   FunctionRefInfo functionRefInfo);
/// @}

/// Retrieve a specific, known protocol.
///
/// \param loc The location at which we need to look for the protocol.
/// \param kind The known protocol we're looking for.
///
/// \returns null if the protocol is not available. This represents a
/// problem with the Standard Library.
ProtocolDecl *getProtocol(ASTContext &ctx, SourceLoc loc,
                          KnownProtocolKind kind);

/// Retrieve the literal protocol for the given expression.
///
/// \returns the literal protocol, if known and available, or null if the
/// expression does not have an associated literal protocol.
ProtocolDecl *getLiteralProtocol(ASTContext &ctx, Expr *expr);

DeclName getObjectLiteralConstructorName(ASTContext &ctx,
                                         ObjectLiteralExpr *expr);

/// Get the module appropriate for looking up standard library types.
///
/// This is "Swift", if that module is imported, or the current module if
/// we're parsing the standard library.
ModuleDecl *getStdlibModule(const DeclContext *dc);

Expr *buildDefaultInitializer(Type type);

/// \name Resilience diagnostics

bool diagnoseInlinableDeclRefAccess(SourceLoc loc, const ValueDecl *D,
                                    const ExportContext &where);

/// Given that a declaration is used from a particular context which
/// exposes it in the interface of the current module, diagnose if it cannot
/// reasonably be shared.
bool diagnoseDeclRefExportability(SourceLoc loc,
                                  const ValueDecl *D,
                                  const ExportContext &where);

/// Given that a conformance is used from a particular context which
/// exposes it in the interface of the current module, diagnose if the
/// conformance is SPI or visible via an implementation-only import.
bool diagnoseConformanceExportability(SourceLoc loc,
                                      const RootProtocolConformance *rootConf,
                                      const ExtensionDecl *ext,
                                      const ExportContext &where,
                                      bool useConformanceAvailabilityErrorsOpt = false);

/// \name Availability checking
///
/// Routines that perform API availability checking and type checking of
/// potentially unavailable API elements
/// @{

/// Returns true if the availability of the witness
/// is sufficient to safely conform to the requirement in the context
/// the provided conformance. On return, requiredAvailability holds th
/// availability levels required for conformance.
bool isAvailabilitySafeForConformance(
    const ProtocolDecl *proto, const ValueDecl *requirement,
    const ValueDecl *witness, const DeclContext *dc,
    AvailabilityRange &requiredAvailability);

/// Returns the most refined `AvailabilityContext` for the given location.
/// If `MostRefined` is not `nullptr`, it will be set to the most refined scope
/// that contains the given location.
AvailabilityContext
availabilityAtLocation(SourceLoc loc, const DeclContext *DC,
                       const AvailabilityScope **MostRefined = nullptr);

/// Returns an over-approximation of the range of operating system versions
/// that could the passed-in location could be executing upon for
/// the target platform. If MostRefined != nullptr, set to the most-refined
/// scope found while approximating.
AvailabilityRange overApproximateAvailabilityAtLocation(
    SourceLoc loc, const DeclContext *DC,
    const AvailabilityScope **MostRefined = nullptr);

/// Walk the AST to build the tree of AvailabilityScopes.
void buildAvailabilityScopes(SourceFile &SF);

/// Build the hierarchy of AvailabilityScopes for the entire
/// source file, if it has not already been built. Returns the root
/// AvailabilityScope for the source file.
AvailabilityScope *getOrBuildAvailabilityScope(SourceFile *SF);

/// Returns a diagnostic indicating why the declaration cannot be annotated
/// with an @available() attribute indicating it is potentially unavailable
/// or None if this is allowed.
std::optional<Diagnostic>
diagnosticIfDeclCannotBePotentiallyUnavailable(const Decl *D);

/// Returns a diagnostic indicating why the declaration cannot be annotated
/// with an @available() attribute indicating it is unavailable or None if this
/// is allowed.
std::optional<Diagnostic> diagnosticIfDeclCannotBeUnavailable(const Decl *D);

bool checkAvailability(
    SourceRange ReferenceRange, AvailabilityRange RequiredAvailability,
    const DeclContext *ReferenceDC,
    llvm::function_ref<InFlightDiagnostic(StringRef, llvm::VersionTuple)>
        Diagnose);

bool checkAvailability(SourceRange ReferenceRange,
                       AvailabilityRange RequiredAvailability,
                       Diag<StringRef, llvm::VersionTuple> Diag,
                       const DeclContext *ReferenceDC);

void checkConcurrencyAvailability(SourceRange ReferenceRange,
                                  const DeclContext *ReferenceDC);
/// @}

/// Checks an "ignored" expression to see if it's okay for it to be ignored.
///
/// An ignored expression is one that is not nested within a larger
/// expression or statement.
void checkIgnoredExpr(Expr *E);

/// Type check a 'distributed actor' declaration.
void checkDistributedActor(SourceFile *SF, NominalTypeDecl *decl);

/// Type check a single 'distributed func' declaration.
///
/// Returns `true` if there was an error.
bool checkDistributedFunc(FuncDecl *func);

/// If `LangOptions::DebugForbidTypecheckPrefixes` is set and the given decl
/// name starts with any of those prefixes, an llvm fatal error is triggered.
/// This is for testing purposes.
void checkForForbiddenPrefix(ASTContext &C, DeclBaseName Name);

/// Check error handling in the given type-checked top-level code.
void checkTopLevelEffects(TopLevelCodeDecl *D);
void checkFunctionEffects(AbstractFunctionDecl *D);
void checkInitializerEffects(Initializer *I, Expr *E);
void checkCallerSideDefaultArgumentEffects(DeclContext *I, Expr *E);
void checkEnumElementEffects(EnumElementDecl *D, Expr *expr);
void checkPropertyWrapperEffects(PatternBindingDecl *binding, Expr *expr);

/// Whether the given expression can throw, and if so, the thrown type.
std::optional<Type> canThrow(ASTContext &ctx, Expr *expr);

/// Whether the given for..each statement can throw, and if so, the thrown
/// error type.
std::optional<Type> canThrow(ASTContext &ctx, ForEachStmt *forEach);

/// Determine the error type that is thrown out of the body of the given
/// do-catch statement.
///
/// The error type is used in the catch clauses and, for a nonexhausive
/// do-catch, is implicitly rethrown out of the do...catch block.
Type catchErrorType(DeclContext *dc, DoCatchStmt *stmt);

/// Given two error types, merge them into the "union" of both error types
/// that is a supertype of both error types.
///
/// The \c simplifyType function is applied to any types that involve type
/// variables, to substitute away the type variables when possible. It need
/// not substitute all type variables, though.
Type errorUnion(Type type1, Type type2,
                llvm::function_ref<Type(Type)> simplifyType);

/// Retrieve the "next" function that should be used for iteration in a
/// for..in loop.
FuncDecl *getForEachIteratorNextFunction(
    DeclContext *dc, SourceLoc loc, bool isAsync);

/// If an expression references 'self.init' or 'super.init' in an
/// initializer context, returns the implicit 'self' decl of the constructor.
/// Otherwise, return nil.
VarDecl *getSelfForInitDelegationInConstructor(DeclContext *DC,
                                               UnresolvedDotExpr *UDE);

/// Diagnose assigning variable to itself.
bool diagnoseSelfAssignment(const Expr *E);

/// Builds a string representing a "default" generic argument list for
/// \p typeDecl. In general, this means taking the bound of each generic
/// parameter. The \p getPreferredType callback can be used to provide a
/// different type from the bound.
///
/// It may not always be possible to find a single appropriate type for a
/// particular parameter (say, if it has two bounds). In this case, an
/// Xcode-style placeholder will be used instead.
///
/// Returns true if the arguments list could be constructed, false if for
/// some reason it could not.
bool getDefaultGenericArgumentsString(
    SmallVectorImpl<char> &buf, const GenericTypeDecl *typeDecl,
    llvm::function_ref<Type(const GenericTypeParamType *)> getPreferredType =
        [](const GenericTypeParamType *) { return Type(); });

/// Attempt to omit needless words from the name of the given declaration.
std::optional<DeclName> omitNeedlessWords(AbstractFunctionDecl *afd);

/// Attempt to omit needless words from the name of the given declaration.
std::optional<Identifier> omitNeedlessWords(VarDecl *var);

/// Calculate edit distance between declaration names.
unsigned getCallEditDistance(DeclNameRef writtenName, DeclName correctedName,
                             unsigned maxEditDistance);

enum : unsigned {
  /// Never consider a candidate that's this distance away or worse.
  UnreasonableCallEditDistance = 8,

  /// Don't consider candidates that score worse than the given distance
  /// from the best candidate.
  MaxCallEditDistanceFromBestCandidate = 1
};

/// Check for a typo correction.
void performTypoCorrection(DeclContext *DC, DeclRefKind refKind,
                           Type baseTypeOrNull,
                           NameLookupOptions lookupOptions,
                           TypoCorrectionResults &corrections,
                           GenericSignature genericSig = GenericSignature(),
                           unsigned maxResults = 4);

/// Check if the given decl has a @_semantics attribute that gives it
/// special case type-checking behavior.
DeclTypeCheckingSemantics getDeclTypeCheckingSemantics(ValueDecl *decl);

/// Infers the differentiability parameter indices for the given
/// original or derivative `AbstractFunctionDecl`.
///
/// The differentiability parameters are inferred to be:
/// - All parameters of the function that conform to `Differentiable`.
/// - If the function result type is a function type (i.e. the function has
///   a curried method type), then also all parameters of the function result
///   type that conform to `Differentiable`.
///
/// Used by `@differentiable` and `@derivative` attribute type-checking.
IndexSubset *
inferDifferentiabilityParameters(AbstractFunctionDecl *AFD,
                                 GenericEnvironment *derivativeGenEnv);

/// Require that the library intrinsics for working with Optional<T>
/// exist.
bool requireOptionalIntrinsics(ASTContext &ctx, SourceLoc loc);

/// Require that the library intrinsics for working with
/// UnsafeMutablePointer<T> exist.
bool requirePointerArgumentIntrinsics(ASTContext &ctx, SourceLoc loc);

/// Require that the library intrinsics for creating
/// array literals exist.
bool requireArrayLiteralIntrinsics(ASTContext &ctx, SourceLoc loc);

/// Gets the \c UnresolvedMemberExpr at the base of a chain of member accesses.
/// If \c expr is not part of a member chain or the base is something other than
/// an \c UnresolvedMemberExpr, \c nullptr is returned.
UnresolvedMemberExpr *getUnresolvedMemberChainBase(Expr *expr);

/// Checks whether a result builder type has a well-formed result builder
/// method with the given name. If provided and non-empty, the argument labels
/// are verified against any candidates.
ResultBuilderOpSupport
checkBuilderOpSupport(Type builderType, DeclContext *dc, Identifier fnName,
                      ArrayRef<Identifier> argLabels = {},
                      SmallVectorImpl<ValueDecl *> *allResults = nullptr);

/// Checks whether a result builder type has a well-formed result builder
/// method with the given name. If provided and non-empty, the argument labels
/// are verified against any candidates.
///
/// This will return \c true even if the builder method is unavailable. Use
/// \c checkBuilderOpSupport if availability should be checked.
bool typeSupportsBuilderOp(Type builderType, DeclContext *dc, Identifier fnName,
                           ArrayRef<Identifier> argLabels = {},
                           SmallVectorImpl<ValueDecl *> *allResults = nullptr);

/// Forces all changes specified by the module's access notes file to be
/// applied to this declaration. It is safe to call this function more than
/// once.
void applyAccessNote(ValueDecl *VD);

/// Returns true if the given type conforms to `Differentiable` in the
/// module of `dc`. If `tangentVectorEqualsSelf` is true, returns true iff
/// the given type additionally satisfies `Self == Self.TangentVector`.
bool isDifferentiable(Type type, bool tangentVectorEqualsSelf, DeclContext *dc,
                      std::optional<TypeResolutionStage> stage);

/// Emits diagnostics if the given function type's parameter/result types are
/// not compatible with the ext info. Returns whether an error was diagnosed.
bool diagnoseInvalidFunctionType(FunctionType *fnTy, SourceLoc loc,
                                 std::optional<FunctionTypeRepr *> repr,
                                 DeclContext *dc,
                                 std::optional<TypeResolutionStage> stage);

/// Walk the parallel structure of a type with user-provided placeholders and
/// an inferred type produced by the type checker. Where placeholders can be
/// found, suggest the corresponding inferred type.
///
/// For example,
///
/// \code
///  func foo(_ x: [_] = [0])
/// \endcode
///
/// Has a written type of `(ArraySlice (Placeholder))` and an inferred type of
/// `(ArraySlice Int)`, so we walk to `Placeholder` and `Int` in each type and
/// suggest replacing `_` with `Int`.
///
/// \param writtenType The interface type usually derived from a user-written
/// type repr. \param inferredType The type inferred by the type checker.
void notePlaceholderReplacementTypes(Type writtenType, Type inferredType);
} // namespace TypeChecker

/// Returns the protocol requirement kind of the given declaration.
/// Used in diagnostics.
///
/// Asserts that the given declaration is a protocol requirement.
diag::RequirementKind getProtocolRequirementKind(ValueDecl *Requirement);

/// Returns true if the given method is an valid implementation of a
/// @dynamicCallable attribute requirement. The method is given to be defined
/// as one of the following: `dynamicallyCall(withArguments:)` or
/// `dynamicallyCall(withKeywordArguments:)`.
bool isValidDynamicCallableMethod(FuncDecl *decl,
                                  bool hasKeywordArguments);

/// Returns true if the given subscript method is an valid implementation of
/// the `subscript(dynamicMember:)` requirement for @dynamicMemberLookup.
/// The method is given to be defined as `subscript(dynamicMember:)`.
bool isValidDynamicMemberLookupSubscript(SubscriptDecl *decl,
                                         bool ignoreLabel = false);

/// Returns true if the given subscript method is an valid implementation of
/// the `subscript(dynamicMember:)` requirement for @dynamicMemberLookup.
/// The method is given to be defined as `subscript(dynamicMember:)` which
/// takes a single non-variadic parameter that conforms to
/// `ExpressibleByStringLiteral` protocol.
bool isValidStringDynamicMemberLookup(SubscriptDecl *decl,
                                      bool ignoreLabel = false);

/// Returns the KeyPath parameter type for a valid implementation of
/// the `subscript(dynamicMember: {Writable}KeyPath<...>)` requirement for
/// @dynamicMemberLookup.
/// The method is given to be defined as `subscript(dynamicMember:)` which
/// takes a single non-variadic parameter of `{Writable}KeyPath<T, U>` type.
///
/// Returns null if the given subscript is not a valid dynamic member lookup
/// implementation.
BoundGenericType *
getKeyPathTypeForDynamicMemberLookup(SubscriptDecl *decl,
                                     bool ignoreLabel = false);

/// Returns true if the given subscript method is an valid implementation of
/// the `subscript(dynamicMember: {Writable}KeyPath<...>)` requirement for
/// @dynamicMemberLookup.
/// The method is given to be defined as `subscript(dynamicMember:)` which
/// takes a single non-variadic parameter of `{Writable}KeyPath<T, U>` type.
bool isValidKeyPathDynamicMemberLookup(SubscriptDecl *decl,
                                       bool ignoreLabel = false);

/// Compute the wrapped value type for the given property that has attached
/// property wrappers, when the backing storage is known to have the given type.
///
/// \param var A property that has attached property wrappers.
/// \param backingStorageType The type of the backing storage property.
/// \param limit How many levels of unwrapping to perform, where 0 means to return the
/// \c backingStorageType directly and the maximum is the number of attached property wrappers
/// (which will produce the original property type). If not specified, defaults to the maximum.
Type computeWrappedValueType(const VarDecl *var, Type backingStorageType,
                             std::optional<unsigned> limit = std::nullopt);

/// Compute the projected value type for the given property that has attached
/// property wrappers when the backing storage is known to have the given type.
Type computeProjectedValueType(const VarDecl *var, Type backingStorageType);

/// Build a call to the init(wrappedValue:) or init(projectedValue:)
/// initializer of the property wrapper, filling in the given \c value
/// as the wrapped or projected value argument.
///
/// Optionally pass a callback that will get invoked with the innermost init
/// apply expression.
Expr *buildPropertyWrapperInitCall(
    const VarDecl *var, Type backingStorageType, Expr *value,
    PropertyWrapperInitKind initKind,
    llvm::function_ref<void(ApplyExpr *)> callback = [](ApplyExpr *) {});

/// Check if this var is the \c wrappedValue property belonging to
/// a property wrapper type declaration.
bool isWrappedValueOfPropWrapper(VarDecl *var);

/// Whether an overriding declaration requires the 'override' keyword.
enum class OverrideRequiresKeyword {
  /// The keyword is never required.
  Never,
  /// The keyword is always required.
  Always,
  /// The keyword can be implicit; it is not required.
  Implicit,
};

/// Determine whether overriding the given declaration requires a keyword.
OverrideRequiresKeyword overrideRequiresKeyword(ValueDecl *overridden);

/// Compute the type of a member that will be used for comparison when
/// performing override checking.
Type getMemberTypeForComparison(const ValueDecl *member,
                                const ValueDecl *derivedDecl = nullptr);

/// Determine whether the given declaration is an override by comparing type
/// information.
bool isOverrideBasedOnType(const ValueDecl *decl, Type declTy,
                           const ValueDecl *parentDecl);

/// Determine whether the given declaration is an operator defined in a
/// protocol. If \p type is not null, check specifically whether \p decl
/// could fulfill a protocol requirement for it.
bool isMemberOperator(FuncDecl *decl, Type type);

/// Returns `true` iff `AdditiveArithmetic` derived conformances are enabled.
bool isAdditiveArithmeticConformanceDerivationEnabled(SourceFile &SF);

/// Diagnose any Objective-C method overrides that aren't reflected
/// as overrides in Swift.
bool diagnoseUnintendedObjCMethodOverrides(SourceFile &sf);

/// Diagnose all conflicts between members that have the same
/// Objective-C selector in the same class.
///
/// \param sf The source file for which we are diagnosing conflicts.
///
/// \returns true if there were any conflicts diagnosed.
bool diagnoseObjCMethodConflicts(SourceFile &sf);

/// Diagnose all conflicts between extensions of the same class that have the
/// same Objective-C category name.
///
/// \param sf The source file for which we are diagnosing conflicts.
///
/// \returns true if there were any conflicts diagnosed.
bool diagnoseObjCCategoryConflicts(SourceFile &sf);

/// Diagnose any unsatisfied @objc optional requirements of
/// protocols that conflict with methods.
bool diagnoseObjCUnsatisfiedOptReqConflicts(SourceFile &sf);

/// Retrieve information about the given Objective-C method for
/// diagnostic purposes, to be used with OBJC_DIAG_SELECT in
/// DiagnosticsSema.def.
std::pair<unsigned, DeclName> getObjCMethodDiagInfo(
                                AbstractFunctionDecl *method);

/// Find the target of a break or continue statement.
///
/// \returns the target, if one was found, or \c nullptr if no such target
/// exists.
LabeledStmt *findBreakOrContinueStmtTarget(ASTContext &ctx,
                                           SourceFile *sourceFile,
                                           SourceLoc loc, Identifier targetName,
                                           SourceLoc targetLoc, bool isContinue,
                                           DeclContext *dc);

/// Check the correctness of a 'fallthrough' statement.
///
/// \returns true if an error occurred.
bool checkFallthroughStmt(FallthroughStmt *stmt);

/// Check for restrictions on the use of the @unknown attribute on a
/// case statement.
void checkUnknownAttrRestrictions(
    ASTContext &ctx, CaseStmt *caseBlock, bool &limitExhaustivityChecks);

/// Bind all of the pattern variables that occur within a case statement and
/// all of its case items to their "parent" pattern variables, forming chains
/// of variables with the same name.
///
/// Given a case such as:
/// \code
/// case .a(let x), .b(let x), .c(let x):
/// \endcode
///
/// Each case item contains a (different) pattern variable named.
/// "x". This function will set the "parent" variable of the
/// second and third "x" variables to the "x" variable immediately
/// to its left. A fourth "x" will be the body case variable,
/// whose parent will be set to the "x" within the final case
/// item.
///
/// Each of the "x" variables must eventually have the same type, and agree on
/// let vs. var. This function does not perform any of that validation, leaving
/// it to later stages.
void bindSwitchCasePatternVars(DeclContext *dc, CaseStmt *stmt);

/// If \p attr was added by an access note, wraps the error in
/// \c diag::wrap_invalid_attr_added_by_access_note and limits it as an access
/// note-related diagnostic should be.
InFlightDiagnostic softenIfAccessNote(const Decl *D, const DeclAttribute *attr,
                                      InFlightDiagnostic &diag);

/// Diagnose an error concerning an incorrect attribute (softening it if it's
/// caused by an access note) and emit a fix-it offering to remove it.
template<typename ...ArgTypes>
InFlightDiagnostic
diagnoseAttrWithRemovalFixIt(const Decl *D, const DeclAttribute *attr,
                             ArgTypes &&...Args) {
  assert(D);

  if (D->hasClangNode() && (!attr || !attr->getAddedByAccessNote())) {
    assert(false && "Clang importer propagated a bogus attribute");
    return InFlightDiagnostic();
  }

  auto &Diags = D->getASTContext().Diags;

  std::optional<InFlightDiagnostic> diag;
  if (!attr || !attr->getLocation().isValid())
    diag.emplace(Diags.diagnose(D, std::forward<ArgTypes>(Args)...));
  else
    diag.emplace(std::move(Diags.diagnose(attr->getLocation(),
                                          std::forward<ArgTypes>(Args)...)
                               .fixItRemove(attr->getRangeWithAt())));

  return softenIfAccessNote(D, attr, *diag);
}

/// Diagnose an error concerning an incorrect attribute (softening it if it's
/// caused by an access note), emit a fix-it offering to remove it, and mark the
/// attribute invalid so that it will be ignored by other parts of the compiler.
template<typename ...ArgTypes>
InFlightDiagnostic
diagnoseAndRemoveAttr(const Decl *D, const DeclAttribute *attr,
                      ArgTypes &&...Args) {
  if (attr)
    // FIXME: Due to problems with the design of DeclAttributes::getAttribute(),
    // many callers try to pass us const DeclAttributes. This is a hacky
    // workaround.
    const_cast<DeclAttribute *>(attr)->setInvalid();

  return diagnoseAttrWithRemovalFixIt(D, attr, std::forward<ArgTypes>(Args)...);
}

/// Look for closure discriminators within an AST.
class DiscriminatorFinder : public ASTWalker {
  unsigned FirstDiscriminator = AbstractClosureExpr::InvalidDiscriminator;
  unsigned NextDiscriminator = 0;

public:
  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Expansion;
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override;

  // Get the next available closure discriminator.
  unsigned getNextDiscriminator();

  unsigned getFirstDiscriminator() const {
    return FirstDiscriminator;
  }
};

using RequiredImportAccessLevelCallback =
    std::function<void(AttributedImport<ImportedModule>)>;

/// Make a note that uses of \p decl in \p dc require that the decl's defining
/// module be imported with an access level that is at least as permissive as \p
/// accessLevel.
void recordRequiredImportAccessLevelForDecl(
    const Decl *decl, const DeclContext *dc, AccessLevel accessLevel,
    RequiredImportAccessLevelCallback remark);

/// Report imports that are marked public but are not used in API.
void diagnoseUnnecessaryPublicImports(SourceFile &SF);

/// Emit or delay a diagnostic that suggests adding a missing import that is
/// necessary to bring \p decl into scope in the containing source file. If
/// delayed, the diagnostic will instead be emitted after type checking the
/// entire file and will include an appropriate fix-it. Returns true if a
/// diagnostic was emitted (and not delayed).
bool maybeDiagnoseMissingImportForMember(const ValueDecl *decl,
                                         const DeclContext *dc, SourceLoc loc);

/// Emit delayed diagnostics regarding imports that should be added to the
/// source file.
void diagnoseMissingImports(SourceFile &sf);
} // end namespace swift

#endif
