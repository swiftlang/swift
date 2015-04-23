//===--- TypeChecker.h - Type Checking Class --------------------*- C++ -*-===//
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
//  This file defines the TypeChecking class.
//
//===----------------------------------------------------------------------===//

#ifndef TYPECHECKING_H
#define TYPECHECKING_H

#include "swift/AST/AST.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Availability.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/ADT/SetVector.h"
#include <functional>

namespace swift {

class ArchetypeBuilder;
class GenericTypeResolver;
class NominalTypeDecl;
class TopLevelContext;
class TypeChecker;

namespace constraints {
  enum class ConstraintKind : char;
  class ConstraintSystem;
  class Solution;
}

/// \brief A mapping from substitutable types to the protocol-conformance
/// mappings for those types.
typedef llvm::DenseMap<SubstitutableType *,
                       SmallVector<ProtocolConformance *, 2>> ConformanceMap;

/// The result of name lookup.
class LookupResult {
  /// The set of results found.
  SmallVector<ValueDecl *, 4> Results;

  friend class TypeChecker;
  
public:
  typedef SmallVectorImpl<ValueDecl *>::iterator iterator;
  iterator begin() { return Results.begin(); }
  iterator end() { return Results.end(); }
  unsigned size() const { return Results.size(); }
  bool empty() const { return Results.empty(); }

  ValueDecl *operator[](unsigned index) const { return Results[index]; }

  ValueDecl *front() const { return Results.front(); }
  ValueDecl *back() const { return Results.back(); }

  /// Add a result to the set of results.
  void addResult(ValueDecl *result) { Results.push_back(result); }

  /// Determine whether the result set is nonempty.
  explicit operator bool() const {
    return !Results.empty();
  }

  /// Filter out any results that aren't accepted by the given predicate.
  void filter(const std::function<bool(ValueDecl *)> &pred);
};

/// The result of name lookup for types.
class LookupTypeResult {
  /// The set of results found.
  SmallVector<std::pair<TypeDecl *, Type>, 4> Results;

  friend class TypeChecker;

public:
  typedef SmallVectorImpl<std::pair<TypeDecl *, Type>>::iterator iterator;
  iterator begin() { return Results.begin(); }
  iterator end() { return Results.end(); }
  unsigned size() const { return Results.size(); }

  std::pair<TypeDecl *, Type> operator[](unsigned index) const {
    return Results[index];
  }

  std::pair<TypeDecl *, Type> front() const { return Results.front(); }
  std::pair<TypeDecl *, Type> back() const { return Results.back(); }

  /// Add a result to the set of results.
  void addResult(std::pair<TypeDecl *, Type> result) {
    Results.push_back(result);
  }

  /// \brief Determine whether this result set is ambiguous.
  bool isAmbiguous() const {
    return Results.size() > 1;
  }

  /// Determine whether the result set is nonempty.
  explicit operator bool() const {
    return !Results.empty();
  }
};

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

/// Specify how we handle the binding of underconstrained (free) type variables
/// within a solution to a constraint system.
enum class FreeTypeVariableBinding {
  /// Disallow any binding of such free type variables.
  Disallow,
  /// Allow the free type variables to persist in the solution.
  Allow,
  /// Bind the type variables to fresh generic parameters.
  GenericParameters
};

/// An abstract interface that can interact with the type checker during
/// the type checking of a particular expression.
class ExprTypeCheckListener {
public:
  virtual ~ExprTypeCheckListener();

  /// Callback invoked once the constraint system has been constructed.
  ///
  /// \param cs The constraint system that has been constructed.
  ///
  /// \param expr The pre-checked expression from which the constraint system
  /// was generated.
  ///
  /// \returns true if an error occurred that is not itself part of the
  /// constraint system, or false otherwise.
  virtual bool builtConstraints(constraints::ConstraintSystem &cs, Expr *expr);

  /// Callback invoked once the constraint system has been solved.
  ///
  /// \param solution The chosen solution.
  virtual void solvedConstraints(constraints::Solution &solution);

  /// Callback invokes once the chosen solution has been applied to the
  /// expression.
  ///
  /// The callback may further alter the expression, returning either a
  /// new expression (to replace the result) or a null pointer to indicate
  /// failure.
  virtual Expr *appliedSolution(constraints::Solution &solution,
                                Expr *expr);

  /// The callback is consulted before reporting the diagnostics in case
  /// typechecking fails.
  ///
  /// \returns true if diagnostic reporting should be suppressed.
  virtual bool suppressDiagnostics() const;
};

/// Flags that describe the context of type checking a pattern or
/// type.
enum TypeResolutionFlags {
  /// Whether to allow unspecified types within a pattern.
  TR_AllowUnspecifiedTypes = 0x01,

  /// Whether the pattern is variadic.
  TR_Variadic = 0x02,

  /// Whether the given type can override the type of a typed pattern.
  TR_OverrideType = 0x04,

  /// Whether to allow unbound generic types.
  TR_AllowUnboundGenerics = 0x08,

  /// Whether we are validating the type for SIL.
  TR_SILType = 0x10,

  /// Whether we are in the input type of a function, or under one level of
  /// tuple type.  This is not set for multi-level tuple arguments.
  TR_FunctionInput = 0x20,
  
  /// Whether this is the immediate input type to a function,
  TR_ImmediateFunctionInput = 0x40,

  /// Whether we are in the result type of a function.
  TR_FunctionResult = 0x80,
  
  /// Whether this is a resolution based on a non-inferred type pattern.
  TR_FromNonInferredPattern = 0x100,
  
  /// Whether we are the variable type in a for/in statement.
  TR_EnumerationVariable = 0x200,
  
  /// Whether we are looking only in the generic signature of the context
  /// we're searching, rather than the entire context.
  TR_GenericSignature = 0x800,

  /// Whether this type is the referent of a global type alias.
  TR_GlobalTypeAlias = 0x1000,

  /// Whether this type is the value carried in an enum case.
  TR_EnumCase = 0x2000,

  /// Whether this type is being used in an expression or local declaration.
  ///
  /// This affects what sort of dependencies are recorded when resolving the
  /// type.
  TR_InExpression = 0x4000,
  
  /// Whether this type resolution is guaranteed not to affect downstream files.
  TR_KnownNonCascadingDependency = 0x8000,

  /// Whether we should allow references to unavailable types.
  TR_AllowUnavailable = 0x10000,
  
  /// Whether this is the payload subpattern of an enum pattern.
  TR_EnumPatternPayload = 0x20000,
};

/// Option set describing how type resolution should work.
typedef OptionSet<TypeResolutionFlags> TypeResolutionOptions;

/// Strip the contextual options from the given type resolution options.
static inline TypeResolutionOptions
withoutContext(TypeResolutionOptions options) {
  options -= TR_ImmediateFunctionInput;
  options -= TR_FunctionInput;
  options -= TR_FunctionResult;
  options -= TR_EnumCase;
  return options;
}

/// Describes the reason why are we trying to apply @objc to a declaration.
///
/// Should only affect diagnostics.
enum class ObjCReason {
  DontDiagnose,
  ExplicitlyDynamic,
  ExplicitlyObjC,
  ExplicitlyIBOutlet,
  ExplicitlyNSManaged,
  MemberOfObjCProtocol
};

/// The Swift type checker, which takes a parsed AST and performs name binding,
/// type checking, and semantic analysis to produce a type-annotated AST.
class TypeChecker final : public LazyResolver {
public:
  ASTContext &Context;
  DiagnosticEngine &Diags;

  /// \brief The list of implicitly-defined functions created by the
  /// type checker.
  std::vector<AbstractFunctionDecl *> implicitlyDefinedFunctions;

  /// \brief The list of function definitions we've encountered.
  std::vector<AbstractFunctionDecl *> definedFunctions;

  /// The list of nominal type declarations that have been validated
  /// during type checking.
  llvm::SetVector<NominalTypeDecl *> ValidatedTypes;

  /// Caches whether a particular type is accessible from a particular file
  /// unit.
  ///
  /// This can't use CanTypes because typealiases may have more limited types
  /// than their underlying types.
  llvm::DenseMap<Type, Accessibility> TypeAccessibilityCache;
  
  // We delay validation of C and Objective-C type-bridging functions in the
  // standard library until we encounter a declaration that requires one. This
  // flag is set to 'true' once the bridge functions have been checked.
  bool HasCheckedBridgeFunctions = false;

  /// A list of closures for the most recently type-checked function, which we
  /// will need to compute captures for.
  std::vector<AnyFunctionRef> ClosuresWithUncomputedCaptures;

  /// Describes an attempt to capture a local function.
  struct LocalFunctionCapture {
    FuncDecl *LocalFunction;
    SourceLoc CaptureLoc;
  };

  /// A list of local function captures, which can only be verified
  /// once we have type-checked the bodies of all of the local functions that it
  /// might reference.
  ///
  /// FIXME: That we need this at all is very unfortunate. We should be able to
  /// check this as part of checking the non-local function (or other
  /// DeclContext) that encloses all of its local functions, but the type
  /// checker doesn't have any point where that happens. Moreover, this
  /// check wouldn't even be needed if we could handle captures within SILGen.
  std::vector<LocalFunctionCapture> LocalFunctionCaptures;
  
  /// A set of local functions from which C function pointers are derived.
  ///
  /// This is used to diagnose the use of local functions with captured context
  /// as C function pointers when the function's captures have not yet been
  /// computed.
  llvm::DenseMap<AnyFunctionRef, std::vector<Expr*>> LocalCFunctionPointers;

private:
  Type IntLiteralType;
  Type FloatLiteralType;
  Type BooleanLiteralType;
  Type CharacterLiteralType;
  Type UnicodeScalarType;
  Type ExtendedGraphemeClusterType;
  Type StringLiteralType;
  Type ArrayLiteralType;
  Type DictionaryLiteralType;
  Type ColorLiteralType;
  Type ImageLiteralType;
  Type StringType;
  Type Int8Type;
  Type UInt8Type;
  Type NSObjectType;
  Type NSErrorType;
  Type ExceptionType;

  /// The \c Swift.UnsafeMutablePointer<T> declaration.
  Optional<NominalTypeDecl *> ArrayDecl;
  
  /// A set of types that can be trivially mapped to Objective-C types.
  llvm::DenseSet<CanType> ObjCMappedTypes;

  /// A set of types that can be mapped to C integer types.
  llvm::DenseSet<CanType> CIntegerTypes;
  
  /// The set of expressions currently being analyzed for failures.
  llvm::DenseSet<Expr *> DiagnosedExprs;

  /// A set of types that are representable in Objective-C, but require
  /// non-trivial bridging.
  ///
  /// The value of the map is a flag indicating whether the bridged
  /// type can be optional.
  llvm::DenseMap<CanType, bool> ObjCRepresentableTypes;

  Module *StdlibModule = nullptr;

  /// The index of the next response metavariable to bind to a REPL result.
  unsigned NextResponseVariableIndex = 0;

  /// If true, the time it takes to type-check each function will be dumped
  /// to llvm::errs().
  bool DebugTimeFunctionBodies = false;

  /// A helper to construct and typecheck call to super.init().
  ///
  /// \returns NULL if the constructed expression does not typecheck.
  Expr* constructCallToSuperInit(ConstructorDecl *ctor,  ClassDecl *ClDecl);

public:
  TypeChecker(ASTContext &Ctx) : TypeChecker(Ctx, Ctx.Diags) { }
  TypeChecker(ASTContext &Ctx, DiagnosticEngine &Diags);
  ~TypeChecker();

  LangOptions &getLangOpts() const { return Context.LangOpts; }

  /// Dump the time it takes to type-check each function to llvm::errs().
  void enableDebugTimeFunctionBodies() {
    DebugTimeFunctionBodies = true;
  }
  
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    return Diags.diagnose(std::forward<ArgTypes>(Args)...);
  }

  Type getArraySliceType(SourceLoc loc, Type elementType);
  Type getDictionaryType(SourceLoc loc, Type keyType, Type valueType);
  Type getOptionalType(SourceLoc loc, Type elementType);
  Type getImplicitlyUnwrappedOptionalType(SourceLoc loc, Type elementType);
  Type getStringType(DeclContext *dc);
  Type getInt8Type(DeclContext *dc);
  Type getUInt8Type(DeclContext *dc);
  Type getNSObjectType(DeclContext *dc);
  Type getNSErrorType(DeclContext *dc);
  Type getExceptionType(DeclContext *dc, SourceLoc loc);

  /// \brief Try to resolve an IdentTypeRepr, returning either the referenced
  /// Type or an ErrorType in case of error.
  Type resolveIdentifierType(DeclContext *DC,
                             IdentTypeRepr *IdType,
                             TypeResolutionOptions options,
                             bool diagnoseErrors,
                             GenericTypeResolver *resolver);
  
  /// \brief Validate the given type.
  ///
  /// Type validation performs name binding, checking of generic arguments,
  /// and so on to determine whether the given type is well-formed and can
  /// be used as a type.
  ///
  /// \param Loc The type (with source location information) to validate.
  /// If the type has already been validated, returns immediately.
  ///
  /// \param DC The context that the type appears in.
  ///
  /// \param options Options that alter type resolution.
  ///
  /// \param resolver A resolver for generic types. If none is supplied, this
  /// routine will create a \c PartialGenericTypeToArchetypeResolver to use.
  ///
  /// \returns true if type validation failed, or false otherwise.
  bool validateType(TypeLoc &Loc, DeclContext *DC,
                    TypeResolutionOptions options = None,
                    GenericTypeResolver *resolver = nullptr);

  /// Check for unsupported protocol types in the given declaration.
  void checkUnsupportedProtocolType(Decl *decl);

  /// Check for unsupported protocol types in the given statement.
  void checkUnsupportedProtocolType(Stmt *stmt);

  /// Expose TypeChecker's handling of GenericParamList to SIL parsing.
  /// We pass in a vector of nested GenericParamLists and a vector of
  /// ArchetypeBuilders with the innermost GenericParamList in the beginning
  /// of the vector.
  bool handleSILGenericParams(SmallVectorImpl<ArchetypeBuilder *> &builders,
                              SmallVectorImpl<GenericParamList *> &gps,
                              DeclContext *DC);

  /// \brief Resolves a TypeRepr to a type.
  ///
  /// Performs name binding, checking of generic arguments, and so on in order
  /// to create a well-formed type.
  ///
  /// \param TyR The type representation to check.
  ///
  /// \param DC The context that the type appears in.
  ///
  /// \param options Options that alter type resolution.
  ///
  /// \param resolver A resolver for generic types. If none is supplied, this
  /// routine will create a \c PartialGenericTypeToArchetypeResolver to use.
  ///
  /// \returns a well-formed type or an ErrorType in case of an error.
  Type resolveType(TypeRepr *TyR, DeclContext *DC,
                   TypeResolutionOptions options,
                   GenericTypeResolver *resolver = nullptr);

  void validateDecl(ValueDecl *D, bool resolveTypeParams = false);

  /// Resolves the accessibility of the given declaration.
  void validateAccessibility(ValueDecl *D);

  /// Validate the given extension declaration, ensuring that it
  /// properly extends the nominal type it names.
  void validateExtension(ExtensionDecl *ext);

  /// \brief Force all members of an external decl, and also add its
  /// conformances.
  void forceExternalDeclMembers(NominalTypeDecl *NTD);

  /// Resolve a reference to the given type declaration within a particular
  /// context.
  ///
  /// This routine aids unqualified name lookup for types by performing the
  /// resolution necessary to rectify the declaration found by name lookup with
  /// the declaration context from which name lookup started.
  ///
  /// \param typeDecl The type declaration found by name lookup.
  /// \param fromDC The declaration context in which the name lookup occurred.
  /// \param isSpecialized Whether this type is immediately specialized.
  /// \param resolver The resolver for generic types.
  ///
  /// \returns the resolved type, or emits a diagnostic and returns null if the
  /// type cannot be resolved.
  Type resolveTypeInContext(TypeDecl *typeDecl, DeclContext *fromDC,
                            bool isSpecialized,
                            GenericTypeResolver *resolver = nullptr);

  /// \brief Substitute the given archetypes for their substitution types
  /// within the given type.
  ///
  /// \param IgnoreMissing Ignore missing mappings; useful when something else
  /// may establish those mappings later, e.g., as in protocol conformance.
  ///
  /// \returns The substituted type, or null if the substitution failed.
  ///
  /// FIXME: We probably want to have both silent and loud failure modes.
  /// However, the only possible failure now is from array slice types, which
  /// occur simply because we don't have Array<T> yet.
  Type substType(Module *module, Type T, TypeSubstitutionMap &Substitutions,
                 bool IgnoreMissing = false);

  /// \brief Apply generic arguments to the given type.
  ///
  /// \param type The unbound generic type to which to apply arguments.
  /// \param loc The source location for diagnostic reporting.
  /// \param dc The context where the arguments are applied.
  /// \param genericArgs The list of generic arguments to apply to the type.
  /// \param isGenericSignature True if we are looking only in the generic
  /// signature of the context.
  /// \param resolver The generic type resolver.
  ///
  /// \returns A BoundGenericType bound to the given arguments, or null on
  /// error.
  Type applyGenericArguments(Type type,
                             SourceLoc loc,
                             DeclContext *dc,
                             MutableArrayRef<TypeLoc> genericArgs,
                             bool isGenericSignature,
                             GenericTypeResolver *resolver);

  /// \brief Substitute the given base type into the type of the given member,
  /// producing the effective type that the member will have.
  ///
  /// \param module The module in which the substitution will be performed.
  /// \param member The member whose type projection is being computed.
  /// \param baseTy The base type that will be substituted for the 'Self' of the
  /// member.
  /// \param isTypeReference Whether this is a reference to a type declaration
  /// within a type context.
  Type substMemberTypeWithBase(Module *module, const ValueDecl *member,
                               Type baseTy, bool isTypeReference);

  /// \brief Retrieve the superclass type of the given type, or a null type if
  /// the type has no supertype.
  Type getSuperClassOf(Type type);

  /// \brief Determine whether one type is a subtype of another.
  ///
  /// \param t1 The potential subtype.
  /// \param t2 The potential supertype.
  /// \param dc The context of the check.
  ///
  /// \returns true if \c t1 is a subtype of \c t2.
  bool isSubtypeOf(Type t1, Type t2, DeclContext *dc);

  /// \brief Determine whether one type is implicitly convertible to another.
  ///
  /// \param t1 The potential source type of the conversion.
  ///
  /// \param t2 The potential destination type of the conversion.
  ///
  /// \param dc The context of the conversion.
  ///
  /// \returns true if \c t1 can be implicitly converted to \c t2.
  bool isConvertibleTo(Type t1, Type t2, DeclContext *dc);

  /// \brief Determine whether one type is explicitly convertible to another,
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

  /// \brief Return true if performing a checked cast from one type to another
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
  
  /// \brief Determine whether a constraint of the given kind can be satisfied
  /// by the two types.
  ///
  /// \param t1 The first type of the constrant.
  ///
  /// \param t2 The second type of the constrant.
  ///
  /// \param dc The context of the conversion.
  ///
  /// \returns true if \c t1 and \c t2 satisfy the constraint.
  bool typesSatisfyConstraint(Type t1, Type t2,
                              constraints::ConstraintKind kind,
                              DeclContext *dc);

  /// \brief Determine whether one type would be a valid substitution for an
  /// archetype.
  ///
  /// \param type The potential type.
  ///
  /// \param archetype The archetype for which type may (or may not) be
  /// substituted.
  ///
  /// \param dc The context of the check.
  ///
  /// \returns true if \c t1 is a valid substitution for \c t2.
  bool isSubstitutableFor(Type type, ArchetypeType *archetype, DeclContext *dc);

  /// If the inputs to an apply expression use a consistent "sugar" type
  /// (that is, a typealias or shorthand syntax) equivalent to the result type
  /// of the function, set the result type of the expression to that sugar type.
  Expr *substituteInputSugarTypeForResult(ApplyExpr *E);

  bool typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                          SourceLoc EndTypeCheckLoc);
  bool typeCheckAbstractFunctionBody(AbstractFunctionDecl *AFD);
  bool typeCheckFunctionBodyUntil(FuncDecl *FD, SourceLoc EndTypeCheckLoc);
  bool typeCheckConstructorBodyUntil(ConstructorDecl *CD,
                                     SourceLoc EndTypeCheckLoc);
  bool typeCheckDestructorBodyUntil(DestructorDecl *DD,
                                    SourceLoc EndTypeCheckLoc);

  void typeCheckClosureBody(ClosureExpr *closure);

  void typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD);

  void processREPLTopLevel(SourceFile &SF, TopLevelContext &TLC,
                           unsigned StartElem);
  Identifier getNextResponseVariableName(DeclContext *DC);

  void typeCheckDecl(Decl *D, bool isFirstPass);

  void checkDeclAttributesEarly(Decl *D);
  void checkDeclAttributes(Decl *D);
  void checkTypeModifyingDeclAttributes(VarDecl *var);

  void checkAutoClosureAttr(ParamDecl *D, AutoClosureAttr *attr);
  void checkNoEscapeAttr(ParamDecl *D, NoEscapeAttr *attr);
  void checkOwnershipAttr(VarDecl *D, OwnershipAttr *attr);

  void computeAccessibility(ValueDecl *D);

  virtual void resolveAccessibility(ValueDecl *VD) override {
    validateAccessibility(VD);
  }

  virtual void resolveDeclSignature(ValueDecl *VD) override {
    validateDecl(VD, true);
  }

  virtual void resolveExtension(ExtensionDecl *ext) override {
    validateExtension(ext);
    checkInheritanceClause(ext);
  }

  virtual void resolveImplicitConstructors(NominalTypeDecl *nominal) override {
    addImplicitConstructors(nominal);
  }

  virtual void
  resolveExternalDeclImplicitMembers(NominalTypeDecl *nominal) override {
    handleExternalDecl(nominal);
  }

  /// Determine whether the given (potentially constrained) protocol extension
  /// is usable for the given type.
  bool isProtocolExtensionUsable(DeclContext *dc, Type type,
                                 ExtensionDecl *protocolExtension) override;

  /// Validate the signature of a generic function.
  ///
  /// \param func The generic function.
  ///
  /// \returns true if an error occurred, or false otherwise.
  bool validateGenericFuncSignature(AbstractFunctionDecl *func);

  /// Revert the signature of a generic function to its pre-type-checked state,
  /// so that it can be type checked again when we have resolved its generic
  /// parameters.
  void revertGenericFuncSignature(AbstractFunctionDecl *func);

  /// Revert the dependent types within the given generic parameter list.
  void revertGenericParamList(GenericParamList *genericParams);

  /// Validate the given generic parameters to produce a generic
  /// signature.
  ///
  /// \param genericParams The generic parameters to validate.
  ///
  /// \param dc The declaration context in which to perform the validation.
  ///
  /// \param outerSignature The generic signature of the outer
  /// context, if not available as part of the \c dc argument (used
  /// for SIL parsing).
  ///
  /// \param inferRequirements When non-empty, callback that will be invoked
  /// to perform any additional requirement inference that contributes to the
  /// generic signature. Returns true if an error occurred.
  ///
  /// \param invalid Will be set true if an error occurs during validation.
  ///
  /// \returns the generic signature that captures the generic
  /// parameters and inferred requirements.
  GenericSignature *validateGenericSignature(
                      GenericParamList *genericParams,
                      DeclContext *dc,
                      GenericSignature *outerSignature,
                      std::function<bool(ArchetypeBuilder &)> inferRequirements,
                      bool &invalid);
                        
  /// Validate the signature of a generic type.
  ///
  /// \param nominal The generic type.
  ///
  /// \returns true if an error occurred, or false otherwise.
  bool validateGenericTypeSignature(NominalTypeDecl *nominal);

  /// Given a type that was produced within the given generic declaration
  /// context, produce the corresponding interface type.
  ///
  /// \param dc The declaration context in which the type was produced.
  ///
  /// \param type The type, which involves archetypes but not dependent types.
  ///
  /// \returns the type after mapping all archetypes to their corresponding
  /// dependent types.
  Type getInterfaceTypeFromInternalType(DeclContext *dc, Type type);

  /// Resolve the superclass of the given class.
  void resolveSuperclass(ClassDecl *classDecl) override;

  /// Resolve the raw type of the given enum.
  void resolveRawType(EnumDecl *enumDecl) override;

  /// Resolve the types in the inheritance clause of the given
  /// declaration context, which will be a nominal type declaration or
  /// extension declaration.
  void resolveInheritanceClause(DeclContext *dc) override;

  /// Check the inheritance clause of the given declaration.
  void checkInheritanceClause(Decl *decl,
                              GenericTypeResolver *resolver = nullptr);

  /// Retrieve the set of protocols to which this nominal type declaration
  /// directly conforms, i.e., as specified in its own inheritance clause.
  ///
  /// Protocols to which this nominal type declaration conforms via extensions
  /// or superclasses need to be extracted separately.
  ArrayRef<ProtocolDecl *> getDirectConformsTo(NominalTypeDecl *nominal);

  /// Retrieve the set of protocols to which this extension directly conforms.
  ArrayRef<ProtocolDecl *> getDirectConformsTo(ExtensionDecl *extension);

  /// \brief Add any implicitly-defined constructors required for the given
  /// struct or class.
  void addImplicitConstructors(NominalTypeDecl *typeDecl);

  /// \brief Add an implicitly-defined destructor, if there is no
  /// user-provided destructor.
  void addImplicitDestructor(ClassDecl *CD);

  /// \brief Add the RawOptionSet (todo:, Equatable, and Hashable) methods to an
  /// imported NS_OPTIONS struct.
  void addImplicitStructConformances(StructDecl *ED);
  
  /// \brief Add the RawRepresentable, Equatable, and Hashable methods to an
  /// enum with a raw type.
  void addImplicitEnumConformances(EnumDecl *ED);

  /// The specified AbstractStorageDecl \c storage was just found to satisfy
  /// the protocol property \c requirement.  Ensure that it has the full
  /// complement of accessors.
  void synthesizeWitnessAccessorsForStorage(AbstractStorageDecl *requirement,
                                            AbstractStorageDecl *storage);

  /// Provide storage and accessor implementations for the given property,
  /// which must be lazy.
  void completeLazyVarImplementation(VarDecl *lazyVar);

  /// \name Name lookup
  ///
  /// Routines that perform name lookup.
  ///
  /// During type checking, these routines should be used instead of
  /// \c MemberLookup and \c UnqualifiedLookup, because these routines will
  /// lazily introduce declarations and (FIXME: eventually) perform recursive
  /// type-checking that the AST-level lookup routines don't.
  ///
  /// @{
private:
  Optional<Type> boolType;
  
public:
  /// \brief Define the default constructor for the given struct or class.
  void defineDefaultConstructor(NominalTypeDecl *decl);

  /// \brief Fold the given sequence expression into an (unchecked) expression
  /// tree.
  Expr *foldSequence(SequenceExpr *expr, DeclContext *dc);
  
  /// \brief Type check the given expression.
  ///
  /// \param expr The expression to type-check, which will be modified in
  /// place.
  ///
  /// \param convertType The type that the expression is being converted to,
  /// or null if the expression is standalone.
  ///
  /// \param contextualType Contextual type information that can be applied to
  /// the expression. (This is distinct from convertType in that while it can
  /// inform the type of the expression, it won't result in a conversion
  /// constraint being applied to the expression.)
  ///
  /// \param discardedExpr True if the result of this expression will be
  /// discarded.
  ///
  /// \param allowFreeTypeVariables Whether free type variables are allowed in
  /// the solution, and what to do with them.
  ///
  /// \param listener If non-null, a listener that will be notified of important
  /// events in the type checking of this expression, and which can introduce
  /// additional constraints.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckExpression(Expr *&expr, DeclContext *dc,
                           Type convertType,
                           Type contextualType,
                           bool discardedExpr,
                           FreeTypeVariableBinding allowFreeTypeVariables
                             = FreeTypeVariableBinding::Disallow,
                           ExprTypeCheckListener *listener = nullptr);

  /// \brief Type check the given expression assuming that its children
  /// have already been fully type-checked.
  ///
  /// \param expr The expression to type-check, which will be modified in
  /// place.
  ///
  /// \param convertType The type that the expression is being converted to,
  /// or null if the expression is standalone.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckExpressionShallow(Expr *&expr, DeclContext *dc,
                                  Type convertType = Type());

  /// \brief "Nullify" an expression tree's type data, to make it suitable for
  /// re-typecheck operations.
  void eraseTypeData(Expr *&expr);

  /// \brief Type check the given expression as a condition, which converts
  /// it to a logic value.
  ///
  /// \param expr The expression to type-check, which will be modified in place
  /// to return a logic value (builtin i1).
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckCondition(Expr *&expr, DeclContext *dc);

  /// \brief Type check the given 'if' or 'while' statement condition, which
  /// either converts an expression to a logic value or bind variables to the
  /// contents of an Optional.
  ///
  /// \param cond The condition to type-check, which will be modified in place.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckCondition(StmtCondition &cond, DeclContext *dc);
  
  /// \brief Determine the semantics of a checked cast operation.
  ///
  /// \param fromType       The source type of the cast.
  /// \param toType         The destination type of the cast.
  /// \param dc             The context of the cast.
  /// \param diagLoc        The location at which to report diagnostics.
  /// \param diagFromRange  The source range of the input operand of the cast.
  /// \param diagToRange    The source range of the destination type.
  /// \param convertToType  A callback called when an implicit conversion
  ///                       to an intermediate type is needed.
  /// \param suppressDiagnostics
  ///                       True if the type check should simply fail instead
  ///                       of printing diagnostics.
  ///
  /// \returns a CheckedCastKind indicating the semantics of the cast. If the
  /// cast is invald, Unresolved is returned. If the cast represents an implicit
  /// conversion, Coercion is returned.
  CheckedCastKind typeCheckCheckedCast(Type fromType,
                                       Type toType,
                                       DeclContext *dc,
                                       SourceLoc diagLoc,
                                       SourceRange diagFromRange,
                                       SourceRange diagToRange,
                                       std::function<bool(Type)> convertToType,
                                       bool suppressDiagnostics);

  /// Retrieves the Objective-C type to which the given value type is
  /// bridged.
  ///
  /// \param dc The declaration context from which we will look for
  /// bridging.
  ///
  /// \param inExpression Whether this type is being used in an expression or
  /// local declaration. This affects what sort of dependencies are recorded
  /// when resolving the type.
  ///
  /// \param type The value type being queried, e.g., String.
  ///
  /// \returns the class type to which the given type is bridged, or null if it
  /// is not bridged.
  Type getBridgedToObjC(const DeclContext *dc, bool inExpression, Type type);

  /// Find the Objective-C class that bridges between a value of the given
  /// dynamic type and the given value type.
  ///
  /// \param dc The declaration context from which we will look for
  /// bridging.
  ///
  /// \param inExpression Whether this type is being used in an expression or
  /// local declaration. This affects what sort of dependencies are recorded
  /// when resolving the type.
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
  Type getDynamicBridgedThroughObjCClass(DeclContext *dc, bool inExpression,
                                         Type dynamicType,
                                         Type valueType);

  /// \brief Resolve ambiguous pattern/expr productions inside a pattern using
  /// name lookup information. Must be done before type-checking the pattern.
  Pattern *resolvePattern(Pattern *P, DeclContext *dc);
  
  /// Type check the given pattern.
  ///
  /// \param P The pattern to type check.
  /// \param dc The context in which type checking occurs.
  /// \param options Options that control type resolution.
  /// \param resolver A generic type resolver.
  ///
  /// \returns true if any errors occurred during type checking.
  bool typeCheckPattern(Pattern *P, DeclContext *dc,
                        TypeResolutionOptions options,
                        GenericTypeResolver *resolver = nullptr);

  /// Coerce a pattern to the given type.
  ///
  /// \param P The pattern, which may be modified by this coercion.
  /// \param dc The context in which this pattern occurs.
  /// \param type the type to coerce the pattern to.
  /// \param options Options describing how to perform this coercion.
  /// \param resolver The generic resolver to use.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool coercePatternToType(Pattern *&P, DeclContext *dc, Type type,
                           TypeResolutionOptions options,
                           GenericTypeResolver *resolver = nullptr);
  bool typeCheckExprPattern(ExprPattern *EP, DeclContext *DC,
                            Type type);

  /// Type-check an initialized variable pattern declaration.
  bool typeCheckBinding(PatternBindingDecl *D, unsigned PatternEntry);

  /// Type-check a for-each loop's pattern binding and sequence together.
  bool typeCheckForEachBinding(DeclContext *dc, ForEachStmt *stmt);

  /// \brief Compute the set of captures for the given function or closure.
  void computeCaptures(AnyFunctionRef AFR);

  /// \brief Change the context of closures in the given initializer
  /// expression to the given context.
  ///
  /// \returns true if any closures were found
  static bool contextualizeInitializer(Initializer *DC, Expr *init);
  static void contextualizeTopLevelCode(TopLevelContext &TLC,
                                        ArrayRef<Decl*> topLevelDecls);

  /// Return the type-of-reference of the given value.  This does not
  /// open values of polymorphic function type.
  ///
  /// \param baseType if non-null, return the type of a member reference to
  ///   this value when the base has the given type
  ///
  /// \param UseDC The context of the access.  Some variables have different
  ///   types depending on where they are used.
  ///
  /// \param wantInterfaceType Whether we want the interface type, if available.
  Type getUnopenedTypeOfReference(ValueDecl *value, Type baseType,
                                  DeclContext *UseDC,
                                  bool wantInterfaceType = false);

  /// Return the non-lvalue type-of-reference of the given value.
  Type getTypeOfRValue(ValueDecl *value, bool wantInterfaceType = false);

  /// \brief Retrieve the default type for the given protocol.
  ///
  /// Some protocols, particularly those that correspond to literals, have
  /// default types associated with them. This routine retrieves that default
  /// type.
  ///
  /// \returns the default type, or null if there is no default type for
  /// this protocol.
  Type getDefaultType(ProtocolDecl *protocol, DeclContext *dc);

  /// \brief Convert the given expression to the given type.
  ///
  /// \param expr The expression, which will be updated in place.
  /// \param type The type to convert to.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool convertToType(Expr *&expr, Type type, DeclContext *dc);
  
  /// \brief Coerce the given expression to an rvalue, if it isn't already.
  Expr *coerceToRValue(Expr *expr);
  
  /// \brief Coerce the given expression to materializable type, if it
  /// isn't already.
  Expr *coerceToMaterializable(Expr *expr);

  /// Require that the library intrinsics for working with Optional<T>
  /// exist.
  bool requireOptionalIntrinsics(SourceLoc loc);

  /// Require that the library intrinsics for working with
  /// UnsafeMutablePointer<T> exist.
  bool requirePointerArgumentIntrinsics(SourceLoc loc);

  /// Require that the library intrinsics for creating
  /// array literals exist.
  bool requireArrayLiteralIntrinsics(SourceLoc loc);

  /// \brief Retrieve the witness type with the given name.
  ///
  /// \param type The type that conforms to the given protocol.
  ///
  /// \param protocol The protocol through which we're looking.
  ///
  /// \param conformance The protocol conformance.
  ///
  /// \param name The name of the associated type.
  ///
  /// \param brokenProtocolDiag Diagnostic to emit if the type cannot be
  /// accessed.
  ///
  /// \return the witness type, or null if an error occurs.
  Type getWitnessType(Type type, ProtocolDecl *protocol,
                      ProtocolConformance *conformance,
                      Identifier name,
                      Diag<> brokenProtocolDiag);

  /// \brief Build a call to the witness with the given name and arguments.
  ///
  /// \param base The base expression, whose witness will be invoked.
  ///
  /// \param protocol The protocol to call through.
  ///
  /// \param conformance The conformance of the base type to the given
  /// protocol.
  ///
  /// \param name The name of the method to call.
  ///
  /// \param arguments The arguments to 
  ///
  /// \param brokenProtocolDiag Diagnostic to emit if the protocol is broken.
  ///
  /// \returns a fully type-checked call, or null if the protocol was broken.
  Expr *callWitness(Expr *base, DeclContext *dc,
                    ProtocolDecl *protocol,
                    ProtocolConformance *conformance,
                    DeclName name,
                    MutableArrayRef<Expr *> arguments,
                    Diag<> brokenProtocolDiag);
  
  /// \brief Determine whether the given type conforms to the given protocol.
  ///
  /// \param DC The context in which to check conformance. This affects, for
  /// example, extension visibility.
  ///
  /// \param InExpression If true, this conformance is being checked as part of
  /// type-checking an expression. This results in dependencies being treated
  /// slightly differently.
  ///
  /// \param Conformance If non-NULL, and the type does conform to the given
  /// protocol, this will be set to the protocol conformance mapping that
  /// maps the given type \c T to the protocol \c Proto. The mapping may be
  /// NULL, if the mapping is trivial due to T being either an archetype or
  /// an existential type that directly implies conformance to \c Proto.
  ///
  /// \param ComplainLoc If valid, then this function will emit diagnostics if
  /// T does not conform to the given protocol. The primary diagnostic will
  /// be placed at this location, with notes for each of the protocol
  /// requirements not satisfied.
  ///
  /// \returns true if T conforms to the protocol Proto, false otherwise.
  ///
  /// \note If this method returns 'true', it only means that there exists a
  /// declared conformance of \c T to \c Proto. It does not mean that this
  /// conformance is valid. Check \c (*Conformance)->getState() for that.
  bool conformsToProtocol(Type T, ProtocolDecl *Proto, DeclContext *DC,
                          bool InExpression,
                          ProtocolConformance **Conformance = 0,
                          SourceLoc ComplainLoc = SourceLoc());

  /// Derive an implicit declaration to satisfy a requirement of a derived
  /// protocol conformance.
  ///
  /// \param TypeDecl     The type for which the requirement is being derived.
  /// \param Requirement  The protocol requirement.
  ///
  /// \returns nullptr if the derivation failed, or the derived declaration
  ///          if it succeeded. If successful, the derived declaration is added
  ///          to TypeDecl's body.
  ValueDecl *deriveProtocolRequirement(NominalTypeDecl *TypeDecl,
                                       ValueDecl *Requirement);
  
  /// Derive an implicit type witness for the given associated type in
  /// the conformance of the given nominal type to some known
  /// protocol.
  Type deriveTypeWitness(NominalTypeDecl *nominal,
                         AssociatedTypeDecl *assocType);

  /// \brief Given a set of archetype substitutions, verify and record all of
  /// the required protocol-conformance relationships.
  bool checkSubstitutions(TypeSubstitutionMap &Substitutions,
                          ConformanceMap &Conformance,
                          DeclContext *DC,
                          SourceLoc ComplainLoc,
                          TypeSubstitutionMap *RecordSubstitutions = nullptr);

  /// \brief Lookup a member in the given type.
  ///
  /// \param type The type in which we will look for a member.
  /// \param name The name of the member to look for.
  /// \param dc The context that needs the member.
  /// \param isKnownPrivate If true, this lookup is counted as a private
  /// dependency.
  /// \param allowDynamicLookup Whether to allow dynamic lookup.
  ///
  /// \returns The result of name lookup.
  LookupResult lookupMember(Type type, DeclName name, DeclContext *dc,
                            bool isKnownPrivate = false,
                            bool allowDynamicLookup = true);

  /// \brief Look up a member type within the given type.
  ///
  /// This routine looks for member types with the given name within the
  /// given type. 
  ///
  /// \param type The type in which we will look for a member type.
  /// \param name The name of the member to look for.
  /// \param dc The context that needs the member.
  /// \param isKnownPrivate If true, this lookup is counted as a private
  /// dependency.
  /// \param allowProtocolMembers If true, allow lookup to find members of
  /// protocols and protocol extensions.
  ///
  /// \returns The result of name lookup.
  LookupTypeResult lookupMemberType(Type type, Identifier name,
                                    DeclContext *dc,
                                    bool isKnownPrivate = false,
                                    bool allowProtocolMembers = true);

  /// \brief Look up the constructors of the given type.
  ///
  /// \param type The type for which we will look for constructors.
  /// \param dc The context that needs the constructor.
  /// \param isKnownPrivate If true, this lookup is counted as a private
  /// dependency.
  ///
  /// \returns the constructors found for this type.
  LookupResult lookupConstructors(Type type, DeclContext *dc,
                                  bool isKnownPrivate = false);

  /// \brief Look up the Bool type in the standard library.
  Type lookupBoolType(const DeclContext *dc);

  /// Diagnose an ambiguous member type lookup result.
  void diagnoseAmbiguousMemberType(Type baseTy, SourceRange baseRange,
                                   Identifier name, SourceLoc nameLoc,
                                   LookupTypeResult &lookup);

  /// @}

  /// Fix the name of the given function to the target name, attaching
  /// Fix-Its to the provided in-flight diagnostic.
  void fixAbstractFunctionNames(InFlightDiagnostic &diag,
                                AbstractFunctionDecl *func,
                                DeclName targetName);

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
  Comparison compareDeclarations(DeclContext *dc,
                                 ValueDecl *decl1,
                                 ValueDecl *decl2);

  /// \brief Build a type-checked reference to the given value.
  Expr *buildCheckedRefExpr(ValueDecl *D, DeclContext *UseDC,
                            SourceLoc nameLoc, bool Implicit);

  /// \brief Build a reference to a declaration, where name lookup returned
  /// the given set of declarations.
  Expr *buildRefExpr(ArrayRef<ValueDecl *> Decls, DeclContext *UseDC,
                     SourceLoc NameLoc, bool Implicit,
                     bool isSpecialized = false);
  /// @}

  /// \brief Retrieve a specific, known protocol.
  ///
  /// \param loc The location at which we need to look for the protocol.
  /// \param kind The known protocol we're looking for.
  ///
  /// \returns null if the protocol is not available. This represents a
  /// problem with the Standard Library.
  ProtocolDecl *getProtocol(SourceLoc loc, KnownProtocolKind kind);

  /// \brief Retrieve the literal protocol for the given expression.
  ///
  /// \returns the literal protocol, if known and available, or null if the
  /// expression does not have an associated literal protocol.
  ProtocolDecl *getLiteralProtocol(Expr *expr);

  DeclName getObjectLiteralConstructorName(ObjectLiteralExpr *expr);

  /// Get the module appropriate for looking up standard library types.
  ///
  /// This is "Swift", if that module is imported, or the current module if
  /// we're parsing the standard library.
  Module *getStdlibModule(const DeclContext *dc);

  /// \name AST Mutation Listener Implementation
  /// @{
  void handleExternalDecl(Decl *decl);

  /// @}

  /// \name Lazy resolution.
  ///
  /// Routines that perform lazy resolution as required for AST operations.
  /// @{
  void checkConformance(NormalProtocolConformance *conformance) override;
  void resolveTypeWitness(const NormalProtocolConformance *conformance,
                          AssociatedTypeDecl *assocType) override;
  void resolveWitness(const NormalProtocolConformance *conformance,
                      ValueDecl *requirement) override;
  Type resolveMemberType(DeclContext *dc, Type type,
                         Identifier name) override;

  bool isCIntegerType(const DeclContext *DC, Type T);
  bool isRepresentableInObjC(const AbstractFunctionDecl *AFD,
                             ObjCReason Reason,
                             Optional<ForeignErrorConvention> &errorConvention);
  bool isRepresentableInObjC(const VarDecl *VD, ObjCReason Reason);
  bool isRepresentableInObjC(const SubscriptDecl *SD, ObjCReason Reason);
  bool isTriviallyRepresentableInObjC(const DeclContext *DC, Type T);
  bool isRepresentableInObjC(const DeclContext *DC, Type T);

  void diagnoseTypeNotRepresentableInObjC(const DeclContext *DC,
                                          Type T, SourceRange TypeRange);

  void fillObjCRepresentableTypeCache(const DeclContext *DC);
  
  ArchetypeBuilder createArchetypeBuilder(Module *mod);

  /// \name Availability checking
  ///
  /// Routines that perform API availability checking and type checking of
  /// potentially unavailable API elements
  /// @{

  /// \brief Returns the version range on which a declaration is available
  ///  We assume a declaration without an annotation is always available.
  static VersionRange availableRange(const Decl *D, ASTContext &C);

  /// \brief Returns the version range on which the declaration for which
  /// declaration is annotated as available, or None if the declaration
  /// has not availability annotation.
  static Optional<VersionRange> annotatedAvailableRange(const Decl *D,
                                                        ASTContext &C);

  /// \brief Returns true if the availability of the overridding declaration
  /// makes it a safe override, given the availability of the base declation.
  bool isAvailabilitySafeForOverride(ValueDecl *override, ValueDecl *base);

  /// Walk the AST to build the hierarchy of TypeRefinementContexts
  ///
  /// \param StartElem Where to start for incremental building of refinement
  /// contexts
  static void buildTypeRefinementContextHierarchy(SourceFile &SF,
                                                  unsigned StartElem);

  /// Build the hierarchy of TypeRefinementContexts for the entire
  /// source file, if it has not already been built. Returns the root
  /// TypeRefinementContext for the source file.
  static TypeRefinementContext *getOrBuildTypeRefinementContext(SourceFile *SF);

  /// Checks whether a declaration is available when referred to at the given
  /// location (this reference location must be in the passed-in
  /// reference DeclContext).
  /// If the declaration is available, return true.
  /// If the declaration is not available, return false and write the
  /// declaration's available version range to the out parameter
  /// OutAvailableRange.
  bool isDeclAvailable(const Decl *D, SourceLoc referenceLoc,
                       const DeclContext *referenceDC,
                       VersionRange &OutAvailableRange);

  /// Checks whether a declaration should be considered unavailable when
  /// referred to at the given location and, if so, returns the reason why the
  /// declaration is unavailable. Returns None is the declaration is
  /// definitely available.
  Optional<UnavailabilityReason>
  checkDeclarationAvailability(const Decl *D, SourceLoc referenceLoc,
                               const DeclContext *referenceDC);

  // Emits a diagnostic, if necessary, for a reference to a declaration
  // that is potentially unavailable at the given source location.
  void diagnosePotentialUnavailability(const ValueDecl *D,
                                       SourceRange ReferenceRange,
                                       const DeclContext *ReferenceDC,
                                       const UnavailabilityReason &Reason);

  // Emits a diagnostic, if necessary, for a reference to a declaration
  // that is potentially unavailable at the given source location, using
  // Name as the diagnostic name.
  void diagnosePotentialUnavailability(const Decl *D, DeclName Name,
                                       SourceRange ReferenceRange,
                                       const DeclContext *ReferenceDC,
                                       const UnavailabilityReason &Reason);

  /// Emits a diagnostic for a reference to a storage accessor that is
  /// potentially unavailable.
  void diagnosePotentialAccessorUnavailability(
      FuncDecl *Accessor, SourceRange ReferenceRange,
      const DeclContext *ReferenceDC, const UnavailabilityReason &Reason,
      bool ForInout);

  /// Returns true if the reference or any of its parents is an
  /// implicit function.
  bool isInsideImplicitFunction(SourceRange ReferenceRange,
                                const DeclContext *DC);

  /// Returns true if the reference or any of its parents is an
  /// unavailable (or obsoleted) declaration.
  bool isInsideUnavailableDeclaration(SourceRange ReferenceRange,
                                     const DeclContext *DC);

  /// Returns the availability attribute indicating deprecation if the
  /// declaration is deprecated or null otherwise.
  static const AvailabilityAttr *getDeprecated(const Decl *D);

  /// Emits a diagnostic for a reference to a declaration that is deprecated.
  void diagnoseDeprecated(SourceRange SourceRange,
                          const DeclContext *ReferenceDC,
                          const AvailabilityAttr *Attr,
                          DeclName Name);
  /// @}

  /// If LangOptions::DebugForbidTypecheckPrefix is set and the given decl
  /// has a name with that prefix, an llvm fatal_error is triggered.
  /// This is for testing purposes.
  void checkForForbiddenPrefix(const Decl *D);
  void checkForForbiddenPrefix(const UnresolvedDeclRefExpr *E);
  void checkForForbiddenPrefix(Identifier Ident);
  void checkForForbiddenPrefix(StringRef Name);

  bool hasEnabledForbiddenTypecheckPrefix() const {
    return !Context.LangOpts.DebugForbidTypecheckPrefix.empty();
  }

  void addExprForDiagnosis(Expr *E) {
    DiagnosedExprs.insert(E);
  }
  bool exprIsBeingDiagnosed(Expr *E) {
    return DiagnosedExprs.count(E) != 0;
  }

};

/// \brief RAII object that cleans up the given expression if not explicitly
/// disabled.
class CleanupIllFormedExpressionRAII {
  constraints::ConstraintSystem &cs;
  Expr **expr;
  
public:
  CleanupIllFormedExpressionRAII(constraints::ConstraintSystem &cs, Expr *&expr)
  : cs(cs), expr(&expr) { }
  
  ~CleanupIllFormedExpressionRAII();
  
  /// \brief Disable the cleanup of this expression; it doesn't need it.
  void disable() {
    expr = nullptr;
  }
};

} // end namespace swift

#endif
