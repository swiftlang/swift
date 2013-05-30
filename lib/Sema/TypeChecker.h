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
#include "swift/AST/ASTMutationListener.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/SetVector.h"
#include <functional>

namespace swift {

class MemberLookup;
class TypeChecker;

/// CoercionResult - Describes the result of attempting to coerce an
/// expression to a given type.
enum class CoercionResult {
  Succeeded,
  Failed,
  Unknowable
};

/// CoercedExpr - Describes the result of coercing an expression to a given
/// type.
class CoercedExpr {
  TypeChecker &TC;
  CoercionResult Kind;
  Expr *E;
  Type DestTy;

  void diagnose() const;

public:
  CoercedExpr(TypeChecker &TC, CoercionResult Kind, Expr *E, Type DestTy)
    : TC(TC), Kind(Kind), E(E), DestTy(DestTy) {}

  CoercionResult getKind() const { return Kind; }
  Expr *getExpr() const { return E; }

  explicit operator bool() const { return Kind == CoercionResult::Succeeded; }

  operator Expr*() const {
    switch (Kind) {
    case CoercionResult::Succeeded:
      return E;

    case CoercionResult::Unknowable:
      diagnose();
      SWIFT_FALLTHROUGH;

    case CoercionResult::Failed:
      return 0;
    }
  }
};

/// \brief Describes the kind of a literal.
enum class LiteralKind : char {
  Int, Float, Char, UTFString, ASCIIString, Array, Dictionary
};

/// \brief A mapping from substitutable types to the protocol-conformance
/// mappings for those types.
typedef llvm::DenseMap<SubstitutableType *,
                       SmallVector<ProtocolConformance *, 2>> ConformanceMap;

/// \brief Describes the kind of coercion being performed.
enum class CoercionKind {
  // Normal coercion to the given type.
  Normal,
  // Coercion of a tuple when calling an assignment operator, which permits
  // the first operand to be an implicit lvalue.
  Assignment,
  // Coercion to an implicit lvalue.
  ImplicitLValue
};

/// \brief Captures information about the context of a type coercion, including
/// the type checker instance and the type substitutions deduced by the
/// coercion.
struct CoercionContext {
  CoercionContext(TypeChecker &TC) : TC(TC) { }
  
  TypeChecker &TC;
  TypeSubstitutionMap Substitutions;

  /// \brief Mapping from each of the substitutable types to the set of
  /// protocol-conformance mappings for each of the requirements on the
  /// type.
  ConformanceMap Conformance;

  /// \brief Identify the set of generic parameters for which we want to
  /// compute substitutions.
  void requestSubstitutionsFor(ArrayRef<DeducibleGenericParamType *> Params);
  
  /// \brief Retrieve the substitution for the given deducible generic
  /// parameter type.
  Type getSubstitution(DeducibleGenericParamType *Deducible) const {
    TypeSubstitutionMap::const_iterator Pos = Substitutions.find(Deducible);
    assert(Pos != Substitutions.end() && "Not deducible");
    return Pos->second;
  }

  /// \brief Determine whether the given coercion context requires
  /// substitutions.
  bool requiresSubstitution() const { return !Substitutions.empty(); }

  /// \brief Determine whether this coercion context has complete substitution
  /// information.
  bool hasCompleteSubstitutions() const;
};

/// \brief The set of known protocols.
enum class KnownProtocolKind : unsigned {
  /// \brief The 'ArrayBound' protocol, used for array bounds.
  ArrayBound,

  /// \brief The 'ArrayLiteralConvertible' protocol, used for array literals.
  ArrayLiteralConvertible,

  /// \brief The 'DictionaryLiteralConvertible' protocol, used for dictionary
  /// literals.
  DictionaryLiteralConvertible,

  /// \brief The 'Enumerable' protocol, used by the for-each loop.
  Enumerable,

  /// \brief The 'Enumerator' protocol, used by the for-each loop.
  Enumerator,

  /// \brief The 'LogicValue' protocol, used for places where a value is
  /// considered to be a logic value, such as in an 'if' statement.
  LogicValue,

  /// \brief The 'StringInterpolationConvertible ' protocol, used for string
  /// interpolation literals.
  StringInterpolationConvertible
};

class TypeChecker : public ASTMutationListener {
public:
  TranslationUnit &TU;
  ASTContext &Context;

  /// \brief The list of implicitly-defined functions created by the
  /// type checker.
  std::vector<FuncExprLike> implicitlyDefinedFunctions;

private:
  /// \brief The number of known protocols.
  static const unsigned numKnownProtocols
    = 1 + static_cast<unsigned>(
            KnownProtocolKind::StringInterpolationConvertible);

  /// \brief The set of known protocols, lazily populated as needed.
  ProtocolDecl *knownProtocols[numKnownProtocols] = { };

  Type IntLiteralType;
  Type FloatLiteralType;
  Type CharacterLiteralType;
  Type StringLiteralType;
  Type ArrayLiteralType;
  Type DictionaryLiteralType;
  
  DiagnosticEngine &Diags;

  /// The index of the next response metavariable to bind to a REPL result.
  unsigned NextResponseVariableIndex = 0;

public:
  TypeChecker(TranslationUnit &TU) : TypeChecker(TU, TU.Ctx.Diags) { }
  TypeChecker(TranslationUnit &TU, DiagnosticEngine &Diags);
  ~TypeChecker();

  LangOptions &getLangOpts() const { return Context.LangOpts; }
  
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes... Args) {
    return Diags.diagnose(Args...);
  }

  Type getArraySliceType(SourceLoc loc, Type elementType);
  Expr *buildArrayInjectionFnRef(ArraySliceType *sliceType,
                                 Type lenTy, SourceLoc Loc);

  bool validateType(TypeLoc &Loc);

  /// \brief Validate the given type, which has no location information
  /// and shall not fail.
  /// FIXME: This concept seems a bit broken.
  void validateTypeSimple(Type T) {
    TypeLoc TL(T, SourceRange());
    bool result = validateType(TL);
    assert(!result && "Validation cannot fail!");
    (void)result;
  }

  /// \brief Transform the given type by applying the given function to
  /// each type node. If the function returns null, the transformation aborts.
  /// If it leaves the given type unchanged, then the transformation will be
  /// applied to all of the child types. If any of those types change, their
  /// parent node will be rebuilt.
  Type transformType(Type type, const std::function<Type(Type)> &fn);

  /// \brief Substitute the given archetypes for their substitution types
  /// within the given type.
  ///
  /// \returns The substituted type, or null if the substitution failed.
  ///
  /// FIXME: We probably want to have both silent and loud failure modes. However,
  /// the only possible failure now is from array slice types, which occur
  /// simply because we don't have Slice<T> yet.
  Type substType(Type T, TypeSubstitutionMap &Substitutions);

  /// \brief Replace the type \c T of a protocol member \c Member given the
  /// type of the base of a member access, \c BaseTy.
  Type substMemberTypeWithBase(Type T, ValueDecl *Member, Type BaseTy);

  /// \brief Retrieve the superclass type of the given type, or a null type if
  /// the type has no supertype.
  Type getSuperClassOf(Type type);

  /// \brief Determine whether one type is a subtype of another.
  ///
  /// \param t1 The first type.
  /// \param t2 The second type.
  ///
  /// \returns true if \c t1 is a subtype of \c t2.
  bool isSubtypeOf(Type t1, Type t2) {
    bool isTrivial;
    return isSubtypeOf(t1, t2, isTrivial);
  }

  /// \brief Determine whether one type is a subtype of another.
  ///
  /// \param t1 The first type.
  ///
  /// \param t2 The second type.
  ///
  /// \param isTrivial Will indicate whether this is a trivial subtyping
  /// relationship.
  ///
  /// \returns true if \c t1 is a subtype of \c t2.
  bool isSubtypeOf(Type t1, Type t2, bool &isTrivial);

  void semaFuncExpr(FuncExpr *FE, bool isFirstPass, bool allowUnknownTypes);

  /// If the inputs to an apply expression use a consistent "sugar" type
  /// (that is, a typealias or shorthand syntax) equivalent to the result type
  /// of the function, set the result type of the expression to that sugar type.
  Expr *substituteInputSugarTypeForResult(ApplyExpr *E);

  void typeCheckIgnoredExpr(Expr *E);
  void typeCheckFunctionBody(FuncExpr *FE);
  void typeCheckConstructorBody(ConstructorDecl *CD);
  void typeCheckDestructorBody(DestructorDecl *DD);
  void typeCheckClosureBody(PipeClosureExpr *closure);
  void typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD);

  void processREPLTopLevel(unsigned StartElem);
  Identifier getNextResponseVariableName();

  void typeCheckDecl(Decl *D, bool isFirstPass);

  /// \brief Add any implicitly-defined constructors required for the given
  /// struct.
  void addImplicitConstructors(StructDecl *structDecl);

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
  /// \brief The list of structs that require default constructors
  /// to be defined.
  ///
  /// This vector lists all structs that have implicitly-defined default
  /// constructors. Those structs not also in
  /// \c structsNeedingDefaultConstructor still need to heave their
  /// default constructors defined.
  std::vector<StructDecl *> structsWithImplicitDefaultConstructor;

  /// \brief The set of structs that still need a default constructor to be
  /// implicitly defined.
  llvm::DenseSet<StructDecl *> structsNeedingImplicitDefaultConstructor;

public:
  /// \brief Determine whether the given type can be default-initialized.
  ///
  /// \param ty The type we are checking.
  ///
  /// \param initializer If non-null, we will assigned an initializer expression
  /// that performs the default initialization.
  bool isDefaultInitializable(Type ty, Expr **initializer);

  /// \brief Define the default constructor for the given struct.
  void defineDefaultConstructor(StructDecl *structDecl);

  /// \brief Define any implicit declarations that are still pending.
  void definePendingImplicitDecls();

  /// \brief Pre-check protocol declaration, validating all of the types
  /// that are involved in conformance requirements.
  void preCheckProtocol(ProtocolDecl *D);

  /// \brief Fold the given sequence expression into an (unchecked) expression
  /// tree.
  Expr *foldSequence(SequenceExpr *expr);

  /// \brief Type check the given expression.
  ///
  /// \param expr The expression to type-check, which will be modified in
  /// place.
  ///
  /// \param convertType The type that the expression is being converted to,
  /// or null if the expression is standalone.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckExpression(Expr *&expr, Type convertType = Type());

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
  bool typeCheckExpressionShallow(Expr *&expr, Type convertType = Type());

  /// \brief Type check the given expression as a condition, which converts
  /// it to a logic value.
  ///
  /// \param expr The expression to type-check, which will be modified in place
  /// to return a logic value (builtin i1).
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckCondition(Expr *&expr);

  /// \brief Type check the given expression as an array bound, which converts
  /// it to a builtin integer value.
  ///
  /// \param expr The expression to type-check, which will be modified in
  /// place to return a builtin integral value (e.g., builtin i64).
  bool typeCheckArrayBound(Expr *&expr, bool requireConstant);

  /// \brief Type check an assignment expression using the constraint-based
  /// type checker.
  ///
  /// \param dest The destination expression.
  /// \param equalLoc The location of the '='.
  /// \param src The source expression.
  ///
  /// \returns a converted (dest, src) expression pair, or (nullptr, nullptr)
  /// if the assignment failed to type-check.
  std::pair<Expr *, Expr *> typeCheckAssignment(Expr *dest,
                                                SourceLoc equalLoc,
                                                Expr *src);

  bool typeCheckPattern(Pattern *P, bool isFirstPass, bool allowUnknownTypes);
  bool coerceToType(Pattern *P, Type Ty);
  
  /// \brief Compute the set of captures for the given function or closure.
  void computeCaptures(CapturingExpr *capturing);

  /// \brief Retrieve the default literal type for the given literal kind.
  Type getDefaultLiteralType(LiteralKind kind);

  /// \brief Retrieve the default literal type for the given literal
  /// expression.
  Type getDefaultLiteralType(LiteralExpr *E);

  /// coerceToType - Do semantic analysis of an expression in a context that
  /// expects a particular type.  This performs a conversion to that type if
  /// the types don't match and diagnoses cases where the conversion cannot be
  /// performed.
  ///
  /// \param Kind The kind of coercion being performed.
  ///
  /// On success, returns the coerced expression. Otherwise, returns either
  /// failure (in which case a diagnostic was produced) or 'unknowable', if
  /// it is unknown whether the coercion can occur (e.g., due to literals that
  /// have not been coerced to any specific type).
  CoercedExpr coerceToType(Expr *E, Type Ty,
                           CoercionKind Kind = CoercionKind::Normal,
                           CoercionContext *CC = nullptr);
    
  /// \brief Coerce the given expression to an rvalue, if it isn't already.
  Expr *coerceToRValue(Expr *expr);
  
  /// \brief Coerce the given expression to materializable type, if it
  /// isn't already.
  Expr *coerceToMaterializable(Expr *expr);

  /// \brief Build a call to the witness with the given name and no arguments.
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
  /// \param brokenProtocolDiag Diagnostic to emit if the protocol is broken.
  ///
  /// \returns a fully type-checked call, or null if the protocol was broken.
  Expr *callWitness(Expr *base, ProtocolDecl *protocol,
                    ProtocolConformance *conformance,
                    Identifier name,
                    Diag<> brokenProtocolDiag);

  /// conformsToProtocol - Determine whether the given type conforms to the
  /// given protocol.
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
  bool conformsToProtocol(Type T, ProtocolDecl *Proto,
                          ProtocolConformance **Conformance = 0,
                          SourceLoc ComplainLoc = SourceLoc());

  /// \brief Given a set of archetype substitutions, verify and recird all of
  /// the required protocol-conformance relationships.
  bool checkSubstitutions(TypeSubstitutionMap &Substitutions,
                          ConformanceMap &Conformance,
                          SourceLoc ComplainLoc,
                          TypeSubstitutionMap *RecordSubstitutions = nullptr);

  /// \brief Determine whether the given type is compatible with an integer
  /// or floating-point literal and what function would perform the conversion.
  ///
  /// \returns The function that will perform the conversion, along with the
  /// type of the argument of this function.
  std::pair<FuncDecl*, Type> isLiteralCompatibleType(
                               Type Ty, SourceLoc Loc,
                               LiteralKind LitTy,
                               bool Complain,
                               bool RequiresBuiltinArg = false);

  /// \brief Lookup a member in the given type.
  ///
  /// \param type The type in which we will look for a member.
  /// \param name The name of the member to look for.
  /// \param members Will be populated with the set of members found.
  ///
  /// \returns true if any members were found, false otherwise.
  bool lookupMember(Type type, Identifier name,
                    SmallVectorImpl<ValueDecl *> &members);

  /// \brief Lookup the constructors of the given type.
  ///
  /// \param type The type for which we will look for constructors.
  /// \param constructors Will be populated with the set of constructors found.
  ///
  /// \returns true if any members were found, false otherwise.
  bool lookupConstructors(Type type,SmallVectorImpl<ValueDecl *> &constructors);

  /// @}

  /// \name Overload resolution
  ///
  /// Routines that perform overload resolution or provide diagnostics related
  /// to overload resolution.
  /// @{

  /// \brief Encode the provided substitutions in the form used by
  /// SpecializeExpr (and another other AST nodes that require specialization).
  ///
  /// \param GenericParams The generic parameters whose substitutions are
  /// being encoded.
  ///
  /// \param Substitutions The set of substitutions.
  ///
  /// \param Conformances The set of protocol conformances.
  ///
  /// \param ArchetypesAreOpen Whether the substitutions refer to the opened
  /// form of the archetypes, as is used by the coercion code.
  ///
  /// \returns an ASTContext-allocate array of substitutions.
  ///
  /// \param OnlyInnermostParams Whether we're specializing only the innermost
  /// generic parameters (rather than all levels of generic parameters).
  ArrayRef<Substitution>
  encodeSubstitutions(const GenericParamList *GenericParams,
                      const TypeSubstitutionMap &Substitutions,
                      const ConformanceMap &Conformances,
                      bool ArchetypesAreOpen,
                      bool OnlyInnermostParams);

  /// \brief Build a new SpecializeExpr wrapping the given subexpression.
  ///
  /// \param Sub The subexpression to wrap, which must have polymorphic
  /// function type.
  ///
  /// \param Ty The type of the resulting SpecializeExpr.
  ///
  /// \param Substitutions The set of substitutions from each of the archetypes
  /// to the replacement type.
  ///
  /// \param Conformances The set of protocol-conformance structures for each
  /// of the substitutions.
  ///
  /// \param ArchetypesAreOpen Whether the substitutions refer to the opened
  /// form of the archetypes, as is used by the coercion code.
  ///
  /// \param OnlyInnermostParams Whether we're specializing only the innermost
  /// generic parameters (rather than all levels of generic parameters).
  SpecializeExpr *buildSpecializeExpr(Expr *Sub, Type Ty,
                                      const TypeSubstitutionMap &Substitutions,
                                      const ConformanceMap &Conformances,
                                      bool ArchetypesAreOpen,
                                      bool OnlyInnermostParams);

  /// \brief Build a reference to a declaration, where name lookup returned
  /// the given set of declarations.
  Expr *buildRefExpr(ArrayRef<ValueDecl *> Decls, SourceLoc NameLoc);
  /// @}

  /// \brief Retrieve a specific, known protocol.
  ///
  /// \returns null if the protocol is not available. This represents a
  /// problem with the Standard Library.
  ProtocolDecl *getProtocol(KnownProtocolKind kind);

  /// \name AST Mutation Listener Implementation
  /// @{
  void handleExternalDecl(Decl *decl);

  /// \brief A new declaration was added to the AST.
  virtual void addedExternalDecl(Decl *decl);

  /// \brief A new type was added to the AST.
  virtual void addedExternalType(Type type);

  /// @}

};

} // end namespace swift

#endif
