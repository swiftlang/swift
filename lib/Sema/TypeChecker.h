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

/// \brief A mapping from substitutable types to the protocol-conformance
/// mappings for those types.
typedef llvm::DenseMap<SubstitutableType *,
                       SmallVector<ProtocolConformance *, 2>> ConformanceMap;

/// \brief The set of known protocols.
enum class KnownProtocolKind : unsigned {
  /// \brief The 'ArrayBound' protocol, used for array bounds.
  ArrayBound,

  /// \brief The 'ArrayLiteralConvertible' protocol, used for array literals.
  ArrayLiteralConvertible,

  /// \brief The 'BuiltinCharacterLiteralConvertible' protocol, used for
  /// character literals.
  BuiltinCharacterLiteralConvertible,

  /// \brief The 'BuiltinFloatLiteralConvertible' protocol, used for floating
  /// point literals.
  BuiltinFloatLiteralConvertible,

  /// \brief The 'BuiltinIntegerLiteralConvertible' protocol, used for integer
  /// literals.
  BuiltinIntegerLiteralConvertible,

  /// \brief The 'BuiltinStringLiteralConvertible' protocol, used for string
  /// literals.
  BuiltinStringLiteralConvertible,

  /// \brief The 'CharacterLiteralConvertible' protocol, used for character
  /// literals.
  CharacterLiteralConvertible,

  /// \brief The 'DictionaryLiteralConvertible' protocol, used for dictionary
  /// literals.
  DictionaryLiteralConvertible,

  /// \brief The 'Enumerable' protocol, used by the for-each loop.
  Enumerable,

  /// \brief The 'Enumerator' protocol, used by the for-each loop.
  Enumerator,

  /// \brief The 'FloatLiteralConvertible' protocol, used for floating
  /// point literals.
  FloatLiteralConvertible,

  /// \brief The 'IntegerLiteralConvertible' protocol, used for integer
  /// literals.
  IntegerLiteralConvertible,

  /// \brief The 'LogicValue' protocol, used for places where a value is
  /// considered to be a logic value, such as in an 'if' statement.
  LogicValue,

  /// \brief The 'StringInterpolationConvertible ' protocol, used for string
  /// interpolation literals.
  StringInterpolationConvertible,

  /// \brief The 'StringLiteralConvertible' protocol, used for string literals.
  StringLiteralConvertible
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
            KnownProtocolKind::StringLiteralConvertible);

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
  Expr *buildArrayInjectionFnRef(DeclContext *dc,
                                 ArraySliceType *sliceType,
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
  /// \param IgnoreMissing Ignore missing mappings; useful when something else
  /// may establish those mappings later, e.g., as in protocol conformance.
  ///
  /// \returns The substituted type, or null if the substitution failed.
  ///
  /// FIXME: We probably want to have both silent and loud failure modes. However,
  /// the only possible failure now is from array slice types, which occur
  /// simply because we don't have Slice<T> yet.
  Type substType(Type T, TypeSubstitutionMap &Substitutions,
                 bool IgnoreMissing = false);

  /// \brief Replace the type \c T of a protocol member \c Member given the
  /// type of the base of a member access, \c BaseTy.
  Type substMemberTypeWithBase(Type T, ValueDecl *Member, Type BaseTy);

  /// \brief Retrieve the superclass type of the given type, or a null type if
  /// the type has no supertype.
  Type getSuperClassOf(Type type);

  /// \brief Determine whether one type is a subtype of another.
  ///
  /// \param t1 The potential subtype.
  /// \param t2 The potential supertype.
  ///
  /// \returns true if \c t1 is a subtype of \c t2.
  bool isSubtypeOf(Type t1, Type t2) {
    bool isTrivial;
    return isSubtypeOf(t1, t2, isTrivial);
  }

  /// \brief Determine whether one type is a subtype of another.
  ///
  /// \param t1 The potential subtype.
  ///
  /// \param t2 The potential supertype.
  ///
  /// \param isTrivial Will indicate whether this is a trivial subtyping
  /// relationship.
  ///
  /// \returns true if \c t1 is a subtype of \c t2.
  bool isSubtypeOf(Type t1, Type t2, bool &isTrivial);
  
  /// \brief Determine whether one type is implicitly convertible to another.
  ///
  /// \param t1 The potential source type of the conversion.
  ///
  /// \param t2 The potential destination type of the conversion.
  ///
  /// \returns true if \c t1 can be implicitly converted to \c t2.
  bool isConvertibleTo(Type t1, Type t2);
  
  /// \brief Determine whether one type would be a valid substitution for an
  /// archetype.
  ///
  /// \param t1 The potential substitution type.
  ///
  /// \param t2 The potential substituted archetype.
  ///
  /// \returns true if \c t1 is a valid substitution for \c t2.
  bool isSubstitutableFor(Type t1, ArchetypeType *t2);

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

  Optional<Type> boolType;
  
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
  bool typeCheckExpression(Expr *&expr, DeclContext *dc,
                           Type convertType = Type());

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

  /// \brief Type check the given expression as a condition, which converts
  /// it to a logic value.
  ///
  /// \param expr The expression to type-check, which will be modified in place
  /// to return a logic value (builtin i1).
  ///
  /// \returns true if an error occurred, false otherwise.
  bool typeCheckCondition(Expr *&expr, DeclContext *dc);

  /// \brief Type check the given expression as an array bound, which converts
  /// it to a builtin integer value.
  ///
  /// \param expr The expression to type-check, which will be modified in
  /// place to return a builtin integral value (e.g., builtin i64).
  bool typeCheckArrayBound(Expr *&expr, bool requireConstant, DeclContext *dc);

  bool typeCheckPattern(Pattern *P, DeclContext *dc,
                        bool isFirstPass, bool allowUnknownTypes);
  bool coerceToType(Pattern *P, DeclContext *dc, Type Ty);
  
  /// \brief Compute the set of captures for the given function or closure.
  void computeCaptures(CapturingExpr *capturing);

  /// \brief Retrieve the default type for the given protocol.
  ///
  /// Some protocols, particularly those that correspond to literals, have
  /// default types associated with them. This routine retrieves that default
  /// type.
  ///
  /// \returns the default type, or null if there is no default type for
  /// this protocol.
  Type getDefaultType(ProtocolDecl *protocol);

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
                    Identifier name,
                    MutableArrayRef<Expr *> arguments,
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

  /// \brief Look up the Bool type in the standard library.
  Type lookupBoolType();
  
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
  /// \returns an ASTContext-allocate array of substitutions.
  ///
  /// \param OnlyInnermostParams Whether we're specializing only the innermost
  /// generic parameters (rather than all levels of generic parameters).
  ArrayRef<Substitution>
  encodeSubstitutions(const GenericParamList *GenericParams,
                      const TypeSubstitutionMap &Substitutions,
                      const ConformanceMap &Conformances,
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
  /// \param OnlyInnermostParams Whether we're specializing only the innermost
  /// generic parameters (rather than all levels of generic parameters).
  SpecializeExpr *buildSpecializeExpr(Expr *Sub, Type Ty,
                                      const TypeSubstitutionMap &Substitutions,
                                      const ConformanceMap &Conformances,
                                      bool OnlyInnermostParams);

  /// \brief Build a reference to a declaration, where name lookup returned
  /// the given set of declarations.
  Expr *buildRefExpr(ArrayRef<ValueDecl *> Decls, SourceLoc NameLoc);
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
