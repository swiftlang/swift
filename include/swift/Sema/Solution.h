//===--- Solution.h - Solution to a constraint system -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A solution records a mapping of type variables to fixed types, a mapping of
// overloads to choices, and the outcome of various other decisions made while
// solving.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_SOLUTION_H
#define SWIFT_SEMA_SOLUTION_H

#include "swift/Sema/Score.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class Type;
class GenericTypeParamType;
class TypeVariableType;

namespace constraints {

class ConstraintSystem;
class SyntacticElementTarget;

/// Describes a dependent type that has been opened to a particular type
/// variable.
using OpenedType = std::pair<GenericTypeParamType *, TypeVariableType *>;

/// An overload that has been selected in a particular solution.
///
/// A selected overload captures the specific overload choice (e.g., a
/// particular declaration) as well as the type to which the reference to the
/// declaration was opened, which may involve type variables.
struct SelectedOverload {
  /// The overload choice.
  const OverloadChoice choice;

  /// The opened type of the base of the reference to this overload, if
  /// we're referencing a member.
  const Type openedFullType;

  /// The opened type of the base of the reference to this overload, adjusted
  /// for `@preconcurrency` or other contextual type-altering attributes.
  const Type adjustedOpenedFullType;

  /// The opened type produced by referring to this overload.
  const Type openedType;

  /// The opened type produced by referring to this overload, adjusted for
  /// `@preconcurrency` or other contextual type-altering attributes.
  const Type adjustedOpenedType;

  /// The type that this overload binds. Note that this may differ from
  /// adjustedOpenedType, for example it will include any IUO unwrapping that has taken
  /// place.
  const Type boundType;
};

enum class ImpliedResultKind {
  /// A regular implied result, this applies to e.g single expression bodies of
  /// function decls, and implied 'then' statements outside of closures.
  Regular,

  /// An implied result for a closure, e.g a single expression body.
  ForClosure
};

/// Describes the arguments to which a parameter binds.
/// FIXME: This is an awful data structure. We want the equivalent of a
/// TinyPtrVector for unsigned values.
using ParamBinding = SmallVector<unsigned, 1>;

/// Describes the algorithm to use for trailing closure matching.
enum class TrailingClosureMatching {
  /// Match a trailing closure to the first parameter that appears to work.
  Forward,

  /// Match a trailing closure to the last parameter.
  Backward,
};

/// The result of calling matchCallArguments().
struct MatchCallArgumentResult {
  /// The direction of trailing closure matching that was performed.
  TrailingClosureMatching trailingClosureMatching;

  /// The parameter bindings determined by the match.
  SmallVector<ParamBinding, 4> parameterBindings;

  /// When present, the forward and backward scans each produced a result,
  /// and the parameter bindings are different. The primary result will be
  /// forwarding, and this represents the backward binding.
  std::optional<SmallVector<ParamBinding, 4>> backwardParameterBindings;

  friend bool operator==(const MatchCallArgumentResult &lhs,
                         const MatchCallArgumentResult &rhs) {
    if (lhs.trailingClosureMatching != rhs.trailingClosureMatching)
      return false;
    if (lhs.parameterBindings != rhs.parameterBindings)
      return false;
    return lhs.backwardParameterBindings == rhs.backwardParameterBindings;
  }

  /// Generate a result that maps the provided number of arguments to the same
  /// number of parameters via forward match.
  static MatchCallArgumentResult forArity(unsigned argCount) {
    SmallVector<ParamBinding, 4> Bindings;
    for (unsigned i : range(argCount))
      Bindings.push_back({i});
    return {TrailingClosureMatching::Forward, Bindings, std::nullopt};
  }
};

/// Provides information about the application of a function argument to a
/// parameter.
class FunctionArgApplyInfo {
  ArgumentList *ArgList;
  Expr *ArgExpr;
  unsigned ArgIdx;
  Type ArgType;

  unsigned ParamIdx;

  Type FnInterfaceType;
  FunctionType *FnType;
  const ValueDecl *Callee;

  FunctionArgApplyInfo(ArgumentList *argList, Expr *argExpr, unsigned argIdx,
                       Type argType, unsigned paramIdx, Type fnInterfaceType,
                       FunctionType *fnType, const ValueDecl *callee)
      : ArgList(argList), ArgExpr(argExpr), ArgIdx(argIdx), ArgType(argType),
        ParamIdx(paramIdx), FnInterfaceType(fnInterfaceType), FnType(fnType),
        Callee(callee) {}

public:
  static std::optional<FunctionArgApplyInfo>
  get(ArgumentList *argList, Expr *argExpr, unsigned argIdx, Type argType,
      unsigned paramIdx, Type fnInterfaceType, FunctionType *fnType,
      const ValueDecl *callee) {
    assert(fnType);

    if (argIdx >= argList->size())
      return std::nullopt;

    if (paramIdx >= fnType->getNumParams())
      return std::nullopt;

    return FunctionArgApplyInfo(argList, argExpr, argIdx, argType, paramIdx,
                                fnInterfaceType, fnType, callee);
  }

  /// \returns The list of the arguments used for this application.
  ArgumentList *getArgList() const { return ArgList; }

  /// \returns The argument being applied.
  Expr *getArgExpr() const { return ArgExpr; }

  /// \returns The position of the argument, starting at 1.
  unsigned getArgPosition() const { return ArgIdx + 1; }

  /// \returns The position of the parameter, starting at 1.
  unsigned getParamPosition() const { return ParamIdx + 1; }

  /// \returns The type of the argument being applied, including any generic
  /// substitutions.
  ///
  /// \param withSpecifier Whether to keep the inout or @lvalue specifier of
  /// the argument, if any.
  Type getArgType(bool withSpecifier = false) const {
    return withSpecifier ? ArgType : ArgType->getWithoutSpecifierType();
  }

  /// \returns The label for the argument being applied.
  Identifier getArgLabel() const {
    return ArgList->getLabel(ArgIdx);
  }

  Identifier getParamLabel() const {
    auto param = FnType->getParams()[ParamIdx];
    return param.getLabel();
  }

  /// \returns A textual description of the argument suitable for diagnostics.
  /// For an argument with an unambiguous label, this will the label. Otherwise
  /// it will be its position in the argument list.
  StringRef getArgDescription(SmallVectorImpl<char> &scratch) const {
    llvm::raw_svector_ostream stream(scratch);

    // Use the argument label only if it's unique within the argument list.
    auto argLabel = getArgLabel();
    auto useArgLabel = [&]() -> bool {
      if (argLabel.empty())
        return false;

      SmallVector<Identifier, 4> scratch;
      return llvm::count(ArgList->getArgumentLabels(scratch), argLabel) == 1;
    };

    if (useArgLabel()) {
      stream << "'";
      stream << argLabel;
      stream << "'";
    } else {
      stream << "#";
      stream << getArgPosition();
    }
    return StringRef(scratch.data(), scratch.size());
  }

  /// Whether the argument is a trailing closure.
  bool isTrailingClosure() const {
    return ArgList->isTrailingClosureIndex(ArgIdx);
  }

  /// \returns The interface type for the function being applied. Note that this
  /// may not a function type, for example it could be a generic parameter.
  Type getFnInterfaceType() const { return FnInterfaceType; }

  /// \returns The function type being applied, including any generic
  /// substitutions.
  FunctionType *getFnType() const { return FnType; }

  /// \returns The callee for the application.
  const ValueDecl *getCallee() const { return Callee; }

private:
  Type getParamTypeImpl(AnyFunctionType *fnTy,
                        bool lookThroughAutoclosure) const {
    auto param = fnTy->getParams()[ParamIdx];
    auto paramTy = param.getPlainType();
    if (lookThroughAutoclosure && param.isAutoClosure() && paramTy->is<FunctionType>())
      paramTy = paramTy->castTo<FunctionType>()->getResult();
    return paramTy;
  }

public:
  /// \returns The type of the parameter which the argument is being applied to,
  /// including any generic substitutions.
  ///
  /// \param lookThroughAutoclosure Whether an @autoclosure () -> T parameter
  /// should be treated as being of type T.
  Type getParamType(bool lookThroughAutoclosure = true) const {
    return getParamTypeImpl(FnType, lookThroughAutoclosure);
  }

  /// \returns The interface type of the parameter which the argument is being
  /// applied to.
  ///
  /// \param lookThroughAutoclosure Whether an @autoclosure () -> T parameter
  /// should be treated as being of type T.
  Type getParamInterfaceType(bool lookThroughAutoclosure = true) const {
    auto interfaceFnTy = FnInterfaceType->getAs<AnyFunctionType>();
    if (!interfaceFnTy) {
      // If the interface type isn't a function, then just return the resolved
      // parameter type.
      return getParamType(lookThroughAutoclosure)->mapTypeOutOfEnvironment();
    }
    return getParamTypeImpl(interfaceFnTy, lookThroughAutoclosure);
  }

  /// \returns The flags of the parameter which the argument is being applied
  /// to.
  ParameterTypeFlags getParameterFlags() const {
    return FnType->getParams()[ParamIdx].getParameterFlags();
  }

  ParameterTypeFlags getParameterFlagsAtIndex(unsigned idx) const {
    return FnType->getParams()[idx].getParameterFlags();
  }
};

/// Describes a potential throw site in the constraint system.
///
/// For example, given `try f() + a[b] + x.y`, each of `f()`, `a[b]`, `x`, and
/// `x.y` is a potential throw site.
struct PotentialThrowSite {
  enum Kind {
    /// The application of a function or subscript.
    Application,

    /// An explicit 'throw'.
    ExplicitThrow,

    /// A non-exhaustive do...catch, which rethrows whatever is thrown from
    /// inside it's `do` block.
    NonExhaustiveDoCatch,

    /// A property access that can throw an error.
    PropertyAccess,
  } kind;

  /// The type that describes the potential throw site, such as the type of the
  /// function being called or type being thrown.
  Type type;

  /// The locator that specifies where the throwing operation occurs.
  ConstraintLocator *locator;

  void print(SourceManager *sm, llvm::raw_ostream &out) const;
};

/// Describes what happened when a result builder transform was applied
/// to a particular closure.
struct AppliedBuilderTransform {
  /// The builder type that was applied to the closure.
  Type builderType;

  /// The result type of the body, to which the returned expression will be
  /// converted. Opaque types should be unopened.
  Type bodyResultType;

  /// If transform is applied to a closure, this type represents
  /// contextual type the closure is converted type (e.g. a parameter
  /// type or or pattern type).
  Type contextualType;

  /// The version of the original body with result builder applied
  /// as AST transformation.
  BraceStmt *transformedBody;
};

/// Describes the information about a case label item that needs to be tracked
/// within the constraint system.
struct CaseLabelItemInfo {
  Pattern *pattern;
  Expr *guardExpr;
};

/// A complete solution to a constraint system.
///
/// A solution to a constraint system consists of type variable bindings to
/// concrete types for every type variable that is used in the constraint
/// system along with a set of mappings from each constraint locator
/// involving an overload set to the selected overload.
class Solution {
  /// The constraint system this solution solves.
  ConstraintSystem *constraintSystem;

  /// The fixed score for this solution.
  Score FixedScore;

  /// The total memory used by this solution.
  std::optional<size_t> TotalMemory;

public:
  /// Create a solution for the given constraint system.
  Solution(ConstraintSystem &cs, const Score &score)
    : constraintSystem(&cs), FixedScore(score) {}

  // Solution is a non-copyable type for performance reasons.
  Solution(const Solution &other) = delete;
  Solution &operator=(const Solution &other) = delete;

  Solution(Solution &&other) = default;
  Solution &operator=(Solution &&other) = default;

  size_t getTotalMemory() const;

  /// Retrieve the constraint system that this solution solves.
  ConstraintSystem &getConstraintSystem() const { return *constraintSystem; }

  DeclContext *getDC() const;

  /// The set of type bindings.
  llvm::MapVector<TypeVariableType *, Type> typeBindings;
  
  /// The set of overload choices along with their types.
  llvm::DenseMap<ConstraintLocator *, SelectedOverload> overloadChoices;

  /// The set of constraint restrictions used to arrive at this restriction,
  /// which informs constraint application.
  llvm::DenseMap<std::pair<CanType, CanType>, ConversionRestrictionKind>
    ConstraintRestrictions;

  /// The list of fixes that need to be applied to the initial expression
  /// to make the solution work.
  std::vector<ConstraintFix *> Fixes;

  /// The list of fixed requirements.
  using FixedRequirement =
      std::tuple<GenericTypeParamType *, unsigned, TypeBase *>;
  std::vector<FixedRequirement> FixedRequirements;

  /// Maps expressions for implied results (e.g implicit 'then' statements,
  /// implicit 'return' statements in single expression body closures) to their
  /// result kind.
  llvm::DenseMap<Expr *, ImpliedResultKind> ImpliedResults;

  /// For locators associated with call expressions, the trailing closure
  /// matching rule and parameter bindings that were applied.
  llvm::DenseMap<ConstraintLocator *, MatchCallArgumentResult>
      argumentMatchingChoices;

  /// The set of disjunction choices used to arrive at this solution,
  /// which informs constraint application.
  llvm::DenseMap<ConstraintLocator *, unsigned> DisjunctionChoices;

  /// A map from applied disjunction constraints to the corresponding
  /// argument function type.
  llvm::DenseMap<ConstraintLocator *, FunctionType *> AppliedDisjunctions;

  /// The set of opened types for a given locator.
  llvm::DenseMap<ConstraintLocator *, ArrayRef<OpenedType>> OpenedTypes;

  /// The opened existential type for a given locator.
  llvm::DenseMap<ConstraintLocator *, ExistentialArchetypeType *>
    OpenedExistentialTypes;

  llvm::DenseMap<PackExpansionType *, TypeVariableType *>
      OpenedPackExpansionTypes;

  /// The generic environment that can open pack elements for a given
  /// pack expansion.
  llvm::DenseMap<PackExpansionExpr *, GenericEnvironment *>
      PackExpansionEnvironments;

  /// The pack expansion expression for a given pack element.
  llvm::DenseMap<PackElementExpr *, PackExpansionExpr *> PackElementExpansions;

  /// The locators of \c Defaultable constraints whose defaults were used.
  llvm::DenseSet<ConstraintLocator *> DefaultedConstraints;

  /// The node -> type mappings introduced by this solution.
  llvm::DenseMap<ASTNode, Type> nodeTypes;

  /// The key path component types introduced by this solution.
  llvm::DenseMap<std::pair<const KeyPathExpr *, unsigned>, Type>
      keyPathComponentTypes;

  /// The key path expression and its root type, value type, and decl context
  /// introduced by this solution.
  llvm::DenseMap<const KeyPathExpr *,
                 std::tuple</*root=*/TypeVariableType *,
                            /*value=*/TypeVariableType *, DeclContext *>>
      KeyPaths;

  /// Contextual types introduced by this solution.
  std::vector<std::pair<ASTNode, ContextualTypeInfo>> contextualTypes;

  /// Unioned types found while analyzing this and similar solutions
  mutable llvm::DenseMap<TypeVariableType *, Type> UnionedTypes;

  /// Maps AST nodes to their target.
  llvm::DenseMap<SyntacticElementTargetKey, SyntacticElementTarget> targets;

  /// Maps case label items to information tracked about them as they are
  /// being solved.
  llvm::DenseMap<const CaseLabelItem *, CaseLabelItemInfo> caseLabelItems;

  /// Maps catch nodes to the set of potential throw sites that will be caught
  /// at that location.

  /// Keep track of all of the potential throw sites.
  std::vector<std::pair<CatchNode, PotentialThrowSite>>
      potentialThrowSites;

  /// A map of expressions to the ExprPatterns that they are being solved as
  /// a part of.
  llvm::DenseMap<Expr *, ExprPattern *> exprPatterns;

  /// The set of parameters that have been inferred to be 'isolated'.
  llvm::DenseSet<ParamDecl *> isolatedParams;

  /// The set of closures that have been inferred to be "isolated by
  /// preconcurrency".
  llvm::DenseSet<const ClosureExpr *> preconcurrencyClosures;

  /// The set of functions that have been transformed by a result builder.
  llvm::MapVector<AnyFunctionRef, AppliedBuilderTransform>
      resultBuilderTransformed;

  /// A map from argument expressions to their applied property wrapper expressions.
  llvm::DenseMap<ASTNode, SmallVector<AppliedPropertyWrapper, 2>> appliedPropertyWrappers;

  ArrayRef<AppliedPropertyWrapper> getAppliedPropertyWrappers(ASTNode anchor) {
    auto found = appliedPropertyWrappers.find(anchor);
    if (found != appliedPropertyWrappers.end())
      return found->second;
    return ArrayRef<AppliedPropertyWrapper>();
  }

  /// A mapping from the constraint locators for references to various
  /// names (e.g., member references, normal name references, possible
  /// constructions) to the argument lists for the call to that locator.
  llvm::DenseMap<ConstraintLocator *, ArgumentList *> argumentLists;

  /// The set of implicitly generated `.callAsFunction` root expressions.
  llvm::DenseMap<ConstraintLocator *, UnresolvedDotExpr *>
      ImplicitCallAsFunctionRoots;

  /// The set of conformances synthesized during solving (i.e. for
  /// ad-hoc distributed `SerializationRequirement` conformances).
  llvm::DenseMap<ConstraintLocator *, ProtocolDecl *>
      SynthesizedConformances;

  /// Record a new argument matching choice for given locator that maps a
  /// single argument to a single parameter.
  void recordSingleArgMatchingChoice(ConstraintLocator *locator);

  /// Simplify the given type by substituting all occurrences of
  /// type variables for their fixed types.
  ///
  /// \param wantInterfaceType If true, maps the resulting type out of context,
  /// and replaces type variables for opened generic parameters with the
  /// generic parameter types. Should only be used for diagnostic logic.
  /// \param forCompletion If true, will produce archetypes instead of
  /// ErrorTypes for generic parameter originators, which is what completion
  /// currently expects for the code completion token.
  Type simplifyType(Type type, bool wantInterfaceType = false,
                    bool forCompletion = false) const;

  // To aid code completion, we need to attempt to convert type placeholders
  // back into underlying generic parameters if possible, since type
  // of the code completion expression is used as "expected" (or contextual)
  // type so it's helpful to know what requirements it has to filter
  // the list of possible member candidates e.g.
  //
  // \code
  // func test<T: P>(_: [T]) {}
  //
  // test(42.#^MEMBERS^#)
  // \code
  //
  // It's impossible to resolve `T` in this case but code completion
  // expression should still have a type of `[T]` instead of `[<<hole>>]`
  // because it helps to produce correct contextual member list based on
  // a conformance requirement associated with generic parameter `T`.
  Type simplifyTypeForCodeCompletion(Type type) const;

  /// Coerce the given expression to the given type.
  ///
  /// This operation cannot fail.
  ///
  /// \param expr The expression to coerce.
  /// \param toType The type to coerce the expression to.
  /// \param locator Locator used to describe the location of this expression.
  ///
  /// \returns the coerced expression, which will have type \c ToType.
  Expr *coerceToType(Expr *expr, Type toType, ConstraintLocator *locator);

  /// Compute the set of substitutions for a generic signature opened at the
  /// given locator.
  ///
  /// \param decl The underlying declaration for which the substitutions are
  /// computed.
  ///
  /// \param sig The generic signature.
  ///
  /// \param locator The locator that describes where the substitutions came
  /// from.
  SubstitutionMap computeSubstitutions(NullablePtr<ValueDecl> decl,
                                       GenericSignature sig,
                                       ConstraintLocator *locator) const;

  /// Resolves the contextual substitutions for a reference to a declaration
  /// at a given locator.
  ConcreteDeclRef
  resolveConcreteDeclRef(ValueDecl *decl, ConstraintLocator *locator) const;

  /// Return the disjunction choice for the given constraint location.
  unsigned getDisjunctionChoice(ConstraintLocator *locator) const {
    assert(DisjunctionChoices.count(locator));
    return DisjunctionChoices.find(locator)->second;
  }

  /// Whether the given expression is the implied result for either a ReturnStmt
  /// or ThenStmt, and if so, the kind of implied result.
  std::optional<ImpliedResultKind> isImpliedResult(const Expr *E) const {
    auto result = ImpliedResults.find(E);
    if (result == ImpliedResults.end())
      return std::nullopt;

    return result->second;
  }

  /// Retrieve the fixed score of this solution
  const Score &getFixedScore() const { return FixedScore; }

  /// Retrieve the fixed score of this solution
  Score &getFixedScore() { return FixedScore; }

  /// Check whether this solution has a fixed binding for the given type
  /// variable.
  bool hasFixedType(TypeVariableType *typeVar) const;

  /// Retrieve the fixed type for the given type variable.
  Type getFixedType(TypeVariableType *typeVar) const;

  /// Try to resolve the given locator to a declaration within this
  /// solution. Note that this only returns a decl for a direct reference such
  /// as \c x.foo and will not return a decl for \c x.foo().
  ConcreteDeclRef resolveLocatorToDecl(ConstraintLocator *locator) const;

  /// Retrieve the overload choice associated with the given
  /// locator.
  SelectedOverload getOverloadChoice(ConstraintLocator *locator) const {
    return getOverloadChoiceIfAvailable(locator).value();
  }

  /// Retrieve the overload choice for the callee associated with the given
  /// locator.
  SelectedOverload getCalleeOverloadChoice(ConstraintLocator *locator) const;

  /// Retrieve the overload choice associated with the given
  /// locator, if any.
  std::optional<SelectedOverload>
  getOverloadChoiceIfAvailable(ConstraintLocator *locator) const {
    auto known = overloadChoices.find(locator);
    if (known != overloadChoices.end())
      return known->second;
    return std::nullopt;
  }

  /// Retrieve the overload choice for the callee associated with the given
  /// locator, if any.
  std::optional<SelectedOverload>
  getCalleeOverloadChoiceIfAvailable(ConstraintLocator *locator) const;

  std::optional<SyntacticElementTarget>
  getTargetFor(SyntacticElementTargetKey key) const;

  ConstraintLocator *getCalleeLocator(ConstraintLocator *locator,
                                      bool lookThroughApply = true) const;

  ConstraintLocator *
  getConstraintLocator(ASTNode anchor,
                       ArrayRef<LocatorPathElt> path = {}) const;

  ConstraintLocator *getConstraintLocator(ConstraintLocator *baseLocator,
                                          ArrayRef<LocatorPathElt> path) const;

  void setExprTypes(Expr *expr) const;

  bool hasType(ASTNode node) const;

  /// Returns \c true if the \p ComponentIndex-th component in \p KP has a type
  /// associated with it.
  bool hasType(const KeyPathExpr *KP, unsigned ComponentIndex) const;

  /// Retrieve the type of the given node, as recorded in this solution.
  Type getType(ASTNode node) const;

  /// Retrieve the type of the \p ComponentIndex-th component in \p KP.
  Type getType(const KeyPathExpr *KP, unsigned ComponentIndex) const;

  TypeVariableType *getKeyPathRootType(const KeyPathExpr *keyPath) const;

  TypeVariableType *
  getKeyPathRootTypeIfAvailable(const KeyPathExpr *keyPath) const;

  /// Retrieve the type of the given node as recorded in this solution
  /// and resolve all of the type variables in contains to form a fully
  /// "resolved" concrete type.
  Type getResolvedType(ASTNode node) const;

  std::optional<ContextualTypeInfo>
  getContextualTypeInfo(ASTNode anchor) const {
    for (const auto &entry : contextualTypes) {
      if (entry.first == anchor)
        return entry.second;
    }
    return std::nullopt;
  }

  Type getContextualType(ASTNode anchor) const {
    if (auto info = getContextualTypeInfo(anchor)) {
      // The contextual information record could contain the purpose
      // without a type i.e. when the context is an optional-some or
      // an invalid pattern binding.
      if (auto contextualTy = info->getType())
          return simplifyType(contextualTy);
    }
    return Type();
  }

  ContextualTypePurpose getContextualTypePurpose(ASTNode anchor) const {
    if (auto info = getContextualTypeInfo(anchor)) {
      return info->purpose;
    }
    return CTP_Unused;
  }

  /// Retrieve the generic environment for the opened element of a given pack
  /// expansion, or \c nullptr if no environment was recorded.
  GenericEnvironment *
  getPackExpansionEnvironment(PackExpansionExpr *expr) const;

  /// For a given locator describing a function argument conversion, or a
  /// constraint within an argument conversion, returns information about the
  /// application of the argument to its parameter. If the locator is not
  /// for an argument conversion, returns \c None.
  std::optional<FunctionArgApplyInfo>
  getFunctionArgApplyInfo(ConstraintLocator *) const;

  /// Retrieve the builder transform that was applied to this function, if any.
  const AppliedBuilderTransform *getAppliedBuilderTransform(
     AnyFunctionRef fn) const {
    auto known = resultBuilderTransformed.find(fn);
    return known != resultBuilderTransformed.end()
        ? &known->second
        : nullptr;
  }

  /// Retrieve the solved ExprPattern that corresponds to provided
  /// sub-expression.
  NullablePtr<ExprPattern> getExprPatternFor(Expr *E) const {
    auto result = exprPatterns.find(E);
    if (result == exprPatterns.end())
      return nullptr;

    return result->second;
  }

  /// This method implements functionality of `Expr::isTypeReference`
  /// with data provided by a given solution.
  bool isTypeReference(Expr *E) const;

  /// Call Expr::isIsStaticallyDerivedMetatype on the given
  /// expression, using a custom accessor for the type on the
  /// expression that reads the type from the Solution
  /// expression type map.
  bool isStaticallyDerivedMetatype(Expr *E) const;

  /// Retrieve the argument list that is associated with a call at the given
  /// locator.
  ArgumentList *getArgumentList(ConstraintLocator *locator) const;

  std::optional<ConversionRestrictionKind>
  getConversionRestriction(CanType type1, CanType type2) const;

  SWIFT_DEBUG_DUMP;

  /// Dump this solution.
  void dump(raw_ostream &OS, unsigned indent) const LLVM_ATTRIBUTE_USED;
};

}  // end namespace constraints

}  // end namespace swift

#endif  // SWIFT_SEMA_SOLUTION_H
