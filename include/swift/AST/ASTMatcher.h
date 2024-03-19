//===--- ASTMatcher.h - Recursive AST Matcher -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines ASTMatcher, which performs a match between two ASTNode
// instances, calling out differences for subclasses to process specifically.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTMATCHER_H
#define SWIFT_AST_ASTMATCHER_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ArgumentList.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SimpleDisplay.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Debug.h"
#include <type_traits>

#define DEBUG_TYPE "astmatcher"

namespace swift {

/// A CRTP class that recursively matches two `ASTNode` instances by value,
/// calling out places where their parsed or (TODO) semantic state are not
/// equivalent and allowing the implementing subclass to take specific actions
/// in response to such mismatches.
///
/// Subclasses must implement at least one public catch-all `mismatch` method,
/// which will be called when a mismatch is found:
///
/// \code
/// Action mismatch(ASTNode lhs, ASTNode rhs);
/// \endcode
///
/// For mismatches that do not cross class hierarchies (for example, a mismatch
/// between `Expr` subclasses, but not between `Expr` and `Stmt` subclasses),
/// `mismatch` is called with instances of final subclasses. Otherwise,
/// `mismatch` is called with two `ASTNode` instances. Hence, a client that
/// wants to, say, very specifically catch `TypeRepr` mismatches of the form
/// `T` vs. `T...` could isolate this case with a `mismatch` overload while
/// having other mismatches fall into the catch-all overload:
///
/// \code
/// Action mismatch(SimpleIdentTypeRepr *lhs, VarargTypeRepr *rhs);
/// \endcode
///
/// A `mismatch` method is to return an `ASTMatcher::Action` to indicate how to
/// proceed with the matching.
///
/// \warning The current implementation assumes that matched nodes share the
/// same `ASTContext` and source buffer.
template <class ImplClass>
class ASTMatcher {
  SourceLoc LhsMatchingLoc;

  class MismatchRelay;

  template <bool MatchingLocations>
  class Traversal;

public:
  ASTMatcher() = default;

  /// An action to perform on a mismatch.
  enum class MismatchAction {
    /// Continue matching the given two nodes.
    Continue,

    /// Abort the matching.
    Stop,
  };

  /// Gets the nearest left-hand side source location to the current matching
  /// position, according to the left-hand side AST.
  ///
  /// \note Not provided in calls to `MatchingLocations = false` instantiations
  /// of `match`.
  SourceLoc getLhsMatchingLoc() const { return LhsMatchingLoc; }

  /// Matches the given two non-null nodes. If the `MatchingLocations` is set to
  /// `true`,
  ///
  /// \returns `true` if the matching was aborted; `false` otherwise.
  template <bool MatchingLocations = false>
  bool match(ASTNode lhs, ASTNode rhs, const ASTContext *ctx) {
    // `ImplClass` must inherit from 'ASTMatcher'.
    static_assert(std::is_base_of_v<ASTMatcher, ImplClass>);

    return Traversal<MatchingLocations>::match(MismatchRelay(*this), lhs, rhs,
                                               ctx);
  }
};

/// An intermediary interface standing between a matcher and a traversal that,
/// most importantly, defines the sets of type pairs with which mismatches may
/// (1) be called out to it for relay and (2) relayed to the matcher's
/// implementing subclass.
template <class ImplClass>
class ASTMatcher<ImplClass>::MismatchRelay final {
  ASTMatcher &Matcher;

public:
  MismatchRelay(ASTMatcher &Matcher) : Matcher(Matcher) {}

  void setLhsMatchingLoc(SourceLoc loc) { Matcher.LhsMatchingLoc = loc; }

private:
  template <class T>
  static constexpr bool isSpecificASTNode = std::bool_constant<
#define IS_SAME(TYPE) std::is_same_v<T, TYPE> ||
#define TYPEREPR(CLASS, _) IS_SAME(CLASS##TypeRepr)
#include "swift/AST/TypeReprNodes.def"
#undef IS_SAME
      false>::value;

public:
  template <class T>
  static constexpr bool isMismatchable =
      std::bool_constant<isSpecificASTNode<T> || std::is_same_v<T, TypeRepr> ||
                         std::is_same_v<T, ASTNode>>::value;

  MismatchAction mismatch(ASTNode lhs, ASTNode rhs) {
    assert(lhs.getTypeDiscriminator() != rhs.getTypeDiscriminator());
    return static_cast<ImplClass &>(Matcher).mismatch(lhs, rhs);
  }

  // Final dispatch: both types are final subclasses.

  template <class LhsTy, class RhsTy,
            std::enable_if_t<isSpecificASTNode<LhsTy> &&
                             isSpecificASTNode<RhsTy>> * = nullptr>
  MismatchAction mismatch(const LhsTy &lhs, const RhsTy &rhs) {
    return static_cast<ImplClass &>(Matcher).mismatch(
        const_cast<LhsTy *>(&lhs), const_cast<RhsTy *>(&rhs));
  }

  // Secondary dispatch: the left-hand side type is a final subclass, the
  // right-hand side type is a base class.

  template <class LhsTy>
  MismatchAction mismatch(const LhsTy &lhs, const TypeRepr &rhs) {
    switch (rhs.getKind()) {
#define TYPEREPR(CLASS, _)                                                     \
  case TypeReprKind::CLASS:                                                    \
    return mismatch(lhs, static_cast<const CLASS##TypeRepr &>(rhs));
#include "swift/AST/TypeReprNodes.def"
    }
  }

  // Primary dispatch: both types are base classes.

  MismatchAction mismatch(const TypeRepr &lhs, const TypeRepr &rhs) {
    switch (lhs.getKind()) {
#define TYPEREPR(CLASS, _)                                                     \
  case TypeReprKind::CLASS:                                                    \
    return mismatch(static_cast<const CLASS##TypeRepr &>(lhs), rhs);
#include "swift/AST/TypeReprNodes.def"
    }
  }
};

// MARK: - Utilities

namespace ast_matcher_internal {

template <class T>
inline static constexpr bool is_integral_or_enum_v =
    std::bool_constant<std::is_integral_v<T> || std::is_enum_v<T>>::value;

template <class T>
struct superclass {
  using type = void;
};

#define SPECIALIZATION(CLASS, PARENT)                                          \
  template <>                                                                  \
  struct superclass<CLASS> {                                                   \
    using type = PARENT;                                                       \
  };
#define TYPEREPR(CLASS, PARENT) SPECIALIZATION(CLASS##TypeRepr, PARENT)
#define ABSTRACT_TYPEREPR(CLASS, PARENT) TYPEREPR(CLASS, PARENT)
#include "swift/AST/TypeReprNodes.def"
#undef SPECIALIZATION

} // end namespace ast_matcher_internal

// NB: Macros that are not designated for direct usage are prefixed with an
// underscore.
//
// MARK: - Metaprogramming

// MARK: Matching macros

/// If `MatchingLocations` is `true`, sets the current matching locations to the
/// given values.
#define SET_MATCHING_LOC(LHS_LOC)                                              \
  if (MatchingLocations) {                                                     \
    if (LHS_LOC.isValid()) {                                                   \
      this->Handler.setLhsMatchingLoc(LHS_LOC);                                \
    }                                                                          \
  }

/// Appends `PROPERTY` to the matched arguments and, if the `MatchingLocations`
/// template parameter is set to `true`, sets the current matching locations to
/// the resulting values if they are not valid.
#define SET_MATCHING_LOC_PROPERTY(PROPERTY) SET_MATCHING_LOC(lhs PROPERTY)

/// Reports a mismatch between the given values and handles the response action.
#define _MISMATCH(LHS, RHS)                                                    \
  LLVM_DEBUG(this->dumpMismatch(llvm::dbgs(), LHS, RHS));                      \
  switch (this->Handler.mismatch(mismatchLhs, mismatchRhs)) {                  \
  case MismatchAction::Continue:                                               \
    break;                                                                     \
  case MismatchAction::Stop:                                                   \
    return true;                                                               \
  }

/// Directly matches the matched arguments via equation.
///
/// \warning This is not a substitute for `MATCH` macros that recurse; use only
/// to implement matching functions for simple types that must be matched by
/// equation.
#define MATCH_EQUATE                                                           \
  if (!(lhs == rhs)) {                                                         \
    _MISMATCH(lhs, rhs)                                                        \
  }

/// Directly matches the given values via equation, but exits upon mismatch even
/// if asked to continue. Use when further mismatches would be unhelpful.
#define MATCH_OR_RETURN(LHS, RHS, PATH)                                        \
  static_assert(std::is_same_v<decltype(LHS), decltype(RHS)>);                 \
  static_assert(ast_matcher_internal::is_integral_or_enum_v<decltype(LHS)>,    \
                "Expected value of integral or enum type");                    \
  LLVM_DEBUG(this->Path.push_back(PATH));                                      \
  if (!(LHS == RHS)) {                                                         \
    _MISMATCH(LHS, RHS)                                                        \
    LLVM_DEBUG(this->Path.pop_back());                                         \
    return false;                                                              \
  }                                                                            \
  LLVM_DEBUG(this->Path.pop_back());

/// Appends `PROPERTY` to the matched arguments and directly matches the
/// resulting values via equation, but exits upon mismatch even if asked to
/// continue. Use when further mismatches would be unhelpful.
#define MATCH_PROPERTY_OR_RETURN(PROPERTY)                                     \
  MATCH_OR_RETURN(lhs PROPERTY, rhs PROPERTY, #PROPERTY)

/// Recursively matches the given pair of values using matching method `FN` and
/// propagates out a result of `true`.
#define _MATCH_WITH_FN(FN, LHS, RHS, PATH)                                     \
  static_assert(std::is_same_v<decltype(LHS), decltype(RHS)>);                 \
  {                                                                            \
    constexpr const bool isPathNil =                                           \
        std::is_same_v<decltype(PATH), const std::nullopt_t>;                  \
    if constexpr (!isPathNil) {                                                \
      LLVM_DEBUG(this->Path.push_back(PATH));                                  \
    }                                                                          \
    bool shouldAbort;                                                          \
    if constexpr (decltype(Handler)::template isMismatchable<                  \
                      llvm::remove_cvref_t<decltype(LHS)>>) {                  \
      shouldAbort = this->FN(LHS, RHS, LHS, RHS);                              \
    } else {                                                                   \
      shouldAbort = this->FN(LHS, RHS, mismatchLhs, mismatchRhs);              \
    }                                                                          \
    if constexpr (!isPathNil) {                                                \
      LLVM_DEBUG(this->Path.pop_back());                                       \
    }                                                                          \
    if (shouldAbort) {                                                         \
      return true;                                                             \
    }                                                                          \
  }

#define _MATCH_WITH_FN_AS_CLASS(FN, CLASS)                                     \
  static_assert(std::is_class_v<CLASS>);                                       \
  _MATCH_WITH_FN(FN, static_cast<const CLASS &>(lhs),                          \
                 static_cast<const CLASS &>(rhs),                              \
                 ".static_cast<const " #CLASS " &>()")

/// Statically casts the matched arguments to the given class type and
/// recursively matches them using matching method `FN`.
#define MATCH_AS_CLASS(CLASS) _MATCH_WITH_FN_AS_CLASS(match##CLASS, CLASS)

/// Recursively matches the given values. Use only if you cannot use
/// `MATCH_PROPERTY`.
#define MATCH(LHS, RHS, PATH)                                                  \
  if constexpr (std::is_pointer_v<llvm::remove_cvref_t<decltype(LHS)>>) {      \
    _MATCH_WITH_FN(matchPointers, LHS, RHS, PATH)                              \
  } else {                                                                     \
    _MATCH_WITH_FN(match, LHS, RHS, PATH)                                      \
  }

/// Appends `PROPERTY` to the matched arguments and recursively matches the
/// resulting values.
#define MATCH_PROPERTY(PROPERTY) MATCH(lhs PROPERTY, rhs PROPERTY, #PROPERTY)

/// Appends `PROPERTY` to the matched arguments and recursively matches the
/// resulting values as containers.
#define MATCH_CONTAINER_PROPERTY(PROPERTY)                                     \
  _MATCH_WITH_FN(matchContainers, lhs PROPERTY, rhs PROPERTY, #PROPERTY)

// MARK: Function macros

#define ARG(...) __VA_ARGS__

#define _FN_MATCH_TYPE_BASE(TMPL_PARAM_LIST, NAME, TYPE, MISMATCH_TYPE)        \
  template <TMPL_PARAM_LIST>                                                   \
  bool NAME(TYPE lhs, TYPE rhs, const MISMATCH_TYPE &mismatchLhs,              \
            const MISMATCH_TYPE &mismatchRhs)

/// Defines the signature of a matching function template with a custom name
/// and type.
///
/// \warning Unlike others, this macro does NOT wrap `TYPE` in a
/// constant reference.
#define FN_MATCH_CUSTOM_TYPE_TEMPLATE(TMPL_PARAM_LIST, NAME, TYPE)             \
  _FN_MATCH_TYPE_BASE(ARG(class MismatchT, TMPL_PARAM_LIST), NAME, TYPE,       \
                      MismatchT)

#define _FN_MATCH_TYPE_TEMPLATE(TMPL_PARAM_LIST, NAME, TYPE)                   \
  FN_MATCH_CUSTOM_TYPE_TEMPLATE(ARG(TMPL_PARAM_LIST), NAME, const TYPE &)

/// Defines the signature of a matching function template. Use when the matching
/// type is dependent on a type name.
#define FN_MATCH_TYPE_TEMPLATE(TMPL_PARAM_LIST, TYPE)                          \
  _FN_MATCH_TYPE_TEMPLATE(ARG(TMPL_PARAM_LIST), match, TYPE)

/// Defines the signature of a matching function.
#define FN_MATCH_TYPE(TYPE) FN_MATCH_TYPE_TEMPLATE(class = void, TYPE)

#define _FN_MATCH_CLASS(CLASS, NAME)                                           \
  _FN_MATCH_TYPE_TEMPLATE(                                                     \
      ARG(class Super = ast_matcher_internal::superclass<CLASS>::type), NAME,  \
      CLASS) {                                                                 \
    if constexpr (!std::is_same_v<void, typename ast_matcher_internal::        \
                                            superclass<Super>::type>) {        \
      _MATCH_WITH_FN_AS_CLASS(matchSuperclass, Super)                          \
    }                                                                          \
    _MATCH_WITH_FN(NAME##Impl, lhs, rhs, std::nullopt)                         \
    return false;                                                              \
  }                                                                            \
  _FN_MATCH_TYPE_TEMPLATE(class = void, NAME##Impl, CLASS)

/// Defines the signature of a matching function for an abstract superclass
/// type.
#define FN_MATCH_SUPERCLASS(CLASS) _FN_MATCH_CLASS(CLASS, matchSuperclass)

/// Defines the signature of a matching function for a subclass type.
#define FN_MATCH_SUBCLASS(CLASS) _FN_MATCH_CLASS(CLASS, match##CLASS)

// MARK: - Matching

/// The class that performs the recursive matching between two `ASTNode`
/// instances.
template <class ImplClass>
template <bool MatchingLocations>
class ASTMatcher<ImplClass>::Traversal final {
  MismatchRelay Handler;

  const ASTContext *Ctx;

  // The current matching path.
  SmallVector<StringRef> Path;

  Traversal(MismatchRelay handler, const ASTContext *ctx)
      : Handler(handler), Ctx(ctx) {
    assert(ctx);
  }

public:
  /// Matches the given two non-null nodes.
  ///
  /// \returns `true` if the matching was aborted; `false` otherwise.
  static bool match(MismatchRelay handler, ASTNode lhs, ASTNode rhs,
                    const ASTContext *ctx) {
    Traversal traversal(handler, ctx);
    return traversal.match(lhs, rhs, lhs, rhs);
  }

private:
  // MARK: Mismatch debugging

  template <class T>
  void dumpMismatch(llvm::raw_ostream &os, T lhs, T rhs) {
    const char *prefix = "[" DEBUG_TYPE "] ";

    const auto dump = [](llvm::raw_ostream &os, T val, const ASTContext *ctx) {
      if constexpr (std::is_same_v<SourceLoc, llvm::remove_cvref_t<T>>) {
        val.print(os, ctx->SourceMgr);
      } else {
        simple_display(os, val);
      }
    };

    // Trim the first leading '.', and leading '.' following a '->' element.
    this->Path.front().consume_front(".");
    for (size_t i = 1, e = this->Path.size(); i < e; ++i) {
      if (this->Path[i] == "->") {
        this->Path[++i].consume_front(".");
      }
    }

    (os << prefix) << "Mismatch at '";
    os << llvm::join(this->Path, "");
    os << "'\n";

    (os << prefix) << "LHS is: ";
    dump(os, lhs, this->Ctx);
    os << "\n";
    (os << prefix) << "RHS is: ";
    dump(os, rhs, this->Ctx);
    os << "\n";
  }

private:
  // MARK: ASTNode

  FN_MATCH_TYPE(ASTNode) {
    assert(lhs && rhs && "Matched nodes must both be non-null");
    assert(lhs.is<TypeRepr *>() || lhs.is<Expr *>() || lhs.is<Pattern *>() ||
           lhs.is<Stmt *>() || lhs.is<StmtConditionElement *>() ||
           lhs.is<CaseLabelItem *>() || lhs.is<Decl *>());

    SET_MATCHING_LOC_PROPERTY(.getStartLoc())
    MATCH_PROPERTY(.dyn_cast<TypeRepr *>())
    MATCH_PROPERTY(.dyn_cast<Expr *>())
    MATCH_PROPERTY(.dyn_cast<Pattern *>())
    MATCH_PROPERTY(.dyn_cast<StmtConditionElement *>())
    MATCH_PROPERTY(.dyn_cast<CaseLabelItem *>())
    MATCH_PROPERTY(.dyn_cast<Stmt *>())
    MATCH_PROPERTY(.dyn_cast<Decl *>())

    return false;
  }

  // MARK: Pointers, containers, integrals

  /// Matches pointers.
  FN_MATCH_CUSTOM_TYPE_TEMPLATE(typename T, matchPointers, const T *) {
    if (lhs == rhs) {
      return false;
    }

    MATCH_OR_RETURN((bool)lhs, (bool)rhs, ".static_cast<bool>()")
    MATCH(*lhs, *rhs, "->")
    return false;
  }

  /// Matches containers.
  FN_MATCH_CUSTOM_TYPE_TEMPLATE(typename C, matchContainers, const C &) {
    auto lhsIt = lhs.begin();
    auto rhsIt = rhs.begin();
    const auto lhsEnd = lhs.end();
    const auto rhsEnd = rhs.end();

    if (lhsIt != lhsEnd && rhsIt != rhsEnd) {
      llvm::SmallString<24> idxStr;
      llvm::raw_svector_ostream os(idxStr);

      for (size_t idx = 0; lhsIt != lhsEnd && rhsIt != rhsEnd;
           ++lhsIt, ++rhsIt, ++idx) {
        LLVM_DEBUG(os << "[" << idx << "]");
        MATCH(*lhsIt, *rhsIt, os.str())
        LLVM_DEBUG(idxStr.clear());
      }
    }

    MATCH((lhsIt == lhsEnd), (rhsIt == rhsEnd), ".size()")
    return false;
  }

  /// Matches values of integral or enum type.
  FN_MATCH_TYPE_TEMPLATE(
      ARG(typename T,
          std::enable_if_t<ast_matcher_internal::is_integral_or_enum_v<T>> * =
              nullptr),
      T) {
    MATCH_EQUATE
    return false;
  }

  // MARK: Common types

  FN_MATCH_TYPE_TEMPLATE(typename... T, llvm::PointerUnion<T...>) {
    if (([&] {
          MATCH_PROPERTY(.template dyn_cast<T>())
          return false;
        }() ||
         ...)) {
      return true;
    }

    return false;
  }

  FN_MATCH_TYPE_TEMPLATE(typename T, std::optional<T>) {
    MATCH_PROPERTY_OR_RETURN(.has_value())
    if (lhs.has_value()) {
      MATCH_PROPERTY(.value())
    }

    return false;
  }

  FN_MATCH_TYPE_TEMPLATE(typename T, Located<T>) {
    MATCH_PROPERTY(.Loc)
    MATCH_PROPERTY(.Item)
    return false;
  }

  FN_MATCH_TYPE(SourceLoc) {
    SET_MATCHING_LOC_PROPERTY()
    MATCH_EQUATE
    return false;
  }

  FN_MATCH_TYPE(LayoutConstraintLoc) {
    MATCH_PROPERTY(.getLoc())
    MATCH_PROPERTY(.getLayoutConstraint())
    return false;
  }

  FN_MATCH_TYPE(RequirementRepr) {
    SET_MATCHING_LOC_PROPERTY(.getSourceRange().Start)
    MATCH_PROPERTY_OR_RETURN(.getKind())
    MATCH_PROPERTY(.isExpansionPattern())
    MATCH_PROPERTY(.FirstType)
    MATCH_PROPERTY(.getSeparatorLoc())

    switch (lhs.getKind()) {
    case RequirementReprKind::TypeConstraint:
    case RequirementReprKind::SameType:
      MATCH_PROPERTY(.SecondType)
      break;
    case RequirementReprKind::LayoutConstraint:
      MATCH_PROPERTY(.getLayoutConstraintLoc())
      break;
    }

    // Sema state.
    MATCH_PROPERTY(.isInvalid())
    return false;
  }

  FN_MATCH_TYPE(GenericParamList) {
    MATCH_PROPERTY(.getLAngleLoc())
    MATCH_CONTAINER_PROPERTY(.getParams())
    MATCH_PROPERTY(.getWhereLoc())
    MATCH_CONTAINER_PROPERTY(.getRequirements())
    MATCH_PROPERTY(.getRAngleLoc())
    return false;
  }

  FN_MATCH_TYPE(Argument) {
    MATCH_PROPERTY(.getLabelLoc())
    MATCH_PROPERTY(.getLabel())
    MATCH_PROPERTY(.getExpr())
    return false;
  }

  FN_MATCH_TYPE(ArgumentList) {
    SET_MATCHING_LOC_PROPERTY(.getLParenLoc())
    MATCH_PROPERTY(.isImplicit())
    MATCH_PROPERTY(.getLParenLoc())
    MATCH_CONTAINER_PROPERTY()
    MATCH_PROPERTY(.getRParenLoc())
    return false;
  }

  FN_MATCH_TYPE(DeclNameLoc) {
    MATCH_PROPERTY(.getBaseNameLoc())
    MATCH_PROPERTY_OR_RETURN(.isCompound())
    MATCH_PROPERTY(.getLParenLoc())
    MATCH_CONTAINER_PROPERTY(.getArgumentLabelLocs())
    MATCH_PROPERTY(.getRParenLoc())
    return false;
  }

#define FN_MATCH_TYPE_SIMPLE(TYPE)                                             \
  FN_MATCH_TYPE(TYPE) {                                                        \
    MATCH_EQUATE                                                               \
    return false;                                                              \
  }
  FN_MATCH_TYPE_SIMPLE(StringRef)
  FN_MATCH_TYPE_SIMPLE(DeclNameRef)
  FN_MATCH_TYPE_SIMPLE(Identifier)
  FN_MATCH_TYPE_SIMPLE(LayoutConstraint)
#undef FN_MATCH_TYPE_SIMPLE

  // MARK: TypeAttribute

  FN_MATCH_TYPE(TypeAttribute) {
    MATCH_PROPERTY(.getAtLoc())
    MATCH_PROPERTY(.isImplicit())
    MATCH_PROPERTY(.getAttrLoc())
    MATCH_PROPERTY_OR_RETURN(.getKind())

    switch (lhs.getKind()) {
#define SIMPLE_TYPE_ATTR(_, CLASS) case TypeAttrKind::CLASS:
#include "swift/AST/TypeAttr.def"
      break;
#define SIMPLE_TYPE_ATTR(_, CLASS)
#define TYPE_ATTR(_, CLASS)                                                    \
  case TypeAttrKind::CLASS:                                                    \
    MATCH_AS_CLASS(CLASS##TypeAttr)                                            \
    break;
#include "swift/AST/TypeAttr.def"
    }

    // Sema state.
    MATCH_PROPERTY(.isInvalid())
    return false;
  }

  FN_MATCH_SUBCLASS(ConventionTypeAttr) {
    MATCH_PROPERTY(.getConventionLoc())
    MATCH_PROPERTY(.getConventionName())
    MATCH_PROPERTY(.getClangTypeLoc())
    MATCH_PROPERTY(.getWitnessMethodProtocol())
    return false;
  }

  FN_MATCH_SUBCLASS(OpaqueReturnTypeOfTypeAttr) {
    MATCH_PROPERTY(.getMangledNameLoc())
    MATCH_PROPERTY(.getMangledName())
    MATCH_PROPERTY(.getIndexLoc())
    MATCH_PROPERTY(.getIndex())
    return false;
  }

  FN_MATCH_SUBCLASS(DifferentiableTypeAttr) {
    MATCH_PROPERTY(.getDifferentiabilityLoc())
    MATCH_PROPERTY(.getDifferentiability())
    return false;
  }

  FN_MATCH_SUBCLASS(OpenedTypeAttr) {
    MATCH_PROPERTY(.getConstraintType())
    return false;
  }

  FN_MATCH_SUBCLASS(PackElementTypeAttr) {
    MATCH_PROPERTY(.getUUIDLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(IsolatedTypeAttr) {
    MATCH_PROPERTY(.getIsolationKindLoc())
    MATCH_PROPERTY(.getIsolationKind())
    return false;
  }

  // MARK: DeclAttribute

  FN_MATCH_TYPE(DeclAttribute) {
    MATCH_PROPERTY(.AtLoc)
    MATCH_PROPERTY(.isImplicit())
    MATCH_PROPERTY(.getRange().Start)
    MATCH_PROPERTY_OR_RETURN(.getKind())

    switch (lhs.getKind()) {
    case DeclAttrKind::Custom: {
      MATCH_AS_CLASS(CustomAttr)
      break;
    }
    default:
      llvm_unreachable("Unimplemented");
    }
    MATCH_PROPERTY(.getRange().End)

    // Sema state.
    MATCH_PROPERTY(.isInvalid())
    MATCH_PROPERTY(.getAddedByAccessNote())
    return false;
  }

  FN_MATCH_SUBCLASS(CustomAttr) {
    MATCH_PROPERTY(.getTypeRepr())
    MATCH_PROPERTY(.getArgs())
    // FIXME: Match 'CustomAttr.initContext'?
    return false;
  }

  // MARK: TypeRepr

  FN_MATCH_TYPE(TypeRepr) {
    SET_MATCHING_LOC_PROPERTY(.getStartLoc())
    MATCH_PROPERTY_OR_RETURN(.getKind())

    switch (lhs.getKind()) {
#define TYPEREPR(CLASS, _)                                                     \
  case TypeReprKind::CLASS:                                                    \
    MATCH_AS_CLASS(CLASS##TypeRepr)                                            \
    break;
#include "swift/AST/TypeReprNodes.def"
    }

    // Sema state.
    MATCH_PROPERTY(.isInvalid())
    MATCH_PROPERTY(.isWarnedAbout())
    return false;
  }

  FN_MATCH_SUBCLASS(ErrorTypeRepr) {
    MATCH_PROPERTY(.getSourceRange().Start)
    MATCH_PROPERTY(.getSourceRange().End)
    return false;
  }

  FN_MATCH_SUBCLASS(PlaceholderTypeRepr) {
    MATCH_PROPERTY(.getUnderscoreLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(FixedTypeRepr) {
    return false; // Constructed only by Sema.
  }

  FN_MATCH_SUBCLASS(SelfTypeRepr) {
    return false; // Constructed only by Sema.
  }

  FN_MATCH_SUBCLASS(ArrayTypeRepr) {
    MATCH_PROPERTY(.getBrackets().Start)
    MATCH_PROPERTY(.getBase())
    MATCH_PROPERTY(.getBrackets().End)
    return false;
  }

  FN_MATCH_SUBCLASS(DictionaryTypeRepr) {
    MATCH_PROPERTY(.getBrackets().Start)
    MATCH_PROPERTY(.getKey())
    MATCH_PROPERTY(.getColonLoc())
    MATCH_PROPERTY(.getValue())
    MATCH_PROPERTY(.getBrackets().End)
    return false;
  }

  FN_MATCH_SUBCLASS(OptionalTypeRepr) {
    MATCH_PROPERTY(.getBase())
    MATCH_PROPERTY(.getQuestionLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(ImplicitlyUnwrappedOptionalTypeRepr) {
    MATCH_PROPERTY(.getBase())
    MATCH_PROPERTY(.getExclamationLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(MetatypeTypeRepr) {
    MATCH_PROPERTY(.getBase())
    MATCH_PROPERTY(.getMetaLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(ProtocolTypeRepr) {
    MATCH_PROPERTY(.getBase())
    MATCH_PROPERTY(.getProtocolLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(VarargTypeRepr) {
    MATCH_PROPERTY(.getElementType())
    MATCH_PROPERTY(.getEllipsisLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(OpaqueReturnTypeRepr) {
    MATCH_PROPERTY(.getOpaqueLoc())
    MATCH_PROPERTY(.getConstraint())
    return false;
  }

  FN_MATCH_SUBCLASS(ExistentialTypeRepr) {
    MATCH_PROPERTY(.getAnyLoc())
    MATCH_PROPERTY(.getConstraint())
    return false;
  }

  FN_MATCH_SUBCLASS(InverseTypeRepr) {
    MATCH_PROPERTY(.getTildeLoc())
    MATCH_PROPERTY(.getConstraint())
    return false;
  }

  FN_MATCH_SUPERCLASS(SpecifierTypeRepr) {
    MATCH_PROPERTY(.getSpecifierLoc())
    MATCH_PROPERTY(.getBase())
    return false;
  }

  FN_MATCH_SUBCLASS(IsolatedTypeRepr) { return false; }
  FN_MATCH_SUBCLASS(CompileTimeConstTypeRepr) { return false; }
  FN_MATCH_SUBCLASS(ResultDependsOnTypeRepr) { return false; }
  FN_MATCH_SUBCLASS(TransferringTypeRepr) { return false; }

  FN_MATCH_SUBCLASS(OwnershipTypeRepr) {
    MATCH_PROPERTY(.getSpecifier())
    return false;
  }

  FN_MATCH_TYPE(LifetimeDependenceSpecifier) {
    MATCH_PROPERTY(.getLoc())
    MATCH_PROPERTY(.getSpecifierKind())
    MATCH_PROPERTY_OR_RETURN(.getLifetimeDependenceKind())
    MATCH_PROPERTY(.getName())
    MATCH_PROPERTY(.getIndex())
    return false;
  }

  FN_MATCH_SUBCLASS(LifetimeDependentReturnTypeRepr) {
    MATCH_CONTAINER_PROPERTY(.getLifetimeDependencies())
    return false;
  }

  FN_MATCH_SUBCLASS(PackExpansionTypeRepr) {
    MATCH_PROPERTY(.getRepeatLoc())
    MATCH_PROPERTY(.getPatternType())
    return false;
  }

  FN_MATCH_SUBCLASS(PackElementTypeRepr) {
    MATCH_PROPERTY(.getEachLoc())
    MATCH_PROPERTY(.getPackType())
    return false;
  }

  FN_MATCH_SUBCLASS(NamedOpaqueReturnTypeRepr) {
    MATCH_PROPERTY(.getGenericParams())
    MATCH_PROPERTY(.getBase())
    return false;
  }

  FN_MATCH_SUBCLASS(PackTypeRepr) {
    MATCH_PROPERTY(.getKeywordLoc())
    MATCH_PROPERTY(.getBracesRange().Start)
    MATCH_CONTAINER_PROPERTY(.getElements())
    MATCH_PROPERTY(.getBracesRange().End)
    return false;
  }

  FN_MATCH_SUPERCLASS(DeclRefTypeRepr) {
    MATCH_PROPERTY(.getNameLoc())
    MATCH_PROPERTY(.getNameRef())
    return false;
  }

  FN_MATCH_SUPERCLASS(IdentTypeRepr) { return false; }
  FN_MATCH_SUBCLASS(SimpleIdentTypeRepr) { return false; }

  FN_MATCH_SUBCLASS(GenericIdentTypeRepr) {
    MATCH_PROPERTY(.getAngleBrackets().Start)
    MATCH_CONTAINER_PROPERTY(.getGenericArgs())
    MATCH_PROPERTY(.getAngleBrackets().End)
    return false;
  }

  FN_MATCH_SUBCLASS(MemberTypeRepr) {
    MATCH_PROPERTY(.getBase())
    return false;
  }

  FN_MATCH_TYPE(TupleTypeReprElement) {
    MATCH_PROPERTY(.NameLoc)
    MATCH_PROPERTY(.Name)
    MATCH_PROPERTY(.UnderscoreLoc)
    MATCH_PROPERTY(.SecondNameLoc)
    MATCH_PROPERTY(.SecondName)
    MATCH_PROPERTY(.ColonLoc)
    MATCH_PROPERTY(.Type)
    MATCH_PROPERTY(.TrailingCommaLoc)
    return false;
  }

  FN_MATCH_SUBCLASS(TupleTypeRepr) {
    MATCH_PROPERTY(.getParens().Start)
    MATCH_CONTAINER_PROPERTY(.getElements())
    MATCH_PROPERTY(.getParens().End)
    return false;
  }

  FN_MATCH_SUBCLASS(CompositionTypeRepr) {
    MATCH_PROPERTY(.getSourceLoc())
    MATCH_PROPERTY(.getCompositionRange().Start)
    MATCH_CONTAINER_PROPERTY(.getTypes())
    MATCH_PROPERTY(.getCompositionRange().End)
    return false;
  }

  FN_MATCH_TYPE(SILBoxTypeReprField) {
    MATCH_PROPERTY(.getLoc())
    MATCH_PROPERTY(.isMutable())
    MATCH_PROPERTY(.getFieldType())
    return false;
  }

  FN_MATCH_SUBCLASS(SILBoxTypeRepr) {
    MATCH_PROPERTY(.getGenericParams())
    MATCH_PROPERTY(.getLBraceLoc())
    MATCH_CONTAINER_PROPERTY(.getFields())
    MATCH_PROPERTY(.getRBraceLoc())
    MATCH_PROPERTY(.getArgumentLAngleLoc())
    MATCH_CONTAINER_PROPERTY(.getGenericArguments())
    MATCH_PROPERTY(.getArgumentRAngleLoc())
    return false;
  }

  FN_MATCH_SUBCLASS(FunctionTypeRepr) {
    MATCH_PROPERTY(.getGenericParams())
    MATCH_PROPERTY(.getPatternGenericParams())
    MATCH_PROPERTY(.getArgsTypeRepr())
    MATCH_PROPERTY(.getAsyncLoc())
    MATCH_PROPERTY(.getThrowsLoc())
    MATCH_PROPERTY(.getArrowLoc())
    MATCH_PROPERTY(.getResultTypeRepr())
    MATCH_CONTAINER_PROPERTY(.getPatternSubstitutions())
    MATCH_CONTAINER_PROPERTY(.getInvocationSubstitutions())
    return false;
  }

  FN_MATCH_SUBCLASS(AttributedTypeRepr) {
    MATCH_CONTAINER_PROPERTY(.getAttrs())
    MATCH_PROPERTY(.getTypeRepr())
    return false;
  }

  // MARK: Expr

  FN_MATCH_TYPE(Expr) { llvm_unreachable("Not implemented"); }

  // MARK: Pattern

  FN_MATCH_TYPE(Pattern) { llvm_unreachable("Not implemented"); }

  // MARK: StmtConditionElement

  FN_MATCH_TYPE(StmtConditionElement) { llvm_unreachable("Not implemented"); }

  // MARK: CaseLabelItem

  FN_MATCH_TYPE(CaseLabelItem) { llvm_unreachable("Not implemented"); }

  // MARK: Stmt

  FN_MATCH_TYPE(Stmt) { llvm_unreachable("Not implemented"); }

  // MARK: Decl

  FN_MATCH_TYPE(Decl) { llvm_unreachable("Not implemented"); }
};

#undef SET_MATCHING_LOC
#undef SET_MATCHING_LOC_PROPERTY

#undef _MISMATCH
#undef MATCH_EQUATE
#undef MATCH_OR_RETURN
#undef MATCH_PROPERTY_OR_RETURN
#undef _MATCH_WITH_FN
#undef _MATCH_WITH_FN_AS_CLASS
#undef MATCH_AS_CLASS
#undef MATCH
#undef MATCH_PROPERTY
#undef MATCH_CONTAINER_PROPERTY

#undef ARG
#undef _FN_MATCH_TYPE_BASE
#undef FN_MATCH_CUSTOM_TYPE_TEMPLATE
#undef _FN_MATCH_TYPE_TEMPLATE
#undef FN_MATCH_TYPE_TEMPLATE
#undef FN_MATCH_TYPE
#undef _FN_MATCH_CLASS
#undef FN_MATCH_SUPERCLASS
#undef FN_MATCH_SUBCLASS

} // end namespace swift

#undef DEBUG_TYPE

#endif // SWIFT_AST_ASTMATCHER_H
