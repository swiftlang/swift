//===--- ArgumentList.h - Function and subscript argument lists -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Argument and ArgumentList classes and support logic.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ARGUMENTLIST_H
#define SWIFT_AST_ARGUMENTLIST_H

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Debug.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
class OriginalArguments;

/// Forward declared trampoline for Expr::getType.
Type __Expr_getType(Expr *E);

/// Represents a single argument in an argument list, including its label
/// information and inout-ness.
///
/// \code
/// foo.bar(arg, label: arg2, other: &arg3)
///         ^^^               ^^^^^^^^^^^^
///         an unlabeled      'other' is the label, 'arg3' is the expr,
///         argument          and isInout() is true.
/// \endcode
class Argument final {
  SourceLoc LabelLoc;
  Identifier Label;
  Expr *ArgExpr;
  // TODO: Store inout bit here.

public:
  Argument(SourceLoc labelLoc, Identifier label, Expr *expr)
      : LabelLoc(labelLoc), Label(label), ArgExpr(expr) {}

  /// Make an unlabeled argument.
  static Argument unlabeled(Expr *expr) {
    return Argument(SourceLoc(), Identifier(), expr);
  }

  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceRange getSourceRange() const;

  /// If the argument has a label with location information, returns the
  /// location.
  ///
  /// Note this may be present even if the label is empty, e.g for multiple
  /// trailing closures where labels are required such as '_: {}'.
  SourceLoc getLabelLoc() const { return LabelLoc; }

  /// The argument label written in the call.
  Identifier getLabel() const { return Label; }

  /// Set a new argument label.
  ///
  /// Note this is marked \c & to prevent its use on an rvalue Argument vended
  /// from an ArgumentList.
  void setLabel(Identifier newLabel) & { Label = newLabel; }

  /// The argument expression.
  Expr *getExpr() const { return ArgExpr; }

  /// Set a new argument expression.
  ///
  /// Note this is marked \c & to prevent its use on an rvalue Argument vended
  /// from an ArgumentList.
  void setExpr(Expr *newExpr) & { ArgExpr = newExpr; }

  /// Whether the argument is \c inout, denoted with the '&' prefix.
  bool isInOut() const;

  bool operator==(const Argument &other) {
    return LabelLoc == other.LabelLoc && Label == other.Label &&
           ArgExpr == other.ArgExpr;
  }
  bool operator!=(const Argument &other) { return !(*this == other); }
};

/// Represents the argument list of a function call or subscript access.
/// \code
/// foo.bar(arg, label: arg2, other: &arg3)
///        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// \endcode
///
/// Note that arguments are split up and stored in separate buffers to optimize
/// ArgumentList's memory footprint for the unlabeled cases.
class alignas(Argument) ArgumentList final
    : public ASTAllocated<ArgumentList>,
      private llvm::TrailingObjects<ArgumentList, Expr *, Identifier,
                                    SourceLoc> {
  friend TrailingObjects;

  SourceLoc LParenLoc;
  SourceLoc RParenLoc;

  /// The number of arguments in the list.
  unsigned NumArgs : 16;

  /// The index of the first trailing closure, or \c NumArgs if there are no
  /// trailing closures. In type-checked AST, this may be an index to a variadic
  /// expansion that contains the first trailing closure.
  unsigned RawFirstTrailingClosureIndex : 16;

  /// Whether the argument list has multiple trailing closures.
  bool HasMultipleTrailingClosures : 1;

  /// Whether the argument list was implicitly generated.
  bool IsImplicit : 1;

  /// Whether any of the arguments are labeled.
  bool HasLabels : 1;

  /// Whether any of the arguments have labels with location info.
  bool HasLabelLocs : 1;

  ArgumentList(SourceLoc lParenLoc, SourceLoc rParenLoc, unsigned numArgs,
               Optional<unsigned> firstTrailingClosureIndex,
               bool hasMultipleTrailingClosures, bool isImplicit,
               bool hasLabels, bool hasLabelLocs)
      : LParenLoc(lParenLoc), RParenLoc(rParenLoc), NumArgs(numArgs),
        HasMultipleTrailingClosures(hasMultipleTrailingClosures),
        IsImplicit(isImplicit), HasLabels(hasLabels),
        HasLabelLocs(hasLabelLocs) {
    assert(NumArgs == numArgs && "Num args truncated");
    if (auto idx = firstTrailingClosureIndex) {
      assert(*idx < numArgs);
      RawFirstTrailingClosureIndex = *idx;
    } else {
      RawFirstTrailingClosureIndex = numArgs;
      assert(!HasMultipleTrailingClosures);
    }
  }

  ArgumentList(const ArgumentList &) = delete;
  ArgumentList &operator=(const ArgumentList &) = delete;

  size_t numTrailingObjects(OverloadToken<Expr *>) const {
    return NumArgs;
  }
  size_t numTrailingObjects(OverloadToken<Identifier>) const {
    return HasLabels ? NumArgs : 0;
  }
  size_t numTrailingObjects(OverloadToken<SourceLoc>) const {
    return HasLabelLocs ? NumArgs : 0;
  }

  ArrayRef<Expr *> getExprsBuffer() const {
    return {getTrailingObjects<Expr *>(), NumArgs};
  }
  MutableArrayRef<Expr *> getExprsBuffer() {
    return {getTrailingObjects<Expr *>(), NumArgs};
  }

  ArrayRef<Identifier> getLabelsBuffer() const {
    assert(HasLabels);
    return {getTrailingObjects<Identifier>(), NumArgs};
  }
  MutableArrayRef<Identifier> getLabelsBuffer() {
    assert(HasLabels);
    return {getTrailingObjects<Identifier>(), NumArgs};
  }

  ArrayRef<SourceLoc> getLabelLocsBuffer() const {
    assert(HasLabelLocs);
    return {getTrailingObjects<SourceLoc>(), NumArgs};
  }
  MutableArrayRef<SourceLoc> getLabelLocsBuffer() {
    assert(HasLabelLocs);
    return {getTrailingObjects<SourceLoc>(), NumArgs};
  }

public:
  /// Create a new ArgumentList.
  ///
  /// \param lParenLoc The location of the opening '('. Note that for a
  /// subscript argument list, this will be for the opening '['.
  /// \param args The list of arguments in the call.
  /// \param rParenLoc The location of the closing ')'. Note that for a
  /// subscript argument list, this will be for the closing ']'.
  /// \param firstTrailingClosureIndex The index of the first trailing closure,
  /// which in type-checked AST may be a variadic expansion with the closure.
  /// \param hasMultipleTrailingClosures Whether there are multiple trailing
  /// closures.
  /// \param isImplicit Whether this is an implicitly generated argument list.
  /// \param arena The arena to allocate the ArgumentList in.
  static ArgumentList *
  create(ASTContext &ctx, SourceLoc lParenLoc, ArrayRef<Argument> args,
         SourceLoc rParenLoc, Optional<unsigned> firstTrailingClosureIndex,
         bool hasMultipleTrailingClosures, bool isImplicit,
         AllocationArena arena = AllocationArena::Permanent);

  /// Create a new ArgumentList.
  ///
  /// \param lParenLoc The location of the opening '('. Note that for a
  /// subscript argument list, this will be for the opening '['.
  /// \param args The list of arguments in the call.
  /// \param rParenLoc The location of the closing ')'. Note that for a
  /// subscript argument list, this will be for the closing ']'.
  /// \param isImplicit Whether this is an implicitly generated argument list.
  /// \param arena The arena to allocate the ArgumentList in.
  ///
  /// The trailing closure information will be inferred from the arguments
  /// provided.
  static ArgumentList *
  create(ASTContext &ctx, SourceLoc lParenLoc, ArrayRef<Argument> args,
         SourceLoc rParenLoc, bool isImplicit,
         AllocationArena arena = AllocationArena::Permanent);

  /// Create a new implicit ArgumentList.
  ///
  /// \param lParenLoc The location of the opening '('. Note that for a
  /// subscript argument list, this will be for the opening '['.
  /// \param args The list of arguments in the call.
  /// \param rParenLoc The location of the closing ')'. Note that for a
  /// subscript argument list, this will be for the closing ']'.
  /// \param arena The arena to allocate the ArgumentList in.
  static ArgumentList *
  createImplicit(ASTContext &ctx, SourceLoc lParenLoc, ArrayRef<Argument> args,
                 SourceLoc rParenLoc,
                 AllocationArena arena = AllocationArena::Permanent);

  /// Create a new implicit ArgumentList with a set of \p args.
  static ArgumentList *
  createImplicit(ASTContext &ctx, ArrayRef<Argument> args,
                 AllocationArena arena = AllocationArena::Permanent);

  /// Create a new implicit ArgumentList with a single labeled argument
  /// expression.
  static ArgumentList *forImplicitSingle(ASTContext &ctx, Identifier label,
                                         Expr *arg);

  /// Create a new implicit ArgumentList with a set of unlabeled arguments.
  static ArgumentList *forImplicitUnlabeled(ASTContext &ctx,
                                            ArrayRef<Expr *> argExprs);

  /// Create a new implicit ArgumentList with a set of argument expressions,
  /// and a DeclNameRef from which to infer the argument labels.
  static ArgumentList *forImplicitCallTo(DeclNameRef fnNameRef,
                                         ArrayRef<Expr *> argExprs,
                                         ASTContext &ctx);

  /// Create a new implicit ArgumentList with a set of argument expressions,
  /// and a ParameterList from which to infer the argument labels.
  static ArgumentList *forImplicitCallTo(ParameterList *params,
                                         ArrayRef<Expr *> argExprs,
                                         ASTContext &ctx);

  /// The location of the opening '('. Note that for a subscript argument list,
  /// this will be for the opening '['.
  SourceLoc getLParenLoc() const { return LParenLoc; }

  /// The location of the closing ')'. Note that for a subscript argument list,
  /// this will be for the closing ']'.
  SourceLoc getRParenLoc() const { return RParenLoc; }

  /// Whether this is an implicitly generated argument list, not written in the
  /// source.
  bool isImplicit() const { return IsImplicit; }

  /// Whether the argument list has any non-empty argument labels.
  bool hasAnyArgumentLabels() const { return HasLabels; }

  SourceLoc getLoc() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceRange getSourceRange() const;

  /// Retrieve the argument expression at a given index.
  Expr *getExpr(unsigned idx) const { return getExprsBuffer()[idx]; }
  void setExpr(unsigned idx, Expr *newExpr) { getExprsBuffer()[idx] = newExpr; }

  /// Retrieve the argument label at a given index.
  Identifier getLabel(unsigned idx) const {
    if (!HasLabels)
      return Identifier();
    return getLabelsBuffer()[idx];
  }

  /// Retrieve the argument label location at a given index.
  SourceLoc getLabelLoc(unsigned idx) const {
    if (!HasLabelLocs)
      return SourceLoc();
    return getLabelLocsBuffer()[idx];
  }

  /// Retrieve the argument at a given index.
  Argument get(unsigned idx) const {
    return Argument(getLabelLoc(idx), getLabel(idx), getExpr(idx));
  }

  unsigned size() const { return NumArgs; }
  bool empty() const { return size() == 0; }

  class iterator final {
    const ArgumentList *ArgList;
    unsigned Idx;

  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = Argument;
    using pointer = const value_type *;
    using reference = value_type;
    using difference_type = unsigned;

    iterator() : ArgList(nullptr), Idx(0) {}
    iterator(const ArgumentList *argList, unsigned idx)
        : ArgList(argList), Idx(idx) {}

    Argument operator*() const {
      assert(ArgList);
      return ArgList->get(Idx);
    }
    Argument operator[](unsigned i) const {
      assert(ArgList);
      return ArgList->get(Idx + i);
    }

    friend bool operator==(const iterator &lhs, const iterator &rhs) {
      assert(lhs.ArgList == rhs.ArgList);
      return lhs.Idx == rhs.Idx;
    }
    friend bool operator!=(const iterator &lhs, const iterator &rhs) {
      return !(lhs == rhs);
    }

    iterator &operator++() { // pre-increment
      ++Idx;
      return *this;
    }
    iterator operator++(int) { // post-increment
      auto tmp = *this;
      ++*this;
      return tmp;
    }
    iterator &operator--() { // pre-decrement
      --Idx;
      return *this;
    }
    iterator operator--(int) { // post-decrement
      auto tmp = *this;
      --*this;
      return tmp;
    }

    iterator &operator+=(unsigned i) {
      Idx += i;
      return *this;
    }
    iterator operator+(unsigned i) const {
      return iterator(ArgList, Idx + i);
    }
    iterator &operator-=(unsigned i) {
      Idx -= i;
      return *this;
    }
    iterator operator-(unsigned i) const {
      return iterator(ArgList, Idx - i);
    }

    unsigned operator-(const iterator &rhs) const {
      assert(ArgList == rhs.ArgList);
      return Idx - rhs.Idx;
    }
    bool operator<(const iterator &rhs) const {
      assert(ArgList == rhs.ArgList);
      return Idx < rhs.Idx;
    }
    bool operator<=(const iterator &rhs) const {
      assert(ArgList == rhs.ArgList);
      return Idx <= rhs.Idx;
    }
    bool operator>(const iterator &rhs) const {
      assert(ArgList == rhs.ArgList);
      return Idx > rhs.Idx;
    }
    bool operator>=(const iterator &rhs) const {
      assert(ArgList == rhs.ArgList);
      return Idx >= rhs.Idx;
    }
  };

  iterator begin() const { return {this, 0}; }
  iterator end() const { return {this, size()}; }

  Argument front() const { return get(0); }
  Argument back() const { return get(size() - 1); }

  Argument operator[](unsigned idx) const { return get(idx); }

  /// Returns the index of the first trailing closure in the argument list, or
  /// \c None if there are no trailing closures.
  ///
  /// Note that in type-checked AST, this may return an index of a variadic
  /// expansion which contains the trailing closure (and it may not necessarily
  /// be the first argument of the expansion).
  Optional<unsigned> getRawFirstTrailingClosureIndex() const {
    if (RawFirstTrailingClosureIndex == NumArgs)
      return None;
    return RawFirstTrailingClosureIndex;
  }

  /// Whether any trailing closures are present in the argument list.
  bool hasAnyTrailingClosures() const {
    return getRawFirstTrailingClosureIndex().hasValue();
  }

  /// Whether this argument list has a single trailing closure.
  bool hasSingleTrailingClosure() const {
    return hasAnyTrailingClosures() && !hasMultipleTrailingClosures();
  }

  /// Whether the argument list has multiple trailing closures.
  bool hasMultipleTrailingClosures() const {
    return HasMultipleTrailingClosures;
  }

  /// Whether a given index is for a labeled or unlabeled trailing closure.
  ///
  /// Note that in type-checked AST, this may return \c true for an index of a
  /// variadic expansion which contains a trailing closure (and it may not
  /// necessarily be the first argument of the expansion).
  bool isRawTrailingClosureIndex(unsigned i) const {
    if (!hasAnyTrailingClosures())
      return false;
    return i >= getRawFirstTrailingClosureIndex() && i < size();
  }

  // NOTE: There are *intentionally* not that many APIs here that provide access
  // to the trailing closures of the argument list. This is because trailing
  // closures have a tricky representation in type-checked AST, and you're
  // likely better off querying getOriginalArguments().
  //
  // When adding additional trailing closure APIs, bear in mind that in
  // type-checked AST:
  // - Trailing closures may be nested within variadic expansion exprs.
  // - Variadic expansion exprs may contain a mix of trailing and non-trailing
  // closures.
  // - Trailing closures may appear *before* non-trailing arguments in certain
  // backward argument matching cases (see comment in getOriginalArguments).

  /// Whether any of the arguments in the argument are inout.
  bool hasAnyInOutArgs() const {
    return llvm::any_of(*this, [](auto arg) { return arg.isInOut(); });
  }

  /// Whether the argument list has a single argument, which may be labeled or
  /// unlabeled.
  bool isUnary() const { return size() == 1; }

  /// Whether the argument list has a single unlabeled argument.
  bool isUnlabeledUnary() const { return isUnary() && getLabel(0).empty(); }

  /// If the argument list has a single argument, which may be labeled or
  /// unlabeled, returns its expression, or \c nullptr if this isn't a unary
  /// argument list.
  Expr *getUnaryExpr() const { return isUnary() ? getExpr(0) : nullptr; }

  /// If the argument list has a single unlabeled argument, returns its
  /// expression, or \c nullptr if this isn't an unlabeled unary argument list.
  Expr *getUnlabeledUnaryExpr() const {
    return isUnlabeledUnary() ? getExpr(0) : nullptr;
  }

  /// Retrieve an array of the argument expressions used in the argument list.
  ArrayRef<Expr *> getArgExprs() const { return getExprsBuffer(); }

  /// Retrieve an array of the argument labels used in the argument list.
  ArrayRef<Identifier>
  getArgumentLabels(SmallVectorImpl<Identifier> &scratch) const;

  /// If the provided expression appears as one of the argument list's
  /// arguments, returns its index. Otherwise returns \c None. By default this
  /// will match against semantic sub-expressions, but that may be disabled by
  /// passing \c false for \c allowSemantic.
  Optional<unsigned> findArgumentExpr(Expr *expr,
                                      bool allowSemantic = true) const;

  /// Creates a TupleExpr or ParenExpr that holds the argument exprs. A
  /// ParenExpr will be returned for a single argument, otherwise a TupleExpr.
  /// Don't use this function unless you actually need an AST transform.
  Expr *packIntoImplicitTupleOrParen(
      ASTContext &ctx,
      llvm::function_ref<Type(Expr *)> getType = __Expr_getType) const;

  /// Avoid adding new usages of this. Creates a TupleType or ParenType
  /// representing the types in the argument list. A ParenType will be returned
  /// for a single argument, otherwise a TupleType.
  Type composeTupleOrParenType(
      ASTContext &ctx,
      llvm::function_ref<Type(Expr *)> getType = __Expr_getType) const;

  /// Whether the argument list matches a given parameter list. This will return
  /// \c false if the arity doesn't match, or any of the canonical types or
  /// labels don't match. Note that this expects types to be present for the
  /// arguments and parameters.
  bool matches(ArrayRef<AnyFunctionType::Param> params,
               llvm::function_ref<Type(Expr *)> getType = __Expr_getType) const;

  /// Whether this argument list has the same arguments as another set of
  /// arguments.
  bool hasSameArgs(ArrayRef<Argument> args) {
    if (args.size() != size())
      return false;
    return llvm::all_of(indices(args),
                        [&](auto i) { return get(i) == args[i]; });
  }

  /// When applying a solution to a constraint system, the type checker rewrites
  /// argument lists of calls to insert default arguments and collect varargs.
  /// Sometimes for diagnostics and IDE functionality we want to work on the
  /// original arguments as written by the user; this performs the reverse
  /// transformation. This has no effect on un-type-checked argument lists.
  OriginalArguments getOriginalArguments() const;

  ArgumentList *walk(ASTWalker &walker);

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &OS, unsigned Indent = 0) const;
};

/// A type that represents the original set of arguments from an ArgumentList.
///
/// This always has the same set of arguments as an un-type-checked argument
/// list. For a type-checked argument list, this strips default arguments,
/// expands variadic arguments, and re-orders trailing closures such that they
/// are as they appear at the call-site.
class OriginalArguments final {
  friend class ArgumentList;
  using Storage = SmallVector<Argument, 4>;

  Storage Args;
  Optional<unsigned> FirstTrailingClosureIndex;

  explicit OriginalArguments(Storage args, Optional<unsigned> firstTrailing)
      : Args(std::move(args)), FirstTrailingClosureIndex(firstTrailing) {}

public:
  using iterator = ArrayRef<Argument>::iterator;
  iterator begin() const { return Args.begin(); }
  iterator end() const { return Args.end(); }
  unsigned size() const { return Args.size(); }
  bool empty() const { return Args.empty(); }
  Argument operator[](unsigned idx) const { return Args[idx]; }

  /// Returns the array of arguments.
  ///
  /// Note this is marked \c & to try avoid memory bugs by ensuring it's not
  /// called on an rvalue.
  ArrayRef<Argument> getArray() /*const*/ & { return Args; }

  /// Retrieve the index of the first trailing closure argument, or \c None if
  /// there are no trailing closures.
  Optional<unsigned> getFirstTrailingClosureIndex() const {
    return FirstTrailingClosureIndex;
  }

  /// The number of trailing closures in the argument list.
  unsigned getNumTrailingClosures() const {
    if (!getFirstTrailingClosureIndex())
      return 0;
    return Args.size() - *getFirstTrailingClosureIndex();
  }

  /// Whether any unlabeled or labeled trailing closures are present.
  bool hasAnyTrailingClosures() const {
    return FirstTrailingClosureIndex.hasValue();
  }

  /// Whether the argument list has multiple trailing closures.
  bool hasMultipleTrailingClosures() const {
    return getNumTrailingClosures() > 1;
  }

  /// Whether a given index is for an unlabeled trailing closure.
  bool isUnlabeledTrailingClosureIndex(unsigned i) const {
    return hasAnyTrailingClosures() && *getFirstTrailingClosureIndex() == i;
  }

  /// Whether a given index is for a labeled trailing closure in an argument
  /// list with multiple trailing closures.
  bool isLabeledTrailingClosureIndex(unsigned i) const {
    if (!hasAnyTrailingClosures())
      return false;
    return i > *getFirstTrailingClosureIndex() && i < size();
  }

  /// Whether a given index is for a labeled or unlabeled trailing closure.
  bool isTrailingClosureIndex(unsigned i) const {
    if (!hasAnyTrailingClosures())
      return false;
    return i >= *getFirstTrailingClosureIndex() && i < size();
  }

  /// Returns the range of arguments excluding any trailing closures.
  ///
  /// Note this is marked \c & to try avoid memory bugs by ensuring it's not
  /// called on an rvalue.
  ArrayRef<Argument> getNonTrailingArgs() /*const*/ & {
    return getArray().drop_back(getNumTrailingClosures());
  }

  /// Returns the range of trailing closure arguments.
  ///
  /// Note this is marked \c & to try avoid memory bugs by ensuring it's not
  /// called on an rvalue.
  ArrayRef<Argument> getTrailingClosures() /*const*/ & {
    return getArray().take_back(getNumTrailingClosures());
  }

  /// Retrieve the first trailing closure argument in the argument list, or
  /// \c None if there are no trailing closures.
  Optional<Argument> getFirstTrailingClosure() const {
    auto idx = getFirstTrailingClosureIndex();
    if (!idx.hasValue())
      return None;
    return Args[*idx];
  }

  /// Retrieve the source range of any trailing closure arguments, or an empty
  /// range if there are no trailing closures.
  SourceRange getTrailingSourceRange() const {
    if (!hasAnyTrailingClosures())
      return SourceRange();

    auto elts = const_cast<OriginalArguments *>(this)->getTrailingClosures();
    return SourceRange(elts.front().getStartLoc(), elts.back().getEndLoc());
  }
};

} // end namespace swift

#endif
