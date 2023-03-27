//===--- SILDebugInfoExpression.h - DIExpression for SIL --------*- C++ -*-===//
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
///
/// \file
/// This file contains types that model debug info expressions in SIL. Including
/// (debug info) operator and operand.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DEBUGINFOEXPRESSION_H
#define SWIFT_SIL_DEBUGINFOEXPRESSION_H

#include "swift/AST/Decl.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class TailAllocatedDebugVariable;

/// Operator in a debug info expression
enum class SILDIExprOperator : unsigned {
  Invalid = 0,

  /// Dereferences the SSA value
  Dereference,

  /// Specifies that the SSA value is a fragment (sub-field) of the associated
  /// source variable. This operator takes a list of VarDecl/Tuple element
  /// operand pointing to a list of field declarations. Note that this directive
  /// can only appear at the end of an expression.
  Fragment,

  /// Perform arithmetic addition on the top two elements of the
  /// expression stack and push the result back to the stack.
  Plus,

  /// Subtract the top element in expression stack by the second
  /// element. Then push the result back to the stack.
  Minus,

  /// Push an unsigned integer constant onto the stack.
  ConstUInt,

  /// Push a signed integer constant onto the stack.
  ConstSInt,
};

/// Represents a single component in a debug info expression.
/// Including operator and operand.
struct SILDIExprElement : llvm::FoldingSetNode {
  enum Kind {
    /// A di-expression operator.
    OperatorKind,
    /// An operand that has declaration type.
    DeclKind,
    /// An integer constant value. Note that
    /// we don't specify its signedness here.
    ConstIntKind
  };

private:
  Kind OpKind;

  union {
    SILDIExprOperator Operator;
    Decl *Declaration;
    uint64_t ConstantInt;
  };

  explicit SILDIExprElement(Kind OpK) : OpKind(OpK) {}

public:
  Kind getKind() const { return OpKind; }

  SILDIExprOperator getAsOperator() const {
    return OpKind == OperatorKind ? Operator : SILDIExprOperator::Invalid;
  }

  Decl *getAsDecl() const { return OpKind == DeclKind ? Declaration : nullptr; }

  Optional<uint64_t> getAsConstInt() const {
    if (OpKind == ConstIntKind)
      return ConstantInt;
    else
      return {};
  }

  static SILDIExprElement *createOperator(SILModule &mod, SILDIExprOperator op);

  static SILDIExprElement *createDecl(SILModule &mod, Decl *decl);

  static SILDIExprElement *createConstInt(SILModule &mod, uint64_t value);

  /// Is this an element that can be the tail of a fragment operator.
  ///
  /// Today, this just includes Decls but with time we will add support for
  /// other things like tuples.
  bool isFragmentTail() const { return bool(getAsDecl()); }

  static void Profile(llvm::FoldingSetNodeID &id, SILDIExprOperator op,
                      Decl *decl, Optional<uint64_t> intValue);

  void Profile(llvm::FoldingSetNodeID &id) const {
    Profile(id, getAsOperator(), getAsDecl(), getAsConstInt());
  }
};

/// Returns the hashcode for the di expr element.
inline llvm::hash_code hash_value(const SILDIExprElement &elt) {
  return llvm::hash_combine(elt.getKind(), elt.getAsDecl(), elt.getAsDecl(),
                            elt.getAsConstInt());
}

/// For a given SILDIExprOperator, provides information
/// like its textual name and operand types.
struct SILDIExprInfo {
  StringRef OpText;
  SmallVector<SILDIExprElement::Kind, 2> OperandKinds;

  static const SILDIExprInfo *get(SILDIExprOperator Op);
};

/// A DIExpr operand is consisting of a SILDIExprOperator and
/// SILDIExprElement arguments following after.
struct SILDIExprOperand : public llvm::ArrayRef<const SILDIExprElement *> {
  // Reuse all the ctors
  using llvm::ArrayRef<const SILDIExprElement *>::ArrayRef;

  SILDIExprOperator getOperator() const {
    assert(size() && "empty DIExpr operand");
    const SILDIExprElement *const &First = front();
    return First->getAsOperator();
  }

  size_t getNumArg() const {
    assert(size() && "empty DIExpr operand");
    return size() - 1;
  }

  llvm::ArrayRef<const SILDIExprElement *> args() const { return drop_front(); }
};

/// Represents a debug info expression in SIL
class SILDebugInfoExpression final
    : public llvm::FoldingSetNode,
      private llvm::TrailingObjects<SILDebugInfoExpression,
                                    const SILDIExprElement *> {
  friend class TailAllocatedDebugVariable;
  friend TrailingObjects;

  unsigned numElts;

  SILDebugInfoExpression() = default;

  using TrailingObjects::getTrailingObjects;
  using TrailingObjects::totalSizeToAlloc;
  ;

public:
  static SILDebugInfoExpression *get(SILModule &mod,
                                     ArrayRef<const SILDIExprElement *> start,
                                     ArrayRef<const SILDIExprElement *> end);
  static SILDebugInfoExpression *
  get(SILModule &mod, ArrayRef<const SILDIExprElement *> elements) {
    return get(mod, elements, {});
  }

  static SILDebugInfoExpression *get(SILModule &mod) {
    return get(mod, {}, {});
  }

  ArrayRef<const SILDIExprElement *> getElements() const {
    return {getTrailingObjects<const SILDIExprElement *>(), numElts};
  }

  size_t getNumElements() const { return numElts; }

  using iterator = ArrayRef<const SILDIExprElement *>::iterator;
  using const_iterator = ArrayRef<const SILDIExprElement *>::const_iterator;

  iterator begin() { return getElements().begin(); }
  iterator end() { return getElements().end(); }
  const_iterator begin() const { return getElements().begin(); }
  const_iterator end() const { return getElements().end(); }

  const SILDIExprElement *getElement(size_t index) const {
    return getElements()[index];
  }

  SILDebugInfoExpression *
  append(SILModule &mod, ArrayRef<const SILDIExprElement *> newElements) const {
    return get(mod, getElements(), newElements);
  }

  SILDebugInfoExpression *append(SILModule &mod,
                                 const SILDebugInfoExpression *other) const {
    return get(mod, getElements(), other->getElements());
  }

  SILDebugInfoExpression *
  prepend(SILModule &mod,
          ArrayRef<const SILDIExprElement *> newElements) const {
    return get(mod, newElements, getElements());
  }

  /// The iterator for SILDIExprOperand
  class op_iterator {
    friend class SILDebugInfoExpression;

    SILDIExprOperand Current;
    llvm::ArrayRef<const SILDIExprElement *> Remain;

    void increment();

    explicit op_iterator(llvm::ArrayRef<const SILDIExprElement *> Remain)
        : Remain(Remain) {
      increment();
    }

  public:
    op_iterator() = default;
    op_iterator(const op_iterator &) = default;

    const SILDIExprOperand &operator*() const { return Current; }
    const SILDIExprOperand *operator->() const { return &Current; }

    // Pre increment
    op_iterator &operator++() {
      increment();
      return *this;
    }

    // Post increment
    op_iterator operator++(int) {
      op_iterator This(*this);
      increment();
      return This;
    }

    bool operator==(const op_iterator &Other) const {
      return (Current.empty() && Other.Current.empty()) ||
             (Current.data() == Other.Current.data() &&
              Current.size() == Other.Current.size());
    }
    bool operator!=(const op_iterator &Other) const {
      return !(Other == *this);
    }
  };

  op_iterator operand_begin() const { return op_iterator(getElements()); }
  op_iterator operand_end() const {
    return op_iterator(llvm::ArrayRef<const SILDIExprElement *>{});
  }

  llvm::iterator_range<op_iterator> operands() const {
    return llvm::make_range(operand_begin(), operand_end());
  }

  /// Return true if this expression is not empty
  inline operator bool() const { return numElts; }

  /// Create a op_fragment expression
  static SILDebugInfoExpression *createFragment(SILModule &mod, VarDecl *Field);

  /// Return true if this DIExpression starts with op_deref
  bool startsWithDeref() const {
    auto elements = getElements();
    auto ii = elements.begin();
    if (ii == elements.end())
      return false;
    return (*ii)->getAsOperator() == SILDIExprOperator::Dereference;
  }

  const SILDebugInfoExpression *dropDeref(SILModule &mod) const {
    if (!startsWithDeref())
      return this;

    return get(mod, getElements().drop_front());
  }

  /// Return true if this DIExpression has op_fragment (at the end)
  bool hasFragment() const {
    // Walk from front to back to see if we have a fragment. The pattern we are
    // looking for is fragment fragment_tail+
    auto elements = getElements();
    auto ii = elements.rbegin(), ie = elements.rend();

    // If we don't have any elements, we can't have a fragment.
    if (ii == ie)
      return false;

    // If our last element is not a fragment tail, then return false. We should
    // /always/ have at least one fragment tail before the fragment operator.
    if (!(*ii)->isFragmentTail())
      return false;
    ++ii;

    // If we had a fragment tail and do not have a fragment operator afterwards,
    // then we are done. Return false.
    if (ii == ie)
      return false;

    // Otherwised, we search for fragment fragment_tail*
    do {
      auto *val = *ii;
      ++ii;

      if (val->getAsOperator() == SILDIExprOperator::Fragment)
        return true;
      if (!val->isFragmentTail())
        return false;
    } while (ii != ie);

    // If we did not find either pattern (for instance, if we saw a tail and no
    // fragment), return false.
    return false;
  }

  static void Profile(llvm::FoldingSetNodeID &id,
                      ArrayRef<const SILDIExprElement *> elements);

  void Profile(llvm::FoldingSetNodeID &id) { Profile(id, getElements()); }

  void
  verify(llvm::function_ref<void(bool, StringRef)> require = nullptr) const;
};

/// Returns the hashcode for the di expr element.
inline llvm::hash_code hash_value(const SILDebugInfoExpression &elt) {
  return llvm::hash_combine_range(elt.begin(), elt.end());
}

} // end namespace swift

#endif
