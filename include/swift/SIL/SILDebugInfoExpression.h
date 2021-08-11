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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
class TailAllocatedDebugVariable;

/// Operator in a debug info expression
enum class SILDIExprOperator : unsigned {
  INVALID = 0,
  /// Dereferences the SSA value
  Dereference,
  /// Specifies that the SSA value is a fragment (sub-field) of the
  /// associated source variable. This operator takes a single
  /// VarDecl operand pointing to the field declaration.
  /// Note that this directive can only appear at the end of an
  /// expression.
  Fragment
};

/// Represents a single component in a debug info expression.
/// Including operator and operand.
struct SILDIExprElement {
  enum Kind {
    /// A di-expression operator
    OperatorKind,
    /// An operand that has declaration type
    DeclKind
  };

private:
  Kind OpKind;

  union {
    SILDIExprOperator Operator;
    Decl *Declaration;
  };

  explicit SILDIExprElement(Kind OpK) : OpKind(OpK) {}

public:
  Kind getKind() const { return OpKind; }

  SILDIExprOperator getAsOperator() const {
    return OpKind == OperatorKind ? Operator : SILDIExprOperator::INVALID;
  }

  Decl *getAsDecl() const { return OpKind == DeclKind ? Declaration : nullptr; }

  static SILDIExprElement createOperator(SILDIExprOperator Op) {
    SILDIExprElement DIOp(OperatorKind);
    DIOp.Operator = Op;
    return DIOp;
  }

  static SILDIExprElement createDecl(Decl *D) {
    SILDIExprElement DIOp(DeclKind);
    DIOp.Declaration = D;
    return DIOp;
  }
};

/// For a given SILDIExprOperator, provides information
/// like its textual name and operand types.
struct SILDIExprInfo {
  StringRef OpText;
  SmallVector<SILDIExprElement::Kind, 2> OperandKinds;

  static const SILDIExprInfo *get(SILDIExprOperator Op);
};

/// A DIExpr operand is consisting of a SILDIExprOperator and
/// SILDIExprElement arguments following after.
struct SILDIExprOperand : public llvm::ArrayRef<SILDIExprElement> {
  // Reuse all the ctors
  using llvm::ArrayRef<SILDIExprElement>::ArrayRef;

  SILDIExprOperator getOperator() const {
    assert(size() && "empty DIExpr operand");
    const SILDIExprElement &First = front();
    return First.getAsOperator();
  }

  size_t getNumArg() const {
    assert(size() && "empty DIExpr operand");
    return size() - 1;
  }

  llvm::ArrayRef<SILDIExprElement> args() const {
    return drop_front();
  }
};

/// Represents a debug info expression in SIL
class SILDebugInfoExpression {
  friend class TailAllocatedDebugVariable;
  llvm::SmallVector<SILDIExprElement, 2> Elements;

public:
  SILDebugInfoExpression() = default;

  explicit SILDebugInfoExpression(llvm::ArrayRef<SILDIExprElement> EL)
      : Elements(EL.begin(), EL.end()) {}

  size_t getNumElements() const { return Elements.size(); }

  using iterator = typename decltype(Elements)::iterator;
  using const_iterator = typename decltype(Elements)::const_iterator;

  iterator element_begin() { return Elements.begin(); }
  iterator element_end() { return Elements.end(); }

  const_iterator element_begin() const { return Elements.begin(); }
  const_iterator element_end() const { return Elements.end(); }

  llvm::iterator_range<iterator> elements() {
    return llvm::make_range(element_begin(), element_end());
  }

  llvm::iterator_range<const_iterator> elements() const {
    return llvm::make_range(element_begin(), element_end());
  }

  const SILDIExprElement &getElement(size_t index) const {
    assert(index < Elements.size());
    return Elements[index];
  }

  void push_back(const SILDIExprElement &Element) {
    Elements.push_back(Element);
  }

  /// The iterator for SILDIExprOperand
  class op_iterator {
    friend class SILDebugInfoExpression;

    SILDIExprOperand Current;
    llvm::ArrayRef<SILDIExprElement> Remain;

    void increment();

    explicit
    op_iterator(llvm::ArrayRef<SILDIExprElement> Remain): Remain(Remain) {
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

  op_iterator operand_begin() const {
    return op_iterator(Elements);
  }
  op_iterator operand_end() const {
    return op_iterator(llvm::ArrayRef<SILDIExprElement>{});
  }

  llvm::iterator_range<op_iterator> operands() const {
    return llvm::make_range(operand_begin(), operand_end());
  }

  /// Return true if this expression is not empty
  inline operator bool() const { return Elements.size(); }
};
} // end namespace swift
#endif
