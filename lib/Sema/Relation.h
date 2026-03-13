//===--- Relation.h - Relations and boolean matrices ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The code here is meant for debugging relations implemented as boolean
// predicates. You can construct a Boolean matrix by evaluating the predicate
// against all possible pairs of values from some range, and then check that
// this matrix satisfies various conditions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_RELATION_H
#define SWIFT_SEMA_RELATION_H

#include "swift/Basic/Assertions.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

namespace swift {

namespace constraints {

struct BooleanMatrix {
  std::vector<std::vector<bool>> elements;

  size_t rows() const {
    return elements.size();
  }

  size_t columns() const {
    return elements[0].size();
  }

  explicit BooleanMatrix(std::vector<std::vector<bool>> elements)
  	: elements(elements) {
    ASSERT(elements.size() > 0);
    size_t columns = elements[0].size();
    for (const auto &row : elements) {
      ASSERT(columns == row.size());
    }
  }

  void dump(llvm::raw_ostream &out) {
  	for (auto &row : elements) {
      llvm::interleave(
        row,
        [&](bool elt) { out << (elt ? "1" : "0"); },
        [&]() { out << " "; });
      out << "\n";
    }
  }

  BooleanMatrix multiply(const BooleanMatrix &other) const {
    std::vector<std::vector<bool>> result;

    ASSERT(columns() == other.rows());

    for (unsigned i = 0; i < rows(); ++i) {
      result.emplace_back();
      for (unsigned j = 0; j < other.columns(); ++j) {
        bool entry = false;
        for (unsigned k = 0; k < columns(); ++k) {
          entry |= elements[i][k] && other.elements[k][j];
        }
        result.back().push_back(entry);
      }
    }

    return BooleanMatrix(result);
  }

  bool isReflexive() const {
    ASSERT(rows() == columns());

    for (unsigned i = 0; i < rows(); ++i) {
      if (!elements[i][i])
        return false;
    }

    return true;
  }

  bool isAntiReflexive() const {
    ASSERT(rows() == columns());

    for (unsigned i = 0; i < rows(); ++i) {
      if (elements[i][i])
        return false;
    }

    return true;
  }

  bool isSymmetric() const {
    ASSERT(rows() == columns());

    for (unsigned i = 0; i < rows(); ++i) {
      for (unsigned j = 0; j < i; ++j) {
        if (elements[i][j] != elements[j][i])
          return false;
      }
    }

    return true;
  }

  bool isAntiSymmetric() const {
    ASSERT(rows() == columns());

    for (unsigned i = 0; i < rows(); ++i) {
      for (unsigned j = 0; j < i; ++j) {
        if (elements[i][j] && elements[j][i])
          return false;
      }
    }

    return true;
  }

  bool isTransitive() const {
    ASSERT(rows() == columns());

    auto square = multiply(*this);
    for (unsigned i = 0; i < square.rows(); ++i) {
      for (unsigned j = 0; j < square.columns(); ++j) {
        if (square.elements[i][j] && !elements[i][j])
          return false;
      }
    }

    return true;
  }

  template<typename Iter, typename Fn>
  static BooleanMatrix forPredicate(Iter begin, Iter end, Fn pred) {
    std::vector<std::vector<bool>> elements;

    for (Iter row = begin; row != end; ++row) {
      elements.emplace_back();
      for (Iter col = begin; col != end; ++col)
        elements.back().push_back(pred(*row, *col));
    }

    return BooleanMatrix(elements);
  }
};

}

}

#endif // SWIFT_SEMA_RELATION_H