//===--- ParserResult.h - Parser Result Wrapper -----------------*- C++ -*-===//
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

#ifndef SWIFT_PARSER_PARSER_RESULT_H
#define SWIFT_PARSER_PARSER_RESULT_H

#include "llvm/ADT/PointerIntPair.h"
#include <type_traits>

namespace swift {

/// \brief A wrapper for a parser AST node result (Decl, Stmt, Expr, Pattern,
/// etc.)
///
/// Contains the pointer to the AST node itself (or null) and additional bits
/// that indicate:
/// \li if there was a parse error;
/// \li if there was a code completion token.
template <typename T> class ParserResult {
  llvm::PointerIntPair<T *, 2> PtrAndBits;

  enum {
    IsError = 0x1,
    IsCodeCompletion = 0x2,
  };

  template <typename U> friend class ParserResult;

public:
  /// Construct a null result with error bit set.
  ParserResult(std::nullptr_t = nullptr) { setIsParseError(); }

  /// Construct a successful parser result.
  explicit ParserResult(T *Result) : PtrAndBits(Result) {
    assert(Result && "a successful parser result can not be null");
  }

  /// Convert from a different but compatible parser result.
  template <typename U, typename Enabler = typename std::enable_if<
                            std::is_base_of<T, U>::value>::type>
  ParserResult(ParserResult<U> Other)
      : PtrAndBits(Other.PtrAndBits.getPointer(), Other.PtrAndBits.getInt()) {}

  /// Return true if this result does not have an AST node.
  ///
  /// If returns true, then error bit is set.
  bool isNull() const { return getPtrOrNull() == nullptr; }

  /// Return true if this result has an AST node.
  ///
  /// Note that this does not tell us if there was a parse error or not.
  bool isNonNull() const { return getPtrOrNull() != nullptr; }

  /// Return the AST node if non-null.
  T *get() const {
    assert(getPtrOrNull() && "not checked for nullptr");
    return getPtrOrNull();
  }

  /// Return the AST node or a null pointer.
  T *getPtrOrNull() const { return PtrAndBits.getPointer(); }

  /// Return true if there was a parse error.
  ///
  /// Note that we can still have an AST node which was constructed during
  /// recovery.
  bool isParseError() const { return PtrAndBits.getInt() & IsError; }

  /// Return true if we found a code completion token while parsing this.
  bool hasCodeCompletion() const {
    return PtrAndBits.getInt() & IsCodeCompletion;
  }

  void setIsParseError() { PtrAndBits.setInt(PtrAndBits.getInt() | IsError); }

  void setHasCodeCompletion() {
    PtrAndBits.setInt(PtrAndBits.getInt() | IsError | IsCodeCompletion);
  }
};

/// Create a successful parser result.
template <typename T> ParserResult<T> makeParserResult(T *Result) {
  return ParserResult<T>(Result);
}

/// Create a result (null or non-null) with error bit set.
template <typename T> ParserResult<T> makeParserErrorResult(T *Result) {
  ParserResult<T> PR(Result);
  PR.setIsParseError();
  return PR;
}

/// Create a result (null or non-null) with error and code completion bits set.
template <typename T>
ParserResult<T> makeParserCodeCompletionResult(T *Result) {
  ParserResult<T> PR(Result);
  PR.setHasCodeCompletion();
  return PR;
}
} // namespace swift

#endif // LLVM_SWIFT_PARSER_PARSER_RESULT_H

