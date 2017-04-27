//===--- ParserResult.h - Parser Result Wrapper -----------------*- C++ -*-===//
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

#ifndef SWIFT_PARSER_PARSER_RESULT_H
#define SWIFT_PARSER_PARSER_RESULT_H

#include "llvm/ADT/PointerIntPair.h"
#include "swift/AST/ParameterList.h"
#include <type_traits>

namespace swift {

class ParserStatus;

/// \brief A wrapper for a parser AST node result (Decl, Stmt, Expr, Pattern,
/// etc.)
///
/// Contains the pointer to the AST node itself (or null) and additional bits
/// that indicate:
/// \li if there was a parse error;
/// \li if there was a code completion token.
///
/// If you want to return an AST node pointer in the Parser, consider using
/// ParserResult instead.
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

  /// Construct a null result with specified error bits set.
  ParserResult(ParserStatus Status);

  /// Construct a successful parser result.
  explicit ParserResult(T *Result) : PtrAndBits(Result) {
    assert(Result && "a successful parser result cannot be null");
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
template <typename T>
static inline ParserResult<T> makeParserResult(T *Result) {
  return ParserResult<T>(Result);
}

/// Create a result (null or non-null) with error bit set.
template <typename T>
static inline ParserResult<T> makeParserErrorResult(T *Result = nullptr) {
  ParserResult<T> PR;
  if (Result)
    PR = ParserResult<T>(Result);
  PR.setIsParseError();
  return PR;
}

/// Create a result (null or non-null) with error and code completion bits set.
template <typename T>
static inline ParserResult<T> makeParserCodeCompletionResult(T *Result =
                                                                 nullptr) {
  ParserResult<T> PR;
  if (Result)
    PR = ParserResult<T>(Result);
  PR.setHasCodeCompletion();
  return PR;
}

/// \brief Same as \c ParserResult, but just the status bits without the AST
/// node.
///
/// Useful when the AST node is returned by some other means (for example, in
/// a vector out parameter).
///
/// If you want to use 'bool' as a result type in the Parser, consider using
/// ParserStatus instead.
class ParserStatus {
  unsigned IsError : 1;
  unsigned IsCodeCompletion : 1;

public:
  /// Construct a successful parser status.
  ParserStatus() : IsError(0), IsCodeCompletion(0) {}

  /// Construct a parser status with specified bits.
  template<typename T>
  ParserStatus(ParserResult<T> Result) : IsError(0), IsCodeCompletion(0) {
    if (Result.isParseError())
      setIsParseError();
    if (Result.hasCodeCompletion())
      setHasCodeCompletion();
  }

  bool isSuccess() const { return !isError(); }
  bool isError() const { return IsError; }

  /// Return true if we found a code completion token while parsing this.
  bool hasCodeCompletion() const { return IsCodeCompletion; }

  void setIsParseError() {
    IsError = true;
  }

  void setHasCodeCompletion() {
    IsError = true;
    IsCodeCompletion = true;
  }

  /// True if we should stop parsing for any reason.
  bool shouldStopParsing() const {
    return IsError || IsCodeCompletion;
  }

  ParserStatus &operator|=(ParserStatus RHS) {
    IsError |= RHS.IsError;
    IsCodeCompletion |= RHS.IsCodeCompletion;
    return *this;
  }

  friend ParserStatus operator|(ParserStatus LHS, ParserStatus RHS) {
    ParserStatus Result = LHS;
    Result |= RHS;
    return Result;
  }
};

/// Create a successful parser status.
static inline ParserStatus makeParserSuccess() {
  return ParserStatus();
}

/// Create a status with error bit set.
static inline ParserStatus makeParserError() {
  ParserStatus Status;
  Status.setIsParseError();
  return Status;
}

/// Create a status with error and code completion bits set.
static inline ParserStatus makeParserCodeCompletionStatus() {
  ParserStatus Status;
  Status.setHasCodeCompletion();
  return Status;
}

/// Create a parser result with specified bits.
template <typename T>
static inline ParserResult<T> makeParserResult(ParserStatus Status,
                                               T *Result) {
  if (Status.isSuccess())
    return makeParserResult(Result);
  if (Status.hasCodeCompletion())
    return makeParserCodeCompletionResult(Result);
  return makeParserErrorResult(Result);
}

template <typename T> ParserResult<T>::ParserResult(ParserStatus Status) {
  assert(Status.isError());
  setIsParseError();
  if (Status.hasCodeCompletion())
    setHasCodeCompletion();
}

enum class ConditionalCompilationExprKind {
  Unknown,
  Error,
  OS,
  Arch,
  LanguageVersion,
  CompilerVersion,
  Binary,
  Paren,
  DeclRef,
  Boolean,
  Integer,
  Import,
};

class ConditionalCompilationExprState {

  uint8_t ConditionActive : 1;
  uint8_t Kind : 7;
public:
  ConditionalCompilationExprState() : ConditionActive(false) {
    setKind(ConditionalCompilationExprKind::Unknown);
  }

  ConditionalCompilationExprState(bool ConditionActive,
                                  ConditionalCompilationExprKind Kind)
  : ConditionActive(ConditionActive) {
    setKind(Kind);
  }

  bool isConditionActive() const {
    return ConditionActive;
  }

  void setConditionActive(bool A) {
    ConditionActive = A;
  }

  ConditionalCompilationExprKind getKind() const {
    return static_cast<ConditionalCompilationExprKind>(Kind);
  }

  void setKind(ConditionalCompilationExprKind K) {
    Kind = static_cast<uint8_t>(K);
    assert(getKind() == K);
  }

  bool shouldParse() const {
    if (getKind() == ConditionalCompilationExprKind::Error)
      return true;
    return ConditionActive ||
    (getKind() != ConditionalCompilationExprKind::CompilerVersion &&
     getKind() != ConditionalCompilationExprKind::LanguageVersion);
  }

  static ConditionalCompilationExprState error() {
    return {false, ConditionalCompilationExprKind::Error};
  }
};

ConditionalCompilationExprState
operator&&(const ConditionalCompilationExprState lhs,
           const ConditionalCompilationExprState rhs);
ConditionalCompilationExprState
operator||(const ConditionalCompilationExprState lhs,
           const ConditionalCompilationExprState rhs);
ConditionalCompilationExprState
operator!(const ConditionalCompilationExprState Result);

} // namespace swift

#endif // LLVM_SWIFT_PARSER_PARSER_RESULT_H

