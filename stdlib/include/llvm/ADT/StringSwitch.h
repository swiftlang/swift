//===--- StringSwitch.h - Switch-on-literal-string Construct --------------===/
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//===----------------------------------------------------------------------===/
//
//  This file implements the StringSwitch template, which mimics a switch()
//  statement whose cases are string literals.
//
//===----------------------------------------------------------------------===/
#ifndef LLVM_ADT_STRINGSWITCH_H
#define LLVM_ADT_STRINGSWITCH_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include <cassert>
#include <cstring>
#include <initializer_list>

inline namespace __swift { inline namespace __runtime {
namespace llvm {

/// A switch()-like statement whose cases are string literals.
///
/// The StringSwitch class is a simple form of a switch() statement that
/// determines whether the given string matches one of the given string
/// literals. The template type parameter \p T is the type of the value that
/// will be returned from the string-switch expression. For example,
/// the following code switches on the name of a color in \c argv[i]:
///
/// \code
/// Color color = StringSwitch<Color>(argv[i])
///   .Case("red", Red)
///   .Case("orange", Orange)
///   .Case("yellow", Yellow)
///   .Case("green", Green)
///   .Case("blue", Blue)
///   .Case("indigo", Indigo)
///   .Cases("violet", "purple", Violet)
///   .Default(UnknownColor);
/// \endcode
template<typename T, typename R = T>
class StringSwitch {
  /// The string we are matching.
  const StringRef Str;

  /// The pointer to the result of this switch statement, once known,
  /// null before that.
  std::optional<T> Result;

public:
  explicit StringSwitch(StringRef S)
  : Str(S), Result() { }

  // StringSwitch is not copyable.
  StringSwitch(const StringSwitch &) = delete;

  // StringSwitch is not assignable due to 'Str' being 'const'.
  void operator=(const StringSwitch &) = delete;
  void operator=(StringSwitch &&other) = delete;

  StringSwitch(StringSwitch &&other)
    : Str(other.Str), Result(std::move(other.Result)) { }

  ~StringSwitch() = default;

  // Case-sensitive case matchers
  StringSwitch &Case(StringLiteral S, T Value) {
    CaseImpl(Value, S);
    return *this;
  }

  StringSwitch& EndsWith(StringLiteral S, T Value) {
    if (!Result && Str.ends_with(S)) {
      Result = std::move(Value);
    }
    return *this;
  }

  StringSwitch& StartsWith(StringLiteral S, T Value) {
    if (!Result && Str.starts_with(S)) {
      Result = std::move(Value);
    }
    return *this;
  }

  StringSwitch &Cases(std::initializer_list<StringLiteral> CaseStrings,
                      T Value) {
    return CasesImpl(Value, CaseStrings);
  }

  // Case-insensitive case matchers.
  StringSwitch &CaseLower(StringLiteral S, T Value) {
    CaseLowerImpl(Value, S);
    return *this;
  }

  StringSwitch &EndsWithLower(StringLiteral S, T Value) {
    if (!Result && Str.ends_with_insensitive(S))
      Result = Value;

    return *this;
  }

  StringSwitch &StartsWithLower(StringLiteral S, T Value) {
    if (!Result && Str.starts_with_insensitive(S))
      Result = std::move(Value);

    return *this;
  }

  StringSwitch &CasesLower(std::initializer_list<StringLiteral> CaseStrings,
                           T Value) {
    return CasesLowerImpl(Value, CaseStrings);
  }

  [[nodiscard]] R Default(T Value) {
    if (Result)
      return std::move(*Result);
    return Value;
  }

  [[nodiscard]] operator R() {
    assert(Result && "Fell off the end of a string-switch");
    return std::move(*Result);
  }

private:
  // Returns true when `Str` matches the `S` argument, and stores the result.
  bool CaseImpl(T &Value, StringLiteral S) {
    if (!Result && Str == S) {
      Result = std::move(Value);
      return true;
    }
    return false;
  }

  // Returns true when `Str` matches the `S` argument (case-insensitive), and
  // stores the result.
  bool CaseLowerImpl(T &Value, StringLiteral S) {
    if (!Result && Str.equals_insensitive(S)) {
      Result = std::move(Value);
      return true;
    }
    return false;
  }

  StringSwitch &CasesImpl(T &Value,
                          std::initializer_list<StringLiteral> Cases) {
    // Stop matching after the string is found.
    for (StringLiteral S : Cases)
      if (CaseImpl(Value, S))
        break;
    return *this;
  }

  StringSwitch &CasesLowerImpl(T &Value,
                               std::initializer_list<StringLiteral> Cases) {
    // Stop matching after the string is found.
    for (StringLiteral S : Cases)
      if (CaseLowerImpl(Value, S))
        break;
    return *this;
  }
};

} // end namespace llvm
}} // namespace swift::runtime

#endif // LLVM_ADT_STRINGSWITCH_H
