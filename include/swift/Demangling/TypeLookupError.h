//===--- TypeLookupError.h - Type lookup error value. -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Provides the TypeLookupError class, which represents errors when demangling
// or looking up types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_TypeLookupError_H
#define SWIFT_DEMANGLING_TypeLookupError_H

#include "swift/Basic/TaggedUnion.h"
#include "swift/Runtime/Portability.h"
#include <cstring>
#include <string>

namespace swift {

/// An error that occurred while looking up a type at runtime from a mangled
/// name.
/// ///
/// This ultimately just provides a string, but is built to take up minimal
/// space when passed around, and perform as much work lazily as possible. We
/// don't want to spend a lot of time building strings when the caller is going
/// to handle the error gracefully and try a fallback. We only want to waste
/// time/space on that when the error message is actually relevant, so build it
/// as late as possible.
/// ///
/// To be as compact as possible, this type holds a context pointer and a
/// callback function. The callback function uses the context pointer to return
/// an error string when requested. The callback function also does quadruple
/// duty to copy/destroy the context as needed, and free the returned error
/// string if needed. Commands are passed to the callback to request the
/// different operations, which means we only have to store one function pointer
/// instead of four.
class TypeLookupError {
public:
  /// The commands that can be passed to the callback function.
  enum class Command {
    /// Return the error string to the caller, as a char *.
    CopyErrorString,

    /// Destroy the error string returned from CopyErrorString, if necessary.
    /// The return value is ignored.
    DestroyErrorString,

    /// Return a copy of the context pointer (used for copying TypeLookupError
    /// objects.)
    CopyContext,

    /// Destroy the context pointer. The return value is ignored.
    DestroyContext,
  };

  /// The callback used to respond to the commands. The parameters are:
  ///  - `context`: the context that the value was initialized with, or the
  ///               context returned from a CopyContext call
  ///  - `command`: the command to respond to
  ///  - `param`: when `command` is `DestroyErrorString`, the string pointer to
  ///             destroy, otherwise NULL
  using Callback = void *(*)(void *context, Command command, void *param);

private:
  void *Context;
  Callback Fn;

  /// A no-op callback used to avoid a bunch of `if (Fn)` checks.
  static void *nop(void *context, Command command, void *param) {
    return nullptr;
  }

  /// Helper functions for getting a C string from a lambda. These allow us to
  /// wrap lambdas returning `char *` or `std::string` and standardize them on
  /// `char *`.
  static char *getCString(char *str) { return str; }

  static char *getCString(const std::string &str) {
#if defined(_WIN32)
    return _strdup(str.c_str());
#else
    return strdup(str.c_str());
#endif
  }

public:
  TypeLookupError(const TypeLookupError &other) {
    Fn = other.Fn;
    Context = other.Fn(other.Context, Command::CopyContext, nullptr);
  }

  TypeLookupError(TypeLookupError &&other) {
    Fn = other.Fn;
    Context = other.Context;

    other.Fn = nop;
    other.Context = nullptr;
  }

  ~TypeLookupError() { Fn(Context, Command::DestroyContext, nullptr); }

  TypeLookupError(void *context, Callback fn) : Context(context), Fn(fn ? fn : nop) {}

  TypeLookupError &operator=(const TypeLookupError &other) {
    if (this == &other)
      return *this;

    Fn(Context, Command::DestroyContext, nullptr);
    Fn = other.Fn;
    Context = Fn(other.Context, Command::CopyContext, nullptr);

    return *this;
  }

  /// Construct a TypeLookupError that just returns a constant C string.
  TypeLookupError(const char *str) {
    Context = const_cast<char *>(str);
    Fn = [](void *context, Command command, void *param) -> void * {
      // The context pointer is the string and works for both copying the string
      // and copying the context. Other commands don't need to do anything.
      return context;
    };
  }

  /// Construct a TypeLookupError that wraps a function returning a string. The
  /// passed-in function can return either a `std::string` or `char *`. If it
  /// returns `char *` then the string will be destroyed with `free()`.
  template <typename F> TypeLookupError(const F &fn) {
    Context = new F(fn);
    Fn = [](void *context, Command command, void *param) -> void * {
      auto castContext = reinterpret_cast<F *>(context);
      switch (command) {
      case Command::CopyErrorString: {
        return TypeLookupError::getCString((*castContext)());
      }
      case Command::DestroyErrorString:
        free(param);
        return nullptr;
      case Command::CopyContext:
        return new F(*castContext);
      case Command::DestroyContext:
        delete castContext;
        return nullptr;
      }
      llvm_unreachable("unhandled command!");
    };
  }

  /// Get the error string from the error value. The value must be passed to
  /// `freeErrorString` when done. (Unless you're just calling a `fatalError`
  /// in which case there's no point.)
  char *copyErrorString() const {
    return reinterpret_cast<char *>(
        Fn(Context, Command::CopyErrorString, nullptr));
  }

  /// Free an error string previously obtained from `copyErrorString`.
  void freeErrorString(char *str) const {
    Fn(Context, Command::DestroyErrorString, str);
  }
};

/// A value that's either a `TypeLookupError` or some parameterized type value `T`. A
/// convenience wrapper around `TaggedUnion<T, TypeLookupError>`.
template <typename T> class TypeLookupErrorOr {
  TaggedUnion<T, TypeLookupError> Value;

public:
  TypeLookupErrorOr() : Value(TypeLookupError("freshly constructed error")) {}

  TypeLookupErrorOr(const T &t, bool ignoreValueCheck = false) : Value(t) {
    if (!t && !ignoreValueCheck)
      Value = TypeLookupError("unknown error");
  }

  TypeLookupErrorOr(const TypeLookupError &te) : Value(te) {}

  T getType() {
    if (auto *ptr = Value.template dyn_cast<T>())
      return *ptr;
    return T();
  }

  TypeLookupError *getError() {
    return Value.template dyn_cast<TypeLookupError>();
  }

  bool isError() { return getError() != nullptr; }
};

/// Construct a TypeLookupError that creates a string using asprintf. The
/// passed-in format string and arguments are passed directly to swift_asprintf
/// when the string is requested. The arguments are captured and the string is
/// only formatted when needed.
///
/// The crazy sizeof(swift_asprintf(... construct gives us compile-time type
/// checking of the format string and arguments, while still letting us use the
/// variadic template to safely capture the arguments in the lambda.
#define TYPE_LOOKUP_ERROR_FMT(...)                                             \
  (sizeof(swift_asprintf(NULL, __VA_ARGS__)), TypeLookupErrorImpl(__VA_ARGS__))

// Implementation for TYPE_LOOKUP_ERROR_FMT. Don't call directly.
template <typename... Args>
static TypeLookupError TypeLookupErrorImpl(const char *fmt, Args... args) {
  return TypeLookupError([=] {
    char *str;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wformat-security"
#pragma clang diagnostic ignored "-Wformat-nonliteral"
    swift_asprintf(&str, fmt, args...);
#pragma clang diagnostic pop
    return str;
  });
}

} // namespace swift

#endif // SWIFT_DEMANGLING_TypeLookupError_H
