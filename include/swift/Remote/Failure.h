//===--- Failure.h - Failure to access remote memory ------------*- C++ -*-===//
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
//
//  This file provides a simple diagnostics library for the various
//  operations for working with a remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_FAILURE_H
#define SWIFT_REMOTE_FAILURE_H

#include "swift/Remote/RemoteAddress.h"

#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <string>
#include <cstring>
#include <type_traits>

namespace swift {
namespace remote {

class Failure {
public:
  /// An enum which unifies all of the failure kinds into a single namespace.
  /// This is how kinds are stored internally.
  enum class Kind {
#define FAILURE(KIND, TEXT, ARGTYS) KIND,
#include "swift/Remote/FailureKinds.def"
  };

  /// A bunch of enums for all the different kinds of failure.
  /// Users should construct a Failure by passing Failure::KIND instead
  /// of Failure::Kind::KIND.
  #define FAILURE(KIND, TEXT, ARGTYS) \
  enum KIND##_t { KIND = unsigned(Kind::KIND) };
  #include "swift/Remote/FailureKinds.def"

private:
  // A whole lot of template machinery to validate that the right argument
  // types were passed:

  // A sequence of types.
  template <class... Tys> struct TypeBundle {};

  // A way of turning (X, Y, Z) into a TypeBundle:
  //   typename TypeBundleForFunctionTypeParameters<void ARGS>::types
  // Partially specialized at the end of this file.
  template <class FnTy> struct TypeBundleForFunctionTypeParameters;

  // A way of getting the expected TypeBundle of (nonce) argument types
  // for a particular failure kind:
  //   typename ArgTypesForFailureKind<KindTy>::types
  // Explicitly specialized for each kind type at the end of this file.
  template <class KindTy> struct ArgTypesForFailureKind;

  // Nonce expected argument types.
  enum ArgType_String {};
  enum ArgType_Address {};

  // A predicate that decides whether the given argument type satisfies
  // the given nonce expected argument type.
  template <class Expected, class Actual> struct IsAcceptableArgType {
    static constexpr bool value = false;
  };

  template <class... Ts>
  static void validateFailureArgsRecursive(TypeBundle<Ts...> expected,
                                           TypeBundle<> actual) {
    static_assert(std::is_same<decltype(expected), decltype(actual)>::value,
                  "too few arguments to diagnostic");
  }

  template <class Expected, class... RestOfExpected,
            class Actual, class... RestOfActual>
  static void validateFailureArgsRecursive(
                            TypeBundle<Expected, RestOfExpected...> expected,
                            TypeBundle<Actual, RestOfActual...> actual) {
    using SimplifiedActual = typename std::decay<Actual>::type;
    static_assert(IsAcceptableArgType<Expected, SimplifiedActual>::value,
                  "argument does no match");
    validateFailureArgsRecursive(TypeBundle<RestOfExpected...>(),
                                 TypeBundle<RestOfActual...>());
  }

public:
  /// Validate that it's okay to construct a Failure with the given arguments.
  template <class KindTy, class... ArgTys>
  static void validateFailureArgs(KindTy ty, ArgTys &&... argTys) {
    using ExpectedArgTypes = typename ArgTypesForFailureKind<KindTy>::types;
    validateFailureArgsRecursive(ExpectedArgTypes(), TypeBundle<ArgTys...>());
  }

private:
  static const char *getTextForKind(Kind kind) {
    switch (kind) {
#define FAILURE(KIND, TEXT, ARGTYS) \
    case Kind::KIND: return TEXT;
#include "swift/Remote/FailureKinds.def"    
    }

    llvm_unreachable("Unhandled FailureKind in switch.");
  }

  union ArgStorage {
    std::string String;
    RemoteAddress Address;

    ArgStorage() {}
    ArgStorage(const ArgStorage &other) = delete;
    ArgStorage &operator=(const ArgStorage &other) = delete;
    ~ArgStorage() {}
  };
  enum class ArgStorageKind : char {
    None,
    String, // std::string
    Address, // RemoteAddress
  };

  enum {
    MaxArgs = 4
  };

  Kind TheKind;
  ArgStorageKind ArgKinds[MaxArgs];
  ArgStorage Args[MaxArgs];

  template <unsigned Index>
  void initArgs() {
    static_assert(Index <= MaxArgs, "index out of bounds");
    for (unsigned i = Index; i < MaxArgs; ++i) {
      ArgKinds[i] = ArgStorageKind::None;
    }
  }

  template <unsigned Index, class T, class... Ts>
  void initArgs(T &&nextArg, Ts &&... restOfArgs) {
    static_assert(Index < MaxArgs, "index out of bounds");
    initArg(Index, std::forward<T>(nextArg));
    initArgs<Index + 1>(std::forward<T>(restOfArgs)...);
  }

  void initArg(unsigned index, std::string &&string) {
    ArgKinds[index] = ArgStorageKind::String;
    new (&Args[index]) std::string(std::move(string));
  }

  void initArg(unsigned index, const std::string &string) {
    ArgKinds[index] = ArgStorageKind::String;
    new (&Args[index]) std::string(string);
  }

  void initArg(unsigned index, RemoteAddress address) {
    ArgKinds[index] = ArgStorageKind::Address;
    new (&Args[index]) RemoteAddress(address);
  }

public:
  template <class KindTy, class... Ts>
  explicit Failure(KindTy kind, Ts &&...args) : TheKind(Kind(kind)) {
    validateFailureArgs(kind, std::forward<Ts>(args)...);
    initArgs<0>(std::forward<Ts>(args)...);
  }

  Failure(const Failure &other) : TheKind(other.TheKind) {
    for (unsigned i = 0; i != MaxArgs; ++i) {
      ArgKinds[i] = other.ArgKinds[i];
      switch (ArgKinds[i]) {
      case ArgStorageKind::None:
        break;
      case ArgStorageKind::String:
        ::new (&Args[i].String) std::string(other.Args[i].String);
        break;
      case ArgStorageKind::Address:
        ::new (&Args[i].Address) RemoteAddress(other.Args[i].Address);
        break;
      }
    }
  }

  Failure(Failure &&other) : TheKind(other.TheKind) {
    for (unsigned i = 0; i != MaxArgs; ++i) {
      ArgKinds[i] = other.ArgKinds[i];
      switch (ArgKinds[i]) {
      case ArgStorageKind::None:
        break;
      case ArgStorageKind::String:
        ::new (&Args[i].String) std::string(std::move(other.Args[i].String));
        break;
      case ArgStorageKind::Address:
        ::new (&Args[i].Address) RemoteAddress(std::move(other.Args[i].Address));
        break;
      }
    }
  }

  Failure &operator=(const Failure &other) {
    this->~Failure();
    ::new (this) Failure(other);
    return *this;
  }

  Failure &operator=(Failure &&other) {
    this->~Failure();
    ::new (this) Failure(std::move(other));
    return *this;
  }

  ~Failure() {
    for (unsigned i = 0; i != MaxArgs; ++i) {
      switch (ArgKinds[i]) {
      case ArgStorageKind::None:
        break;
      case ArgStorageKind::String:
        Args[i].String.~basic_string();
        break;
      case ArgStorageKind::Address:
        Args[i].Address.~RemoteAddress();
        break;
      }
    }
  }

  /// Return the kind of failure.
  Kind getKind() const { return TheKind; }

  /// Return the number of arguments to the failure.
  unsigned getNumArgs() const {
    unsigned i = 0;
    for (; i != MaxArgs; ++i) {
      // Stop at the first missing argument.
      if (ArgKinds[i] == ArgStorageKind::None)
        break;
    }
    return i;
  }

  /// Render the failure as an error message.
  std::string render() const {
    std::string result;

    const char *text = getTextForKind(TheKind);
    while (const char *next = std::strchr(text, '%')) {
      // Append everything we just skipped over.
      result.append(text, next - text);

      // Skip the '%'.
      next++;

      // Do something based on the character after '%'.
      char c = *next++;
      text = next;

      if (c == '%') {
        result += c;
        continue;
      }

      assert('0' <= c && c <= '9');
      unsigned argIndex = c - '0';
      assert(argIndex < MaxArgs);

      switch (ArgKinds[argIndex]) {
      case ArgStorageKind::None:
        LLVM_BUILTIN_UNREACHABLE;

      // Stringize a string argument by just appending it.
      case ArgStorageKind::String:
        result += Args[argIndex].String;
        continue;

      // Stringize an address argument as a hex number.  We assume that an
      // address less than 2^32 is for a 32-bit target and accordingly
      // only print 8 digits for it.
      //
      // It is really silly to provide our own hex-stringization here,
      // but swift::remote is supposed to be a minimal, header-only library.
      case ArgStorageKind::Address: {
        result += '0';
        result += 'x';
        uint64_t address = Args[argIndex].Address.getAddressData();
        unsigned max = ((address >> 32) != 0 ? 16 : 8);
        for (unsigned i = 0; i != max; ++i) {
          result += "0123456789abcdef"[(address >> (max - 1 - i) * 4) & 0xF];
        }
        continue;
      }
      }
      LLVM_BUILTIN_UNREACHABLE;
    }

    // Append the rest of the string.
    result.append(text);

    return result;
  }
};

template <class... Tys>
struct Failure::TypeBundleForFunctionTypeParameters<void(Tys...)> {
  using types = TypeBundle<Tys...>;
};

template <class... Tys> class TypeBundle {};

#define FAILURE(KIND, TEXT, ARGTYS)                                      \
template <> struct Failure::ArgTypesForFailureKind<Failure::KIND##_t> {  \
  using String = ArgType_String;                                         \
  using Address = ArgType_Address;                                       \
  using types = TypeBundleForFunctionTypeParameters<void ARGTYS>::types; \
};
#include "swift/Remote/FailureKinds.def"

template <class T>
struct Failure::IsAcceptableArgType<Failure::ArgType_String, T> {
  static constexpr bool value = std::is_convertible<T, std::string>::value;
};

template <>
struct Failure::IsAcceptableArgType<Failure::ArgType_Address, RemoteAddress> {
  static constexpr bool value = true;
};

} // end namespace remote
} // end namespace swift

#endif // SWIFT_REMOTE_FAILURE_H

