//===- Optional.h - Simple variant for passing optional values --*- C++ -*-===//
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
//
//  This file provides Optional, a template class modeled in the spirit of
//  OCaml's 'opt' variant.  The idea is to strongly type whether or not
//  a value can be optional.
//
//  Note that this is a C++11-only re-implementation of LLVM's Optional class,
//  which provides the benefit of not constructing the object. This could also
//  be implemented in C++98/03 for LLVM (and will eventually be), but it's a
//  pain to re-implement unrestricted unions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_OPTIONAL_H
#define SWIFT_BASIC_OPTIONAL_H

#include <type_traits>
#include <utility>
#include <cassert>

namespace swift {
  /// An enum whose purpose is to make it easier to initialize an
  /// empty optional.
  enum Nothing_t {
    Nothing
  };

  template<typename T>
  class Optional {
    // Place Value in an anonymous union to suppress implicit value semantics.
    union {
      T Value;
    };
    unsigned HasValue : 1;
    
  public:
    /// \brief Construct an empty instance.
    Optional() : HasValue(false) { }

    /// \brief Construct an empty instance.
    Optional(Nothing_t _) : HasValue(false) { }
    
    /// \brief Construct an instance containing a value of type \c T
    /// constructed with the given arguments.
    ///
    /// \param Args The arguments with which the \c T object will be
    /// direct-initialized.
    template<typename ...ArgTypes>
    Optional(ArgTypes &&...Args)
      : Value(std::forward<ArgTypes>(Args)...), HasValue(true)
    {
    }

    Optional(Optional &Other) : HasValue(Other.HasValue) {
      if (HasValue)
        ::new ((void*)&Value) T(Other.Value);
    }

    Optional(const Optional &Other) : HasValue(Other.HasValue) {
      if (HasValue)
        ::new ((void*)&Value) T(Other.Value);
    }
    
    Optional(Optional &&Other) : HasValue(Other.HasValue) {
      if (HasValue) {
        ::new ((void*)&Value) T(std::move(Other.Value));
        Other.HasValue = false;
      }
    }
    
    Optional &operator=(const Optional &Other) {
      if (HasValue && Other.HasValue) {
        Value = Other.Value;
        return *this;
      }
      
      if (HasValue) {
        reset();
        return *this;
      }
      
      if (Other.HasValue) {
        HasValue = true;
        ::new ((void*)&Value) T(Other.Value);
      }
      
      return *this;
    }
    
    Optional &operator=(Optional &&Other) {
      if (HasValue && Other.HasValue) {
        Value = std::move(Other.Value);
        Other.reset();
        return *this;
      }
      
      if (HasValue) {
        reset();
        return *this;
      }
      
      if (Other.HasValue) {
        HasValue = true;
        ::new ((void*)&Value) T(std::move(Other.Value));
        Other.reset();
      }
      
      return *this;
    }
    
    ~Optional() { reset(); }

    // Create a new object by constructing it in place with the given arguments.
    template<typename ...ArgTypes>
    void emplace(ArgTypes &&...Args) {
      reset();
      HasValue = true;
      ::new ((void*)&Value) T(std::forward<ArgTypes>(Args)...);
    }

    void reset() {
      if (!HasValue)
        return;
      
      Value.~T();
      HasValue = false;
    }
    
    T &getValue() & { assert(HasValue); return Value; }
    const T &getValue() const & { assert(HasValue); return Value; }
    
    T getValue() && {
      assert(HasValue);
      T result = std::move(Value);
      reset();
      return result;
    }
    
    bool hasValue() const { return HasValue; }
    explicit operator bool() const { return HasValue; }
    
    const T* operator->() const { assert(HasValue); return &Value; }
          T* operator->()       { assert(HasValue); return &Value; }
    const T& operator*() const & { assert(HasValue); return Value; }
          T& operator*() &       { assert(HasValue); return Value; }
    
    T operator*() && {
      assert(HasValue);
      T result = std::move(Value);
      reset();
      return result;
    }
  };
}

#endif
