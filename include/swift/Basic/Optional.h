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
//  pain to re-implement std::aligned_storage.
//
//===----------------------------------------------------------------------===//

#include <type_traits>
#include <utility>

#ifndef SWIFT_BASIC_OPTIONAL_H
#define SWIFT_BASIC_OPTIONAL_H

namespace swift {
  /// An enum whose purpose is to make it easier to initialize an
  /// empty optional.
  enum Nothing_t {
    Nothing
  };

  template<typename T>
  class Optional {
    union {
      std::aligned_storage<sizeof(T), alignof(T)> Aligner;
      char Bytes[sizeof(T)];
    } Storage;
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
    Optional(ArgTypes &&...Args) : HasValue(true) {
      new (getPointer()) T(std::forward<ArgTypes>(Args)...);
    }
    
    Optional(const Optional &Other) : HasValue(Other.HasValue) {
      if (Other)
        new (getPointer()) T(Other.getValue());
    }
    
    Optional(Optional &&Other) : HasValue(Other.HasValue) {
      if (Other) {
        new (getPointer()) T(std::move(Other.getValue()));
        Other.HasValue = false;
      }
    }
    
    Optional &operator=(const Optional &Other) {
      if (HasValue && Other.HasValue) {
        getValue() = Other.getValue();
        return *this;
      }
      
      if (HasValue) {
        reset();
        return *this;
      }
      
      HasValue = true;
      new (getPointer()) T(Other.getValue());
      return *this;
    }
    
    Optional &operator=(Optional &&Other) {
      if (HasValue && Other.HasValue) {
        getValue() = std::move(Other.getValue());
        Other.HasValue = false;
        return *this;
      }
      
      if (HasValue) {
        reset();
        return *this;
      }
      
      HasValue = true;
      new (getPointer()) T(std::move(Other.getValue()));
      Other.HasValue = false;
      return *this;
    }
    
    ~Optional() { reset(); }
    
    void reset() {
      if (!HasValue)
        return;
      
      getPointer()->~T();
      HasValue = false;
    }
    
    T *getPointer() { 
      assert(HasValue); 
      return reinterpret_cast<T *>(&Storage.Bytes[0]);
    }
    
    const T *getPointer() const { 
      assert(HasValue); 
      return reinterpret_cast<const T *>(&Storage.Bytes[0]);
    }
    
    T &getValue() { return *getPointer(); }
    const T &getValue() const { return *getPointer(); }
    
    bool hasValue() const { return HasValue; }
    explicit operator bool() const { return HasValue; }
    
    const T* operator->() const { return getPointer(); }
          T* operator->()       { return getPointer(); }
    const T& operator*() const { return getValue(); }
          T& operator*()       { return getValue(); }
  };
}

#endif
