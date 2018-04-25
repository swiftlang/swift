//===--- SimpleRequest.h - Simple Request Instances -------------*- C++ -*-===//
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
//  This file defines the SimpleRequest class template, which makes it easier
//  to define new request kinds.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_BASIC_SIMPLEREQUEST_H
#define SWIFT_BASIC_SIMPLEREQUEST_H

#include "swift/Basic/SimpleDisplay.h"
#include "swift/Basic/TypeID.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include <tuple>

namespace swift {

class Evaluator;

/// CRTP base class that describes a request operation that takes values
/// with the given input types (\c Inputs...) and produces an output of
/// the given type.
template<typename Derived, typename Output, typename ...Inputs>
class SimpleRequest {
  std::tuple<Inputs...> storage;

  Derived &asDerived() {
    return *static_cast<Derived *>(this);
  }

  const Derived &asDerived() const {
    return *static_cast<const Derived *>(this);
  }

  template<size_t ...Indices>
  Output callDerived(Evaluator &evaluator,
                     llvm::index_sequence<Indices...>) const {
    static_assert(sizeof...(Indices) > 0, "Subclass must define operator()");
    return asDerived()(evaluator, std::get<Indices>(storage)...);
  }

protected:
  /// Retrieve the storage value directly.
  const std::tuple<Inputs...> &getStorage() const { return storage; }

public:
  using OutputType = Output;
  
  explicit SimpleRequest(const Inputs& ...inputs)
    : storage(inputs...) { }

  OutputType operator()(Evaluator &evaluator) const {
    return callDerived(evaluator, llvm::index_sequence_for<Inputs...>());
  }
  
  friend bool operator==(const SimpleRequest &lhs, const SimpleRequest &rhs) {
    return lhs.storage == rhs.storage;
  }

  friend bool operator!=(const SimpleRequest &lhs, const SimpleRequest &rhs) {
    return !(lhs == rhs);
  }

  friend hash_code hash_value(const SimpleRequest &request) {
    using llvm::hash_combine;

    return hash_combine(TypeID<Derived>::value, request.storage);
  }

  friend void simple_display(llvm::raw_ostream &out,
                             const Derived &request) {
    out << TypeID<Derived>::getName();
    simple_display(out, request.storage);
  }
};

}

#endif // SWIFT_BASIC_SIMPLEREQUEST_H
