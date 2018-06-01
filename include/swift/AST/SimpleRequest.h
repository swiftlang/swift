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
#ifndef SWIFT_AST_SIMPLEREQUEST_H
#define SWIFT_AST_SIMPLEREQUEST_H

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsCommon.h"
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
///
/// \tparam Derived The final, derived class type for the request.
/// \tparam Output The type of the result produced by evaluating this request.
/// \tparam Inputs The types of the inputs to this request, i.e., the values
/// that comprise the request itself. These will determine the uniqueness of
/// the request.
///
/// The Derived class needs to implement several operations. The most important
/// one takes an evaluator and the input values, then computes the final
/// result:
/// \code
///   Output operator()(Evaluator &evaluator, Inputs...) const;
/// \endcode
///
/// The Derived class will also need to implement an operation to break a
/// cycle if one is found, i.e.,
/// \code
///   OutputType breakCycle() const;
/// \endcode
///
/// Cycle diagnostics can be handled in one of two ways. Either the Derived
/// class can implement the two cycle-diagnosing operations directly:
/// \code
///   void diagnoseCycle(DiagnosticEngine &diags) const;
///   void noteCycleStep(DiagnosticEngine &diags) const;
/// \endcode
///
/// Or the Derived class can provide a "diagnostic location" operation and
/// diagnostic values for the main cycle diagnostic and a "note" describing a
/// step within the chain of diagnostics:
/// \code
///   T getCycleDiagnosticLoc(Inputs...) const;
///   static constexpr Diag<Inputs...> cycleDiagnostic = ...;
///   static constexpr Diag<Inputs...> cycleStepDiagnostic = ...;
/// \endcode
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

  template<size_t ...Indices>
  void diagnoseImpl(DiagnosticEngine &diags, Diag<Inputs...> diag,
                    llvm::index_sequence<Indices...>) const {
    diags.diagnose(
      asDerived().getCycleDiagnosticLoc(std::get<Indices>(storage)...),
      diag, std::get<Indices>(storage)...);
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

  void diagnoseCycle(DiagnosticEngine &diags) const {
    diagnoseImpl(diags, Derived::cycleDiagnostic,
                 llvm::index_sequence_for<Inputs...>());
  }

  void noteCycleStep(DiagnosticEngine &diags) const {
    diagnoseImpl(diags, Derived::cycleStepDiagnostic,
                 llvm::index_sequence_for<Inputs...>());
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
