//===--- IterativeTypeChecker.h - Iterative Type Checker --------*- C++ -*-===//
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
//  This file defines the IterativeTypeChecker class, which performs
//  iterative type checking by tracking the set of outstanding
//  type-checking requests and servicing them as needed.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_ITERATIVE_TYPE_CHECKER_H
#define SWIFT_SEMA_ITERATIVE_TYPE_CHECKER_H

#include "swift/Sema/TypeCheckRequest.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class ASTContext;
class TypeChecker;

/// An iterative type checker that processes type check requests to
/// ensure that the AST has the information needed by the client.
class IterativeTypeChecker {
  /// The underlying (non-iterative) type checker on which this iterative
  /// type checker depends.
  ///
  /// Each dependency on the non-iterative type checker potentially
  /// introduces recursion and should be eliminated.
  TypeChecker &TC;

  /// A stack of the currently-active requests.
  SmallVector<TypeCheckRequest, 4> ActiveRequests;

  // Declare the is<request kind>Satisfied predicates,
  // enumerateDependenciesOf<request kind> functions, and
  // satisfy<request kind> functions.
#define TYPE_CHECK_REQUEST(Request,PayloadName)               \
  bool is##Request##Satisfied(                                \
         TypeCheckRequest::PayloadName##PayloadType payload); \
  void process##Request(                                      \
         TypeCheckRequest::PayloadName##PayloadType payload,  \
         UnsatisfiedDependency unsatisfiedDependency);        \
  bool breakCycleFor##Request(                                \
         TypeCheckRequest::PayloadName##PayloadType payload);

#include "swift/Sema/TypeCheckRequestKinds.def"

  void process(TypeCheckRequest request,
               UnsatisfiedDependency unsatisfiedDependency);

  /// Attempt to break a cyclic reference for this request after
  /// diagnosing it.
  ///
  /// \returns true if the cycle could be broken by this request, and
  /// false if it couldn't be broken by altering this request.
  bool breakCycle(TypeCheckRequest request);

  /// Diagnose a reference cycle.
  void diagnoseCircularReference(ArrayRef<TypeCheckRequest> requests);

  /// Determine whether the given request has already been satisfied.
  bool isSatisfied(TypeCheckRequest request);

public:
  IterativeTypeChecker(TypeChecker &tc) : TC(tc) { }

  ASTContext &getASTContext() const;

  DiagnosticEngine &getDiags() const;

  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    return getDiags().diagnose(std::forward<ArgTypes>(Args)...);
  }

  /// Satisfy the given request.
  ///
  /// Ensures that the given type check request has been satisfied. On
  /// completion of this operation, one can query the AST for
  /// information regarding this particular request, e.g., get the
  /// type of a declaration, perform name lookup into a particular
  /// context, and so on.
  void satisfy(TypeCheckRequest request);
};

}

#endif /* SWIFT_SEMA_ITERATIVE_TYPE_CHECKER_H */
