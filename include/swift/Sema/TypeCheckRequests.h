//===--- TypeCheckRequests.h - Type Checking Requests -----------*- C++ -*-===//
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
//  This file defines type checking requests.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_REQUESTS_H
#define SWIFT_SEMA_REQUESTS_H

#include "swift/AST/Type.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/SimpleRequest.h"
#include "llvm/ADT/Hashing.h"

namespace swift {

struct TypeLoc;

/// Display a nominal type or extension thereof.
void simple_display(
       llvm::raw_ostream &out,
       const llvm::PointerUnion<TypeDecl *, ExtensionDecl *> &value);

/// Request the type from the ith entry in the inheritance clause for the
/// given declaration.
class InheritedTypeRequest :
    public SimpleRequest<InheritedTypeRequest,
                         CacheKind::SeparatelyCached,
                         Type,
                         llvm::PointerUnion<TypeDecl *, ExtensionDecl *>,
                         unsigned>
{
  /// Retrieve the TypeLoc for this inherited type.
  TypeLoc &getTypeLoc(llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                      unsigned index) const;

public:
  using SimpleRequest::SimpleRequest;
  using SimpleRequest::operator();

private:
  friend class SimpleRequest;

  // Evaluation.
  Type operator()(Evaluator &evaluator,
                  llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                  unsigned index) const;

public:
  // Cycle handling
  Type breakCycle() const { return Type(); }
  void diagnoseCycle(DiagnosticEngine &diags) const { }
  void noteCycleStep(DiagnosticEngine &diags) const { }

  // Caching
  bool isCached() const { return true; }
  Optional<Type> getCachedResult() const;
  void cacheResult(Type value) const;
};

/// Request the superclass type for the given class.
class SuperclassTypeRequest :
    public SimpleRequest<SuperclassTypeRequest,
                         CacheKind::SeparatelyCached,
                         Type,
                         ClassDecl *> {
public:
  using SimpleRequest::SimpleRequest;
  using SimpleRequest::operator();

private:
  friend class SimpleRequest;

  // Evaluation.
  Type operator()(Evaluator &evaluator, ClassDecl *classDecl) const;

public:
  // Cycle handling
  Type breakCycle() const { return Type(); }
  void diagnoseCycle(DiagnosticEngine &diags) const;
  void noteCycleStep(DiagnosticEngine &diags) const;

  // Separate caching.
  bool isCached() const { return true; }
  Optional<Type> getCachedResult() const;
  void cacheResult(Type value) const;
};

#define SWIFT_TYPEID_ZONE 10
#define SWIFT_TYPEID_HEADER "swift/Sema/TypeCheckerTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"

} // end namespace swift

#endif // SWIFT_SEMA_REQUESTS_H
