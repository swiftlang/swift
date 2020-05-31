//===--- OperatorNameLookup.h - Operator and Precedence Lookup --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines interfaces for looking up operator and precedence group
// declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_OPERATOR_NAME_LOOKUP_H
#define SWIFT_AST_OPERATOR_NAME_LOOKUP_H

#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "llvm/ADT/TinyPtrVector.h"

namespace swift {

/// Base class for infix operator and precedence group lookup results.
template <typename Derived, typename DeclTy>
class OperatorLookupResultBase {
protected:
  const DeclContext *ModuleDC;
  Identifier Name;
  TinyPtrVector<DeclTy *> Results;

private:
  const Derived &asDerived() const {
    return *static_cast<const Derived *>(this);
  }

public:
  OperatorLookupResultBase(const DeclContext *moduleDC, Identifier name,
                           TinyPtrVector<DeclTy *> &&results)
      : ModuleDC(moduleDC), Name(name), Results(std::move(results)) {}

  /// Retrieve the underlying results vector.
  TinyPtrVector<DeclTy *> get() && { return std::move(Results); }

  /// If the lookup produced a single result, returns it. Otherwise returns
  /// \c nullptr.
  DeclTy *getSingle() const {
    return Results.size() == 1 ? Results[0] : nullptr;
  }

  /// If the lookup produced a single result, returns it. Otherwise, emits an
  /// ambiguity or missing decl diagnostic, and returns \c nullptr.
  DeclTy *getSingleOrDiagnose(SourceLoc loc, bool forBuiltin = false) const {
    if (auto single = getSingle())
      return single;

    if (isAmbiguous()) {
      asDerived().diagnoseAmbiguity(loc);
    } else {
      assert(!hasResults());
      asDerived().diagnoseMissing(loc, forBuiltin);
    }
    return nullptr;
  }

  using const_iterator = typename decltype(Results)::const_iterator;
  const_iterator begin() const { return Results.begin(); }
  const_iterator end() const { return Results.end(); }

  /// Whether the lookup produced at least one result.
  bool hasResults() const { return !Results.empty(); }

  /// Whether the lookup produced multiple ambiguous results.
  bool isAmbiguous() const { return Results.size() > 1; }

  friend bool operator==(const OperatorLookupResultBase &lhs,
                         const OperatorLookupResultBase &rhs) {
    return lhs.Results == rhs.Results;
  }
  friend bool operator!=(const OperatorLookupResultBase &lhs,
                         const OperatorLookupResultBase &rhs) {
    return !(lhs == rhs);
  }
  friend void simple_display(llvm::raw_ostream &out,
                             const OperatorLookupResultBase &result) {
    simple_display(out, result.Results);
  }
};

/// The result of a precedence group lookup.
class PrecedenceGroupLookupResult final
    : public OperatorLookupResultBase<PrecedenceGroupLookupResult,
                                      PrecedenceGroupDecl> {
  friend class OperatorLookupResultBase<PrecedenceGroupLookupResult,
                                        PrecedenceGroupDecl>;

public:
  PrecedenceGroupLookupResult(const DeclContext *moduleDC, Identifier name,
                              TinyPtrVector<PrecedenceGroupDecl *> &&results)
      : OperatorLookupResultBase(moduleDC, name, std::move(results)) {}

  void diagnoseAmbiguity(SourceLoc loc) const;
  void diagnoseMissing(SourceLoc loc, bool forBuiltin) const;
};

/// The result of an infix operator lookup.
class InfixOperatorLookupResult final
    : public OperatorLookupResultBase<InfixOperatorLookupResult,
                                      InfixOperatorDecl> {
  friend class OperatorLookupResultBase<InfixOperatorLookupResult,
                                        InfixOperatorDecl>;

public:
  InfixOperatorLookupResult(const DeclContext *moduleDC, Identifier name,
                            TinyPtrVector<InfixOperatorDecl *> &&results)
      : OperatorLookupResultBase(moduleDC, name, std::move(results)) {}

  void diagnoseAmbiguity(SourceLoc loc) const;
  void diagnoseMissing(SourceLoc loc, bool forBuiltin) const;
};

} // end namespace swift

#endif
