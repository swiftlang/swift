//===--- NameLookup.h - Swift Name Lookup Routines --------------*- C++ -*-===//
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
// This file defines interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_NAME_LOOKUP_H
#define SWIFT_AST_NAME_LOOKUP_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
  class ASTContext;
  class DeclContext;
  class DeclName;
  class Expr;
  class LazyResolver;
  class TupleType;
  class Type;
  class TypeDecl;
  class ValueDecl;

/// UnqualifiedLookupResult - One result of unqualified lookup.
struct UnqualifiedLookupResult {
private:
  ValueDecl *Base;
  ValueDecl *Value;

public:
  UnqualifiedLookupResult(ValueDecl *value) : Base(nullptr), Value(value) { }

  UnqualifiedLookupResult(ValueDecl *base, ValueDecl *value)
    : Base(base), Value(value) { }

  ValueDecl *getValueDecl() const {
    return Value;
  }
  
  ValueDecl *getBaseDecl() const {
    return Base;
  }
};

/// \brief This class implements and represents the result of performing
/// unqualified lookup (i.e. lookup for a plain identifier).
class UnqualifiedLookup {
public:
  /// \brief Lookup an unqualified identifier \p Name in the context.
  ///
  /// If the current DeclContext is nested in a function body, the SourceLoc
  /// is used to determine which declarations in that body are visible.
  UnqualifiedLookup(DeclName Name, DeclContext *DC,
                    LazyResolver *TypeResolver,
                    bool IsKnownPrivate = false,
                    SourceLoc Loc = SourceLoc(),
                    bool IsTypeLookup = false,
                    bool AllowProtocolMembers = false,
                    bool IgnoreAccessControl = false);

  SmallVector<UnqualifiedLookupResult, 4> Results;

  /// \brief Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }

  /// \brief Get the result as a single type, or a null type if that fails.
  TypeDecl *getSingleTypeResult();
};

/// Describes the reason why a certain declaration is visible.
enum class DeclVisibilityKind {
  /// Declaration is a local variable or type.
  LocalVariable,

  /// Declaration is a function parameter.
  FunctionParameter,

  /// Declaration is a generic parameter.
  GenericParameter,

  /// Declaration is a member of the immediately enclosing nominal decl.
  ///
  /// For example, 'Foo' is visible at (1) because of this.
  /// \code
  ///   struct A {
  ///     typealias Foo = Int
  ///     func f() {
  ///       // (1)
  ///     }
  ///   }
  /// \endcode
  MemberOfCurrentNominal,

  /// Declaration that is a requirement of a protocol implemented by the
  /// immediately enclosing nominal decl, in case the nominal decl does not
  /// supply a witness for this requirement.
  ///
  /// For example, 'foo' is visible at (1) because of this.
  /// \code
  /// protocol P {
  ///   func foo()
  /// }
  /// struct A : P {
  ///   func bar() {
  ///     // (1)
  ///   }
  /// }
  /// \endcode
  MemberOfProtocolImplementedByCurrentNominal,

  /// Declaration is a member of the superclass of the immediately enclosing
  /// nominal decl.
  MemberOfSuper,

  /// Declaration is a member of the non-immediately enclosing nominal decl.
  ///
  /// For example, 'Foo' is visible at (1) because of this.
  /// \code
  ///   struct A {
  ///     typealias Foo = Int
  ///     struct B {
  ///       func foo() {
  ///         // (1)
  ///       }
  ///     }
  ///   }
  /// \endcode
  MemberOfOutsideNominal,

  /// Declaration is visible at the top level because it is declared in this
  /// module or in an imported module.
  VisibleAtTopLevel,

  /// Declaration was found via \c AnyObject or \c AnyObject.Type.
  DynamicLookup,
};

/// An abstract base class for a visitor that consumes declarations found within
/// a given context.
class VisibleDeclConsumer {
  virtual void anchor();
public:
  virtual ~VisibleDeclConsumer() = default;

  /// This method is called by findVisibleDecls() every time it finds a decl.
  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) = 0;
};

/// An implementation of VisibleDeclConsumer that's built from a lambda.
template <class Fn>
class LambdaDeclConsumer : public VisibleDeclConsumer {
  Fn Callback;
public:
  LambdaDeclConsumer(Fn &&callback) : Callback(std::move(callback)) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind reason) {
    Callback(VD, reason);
  }
};
template <class Fn>
LambdaDeclConsumer<Fn> makeDeclConsumer(Fn &&callback) {
  return LambdaDeclConsumer<Fn>(std::move(callback));
}

/// A consumer that inserts found decls into an externally-owned SmallVector.
class VectorDeclConsumer : public VisibleDeclConsumer {
  virtual void anchor() override;
public:
  SmallVectorImpl<ValueDecl *> &results;
  explicit VectorDeclConsumer(SmallVectorImpl<ValueDecl *> &decls)
    : results(decls) {}

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    results.push_back(VD);
  }
};

/// A consumer that inserts found decls with a matching name into an
/// externally-owned SmallVector.
class NamedDeclConsumer : public VisibleDeclConsumer {
  virtual void anchor() override;
public:
  DeclName name;
  SmallVectorImpl<UnqualifiedLookupResult> &results;
  bool isTypeLookup;

  NamedDeclConsumer(DeclName name,
                    SmallVectorImpl<UnqualifiedLookupResult> &results,
                    bool isTypeLookup)
    : name(name), results(results), isTypeLookup(isTypeLookup) {}

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    // Give clients an opportunity to filter out non-type declarations early,
    // to avoid circular validation.
    if (isTypeLookup && !isa<TypeDecl>(VD))
      return;
    if (VD->getFullName().matchesRef(name))
      results.push_back(UnqualifiedLookupResult(VD));
  }
};

/// A consumer that filters out decls that are not accessible from a given
/// DeclContext.
class AccessFilteringDeclConsumer final : public VisibleDeclConsumer {
  const DeclContext *DC;
  VisibleDeclConsumer &ChainedConsumer;
  LazyResolver *TypeResolver;
public:
  AccessFilteringDeclConsumer(const DeclContext *DC,
                              VisibleDeclConsumer &consumer,
                              LazyResolver *typeResolver)
    : DC(DC), ChainedConsumer(consumer), TypeResolver(typeResolver) {}

  void foundDecl(ValueDecl *D, DeclVisibilityKind reason) override;
};

/// \brief Remove any declarations in the given set that were overridden by
/// other declarations in that set.
///
/// \returns true if any declarations were removed, false otherwise.
bool removeOverriddenDecls(SmallVectorImpl<ValueDecl*> &decls);

/// \brief Remove any declarations in the given set that are shadowed by
/// other declarations in that set.
///
/// \param decls The set of declarations being considered.
/// \param curModule The current module.
/// \param typeResolver Used to resolve overload types.
///
/// \returns true if any shadowed declarations were removed.
bool removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                         const ModuleDecl *curModule,
                         LazyResolver *typeResolver);

/// Finds decls visible in the given context and feeds them to the given
/// VisibleDeclConsumer.  If the current DeclContext is nested in a function,
/// the SourceLoc is used to determine which declarations in that function
/// are visible.
void lookupVisibleDecls(VisibleDeclConsumer &Consumer,
                        const DeclContext *DC,
                        LazyResolver *typeResolver,
                        bool IncludeTopLevel,
                        SourceLoc Loc = SourceLoc());

/// Finds decls visible as members of the given type and feeds them to the given
/// VisibleDeclConsumer.
///
/// \param CurrDC the DeclContext from which the lookup is done.
void lookupVisibleMemberDecls(VisibleDeclConsumer &Consumer,
                              Type BaseTy,
                              const DeclContext *CurrDC,
                              LazyResolver *typeResolver,
                              bool includeInstanceMembers);

namespace namelookup {
enum class ResolutionKind {
  /// Lookup can match any number of decls, as long as they are all
  /// overloadable.
  ///
  /// If non-overloadable decls are returned, this indicates ambiguous lookup.
  Overloadable,

  /// Lookup should match a single decl.
  Exact,

  /// Lookup should match a single decl that declares a type.
  TypesOnly
};

/// Performs a lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
///
/// \param module The module that will contain the name.
/// \param accessPath The import scope on \p module.
/// \param name The name to look up.
/// \param[out] decls Any found decls will be added to this vector.
/// \param lookupKind Whether this lookup is qualified or unqualified.
/// \param resolutionKind What sort of decl is expected.
/// \param typeResolver The type resolver for decls that need to be
///        type-checked. This is needed for shadowing resolution.
/// \param moduleScopeContext The top-level context from which the lookup is
///        being performed, for checking accessibility. This must be either a
///        FileUnit or a Module.
/// \param extraImports Private imports to include in this search.
void lookupInModule(ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
                    DeclName name, SmallVectorImpl<ValueDecl *> &decls,
                    NLKind lookupKind, ResolutionKind resolutionKind,
                    LazyResolver *typeResolver,
                    const DeclContext *moduleScopeContext,
                    ArrayRef<ModuleDecl::ImportedModule> extraImports = {});

} // end namespace namelookup
} // end namespace swift

#endif
