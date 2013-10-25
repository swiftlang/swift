//===--- NameLookup.h - Swift Name Lookup Routines --------------*- C++ -*-===//
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
// This file defines interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_NAME_LOOKUP_H
#define SWIFT_AST_NAME_LOOKUP_H

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Optional.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
  class ASTContext;
  class DeclContext;
  class Expr;
  class LazyResolver;
  class Module;
  class TupleType;
  class Type;
  class TypeDecl;
  class ValueDecl;

/// UnqualifiedLookupResult - One result of unqualified lookup.
struct UnqualifiedLookupResult {
private:
  ValueDecl *Base;
  union {
    ValueDecl *Value;
    Module *NamedModule;
  };

public:
  /// Kind - The kind of reference.
  enum KindTy {
    /// ModuleMember - "x" refers to a value declared at module scope.
    ModuleMember,

    /// LocalDecl - "x" refers to a value declared in a local scope.
    LocalDecl,

    /// MemberProperty - "x" refers to an instance property of a type which
    /// is a containing scope.
    MemberProperty,

    /// MemberFunction - "x" refers to an instance function of a type
    /// is a containing scope.  If the lookup is inside an instance function
    /// of the containing type, it refers to the instance function; otherwise,
    /// it refers to the curried function on the metatype.
    MemberFunction,

    /// MetatypeMember - "x" refers to a member of a metatype "A", which is a
    /// referred to by BaseDecl.
    MetatypeMember,
  
    /// ExistentialMember - "x" refers to a member of an existential type,
    /// which is referred to by BaseDecl.
    ExistentialMember,

    /// ArchetypeMember - "x" refers to a member of an archetype type,
    /// which is referred to by BaseDecl.
    ArchetypeMember,

    /// MetaArchetypeMember - "x" refers to a member of the metatype of an
    /// archetype type, which is referred to by BaseDecl. The base is evaluated
    /// and ignored.
    MetaArchetypeMember,

    /// ModuleName - "x" refers to a module, either the current
    /// module or an imported module.
    ModuleName
  } Kind;

  bool hasValueDecl() const {
    return Kind != ModuleName;
  }

  ValueDecl *getValueDecl() const {
    assert(hasValueDecl());
    return Value;
  }

  Module *getNamedModule() const {
    assert(Kind == ModuleName);
    return NamedModule;
  }

  ValueDecl *getBaseDecl() const {
    return Base;
  }

  static UnqualifiedLookupResult getModuleMember(ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = nullptr;
    R.Value = value;
    R.Kind = ModuleMember;
    return R;
  }

  static UnqualifiedLookupResult getLocalDecl(ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = nullptr;
    R.Value = value;
    R.Kind = LocalDecl;
    return R;
  }

  static UnqualifiedLookupResult getMemberProperty(ValueDecl *base,
                                                   ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MemberProperty;
    return R;
  }

  static UnqualifiedLookupResult getMemberFunction(ValueDecl *base,
                                                   ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MemberFunction;
    return R;
  }

  static UnqualifiedLookupResult getMetatypeMember(ValueDecl *base,
                                                   ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MetatypeMember;
    return R;
  }

  static UnqualifiedLookupResult getExistentialMember(ValueDecl *base,
                                                      ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = ExistentialMember;
    return R;
  }

  static UnqualifiedLookupResult getArchetypeMember(ValueDecl *base,
                                                    ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = ArchetypeMember;
    return R;
  }

  static UnqualifiedLookupResult getMetaArchetypeMember(ValueDecl *base,
                                                        ValueDecl *value) {
    UnqualifiedLookupResult R;
    R.Base = base;
    R.Value = value;
    R.Kind = MetaArchetypeMember;
    return R;
  }

  static UnqualifiedLookupResult getModuleName(Module *m) {
    UnqualifiedLookupResult R;
    R.Base = nullptr;
    R.NamedModule = m;
    R.Kind = ModuleName;
    return R;
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
  UnqualifiedLookup(Identifier Name, DeclContext *DC,
                    LazyResolver *TypeResolver,
                    SourceLoc Loc = SourceLoc(),
                    bool IsTypeLookup = false);

  /// \brief Look up an identifier \p Name in the module named \p Module.
  ///
  /// Note that this will not resolve types.
  static Optional<UnqualifiedLookup> forModuleAndName(ASTContext &C,
                                                      StringRef Module,
                                                      StringRef Name);

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
  /// module or in a imported module.
  VisibleAtTopLevel,

  /// Declaration was found via \c DynamicLookup or \c DynamicLookup.metatype.
  DynamicLookup,
};

/// VisibleDeclConsumer - An abstract base class for a visitor that consumes
/// visible declarations found within a given context. Subclasses of this class
/// can be used with lookupVisibleDecls().
class VisibleDeclConsumer {
public:
  virtual ~VisibleDeclConsumer();

  /// This method is called by findVisibleDecls() every time it finds a decl.
  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) = 0;
};

/// \brief Remove any declarations in the given set that are shadowed by
/// other declarations in that set.
///
/// \param decls The set of declarations being considered.
/// \param curModule The current module.
/// \param typeResolver Used to resolve overload types.
void removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                         const Module *curModule,
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
                              LazyResolver *typeResolver);

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

/// Performs a qualified lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
void
lookupVisibleDeclsInModule(Module *M, Module::AccessPathTy accessPath,
                           SmallVectorImpl<ValueDecl *> &decls,
                           NLKind lookupKind,
                           ResolutionKind resolutionKind,
                           LazyResolver *typeResolver,
                           ArrayRef<Module::ImportedModule> extraImports = {});
} // end namespace namelookup
} // end namespace swift

#endif
