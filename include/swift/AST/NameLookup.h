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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
  class ASTContext;
  class DeclContext;
  class DeclName;
  class Expr;
  class GenericSignatureBuilder;
  class LazyResolver;
  class TupleType;
  class Type;
  class TypeDecl;
  class ValueDecl;
  struct SelfBounds;

/// LookupResultEntry - One result of unqualified lookup.
struct LookupResultEntry {
private:

  /// The declaration context through which we found Value. For instance,
  /// class BaseClass {
  ///   func foo() {}
  /// }
  ///
  /// class DerivedClass : BaseClass {
  ///   func bar() {}
  /// }
  ///
  /// When finding foo() from the body of DerivedClass, BaseDC is DerivedClass.
  ///
  /// Another example:
  ///
  /// class BaseClass {
  ///   func bar() {}
  ///   func foo() {}
  /// }
  ///
  /// When finding bar() from the function body of foo(), BaseDC is the method
  /// foo().
  DeclContext *BaseDC;

  /// The declaration corresponds to the given name; i.e. the decl we are
  /// looking up.
  ValueDecl *Value;

public:
  LookupResultEntry(ValueDecl *value) : BaseDC(nullptr), Value(value) { }

  LookupResultEntry(DeclContext *baseDC, ValueDecl *value)
    : BaseDC(baseDC), Value(value) { }

  ValueDecl *getValueDecl() const {
    return Value;
  }

  DeclContext *getDeclContext() const {
    return BaseDC;
  }

  ValueDecl *getBaseDecl() const;
};

/// This class implements and represents the result of performing
/// unqualified lookup (i.e. lookup for a plain identifier).
class UnqualifiedLookup {
public:
  enum class Flags {
    /// This lookup is known to not affect downstream files.
    KnownPrivate = 0x01,
    /// This lookup should only return types.
    TypeLookup = 0x02,
    /// This lookup should consider declarations within protocols to which the
    /// context type conforms.
    AllowProtocolMembers = 0x04,
    /// Don't check access when doing lookup into a type.
    IgnoreAccessControl = 0x08,
    /// This lookup should include results from outside the innermost scope with
    /// results.
    IncludeOuterResults = 0x10,
  };
  using Options = OptionSet<Flags>;

  /// Lookup an unqualified identifier \p Name in the context.
  ///
  /// If the current DeclContext is nested in a function body, the SourceLoc
  /// is used to determine which declarations in that body are visible.
  UnqualifiedLookup(DeclName Name, DeclContext *DC, LazyResolver *TypeResolver,
                    SourceLoc Loc = SourceLoc(), Options options = Options());

  SmallVector<LookupResultEntry, 4> Results;
  /// The index of the first result that isn't from the innermost scope
  /// with results.
  ///
  /// That is, \c makeArrayRef(Results).take_front(IndexOfFirstOuterResults)
  /// will be Results from the innermost scope that had results, and the
  /// remaining elements of Results will be from parent scopes of this one.
  size_t IndexOfFirstOuterResult;

  /// Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }

  /// Get the result as a single type, or a null type if that fails.
  TypeDecl *getSingleTypeResult();
};

inline UnqualifiedLookup::Options operator|(UnqualifiedLookup::Flags flag1,
                                            UnqualifiedLookup::Flags flag2) {
  return UnqualifiedLookup::Options(flag1) | flag2;
}

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
  SmallVectorImpl<LookupResultEntry> &results;
  bool isTypeLookup;

  NamedDeclConsumer(DeclName name,
                    SmallVectorImpl<LookupResultEntry> &results,
                    bool isTypeLookup)
    : name(name), results(results), isTypeLookup(isTypeLookup) {}

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    // Give clients an opportunity to filter out non-type declarations early,
    // to avoid circular validation.
    if (isTypeLookup && !isa<TypeDecl>(VD))
      return;
    if (VD->getFullName().matchesRef(name))
      results.push_back(LookupResultEntry(VD));
  }
};

/// A consumer that filters out decls that are not accessible from a given
/// DeclContext.
class AccessFilteringDeclConsumer final : public VisibleDeclConsumer {
  const DeclContext *DC;
  VisibleDeclConsumer &ChainedConsumer;

public:
  AccessFilteringDeclConsumer(const DeclContext *DC,
                              VisibleDeclConsumer &consumer)
    : DC(DC), ChainedConsumer(consumer) {}

  void foundDecl(ValueDecl *D, DeclVisibilityKind reason) override;
};

/// Remove any declarations in the given set that were overridden by
/// other declarations in that set.
///
/// \returns true if any declarations were removed, false otherwise.
bool removeOverriddenDecls(SmallVectorImpl<ValueDecl*> &decls);

/// Remove any declarations in the given set that are shadowed by
/// other declarations in that set.
///
/// \param decls The set of declarations being considered.
/// \param curModule The current module.
///
/// \returns true if any shadowed declarations were removed.
bool removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                         const ModuleDecl *curModule);

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
                              bool includeInstanceMembers,
                              GenericSignatureBuilder *GSB = nullptr);

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
///        being performed, for checking access. This must be either a
///        FileUnit or a Module.
/// \param extraImports Private imports to include in this search.
void lookupInModule(ModuleDecl *module, ModuleDecl::AccessPathTy accessPath,
                    DeclName name, SmallVectorImpl<ValueDecl *> &decls,
                    NLKind lookupKind, ResolutionKind resolutionKind,
                    LazyResolver *typeResolver,
                    const DeclContext *moduleScopeContext,
                    ArrayRef<ModuleDecl::ImportedModule> extraImports = {});

} // end namespace namelookup

/// Retrieve the set of nominal type declarations that are directly
/// "inherited" by the given declaration at a particular position in the
/// list of "inherited" types.
///
/// Add anything we find to the \c result vector. If we come across the
/// AnyObject type, set \c anyObject true.
void getDirectlyInheritedNominalTypeDecls(
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
    unsigned i,
    llvm::SmallVectorImpl<std::pair<SourceLoc, NominalTypeDecl *>> &result,
    bool &anyObject);

/// Retrieve the set of nominal type declarations that are directly
/// "inherited" by the given declaration, looking through typealiases
/// and splitting out the components of compositions.
///
/// If we come across the AnyObject type, set \c anyObject true.
SmallVector<std::pair<SourceLoc, NominalTypeDecl *>, 4>
getDirectlyInheritedNominalTypeDecls(
                      llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                      bool &anyObject);

/// Retrieve the set of nominal type declarations that appear as the
/// constraint type of any "Self" constraints in the where clause of the
/// given protocol or protocol extension.
SelfBounds getSelfBoundsFromWhereClause(
    llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl);

namespace namelookup {

/// Performs a qualified lookup into the given module and, if necessary, its
/// reexports, observing proper shadowing rules.
void
lookupVisibleDeclsInModule(ModuleDecl *M, ModuleDecl::AccessPathTy accessPath,
                           SmallVectorImpl<ValueDecl *> &decls,
                           NLKind lookupKind,
                           ResolutionKind resolutionKind,
                           LazyResolver *typeResolver,
                           const DeclContext *moduleScopeContext,
                           ArrayRef<ModuleDecl::ImportedModule> extraImports = {});

/// Searches through statements and patterns for local variable declarations.
class FindLocalVal : public StmtVisitor<FindLocalVal> {
  friend class ASTVisitor<FindLocalVal>;

  const SourceManager &SM;
  SourceLoc Loc;
  VisibleDeclConsumer &Consumer;

public:
  FindLocalVal(const SourceManager &SM, SourceLoc Loc,
               VisibleDeclConsumer &Consumer)
      : SM(SM), Loc(Loc), Consumer(Consumer) {}

  void checkValueDecl(ValueDecl *D, DeclVisibilityKind Reason) {
    Consumer.foundDecl(D, Reason);
  }

  void checkPattern(const Pattern *Pat, DeclVisibilityKind Reason);
  
  void checkParameterList(const ParameterList *params);

  void checkGenericParams(GenericParamList *Params);

  void checkSourceFile(const SourceFile &SF);

private:
  bool isReferencePointInRange(SourceRange R) {
    return SM.rangeContainsTokenLoc(R, Loc);
  }

  void visitBreakStmt(BreakStmt *) {}
  void visitContinueStmt(ContinueStmt *) {}
  void visitFallthroughStmt(FallthroughStmt *) {}
  void visitFailStmt(FailStmt *) {}
  void visitReturnStmt(ReturnStmt *) {}
  void visitYieldStmt(YieldStmt *) {}
  void visitThrowStmt(ThrowStmt *) {}
  void visitPoundAssertStmt(PoundAssertStmt *) {}
  void visitDeferStmt(DeferStmt *DS) {
    // Nothing in the defer is visible.
  }

  void checkStmtCondition(const StmtCondition &Cond);

  void visitIfStmt(IfStmt *S);
  void visitGuardStmt(GuardStmt *S);

  void visitWhileStmt(WhileStmt *S);
  void visitRepeatWhileStmt(RepeatWhileStmt *S);
  void visitDoStmt(DoStmt *S);

  void visitForEachStmt(ForEachStmt *S);

  void visitBraceStmt(BraceStmt *S, bool isTopLevelCode = false);
  
  void visitSwitchStmt(SwitchStmt *S);

  void visitCaseStmt(CaseStmt *S);

  void visitDoCatchStmt(DoCatchStmt *S);
  void visitCatchClauses(ArrayRef<CatchStmt*> clauses);
  void visitCatchStmt(CatchStmt *S);
  
};

} // end namespace namelookup
} // end namespace swift

#endif
