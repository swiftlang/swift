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
#include "swift/Basic/Compiler.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {
class ASTContext;
class DeclName;
class GenericSignatureBuilder;
class Type;
class TypeDecl;
class ValueDecl;
struct SelfBounds;
class NominalTypeDecl;

namespace ast_scope {
class ASTSourceFileScope;
class ASTScopeImpl;
} // namespace ast_scope

/// LookupResultEntry - One result of unqualified lookup.
struct LookupResultEntry {
private:
  /// The declaration context through which we found \c Value. For instance,
  /// \code
  /// class BaseClass {
  ///   func foo() {}
  /// }
  ///
  /// class DerivedClass : BaseClass {
  ///   func bar() {}
  /// }
  /// \endcode
  ///
  /// When finding \c foo() from the body of \c DerivedClass, \c BaseDC is \c
  /// DerivedClass.
  ///
  /// Another example:
  /// \code
  /// class BaseClass {
  ///   func bar() {}
  ///   func foo() {}
  /// }
  /// \endcode
  ///
  /// When finding \c bar() from the function body of \c foo(), \c BaseDC is
  /// the method \c foo().
  ///
  /// \c BaseDC will be the method if \c self is needed for the lookup,
  /// and will be the type if not.
  /// In other words: If \c baseDC is a method, it means you found an instance
  /// member and you should add an implicit 'self.' (Each method has its own
  /// implicit self decl.) There's one other kind of non-method context that
  /// has a 'self.' -- a lazy property initializer, which unlike a non-lazy
  /// property can reference \c self) Hence: \code
  ///  class Outer {
  ///    static func s()
  ///    func i()
  ///    class Inner {
  ///      static func ss()
  ///      func ii() {
  ///        func F() {
  ///          ii() // OK! implicitly self.ii; BaseDC is the method
  ///          s()  // OK! s() is defined in an outer type; BaseDC is the type
  ///          ss() // error: must write /Inner.ss() here since its static
  ///          i()  // error: there's no outer 'self.'
  ///        }
  ///      }
  /// \endcode
  ///
  /// To sum up:  The distinction is whether you need to know the run-time
  /// value of \c self. It might be clearer if \code baseDC was always a type,
  /// and there was an additional \c ParamDecl field in \c LookupResult which
  /// would store the implicit self, if any. \c BaseDC is always one of your
  /// outer DCs. if you're inside a type it should never be an extension of
  /// that type. And if you're inside an extension it will always be an
  /// extension (if it found something at that level).
  DeclContext *BaseDC;

  /// The declaration corresponds to the given name; i.e. the decl we are
  /// looking up.
  ValueDecl *Value;

public:
  LookupResultEntry(ValueDecl *value) : BaseDC(nullptr), Value(value) {}

  LookupResultEntry(DeclContext *baseDC, ValueDecl *value)
    : BaseDC(baseDC), Value(value) {}

  ValueDecl *getValueDecl() const { return Value; }

  DeclContext *getDeclContext() const { return BaseDC; }

  ValueDecl *getBaseDecl() const;

  void print(llvm::raw_ostream &) const;
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
  UnqualifiedLookup(DeclName Name, DeclContext *DC,
                    SourceLoc Loc = SourceLoc(), Options options = Options());
  
  using ResultsVector = SmallVector<LookupResultEntry, 4>;
  ResultsVector Results;
  
  /// The index of the first result that isn't from the innermost scope
  /// with results.
  ///
  /// That is, \c makeArrayRef(Results).take_front(IndexOfFirstOuterResults)
  /// will be \c Results from the innermost scope that had results, and the
  /// remaining elements of Results will be from parent scopes of this one.
  ///
  /// Allows unqualified name lookup to return results from outer scopes.
  /// This is necessary for disambiguating calls to functions like `min` and
  /// `max`.
  size_t IndexOfFirstOuterResult;

  /// Return true if anything was found by the name lookup.
  bool isSuccess() const { return !Results.empty(); }

  /// Get the result as a single type, or a null type if that fails.
  TypeDecl *getSingleTypeResult() const;
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

/// For Decls found with DeclVisibilityKind::DynamicLookup, contains details of
/// how they were looked up. For example, the SubscriptDecl used to find a
/// KeyPath dynamic member.
class DynamicLookupInfo {
public:
  enum Kind {
    None,
    AnyObject,
    KeyPathDynamicMember,
  };

  struct KeyPathDynamicMemberInfo {
    /// The subscript(dynamicMember:) by which we found the declaration.
    SubscriptDecl *subscript = nullptr;

    /// The type context of `subscript`, which may be different than the
    /// original base type of the lookup if this declaration was found by nested
    /// dynamic lookups.
    Type baseType = Type();

    /// Visibility of the declaration itself without dynamic lookup.
    ///
    /// For example, dynamic lookup for KeyPath<Derived, U>, might find
    /// Base::foo with originalVisibility == MemberOfSuper.
    DeclVisibilityKind originalVisibility = DeclVisibilityKind::DynamicLookup;
  };

  Kind getKind() const { return kind; }

  const KeyPathDynamicMemberInfo &getKeyPathDynamicMember() const;

  DynamicLookupInfo() : kind(None) {}
  DynamicLookupInfo(Kind kind) : kind(kind) {
    assert(kind != KeyPathDynamicMember && "use KeyPathDynamicMemberInfo ctor");
  }

  /// Construct for a KeyPath dynamic member lookup.
  DynamicLookupInfo(SubscriptDecl *subscript, Type baseType,
                    DeclVisibilityKind originalVisibility);

private:
  Kind kind;
  KeyPathDynamicMemberInfo keypath = {};
};

/// An abstract base class for a visitor that consumes declarations found within
/// a given context.
class VisibleDeclConsumer {
  virtual void anchor();
public:
  virtual ~VisibleDeclConsumer() = default;

  /// This method is called by findVisibleDecls() every time it finds a decl.
  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                         DynamicLookupInfo dynamicLookupInfo = {}) = 0;
};

/// An implementation of VisibleDeclConsumer that's built from a lambda.
template <class Fn>
class LambdaDeclConsumer : public VisibleDeclConsumer {
  Fn Callback;
public:
  LambdaDeclConsumer(Fn &&callback) : Callback(std::move(callback)) {}

  void foundDecl(ValueDecl *VD, DeclVisibilityKind reason, DynamicLookupInfo) {
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

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                         DynamicLookupInfo) override {
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

  virtual void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                         DynamicLookupInfo dynamicLookupInfo = {}) override {
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

  void foundDecl(ValueDecl *D, DeclVisibilityKind reason,
                 DynamicLookupInfo dynamicLookupInfo = {}) override;
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
/// \param dc The DeclContext from which the lookup was performed.
///
/// \returns true if any shadowed declarations were removed.
bool removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                         const DeclContext *dc);

/// Finds decls visible in the given context and feeds them to the given
/// VisibleDeclConsumer.  If the current DeclContext is nested in a function,
/// the SourceLoc is used to determine which declarations in that function
/// are visible.
void lookupVisibleDecls(VisibleDeclConsumer &Consumer,
                        const DeclContext *DC,
                        bool IncludeTopLevel,
                        SourceLoc Loc = SourceLoc());

/// Finds decls visible as members of the given type and feeds them to the given
/// VisibleDeclConsumer.
///
/// \param CurrDC the DeclContext from which the lookup is done.
void lookupVisibleMemberDecls(VisibleDeclConsumer &Consumer,
                              Type BaseTy,
                              const DeclContext *CurrDC,
                              bool includeInstanceMembers,
                              GenericSignatureBuilder *GSB = nullptr);

namespace namelookup {

/// Once name lookup has gathered a set of results, perform any necessary
/// steps to prune the result set before returning it to the caller.
void pruneLookupResultSet(const DeclContext *dc, NLOptions options,
                          SmallVectorImpl<ValueDecl *> &decls);

/// Do nothing if debugClient is null.
template <typename Result>
void filterForDiscriminator(SmallVectorImpl<Result> &results,
                            DebuggerClient *debugClient);

void recordLookupOfTopLevelName(DeclContext *topLevelContext, DeclName name,
                                bool isCascading);

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

/// Retrieve the TypeLoc at the given \c index from among the set of
/// type declarations that are directly "inherited" by the given declaration.
inline TypeLoc &
getInheritedTypeLocAtIndex(llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                           unsigned index) {
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>())
    return typeDecl->getInherited()[index];

  return decl.get<ExtensionDecl *>()->getInherited()[index];
}

namespace namelookup {

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
  
  
/// The bridge between the legacy UnqualifedLookupFactory and the new ASTScope
/// lookup system
class AbstractASTScopeDeclConsumer {
public:
  AbstractASTScopeDeclConsumer() {}

  virtual ~AbstractASTScopeDeclConsumer() = default;

  /// Called for every ValueDecl visible from the lookup.
  /// Returns true if the lookup can be stopped at this point.
  /// BaseDC is per legacy
  /// Takes an array in order to batch the consumption before setting
  /// IndexOfFirstOuterResult when necessary.
  virtual bool consume(ArrayRef<ValueDecl *> values, DeclVisibilityKind vis,
                       NullablePtr<DeclContext> baseDC = nullptr) = 0;

  /// Eventually this functionality should move into ASTScopeLookup
  virtual bool
  lookInMembers(NullablePtr<DeclContext> selfDC, DeclContext *const scopeDC,
                NominalTypeDecl *const nominal,
                function_ref<bool(Optional<bool>)> calculateIsCascadingUse) = 0;

#ifndef NDEBUG
  virtual void startingNextLookupStep() = 0;
  virtual void finishingLookup(std::string) const = 0;
  virtual bool isTargetLookup() const = 0;
#endif
};
  
/// Just used to print
/// Used to gather lookup results
class ASTScopeDeclGatherer : public AbstractASTScopeDeclConsumer {
  SmallVector<ValueDecl *, 32> values;

public:
  virtual ~ASTScopeDeclGatherer() = default;

  bool consume(ArrayRef<ValueDecl *> values, DeclVisibilityKind vis,
               NullablePtr<DeclContext> baseDC = nullptr) override;

  /// Eventually this functionality should move into ASTScopeLookup
  bool lookInMembers(NullablePtr<DeclContext>, DeclContext *const,
                     NominalTypeDecl *const,
                     function_ref<bool(Optional<bool>)>) override {
    return false;
  }

#ifndef NDEBUG
  void startingNextLookupStep() override {}
  void finishingLookup(std::string) const override {}
  bool isTargetLookup() const override { return false; }
#endif

  ArrayRef<ValueDecl *> getDecls() { return values; }
};
} // end namespace namelookup

/// The interface into the ASTScope subsystem
class ASTScope {
  friend class ast_scope::ASTScopeImpl;
  ast_scope::ASTSourceFileScope *const impl;

public:
  ASTScope(SourceFile *);

  /// \return the scopes traversed
  static llvm::SmallVector<const ast_scope::ASTScopeImpl *, 0>
  unqualifiedLookup(SourceFile *, DeclName, SourceLoc,
                    const DeclContext *startingContext,
                    namelookup::AbstractASTScopeDeclConsumer &);

  static Optional<bool>
  computeIsCascadingUse(ArrayRef<const ast_scope::ASTScopeImpl *> history,
                        Optional<bool> initialIsCascadingUse);

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");
  void print(llvm::raw_ostream &) const;
  void dumpOneScopeMapLocation(std::pair<unsigned, unsigned>);

  // Make vanilla new illegal for ASTScopes.
  void *operator new(size_t bytes) = delete;
  // Need this because have virtual destructors
  void operator delete(void *data) {}

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ASTScope));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

private:
  static ast_scope::ASTSourceFileScope *createScopeTree(SourceFile *);
};

} // end namespace swift

#endif
