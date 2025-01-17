//===--- Stmt.h - Swift Language Statement ASTs -----------------*- C++ -*-===//
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
// This file defines the Stmt class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_STMT_H
#define SWIFT_AST_STMT_H

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/IfConfigClause.h"
#include "swift/AST/ThrownErrorDestination.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/NullablePtr.h"
#include "llvm/Support/TrailingObjects.h"
#include <optional>

namespace swift {

class AnyPattern;
class ASTContext;
class ASTWalker;
class Decl;
class DeclContext;
class Evaluator;
class Expr;
class FuncDecl;
class AbstractFunctionDecl;
class Pattern;
class PatternBindingDecl;
class VarDecl;
class CaseStmt;
class DoCatchStmt;
class IsSingleValueStmtResult;
class SwitchStmt;

enum class StmtKind {
#define STMT(ID, PARENT) ID,
#define LAST_STMT(ID) Last_Stmt = ID,
#define STMT_RANGE(Id, FirstId, LastId) \
  First_##Id##Stmt = FirstId, Last_##Id##Stmt = LastId,
#include "swift/AST/StmtNodes.def"
};
enum : unsigned { NumStmtKindBits =
  countBitsUsed(static_cast<unsigned>(StmtKind::Last_Stmt)) };

/// Stmt - Base class for all statements in swift.
class alignas(8) Stmt : public ASTAllocated<Stmt> {
  Stmt(const Stmt&) = delete;
  Stmt& operator=(const Stmt&) = delete;

protected:
  // clang-format off
  union { uint64_t OpaqueBits;

  SWIFT_INLINE_BITFIELD_BASE(Stmt, bitmax(NumStmtKindBits,8) + 1,
    /// Kind - The subclass of Stmt that this is.
    Kind : bitmax(NumStmtKindBits,8),

    /// Implicit - Whether this statement is implicit.
    Implicit : 1
  );

  SWIFT_INLINE_BITFIELD_FULL(BraceStmt, Stmt, 32,
    : NumPadBits,
    NumElements : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(CaseStmt, Stmt, 32,
    : NumPadBits,
    NumPatterns : 32
  );

  SWIFT_INLINE_BITFIELD_EMPTY(LabeledStmt, Stmt);

  SWIFT_INLINE_BITFIELD_FULL(DoCatchStmt, LabeledStmt, 32,
    : NumPadBits,
    NumCatches : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(SwitchStmt, LabeledStmt, 32,
    : NumPadBits,
    CaseCount : 32
  );
    
  SWIFT_INLINE_BITFIELD(ReturnStmt, Stmt, 1,
    /// Whether the result is an implied return, e.g for an implicit single
    /// expression return.
    IsImplied : 1
  );

  SWIFT_INLINE_BITFIELD_FULL(YieldStmt, Stmt, 32,
    : NumPadBits,
    NumYields : 32
  );

  } Bits;
  // clang-format on

  /// Return the given value for the 'implicit' flag if present, or if None,
  /// return true if the location is invalid.
  static bool getDefaultImplicitFlag(std::optional<bool> implicit,
                                     SourceLoc keyLoc) {
    return implicit.has_value() ? *implicit : keyLoc.isInvalid();
  }

public:
  Stmt(StmtKind kind, bool implicit) {
    Bits.OpaqueBits = 0;
    Bits.Stmt.Kind = static_cast<unsigned>(kind);
    Bits.Stmt.Implicit = implicit;
  }

  StmtKind getKind() const { return StmtKind(Bits.Stmt.Kind); }

  /// Retrieve the name of the given statement kind.
  ///
  /// This name should only be used for debugging dumps and other
  /// developer aids, and should never be part of a diagnostic or exposed
  /// to the user of the compiler in any way.
  static StringRef getKindName(StmtKind kind);

  /// Retrieve the descriptive kind name for a given statement. This is suitable
  /// for use in diagnostics.
  static StringRef getDescriptiveKindName(StmtKind K);

  /// Return the location of the start of the statement.
  SourceLoc getStartLoc() const;
  
  /// Return the location of the end of the statement.
  SourceLoc getEndLoc() const;
  
  SourceRange getSourceRange() const;
  SourceLoc TrailingSemiLoc;

  /// Whether the statement can produce a single value, and as such may be
  /// treated as an expression.
  IsSingleValueStmtResult mayProduceSingleValue(ASTContext &ctx) const;

  /// isImplicit - Determines whether this statement was implicitly-generated,
  /// rather than explicitly written in the AST.
  bool isImplicit() const { return Bits.Stmt.Implicit; }

  /// walk - This recursively walks the AST rooted at this statement.
  Stmt *walk(ASTWalker &walker);
  Stmt *walk(ASTWalker &&walker) { return walk(walker); }

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &OS, const ASTContext *Ctx = nullptr, unsigned Indent = 0) const;
};

/// BraceStmt - A brace enclosed sequence of expressions, stmts, or decls, like
/// { var x = 10; print(10) }.
class BraceStmt final : public Stmt,
    private llvm::TrailingObjects<BraceStmt, ASTNode> {
  friend TrailingObjects;

  SourceLoc LBLoc;
  SourceLoc RBLoc;

  BraceStmt(SourceLoc lbloc, ArrayRef<ASTNode> elements, SourceLoc rbloc,
            std::optional<bool> implicit);

public:
  static BraceStmt *create(ASTContext &ctx, SourceLoc lbloc,
                           ArrayRef<ASTNode> elements, SourceLoc rbloc,
                           std::optional<bool> implicit = std::nullopt);

  static BraceStmt *createImplicit(ASTContext &ctx,
                                   ArrayRef<ASTNode> elements) {
    return create(ctx, /*lbloc=*/SourceLoc(), elements, /*rbloc=*/SourceLoc(),
                  /*implicit=*/true);
  }

  SourceLoc getLBraceLoc() const { return LBLoc; }
  SourceLoc getRBraceLoc() const { return RBLoc; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;

  SourceLoc getContentStartLoc() const;
  SourceLoc getContentEndLoc() const;
  /// The range of the brace statement without the braces.
  SourceRange getContentRange() const {
    return {getContentStartLoc(), getContentEndLoc()};
  }

  bool empty() const { return getNumElements() == 0; }
  unsigned getNumElements() const { return Bits.BraceStmt.NumElements; }

  ASTNode getFirstElement() const { return getElements().front(); }
  ASTNode getLastElement() const { return getElements().back(); }

  void setFirstElement(ASTNode node) { getElements().front() = node; }
  void setLastElement(ASTNode node) { getElements().back() = node; }

  /// The elements contained within the BraceStmt.
  MutableArrayRef<ASTNode> getElements() {
    return {getTrailingObjects<ASTNode>(), Bits.BraceStmt.NumElements};
  }

  /// The elements contained within the BraceStmt (const version).
  ArrayRef<ASTNode> getElements() const {
    return {getTrailingObjects<ASTNode>(), Bits.BraceStmt.NumElements};
  }

  ASTNode findAsyncNode();

  /// If this brace contains a single ASTNode, or a \c #if that has a single active
  /// element, returns it. This will always be the last element of the brace.
  /// Otherwise returns \c nullptr.
  ASTNode getSingleActiveElement() const;

  /// If this brace is wrapping a single active expression, returns it. This
  /// includes both a single expression element, or a single expression in an
  /// active \c #if. Otherwise returns \c nullptr.
  Expr *getSingleActiveExpression() const;

  /// If this brace is wrapping a single active statement, returns it. This
  /// includes both a single statement element, or a single statement in an
  /// active \c #if. Otherwise returns \c nullptr.
  Stmt *getSingleActiveStatement() const;

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Brace; }
};

/// ReturnStmt - A return statement.  The result is optional; "return" without
/// an expression is semantically equivalent to "return ()".
///    return 42
class ReturnStmt : public Stmt {
  SourceLoc ReturnLoc;
  Expr *Result;

  ReturnStmt(SourceLoc returnLoc, Expr *result, bool isImplicit)
      : Stmt(StmtKind::Return, isImplicit), ReturnLoc(returnLoc),
        Result(result) {
    Bits.ReturnStmt.IsImplied = false;
  }

public:
  static ReturnStmt *createParsed(ASTContext &ctx, SourceLoc returnLoc,
                                  Expr *result) {
    return new (ctx) ReturnStmt(returnLoc, result, /*isImplicit*/ false);
  }

  static ReturnStmt *createImplicit(ASTContext &ctx, SourceLoc returnLoc,
                                    Expr *result) {
    return new (ctx) ReturnStmt(returnLoc, result, /*isImplicit*/ true);
  }

  static ReturnStmt *createImplicit(ASTContext &ctx, Expr *result) {
    return createImplicit(ctx, SourceLoc(), result);
  }

  /// Create an implicit implied ReturnStmt for e.g a single expression body.
  static ReturnStmt *createImplied(ASTContext &ctx, Expr *result) {
    assert(result && "Result must be present to be implied");
    auto *RS = createImplicit(ctx, result);
    RS->Bits.ReturnStmt.IsImplied = true;
    return RS;
  }

  SourceLoc getReturnLoc() const { return ReturnLoc; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;

  /// Whether the result is an implied return, e.g for an implicit single
  /// expression return.
  bool isImplied() const {
    return Bits.ReturnStmt.IsImplied;
  }

  bool hasResult() const { return Result != 0; }
  Expr *getResult() const {
    assert(Result && "ReturnStmt doesn't have a result");
    return Result;
  }
  void setResult(Expr *e) { Result = e; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Return;}
};

/// YieldStmt - A yield statement.  The yield-values sequence is not optional,
/// but the parentheses are.
///    yield 42
class YieldStmt final
    : public Stmt, private llvm::TrailingObjects<YieldStmt, Expr*> {
  friend TrailingObjects;

  SourceLoc YieldLoc;
  SourceLoc LPLoc;
  SourceLoc RPLoc;

  YieldStmt(SourceLoc yieldLoc, SourceLoc lpLoc, ArrayRef<Expr *> yields,
            SourceLoc rpLoc, std::optional<bool> implicit = std::nullopt)
      : Stmt(StmtKind::Yield, getDefaultImplicitFlag(implicit, yieldLoc)),
        YieldLoc(yieldLoc), LPLoc(lpLoc), RPLoc(rpLoc) {
    Bits.YieldStmt.NumYields = yields.size();
    memcpy(getMutableYields().data(), yields.data(),
           yields.size() * sizeof(Expr*));
  }

public:
  static YieldStmt *create(const ASTContext &ctx, SourceLoc yieldLoc,
                           SourceLoc lp, ArrayRef<Expr *> yields, SourceLoc rp,
                           std::optional<bool> implicit = std::nullopt);

  SourceLoc getYieldLoc() const { return YieldLoc; }
  SourceLoc getLParenLoc() const { return LPLoc; }
  SourceLoc getRParenLoc() const { return RPLoc; }

  SourceLoc getStartLoc() const { return YieldLoc; }
  SourceLoc getEndLoc() const;

  ArrayRef<Expr*> getYields() const {
    return {getTrailingObjects<Expr*>(), Bits.YieldStmt.NumYields};
  }
  MutableArrayRef<Expr*> getMutableYields() {
    return {getTrailingObjects<Expr*>(), Bits.YieldStmt.NumYields};
  }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Yield; }
};

/// The statement `then <expr>`. This is used within if/switch expressions to
/// indicate the value being produced by a given branch.
class ThenStmt : public Stmt {
  SourceLoc ThenLoc;
  Expr *Result;

  ThenStmt(SourceLoc thenLoc, Expr *result, bool isImplicit)
      : Stmt(StmtKind::Then, isImplicit), ThenLoc(thenLoc), Result(result) {
    assert(Result && "Must have non-null result");
  }

public:
  /// Create a new parsed ThenStmt.
  static ThenStmt *createParsed(ASTContext &ctx, SourceLoc thenLoc,
                                Expr *result);

  /// Create an implicit ThenStmt.
  ///
  /// Note that such statements will be elided during the result builder
  /// transform.
  static ThenStmt *createImplicit(ASTContext &ctx, Expr *result);

  SourceLoc getThenLoc() const { return ThenLoc; }

  SourceRange getSourceRange() const;

  Expr *getResult() const { return Result; }
  void setResult(Expr *e) { Result = e; }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Then; }
};

/// DeferStmt - A 'defer' statement.  This runs the substatement it contains
/// when the enclosing scope is exited.
///
///    defer { cleanUp() }
///
/// The AST representation for a defer statement is a bit weird.  We model this
/// as if they wrote:
///
///    func tmpClosure() { body }
///    tmpClosure()   // This is emitted on each path that needs to run this.
///
/// As such, the body of the 'defer' is actually type checked within the
/// closure's DeclContext.  We do this because of unfortunateness in SILGen,
/// some expressions (e.g. OpenExistentialExpr) cannot be multiply emitted in a
/// composable way.  When this gets fixed, patches r27767 and r27768 can be
/// reverted to go back to the simpler and more obvious representation.
///
class DeferStmt : public Stmt {
  SourceLoc DeferLoc;
  
  /// This is the bound temp function.
  FuncDecl *tempDecl;

  /// This is the invocation of the closure, which is to be emitted on any error
  /// paths.
  Expr *callExpr;

  DeferStmt(SourceLoc DeferLoc,
            FuncDecl *tempDecl, Expr *callExpr)
    : Stmt(StmtKind::Defer, /*implicit*/false),
      DeferLoc(DeferLoc), tempDecl(tempDecl),
      callExpr(callExpr) {}

public:
  /// Create a 'defer' statement. This automatically creates the "temp decl" and
  /// the call expression. It's the caller's responsibility to populate the
  /// body of the func decl.
  static DeferStmt *create(DeclContext *dc, SourceLoc deferLoc);

  SourceLoc getDeferLoc() const { return DeferLoc; }
  
  SourceLoc getStartLoc() const { return DeferLoc; }
  SourceLoc getEndLoc() const;

  FuncDecl *getTempDecl() const { return tempDecl; }
  Expr *getCallExpr() const { return callExpr; }
  void setCallExpr(Expr *E) { callExpr = E; }

  /// Dig the original user's body of the defer out for AST fidelity.
  BraceStmt *getBodyAsWritten() const;
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Defer; }
};

/// Represent `let`/`var` optional binding, or `case` pattern matching in
/// conditional statements (i.e. `if`, `guard`, `while`).
class alignas(8) ConditionalPatternBindingInfo
    : public ASTAllocated<ConditionalPatternBindingInfo> {
  /// Location of the var/let/case keyword.
  SourceLoc IntroducerLoc;

  /// Pattern being matched. In the case of an "implicit optional" pattern, the
  /// OptionalSome pattern is explicitly added to this as an 'implicit' pattern.
  Pattern *ThePattern;

  /// The value for matching.
  Expr *Initializer;

  ConditionalPatternBindingInfo(SourceLoc IntroducerLoc, Pattern *ThePattern,
                                Expr *Initializer)
      : IntroducerLoc(IntroducerLoc), ThePattern(ThePattern),
        Initializer(Initializer) {}

public:
  static ConditionalPatternBindingInfo *create(ASTContext &ctx,
                                               SourceLoc IntroducerLoc,
                                               Pattern *ThePattern,
                                               Expr *Initializer) {
    return new (ctx)
        ConditionalPatternBindingInfo(IntroducerLoc, ThePattern, Initializer);
  }

  SourceLoc getIntroducerLoc() const { return IntroducerLoc; }
  void setIntroducerLoc(SourceLoc Loc) { IntroducerLoc = Loc; }
  Pattern *getPattern() const { return ThePattern; }
  void setPattern(Pattern *P) { ThePattern = P; }
  Expr *getInitializer() const { return Initializer; }
  void setInitializer(Expr *E) { Initializer = E; }

  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; };
  SourceLoc getEndLoc() const { return getSourceRange().End; };
};

/// An expression that guards execution based on whether the run-time
/// configuration supports a given API, e.g.,
/// #available(OSX >= 10.9, iOS >= 7.0).
class alignas(8) PoundAvailableInfo final :
    private llvm::TrailingObjects<PoundAvailableInfo, AvailabilitySpec *> {
  friend TrailingObjects;

  SourceLoc PoundLoc;
  SourceLoc LParenLoc;
  SourceLoc RParenLoc;

  // The number of queries tail allocated after this object.
  unsigned NumQueries;
  
  /// The version range when this query will return true. This value is
  /// filled in by Sema.
  VersionRange AvailableRange;

  /// For zippered builds, this is the version range for the target variant
  /// that must hold for the query to return true. For example, when
  /// compiling with target x86_64-macosx10.15 and target-variant
  /// x86_64-ios13.0 a query of #available(macOS 10.22, iOS 20.0, *) will
  /// have a variant range of [20.0, +inf).
  /// This is filled in by Sema.
  VersionRange VariantAvailableRange;

  /// Indicates that the expression is checking if a version range 
  /// is **not** available.
  bool _isUnavailability;

  PoundAvailableInfo(SourceLoc PoundLoc, SourceLoc LParenLoc,
                     ArrayRef<AvailabilitySpec *> queries, SourceLoc RParenLoc,
                     bool isUnavailability)
   : PoundLoc(PoundLoc), LParenLoc(LParenLoc), RParenLoc(RParenLoc),
     NumQueries(queries.size()), AvailableRange(VersionRange::empty()),
     VariantAvailableRange(VersionRange::empty()), 
     _isUnavailability(isUnavailability) {
    std::uninitialized_copy(queries.begin(), queries.end(),
                            getTrailingObjects<AvailabilitySpec *>());
  }
  
public:
  static PoundAvailableInfo *create(ASTContext &ctx, SourceLoc PoundLoc,
                                    SourceLoc LParenLoc,
                                    ArrayRef<AvailabilitySpec *> queries,
                                    SourceLoc RParenLoc,
                                    bool isUnavailability);
  
  ArrayRef<AvailabilitySpec *> getQueries() const {
    return llvm::ArrayRef(getTrailingObjects<AvailabilitySpec *>(), NumQueries);
  }
  
  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  SourceLoc getStartLoc() const { return PoundLoc; }
  SourceLoc getEndLoc() const;
  SourceLoc getLoc() const { return PoundLoc; }
  SourceRange getSourceRange() const { return SourceRange(getStartLoc(),
                                                          getEndLoc()); }
  
  const VersionRange &getAvailableRange() const { return AvailableRange; }
  void setAvailableRange(const VersionRange &Range) { AvailableRange = Range; }

  const VersionRange &getVariantAvailableRange() const {
    return VariantAvailableRange;
  }
  void setVariantAvailableRange(const VersionRange &Range) {
    VariantAvailableRange = Range;
  }

  bool isUnavailability() const { return _isUnavailability; }
};

/// An expression that guards execution based on whether the symbols for the
/// declaration identified by the given expression are non-null at run-time, e.g.
///
///   if #_hasSymbol(foo(_:)) { foo(42) }
///
class PoundHasSymbolInfo final : public ASTAllocated<PoundHasSymbolInfo> {
  Expr *SymbolExpr;
  ConcreteDeclRef ReferencedDecl;
  bool Invalid;

  SourceLoc PoundLoc;
  SourceLoc LParenLoc;
  SourceLoc RParenLoc;

  PoundHasSymbolInfo(SourceLoc PoundLoc, SourceLoc LParenLoc, Expr *SymbolExpr,
                     SourceLoc RParenLoc)
      : SymbolExpr(SymbolExpr), ReferencedDecl(), Invalid(), PoundLoc(PoundLoc),
        LParenLoc(LParenLoc), RParenLoc(RParenLoc){};

public:
  static PoundHasSymbolInfo *create(ASTContext &Ctx, SourceLoc PoundLoc,
                                    SourceLoc LParenLoc, Expr *SymbolExpr,
                                    SourceLoc RParenLoc);

  Expr *getSymbolExpr() const { return SymbolExpr; }
  void setSymbolExpr(Expr *E) { SymbolExpr = E; }

  ConcreteDeclRef getReferencedDecl() { return ReferencedDecl; }
  void setReferencedDecl(ConcreteDeclRef CDR) { ReferencedDecl = CDR; }

  /// Returns true if the referenced decl has been diagnosed as invalid.
  bool isInvalid() const { return Invalid; }
  void setInvalid() { Invalid = true; }

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }
  SourceLoc getStartLoc() const { return PoundLoc; }
  SourceLoc getEndLoc() const { return RParenLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(getStartLoc(), getEndLoc());
  }
};

/// This represents an entry in an "if" or "while" condition.
/// Either a boolean expression, optional binding, pattern matching,
/// `#available`, or `#_hasSymbol`.
///
/// E.g. this 'if' statement has 5 'StmtConditionElement'.
///   if
///     list.count == 1,                          // CK_Boolean
///     let firstElem = list.first,               // CK_PatternBinding
///     case .foo(let value?, "int") = firstElem, // CK_PatternBinding
///     #available(myOS 13),                      // CK_Availability
///     #_hasSymbol(MyStruct.peform(operation:))  // CK_HasSymbol
///   { ... }
///
class alignas(1 << PatternAlignInBits) StmtConditionElement {
private:
  llvm::PointerUnion<Expr *, ConditionalPatternBindingInfo *,
                     PoundAvailableInfo *, PoundHasSymbolInfo *>
      Condition;

public:
  StmtConditionElement() : Condition(nullptr) {}
  StmtConditionElement(Expr *cond) : Condition(cond) {}
  StmtConditionElement(ConditionalPatternBindingInfo *Info) : Condition(Info) {}
  StmtConditionElement(PoundAvailableInfo *Info) : Condition(Info) {}
  StmtConditionElement(PoundHasSymbolInfo *Info) : Condition(Info) {}

  static StmtConditionElement fromOpaqueValue(void *opaque) {
    StmtConditionElement val;
    val.Condition = decltype(Condition)::getFromOpaqueValue(opaque);
    return val;
  }

  void *getOpaqueValue() const { return Condition.getOpaqueValue(); }

  /// ConditionKind - This indicates the sort of condition this is.
  enum ConditionKind {
    CK_Boolean,
    CK_PatternBinding,
    CK_Availability,
    CK_HasSymbol,
  };

  ConditionKind getKind() const {
    if (Condition.is<Expr *>())
      return CK_Boolean;
    if (Condition.is<ConditionalPatternBindingInfo *>())
      return CK_PatternBinding;
    if (Condition.is<PoundAvailableInfo *>())
      return CK_Availability;
    if (Condition.is<PoundHasSymbolInfo *>())
      return CK_HasSymbol;
    return CK_Boolean;
  }

  /// Boolean Condition Accessors.
  Expr *getBooleanOrNull() const { return Condition.dyn_cast<Expr *>(); }

  Expr *getBoolean() const {
    assert(getKind() == CK_Boolean && "Not a condition");
    return Condition.get<Expr *>();
  }
  void setBoolean(Expr *E) {
    assert(getKind() == CK_Boolean && "Not a condition");
    Condition = E;
  }

  /// Pattern Binding Accessors.
  ConditionalPatternBindingInfo *getPatternBindingOrNull() const {
    return Condition.dyn_cast<ConditionalPatternBindingInfo *>();
  }

  ConditionalPatternBindingInfo *getPatternBinding() const {
    assert(getKind() == CK_PatternBinding && "Not a pattern binding condition");
    return Condition.get<ConditionalPatternBindingInfo *>();
  }

  SourceLoc getIntroducerLoc() const {
    return getPatternBinding()->getIntroducerLoc();
  }

  void setIntroducerLoc(SourceLoc loc) {
    getPatternBinding()->setIntroducerLoc(loc);
  }

  Pattern *getPatternOrNull() const {
    if (auto *binding = getPatternBindingOrNull())
      return binding->getPattern();
    return nullptr;
  }

  Pattern *getPattern() const { return getPatternBinding()->getPattern(); }

  void setPattern(Pattern *P) { getPatternBinding()->setPattern(P); }

  Expr *getInitializerOrNull() const {
    if (auto *binding = getPatternBindingOrNull())
      return binding->getInitializer();
    return nullptr;
  }

  Expr *getInitializer() const { return getPatternBinding()->getInitializer(); }

  void setInitializer(Expr *E) { getPatternBinding()->setInitializer(E); }

  // Availability Accessors
  PoundAvailableInfo *getAvailability() const {
    assert(getKind() == CK_Availability && "Not an #available condition");
    return Condition.get<PoundAvailableInfo *>();
  }

  void setAvailability(PoundAvailableInfo *Info) {
    assert(getKind() == CK_Availability && "Not an #available condition");
    Condition = Info;
  }

  // #_hasSymbol Accessors
  PoundHasSymbolInfo *getHasSymbolInfo() const {
    assert(getKind() == CK_HasSymbol && "Not a #_hasSymbol condition");
    return Condition.get<PoundHasSymbolInfo *>();
  }

  void setHasSymbolInfo(PoundHasSymbolInfo *Info) {
    assert(getKind() == CK_HasSymbol && "Not a #_hasSymbol condition");
    Condition = Info;
  }

  /// Whether or not this conditional stmt rebinds self with a `let self`
  /// or `let self = self` condition.
  ///  - If `requiresCaptureListRef` is `true`, additionally requires that the
  ///    RHS of the self condition references a var defined in a capture list.
  ///  - If `requireLoadExpr` is `true`, additionally requires that the RHS of
  ///    the self condition is a `LoadExpr`.
  bool rebindsSelf(ASTContext &Ctx, bool requiresCaptureListRef = false,
                   bool requireLoadExpr = false) const;

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  /// Recursively walks the AST rooted at this statement condition element
  StmtConditionElement *walk(ASTWalker &walker);
  StmtConditionElement *walk(ASTWalker &&walker) { return walk(walker); }
};

struct LabeledStmtInfo {
  Identifier Name;
  SourceLoc Loc;
  
  // Evaluates to true if set.
  operator bool() const { return !Name.empty(); }
};
  
/// LabeledStmt - Common base class between the labeled statements (loops and
/// switch).
class LabeledStmt : public Stmt {
  LabeledStmtInfo LabelInfo;
protected:
  SourceLoc getLabelLocOrKeywordLoc(SourceLoc L) const {
    return LabelInfo ? LabelInfo.Loc : L;
  }
public:
  LabeledStmt(StmtKind Kind, bool Implicit, LabeledStmtInfo LabelInfo)
    : Stmt(Kind, Implicit), LabelInfo(LabelInfo) {}
  
  LabeledStmtInfo getLabelInfo() const { return LabelInfo; }
  void setLabelInfo(LabeledStmtInfo L) { LabelInfo = L; }

  /// Is this statement a valid target of "continue" if labeled?
  ///
  /// For the most part, non-looping constructs shouldn't be
  /// continue-able, but we threw in "do" as a sop.
  bool isPossibleContinueTarget() const;

  /// Is this statement a valid target of an unlabeled "break" or
  /// "continue"?
  ///
  /// The nice, consistent language rule is that unlabeled "break" and
  /// "continue" leave the innermost loop.  We have to include
  /// "switch" (for "break") for consistency with C: Swift doesn't
  /// require "break" to leave a switch case, but it's still way too
  /// similar to C's switch to allow different behavior for "break".
  bool requiresLabelOnJump() const;

  static bool classof(const Stmt *S) {
    return S->getKind() >= StmtKind::First_LabeledStmt &&
           S->getKind() <= StmtKind::Last_LabeledStmt;
  }
};


/// DoStmt - do statement, without any trailing clauses.
class DoStmt : public LabeledStmt {
  SourceLoc DoLoc;
  BraceStmt *Body;
  
public:
  DoStmt(LabeledStmtInfo labelInfo, SourceLoc doLoc, BraceStmt *body,
         std::optional<bool> implicit = std::nullopt)
      : LabeledStmt(StmtKind::Do, getDefaultImplicitFlag(implicit, doLoc),
                    labelInfo),
        DoLoc(doLoc), Body(body) {}

  static DoStmt *createImplicit(ASTContext &C, LabeledStmtInfo labelInfo,
                                ArrayRef<ASTNode> body);

  SourceLoc getDoLoc() const { return DoLoc; }
  
  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  
  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *s) { Body = s; }

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Do; }
};

/// Either an "if let" case or a simple boolean expression can appear as the
/// condition of an 'if', 'guard', or 'while' statement.
using StmtCondition = MutableArrayRef<StmtConditionElement>;

/// This is the common base class between statements that can have labels, and
/// also have complex "if let" style conditions: 'if' and 'while'.
class LabeledConditionalStmt : public LabeledStmt {
  StmtCondition Cond;
public:
  LabeledConditionalStmt(StmtKind Kind, bool Implicit,
                         LabeledStmtInfo LabelInfo, StmtCondition Cond)
    : LabeledStmt(Kind, Implicit, LabelInfo) {
    setCond(Cond);
  }

  StmtCondition getCond() const { return Cond; }
  void setCond(StmtCondition e);

  /// FIXME: Find a better way to implement this. Allows conditions to be
  ///        stored in \c ASTNode.
  StmtCondition *getCondPointer() { return &Cond; }

  /// Whether or not this conditional stmt rebinds self with a `let self`
  /// or `let self = self` condition.
  ///  - If `requiresCaptureListRef` is `true`, additionally requires that the
  ///    RHS of the self condition references a var defined in a capture list.
  ///  - If `requireLoadExpr` is `true`, additionally requires that the RHS of
  ///    the self condition is a `LoadExpr`.
  bool rebindsSelf(ASTContext &Ctx, bool requiresCaptureListRef = false,
                   bool requireLoadExpr = false) const;

  static bool classof(const Stmt *S) {
    return S->getKind() >= StmtKind::First_LabeledConditionalStmt &&
           S->getKind() <= StmtKind::Last_LabeledConditionalStmt;
  }
};
  
  
/// IfStmt - if/then/else statement.  If no 'else' is specified, then the
/// ElseLoc location is not specified and the Else statement is null. After
/// type-checking, the condition is of type Builtin.Int1.
class IfStmt : public LabeledConditionalStmt {
  SourceLoc IfLoc;
  SourceLoc ElseLoc;
  BraceStmt *Then;
  Stmt *Else;
  
public:
  IfStmt(LabeledStmtInfo LabelInfo, SourceLoc IfLoc, StmtCondition Cond,
         BraceStmt *Then, SourceLoc ElseLoc, Stmt *Else,
         std::optional<bool> implicit = std::nullopt)
      : LabeledConditionalStmt(StmtKind::If,
                               getDefaultImplicitFlag(implicit, IfLoc),
                               LabelInfo, Cond),
        IfLoc(IfLoc), ElseLoc(ElseLoc), Then(Then), Else(Else) {
    assert(Then && "Must have non-null 'then' statement");
    assert(!Else || isa<BraceStmt>(Else) ||
           isa<IfStmt>(Else) &&
               "Else statement must either be BraceStmt or IfStmt");
  }

  IfStmt(SourceLoc IfLoc, Expr *Cond, BraceStmt *Then, SourceLoc ElseLoc,
         Stmt *Else, std::optional<bool> implicit, ASTContext &Ctx);

  SourceLoc getIfLoc() const { return IfLoc; }
  SourceLoc getElseLoc() const { return ElseLoc; }

  SourceLoc getStartLoc() const {
    return getLabelLocOrKeywordLoc(IfLoc);
  }
  SourceLoc getEndLoc() const {
    return (Else ? Else->getEndLoc() : Then->getEndLoc());
  }

  BraceStmt *getThenStmt() const { return Then; }
  void setThenStmt(BraceStmt *s) { Then = s; }

  Stmt *getElseStmt() const { return Else; }
  void setElseStmt(Stmt *s) { Else = s; }

  /// Retrieve the complete set of branches for this if statement, including
  /// else if statements.
  ArrayRef<Stmt *> getBranches(SmallVectorImpl<Stmt *> &scratch) const;

  /// Whether the if statement has an unconditional \c else.
  bool isSyntacticallyExhaustive() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::If; }
};

/// GuardStmt - 'guard' statement.  Evaluate a condition and if it fails, run
/// its body.  The body is always guaranteed to exit the current scope (or
/// abort), it never falls through.
///
class GuardStmt : public LabeledConditionalStmt {
  SourceLoc GuardLoc;
  BraceStmt *Body;
  
public:
  GuardStmt(SourceLoc GuardLoc, StmtCondition Cond, BraceStmt *Body,
            std::optional<bool> implicit = std::nullopt)
      : LabeledConditionalStmt(StmtKind::Guard,
                               getDefaultImplicitFlag(implicit, GuardLoc),
                               LabeledStmtInfo(), Cond),
        GuardLoc(GuardLoc), Body(Body) {}

  GuardStmt(SourceLoc GuardLoc, Expr *Cond, BraceStmt *Body,
            std::optional<bool> implicit, ASTContext &Ctx);

  SourceLoc getGuardLoc() const { return GuardLoc; }
  
  SourceLoc getStartLoc() const {
    return getLabelLocOrKeywordLoc(GuardLoc);
  }
  SourceLoc getEndLoc() const {
    return Body->getEndLoc();
  }
  
  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *s) { Body = s; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Guard; }
};

/// WhileStmt - while statement. After type-checking, the condition is of
/// type Builtin.Int1.
class WhileStmt : public LabeledConditionalStmt {
  SourceLoc WhileLoc;
  StmtCondition Cond;
  Stmt *Body;
  
public:
  WhileStmt(LabeledStmtInfo LabelInfo, SourceLoc WhileLoc, StmtCondition Cond,
            Stmt *Body, std::optional<bool> implicit = std::nullopt)
      : LabeledConditionalStmt(StmtKind::While,
                               getDefaultImplicitFlag(implicit, WhileLoc),
                               LabelInfo, Cond),
        WhileLoc(WhileLoc), Body(Body) {}

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(WhileLoc); }
  SourceLoc getEndLoc() const { return Body->getEndLoc(); }
  SourceLoc getWhileLoc() const { return WhileLoc; }

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }
  
  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::While; }
};
  
/// RepeatWhileStmt - repeat/while statement. After type-checking, the
/// condition is of type Builtin.Int1.
class RepeatWhileStmt : public LabeledStmt {
  SourceLoc RepeatLoc, WhileLoc;
  Stmt *Body;
  Expr *Cond;
  
public:
  RepeatWhileStmt(LabeledStmtInfo LabelInfo, SourceLoc RepeatLoc, Expr *Cond,
                  SourceLoc WhileLoc, Stmt *Body,
                  std::optional<bool> implicit = std::nullopt)
      : LabeledStmt(StmtKind::RepeatWhile,
                    getDefaultImplicitFlag(implicit, RepeatLoc), LabelInfo),
        RepeatLoc(RepeatLoc), WhileLoc(WhileLoc), Body(Body), Cond(Cond) {}

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(RepeatLoc); }
  SourceLoc getEndLoc() const;
  SourceLoc getRepeatLoc() const { return RepeatLoc; }
  
  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }

  Expr *getCond() const { return Cond; }
  void setCond(Expr *e) { Cond = e; }
  
  static bool classof(const Stmt *S) {return S->getKind() == StmtKind::RepeatWhile;}
};

/// ForEachStmt - foreach statement that iterates over the elements in a
/// container.
///
/// Example:
/// \code
/// for i in 0...10 {
///   print(String(i))
/// }
/// \endcode
class ForEachStmt : public LabeledStmt {
  SourceLoc ForLoc;
  SourceLoc TryLoc;
  SourceLoc AwaitLoc;
  Pattern *Pat;
  SourceLoc InLoc;
  Expr *Sequence;
  SourceLoc WhereLoc;
  Expr *WhereExpr = nullptr;
  BraceStmt *Body;

  // Set by Sema:
  ProtocolConformanceRef sequenceConformance = ProtocolConformanceRef();
  Type sequenceType;
  PatternBindingDecl *iteratorVar = nullptr;
  Expr *nextCall = nullptr;
  OpaqueValueExpr *elementExpr = nullptr;
  Expr *convertElementExpr = nullptr;

public:
  ForEachStmt(LabeledStmtInfo LabelInfo, SourceLoc ForLoc, SourceLoc TryLoc,
              SourceLoc AwaitLoc, Pattern *Pat, SourceLoc InLoc, Expr *Sequence,
              SourceLoc WhereLoc, Expr *WhereExpr, BraceStmt *Body,
              std::optional<bool> implicit = std::nullopt)
      : LabeledStmt(StmtKind::ForEach, getDefaultImplicitFlag(implicit, ForLoc),
                    LabelInfo),
        ForLoc(ForLoc), TryLoc(TryLoc), AwaitLoc(AwaitLoc), Pat(nullptr),
        InLoc(InLoc), Sequence(Sequence), WhereLoc(WhereLoc),
        WhereExpr(WhereExpr), Body(Body) {
    setPattern(Pat);
  }

  void setIteratorVar(PatternBindingDecl *var) { iteratorVar = var; }
  PatternBindingDecl *getIteratorVar() const { return iteratorVar; }

  void setNextCall(Expr *next) { nextCall = next; }
  Expr *getNextCall() const { return nextCall; }

  void setElementExpr(OpaqueValueExpr *expr) { elementExpr = expr; }
  OpaqueValueExpr *getElementExpr() const { return elementExpr; }

  void setConvertElementExpr(Expr *expr) { convertElementExpr = expr; }
  Expr *getConvertElementExpr() const { return convertElementExpr; }

  void setSequenceConformance(Type type,
                              ProtocolConformanceRef conformance) {
    sequenceType = type;
    sequenceConformance = conformance;
  }
  Type getSequenceType() const { return sequenceType; }
  ProtocolConformanceRef getSequenceConformance() const {
    return sequenceConformance;
  }

  /// getForLoc - Retrieve the location of the 'for' keyword.
  SourceLoc getForLoc() const { return ForLoc; }

  /// getInLoc - Retrieve the location of the 'in' keyword.
  SourceLoc getInLoc() const { return InLoc; }

  /// getWhereLoc - Retrieve the location of the 'where' keyword.
  SourceLoc getWhereLoc() const { return WhereLoc; }

  SourceLoc getAwaitLoc() const { return AwaitLoc; }
  SourceLoc getTryLoc() const { return TryLoc; }
  
  /// getPattern - Retrieve the pattern describing the iteration variables.
  /// These variables will only be visible within the body of the loop.
  Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *p);
  
  Expr *getWhere() const { return WhereExpr; }
  void setWhere(Expr *W) { WhereExpr = W; }

  /// getSequence - Retrieve the Sequence whose elements will be visited
  /// by this foreach loop, as it was written in the source code and
  /// subsequently type-checked. To determine the semantic behavior of this
  /// expression to extract a range, use \c getRangeInit().
  Expr *getParsedSequence() const { return Sequence; }
  void setParsedSequence(Expr *S) { Sequence = S; }

  /// Type-checked version of the sequence or nullptr if this statement
  /// yet to be type-checked.
  Expr *getTypeCheckedSequence() const;

  /// getBody - Retrieve the body of the loop.
  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *B) { Body = B; }
  
  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(ForLoc); }
  SourceLoc getEndLoc() const { return Body->getEndLoc(); }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::ForEach;
  }
};

/// A pattern and an optional guard expression used in a 'case' statement.
class alignas(1 << PatternAlignInBits) CaseLabelItem {
  enum class Kind {
    /// A normal pattern
    Normal = 0,
    /// `default`
    Default,
  };

  llvm::PointerIntPair<Pattern *, 1, bool> CasePatternAndResolved;
  SourceLoc WhereLoc;
  llvm::PointerIntPair<Expr *, 1, Kind> GuardExprAndKind;

  CaseLabelItem(Kind kind, Pattern *casePattern, SourceLoc whereLoc,
                Expr *guardExpr)
    : CasePatternAndResolved(casePattern, false), WhereLoc(whereLoc),
      GuardExprAndKind(guardExpr, kind) {}

public:
  CaseLabelItem(const CaseLabelItem &) = default;

  CaseLabelItem(Pattern *casePattern, SourceLoc whereLoc, Expr *guardExpr)
    : CaseLabelItem(Kind::Normal, casePattern, whereLoc, guardExpr) {}
  explicit CaseLabelItem(Pattern *casePattern)
    : CaseLabelItem(casePattern, SourceLoc(), nullptr) {}

  static CaseLabelItem getDefault(AnyPattern *pattern,
                                  SourceLoc whereLoc,
                                  Expr *guardExpr) {
    assert(pattern);
    return CaseLabelItem(Kind::Default, reinterpret_cast<Pattern *>(pattern),
                         whereLoc, guardExpr);
  }
  static CaseLabelItem getDefault(AnyPattern *pattern) {
    return getDefault(pattern, SourceLoc(), nullptr);
  }

  SourceLoc getWhereLoc() const { return WhereLoc; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  Pattern *getPattern() {
    return CasePatternAndResolved.getPointer();
  }
  const Pattern *getPattern() const {
    return CasePatternAndResolved.getPointer();
  }
  bool isPatternResolved() const {
    return CasePatternAndResolved.getInt();
  }
  void setPattern(Pattern *CasePattern, bool resolved) {
    this->CasePatternAndResolved.setPointer(CasePattern);
    this->CasePatternAndResolved.setInt(resolved);
  }

  /// Return the guard expression if present, or null if the case label has
  /// no guard.
  Expr *getGuardExpr() { return GuardExprAndKind.getPointer(); }
  const Expr *getGuardExpr() const {
    return GuardExprAndKind.getPointer();
  }
  void setGuardExpr(Expr *e) { GuardExprAndKind.setPointer(e); }

  /// Returns true if this is syntactically a 'default' label.
  bool isDefault() const {
    return GuardExprAndKind.getInt() == Kind::Default;
  }

  bool isSyntacticallyExhaustive() const;
};

/// FallthroughStmt - The keyword "fallthrough".
class FallthroughStmt : public Stmt {
  SourceLoc Loc;
  DeclContext *DC;

  FallthroughStmt(SourceLoc Loc, DeclContext *DC,
                  std::optional<bool> implicit = std::nullopt)
      : Stmt(StmtKind::Fallthrough, getDefaultImplicitFlag(implicit, Loc)),
        Loc(Loc), DC(DC) {}
public:
  static FallthroughStmt *createParsed(SourceLoc Loc, DeclContext *DC);

  SourceLoc getLoc() const { return Loc; }

  SourceRange getSourceRange() const { return Loc; }

  DeclContext *getDeclContext() const { return DC; }
  void setDeclContext(DeclContext *newDC) { DC = newDC; }

  /// Get the CaseStmt block from which the fallthrough transfers control.
  /// Returns \c nullptr if the fallthrough is invalid.
  CaseStmt *getFallthroughSource() const;

  /// Get the CaseStmt block to which the fallthrough transfers control.
  /// Returns \c nullptr if the fallthrough is invalid.
  CaseStmt *getFallthroughDest() const;

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Fallthrough;
  }
};

enum CaseParentKind { Switch, DoCatch };

/// A 'case' or 'default' block of a switch statement, or a 'catch' clause of a
/// do-catch statement.  Only valid as the substatement of a SwitchStmt or
/// DoCatchStmt.  A case block begins either with one or more CaseLabelItems or
/// a single 'default' label.
///
/// Some examples:
/// \code
///   case 1:
///   case 2, 3:
///   case Foo(var x, var y) where x < y:
///   case 2 where foo(), 3 where bar():
///   default:
/// \endcode
///
class CaseStmt final
    : public Stmt,
      private llvm::TrailingObjects<CaseStmt, FallthroughStmt *,
                                    CaseLabelItem> {
  friend TrailingObjects;

  Stmt *ParentStmt = nullptr;
  SourceLoc UnknownAttrLoc;
  SourceLoc ItemIntroducerLoc;
  SourceLoc ItemTerminatorLoc;
  CaseParentKind ParentKind;

  llvm::PointerIntPair<BraceStmt *, 1, bool> BodyAndHasFallthrough;

  std::optional<MutableArrayRef<VarDecl *>> CaseBodyVariables;

  CaseStmt(CaseParentKind ParentKind, SourceLoc ItemIntroducerLoc,
           ArrayRef<CaseLabelItem> CaseLabelItems, SourceLoc UnknownAttrLoc,
           SourceLoc ItemTerminatorLoc, BraceStmt *Body,
           std::optional<MutableArrayRef<VarDecl *>> CaseBodyVariables,
           std::optional<bool> Implicit,
           NullablePtr<FallthroughStmt> fallthroughStmt);

public:
  /// Create a parsed 'case'/'default' for 'switch' statement.
  static CaseStmt *
  createParsedSwitchCase(ASTContext &C, SourceLoc ItemIntroducerLoc,
                         ArrayRef<CaseLabelItem> CaseLabelItems,
                         SourceLoc UnknownAttrLoc, SourceLoc ColonLoc,
                         BraceStmt *Body);

  /// Create a parsed 'catch' for 'do' statement.
  static CaseStmt *createParsedDoCatch(ASTContext &C, SourceLoc CatchLoc,
                                       ArrayRef<CaseLabelItem> CaseLabelItems,
                                       BraceStmt *Body);

  static CaseStmt *
  create(ASTContext &C, CaseParentKind ParentKind, SourceLoc ItemIntroducerLoc,
         ArrayRef<CaseLabelItem> CaseLabelItems, SourceLoc UnknownAttrLoc,
         SourceLoc ItemTerminatorLoc, BraceStmt *Body,
         std::optional<MutableArrayRef<VarDecl *>> CaseBodyVariables,
         std::optional<bool> Implicit = std::nullopt,
         NullablePtr<FallthroughStmt> fallthroughStmt = nullptr);

  CaseParentKind getParentKind() const { return ParentKind; }

  Stmt *getParentStmt() const { return ParentStmt; }
  void setParentStmt(Stmt *S) {
    assert(S && "Parent statement must be SwitchStmt or DoCatchStmt");
    assert((ParentKind == CaseParentKind::Switch && isa<SwitchStmt>(S)) ||
           (ParentKind == CaseParentKind::DoCatch && isa<DoCatchStmt>(S)));
    ParentStmt = S;
  }

  ArrayRef<CaseLabelItem> getCaseLabelItems() const {
    return {getTrailingObjects<CaseLabelItem>(), Bits.CaseStmt.NumPatterns};
  }

  MutableArrayRef<CaseLabelItem> getMutableCaseLabelItems() {
    return {getTrailingObjects<CaseLabelItem>(), Bits.CaseStmt.NumPatterns};
  }

  unsigned getNumCaseLabelItems() const { return Bits.CaseStmt.NumPatterns; }

  FallthroughStmt *getFallthroughStmt() const {
    return hasFallthroughDest() ? *getTrailingObjects<FallthroughStmt *>()
                                : nullptr;
  }

  NullablePtr<CaseStmt> getFallthroughDest() const {
    return const_cast<CaseStmt &>(*this).getFallthroughDest();
  }

  NullablePtr<CaseStmt> getFallthroughDest() {
    if (!hasFallthroughDest())
      return nullptr;
    return (*getTrailingObjects<FallthroughStmt *>())->getFallthroughDest();
  }

  bool hasFallthroughDest() const { return BodyAndHasFallthrough.getInt(); }

  BraceStmt *getBody() const { return BodyAndHasFallthrough.getPointer(); }
  void setBody(BraceStmt *body) { BodyAndHasFallthrough.setPointer(body); }

  /// True if the case block declares any patterns with local variable bindings.
  bool hasBoundDecls() const { return CaseBodyVariables.has_value(); }

  /// Get the source location of the 'case', 'default', or 'catch' of the first
  /// label.
  SourceLoc getLoc() const { return ItemIntroducerLoc; }

  SourceLoc getStartLoc() const {
    if (UnknownAttrLoc.isValid())
      return UnknownAttrLoc;
    if (ItemIntroducerLoc.isValid())
      return ItemIntroducerLoc;
    return getBody()->getStartLoc();
  }
  SourceLoc getEndLoc() const { return getBody()->getEndLoc(); }

  SourceLoc getItemTerminatorLoc() const { return ItemTerminatorLoc; }

  SourceRange getLabelItemsRange() const {
    switch (ParentKind) {
    case CaseParentKind::Switch:
      // The range extends from 'case' to the colon at the end.
      return ItemTerminatorLoc.isValid()
                 ? SourceRange(getLoc(), ItemTerminatorLoc)
                 : getSourceRange();
    case CaseParentKind::DoCatch: {
      // The range extends from 'catch' to the end of the last non-implicit
      // item.
      auto items = getCaseLabelItems();
      for (auto item = items.rbegin(), end = items.rend(); item != end;
           ++item) {
        auto itemEndLoc = item->getEndLoc();
        if (itemEndLoc.isValid())
          return SourceRange(getLoc(), itemEndLoc);
      }
      // Handle the 'catch {' case.
      return SourceRange(getLoc(), getLoc());
    }
    }
    llvm_unreachable("invalid parent kind");
  }

  bool isDefault() { return getCaseLabelItems()[0].isDefault(); }

  bool hasUnknownAttr() const {
    // Note: This representation doesn't allow for synthesized @unknown cases.
    // However, that's probably sensible; the purpose of @unknown is for
    // diagnosing otherwise-non-exhaustive switches, and the user can't edit
    // a synthesized case.
    return UnknownAttrLoc.isValid();
  }

  /// Return an ArrayRef containing the case body variables of this CaseStmt.
  ///
  /// Asserts if case body variables was not explicitly initialized. In contexts
  /// where one wants a non-asserting version, \see
  /// getCaseBodyVariablesOrEmptyArray.
  ArrayRef<VarDecl *> getCaseBodyVariables() const {
    ArrayRef<VarDecl *> a = *CaseBodyVariables;
    return a;
  }

  bool hasCaseBodyVariables() const { return CaseBodyVariables.has_value(); }

  /// Return an MutableArrayRef containing the case body variables of this
  /// CaseStmt.
  ///
  /// Asserts if case body variables was not explicitly initialized. In contexts
  /// where one wants a non-asserting version, \see
  /// getCaseBodyVariablesOrEmptyArray.
  MutableArrayRef<VarDecl *> getCaseBodyVariables() {
    return *CaseBodyVariables;
  }

  ArrayRef<VarDecl *> getCaseBodyVariablesOrEmptyArray() const {
    if (!CaseBodyVariables)
      return ArrayRef<VarDecl *>();
    ArrayRef<VarDecl *> a = *CaseBodyVariables;
    return a;
  }

  MutableArrayRef<VarDecl *> getCaseBodyVariablesOrEmptyArray() {
    if (!CaseBodyVariables)
      return MutableArrayRef<VarDecl *>();
    return *CaseBodyVariables;
  }

  /// Find the next case statement within the same 'switch' or 'do-catch',
  /// if there is one.
  CaseStmt *findNextCaseStmt() const;

  static bool classof(const Stmt *S) { return S->getKind() == StmtKind::Case; }

  size_t numTrailingObjects(OverloadToken<CaseLabelItem>) const {
    return getNumCaseLabelItems();
  }

  size_t numTrailingObjects(OverloadToken<FallthroughStmt *>) const {
    return hasFallthroughDest() ? 1 : 0;
  }
};

/// Switch statement.
class SwitchStmt final : public LabeledStmt,
    private llvm::TrailingObjects<SwitchStmt, ASTNode> {
  friend TrailingObjects;

  SourceLoc SwitchLoc, LBraceLoc, RBraceLoc;
  /// The location of the last token in the 'switch' statement. For valid
  /// 'switch' statements this is the same as \c RBraceLoc. If the '}' is
  /// missing this points to the last token before the '}' was expected.
  SourceLoc EndLoc;
  Expr *SubjectExpr;

  SwitchStmt(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc, Expr *SubjectExpr,
             SourceLoc LBraceLoc, unsigned CaseCount, SourceLoc RBraceLoc,
             SourceLoc EndLoc, std::optional<bool> implicit = std::nullopt)
      : LabeledStmt(StmtKind::Switch,
                    getDefaultImplicitFlag(implicit, SwitchLoc), LabelInfo),
        SwitchLoc(SwitchLoc), LBraceLoc(LBraceLoc), RBraceLoc(RBraceLoc),
        EndLoc(EndLoc), SubjectExpr(SubjectExpr) {
    Bits.SwitchStmt.CaseCount = CaseCount;
  }

public:
  /// Allocate a new SwitchStmt in the given ASTContext.
  static SwitchStmt *create(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc,
                            Expr *SubjectExpr,
                            SourceLoc LBraceLoc,
                            ArrayRef<ASTNode> Cases,
                            SourceLoc RBraceLoc,
                            SourceLoc EndLoc,
                            ASTContext &C);

  static SwitchStmt *createImplicit(LabeledStmtInfo LabelInfo,
                                    Expr *SubjectExpr, ArrayRef<ASTNode> Cases,
                                    ASTContext &C) {
    return SwitchStmt::create(LabelInfo, /*SwitchLoc=*/SourceLoc(), SubjectExpr,
                              /*LBraceLoc=*/SourceLoc(), Cases,
                              /*RBraceLoc=*/SourceLoc(), /*EndLoc=*/SourceLoc(),
                              C);
  }

  /// Get the source location of the 'switch' keyword.
  SourceLoc getSwitchLoc() const { return SwitchLoc; }
  /// Get the source location of the opening brace.
  SourceLoc getLBraceLoc() const { return LBraceLoc; }
  /// Get the source location of the closing brace.
  SourceLoc getRBraceLoc() const { return RBraceLoc; }
  
  SourceLoc getLoc() const { return SwitchLoc; }

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(SwitchLoc); }
  SourceLoc getEndLoc() const { return EndLoc; }

  /// Get the subject expression of the switch.
  Expr *getSubjectExpr() const { return SubjectExpr; }
  void setSubjectExpr(Expr *e) { SubjectExpr = e; }

  ArrayRef<ASTNode> getRawCases() const {
    return {getTrailingObjects<ASTNode>(), Bits.SwitchStmt.CaseCount};
  }

private:
  struct AsCaseStmtWithSkippingNonCaseStmts {
    AsCaseStmtWithSkippingNonCaseStmts() {}
    std::optional<CaseStmt *> operator()(const ASTNode &N) const {
      if (auto *CS = llvm::dyn_cast_or_null<CaseStmt>(N.dyn_cast<Stmt*>()))
        return CS;
      return std::nullopt;
    }
  };

public:
  using AsCaseStmtRange = OptionalTransformRange<ArrayRef<ASTNode>,
                            AsCaseStmtWithSkippingNonCaseStmts>;
  
  /// Get the list of case clauses.
  AsCaseStmtRange getCases() const {
    return AsCaseStmtRange(getRawCases(), AsCaseStmtWithSkippingNonCaseStmts());
  }

  /// Retrieve the complete set of branches for this switch statement.
  ArrayRef<Stmt *> getBranches(SmallVectorImpl<Stmt *> &scratch) const;

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Switch;
  }
};

/// DoCatchStmt - do statement with trailing 'catch' clauses.
class DoCatchStmt final
    : public LabeledStmt,
      private llvm::TrailingObjects<DoCatchStmt, CaseStmt *> {
  friend TrailingObjects;
  friend class ExplicitCaughtTypeRequest;

  DeclContext *DC;

  SourceLoc DoLoc;

  /// Location of the 'throws' token.
  SourceLoc ThrowsLoc;

  /// The error type that is being thrown.
  TypeLoc ThrownType;

  Stmt *Body;
  ThrownErrorDestination RethrowDest;

  DoCatchStmt(DeclContext *dc, LabeledStmtInfo labelInfo, SourceLoc doLoc,
              SourceLoc throwsLoc, TypeLoc thrownType, Stmt *body,
              ArrayRef<CaseStmt *> catches, std::optional<bool> implicit)
      : LabeledStmt(StmtKind::DoCatch, getDefaultImplicitFlag(implicit, doLoc),
                    labelInfo),
        DC(dc), DoLoc(doLoc), ThrowsLoc(throwsLoc), ThrownType(thrownType),
        Body(body) {
    Bits.DoCatchStmt.NumCatches = catches.size();
    std::uninitialized_copy(catches.begin(), catches.end(),
                            getTrailingObjects<CaseStmt *>());
    for (auto *catchStmt : getCatches())
      catchStmt->setParentStmt(this);
  }

public:
  static DoCatchStmt *create(DeclContext *dc, LabeledStmtInfo labelInfo,
                             SourceLoc doLoc, SourceLoc throwsLoc,
                             TypeLoc thrownType, Stmt *body,
                             ArrayRef<CaseStmt *> catches,
                             std::optional<bool> implicit = std::nullopt);

  DeclContext *getDeclContext() const { return DC; }

  SourceLoc getDoLoc() const { return DoLoc; }

  /// Retrieve the location of the 'throws' keyword, if present.
  SourceLoc getThrowsLoc() const { return ThrowsLoc; }

  SourceLoc getStartLoc() const { return getLabelLocOrKeywordLoc(DoLoc); }
  SourceLoc getEndLoc() const { return getCatches().back()->getEndLoc(); }

  /// Retrieves the type representation for the caught type.
  TypeRepr *getCaughtTypeRepr() const {
    return ThrownType.getTypeRepr();
  }

  // Get the explicitly-specified caught error type.
  Type getExplicitCaughtType() const;

  Stmt *getBody() const { return Body; }
  void setBody(Stmt *s) { Body = s; }

  ArrayRef<CaseStmt *> getCatches() const {
    return {getTrailingObjects<CaseStmt *>(), Bits.DoCatchStmt.NumCatches};
  }
  MutableArrayRef<CaseStmt *> getMutableCatches() {
    return {getTrailingObjects<CaseStmt *>(), Bits.DoCatchStmt.NumCatches};
  }

  /// Retrieve the complete set of branches for this do-catch statement.
  ArrayRef<Stmt *> getBranches(SmallVectorImpl<Stmt *> &scratch) const;

  /// Does this statement contain a syntactically exhaustive catch
  /// clause?
  ///
  /// Note that an exhaustive do/catch statement can still throw
  /// errors out of its catch block(s).
  bool isSyntacticallyExhaustive() const;

  // Determines the type of the error that is thrown out of the 'do' block
  // and caught by the various 'catch' clauses. If this the catch clauses
  // aren't exhausive, this is also the type of the error that is implicitly
  // rethrown.
  Type getCaughtErrorType() const;

  /// Retrieves the rethrown error and its conversion to the error type
  /// expected by the enclosing context.
  ThrownErrorDestination rethrows() const { return RethrowDest; }

  void setRethrows(ThrownErrorDestination rethrows) {
    assert(!RethrowDest);
    RethrowDest = rethrows;
  }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::DoCatch;
  }
};

/// BreakStmt - The "break" and "break label" statement.
class BreakStmt : public Stmt {
  SourceLoc Loc;
  Identifier TargetName; // Named target statement, if specified in the source.
  SourceLoc TargetLoc;
  DeclContext *DC;

public:
  BreakStmt(SourceLoc Loc, Identifier TargetName, SourceLoc TargetLoc,
            DeclContext *DC, std::optional<bool> implicit = std::nullopt)
      : Stmt(StmtKind::Break, getDefaultImplicitFlag(implicit, Loc)), Loc(Loc),
        TargetName(TargetName), TargetLoc(TargetLoc), DC(DC) {}

  SourceLoc getLoc() const { return Loc; }

  Identifier getTargetName() const { return TargetName; }
  void setTargetName(Identifier N) { TargetName = N; }
  SourceLoc getTargetLoc() const { return TargetLoc; }
  void setTargetLoc(SourceLoc L) { TargetLoc = L; }

  /// Retrieve the target statement being jumped out of.
  LabeledStmt *getTarget() const;

  SourceLoc getStartLoc() const { return Loc; }
  SourceLoc getEndLoc() const {
    return (TargetLoc.isValid() ? TargetLoc : Loc);
  }

  DeclContext *getDeclContext() const { return DC; }
  void setDeclContext(DeclContext *newDC) { DC = newDC; }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Break;
  }
};

/// ContinueStmt - The "continue" and "continue label" statement.
class ContinueStmt : public Stmt {
  SourceLoc Loc;
  Identifier TargetName; // Named target statement, if specified in the source.
  SourceLoc TargetLoc;
  DeclContext *DC;

public:
  ContinueStmt(SourceLoc Loc, Identifier TargetName, SourceLoc TargetLoc,
               DeclContext *DC, std::optional<bool> implicit = std::nullopt)
      : Stmt(StmtKind::Continue, getDefaultImplicitFlag(implicit, Loc)),
        Loc(Loc), TargetName(TargetName), TargetLoc(TargetLoc), DC(DC) {}

  Identifier getTargetName() const { return TargetName; }
  void setTargetName(Identifier N) { TargetName = N; }
  SourceLoc getTargetLoc() const { return TargetLoc; }
  void setTargetLoc(SourceLoc L) { TargetLoc = L; }

  /// Retrieve the target statement being jumped to.
  LabeledStmt *getTarget() const;
  
  SourceLoc getLoc() const { return Loc; }
  
  SourceLoc getStartLoc() const { return Loc; }
  SourceLoc getEndLoc() const {
    return (TargetLoc.isValid() ? TargetLoc : Loc);
  }

  DeclContext *getDeclContext() const { return DC; }
  void setDeclContext(DeclContext *newDC) { DC = newDC; }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Continue;
  }
};

/// FailStmt - A statement that indicates a failable, which is currently
/// spelled as "return nil" and can only be used within failable initializers.
class FailStmt : public Stmt {
  SourceLoc ReturnLoc;
  SourceLoc NilLoc;

public:
  FailStmt(SourceLoc returnLoc, SourceLoc nilLoc,
           std::optional<bool> implicit = std::nullopt)
      : Stmt(StmtKind::Fail, getDefaultImplicitFlag(implicit, returnLoc)),
        ReturnLoc(returnLoc), NilLoc(nilLoc) {}

  SourceRange getSourceRange() const {
    return SourceRange::combine(ReturnLoc, NilLoc);
  }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Fail;
  }
};

/// ThrowStmt - Throws an error.
class ThrowStmt : public Stmt {
  Expr *SubExpr;
  SourceLoc ThrowLoc;
  
public:
  explicit ThrowStmt(SourceLoc throwLoc, Expr *subExpr)
  : Stmt(StmtKind::Throw, /*Implicit=*/false),
    SubExpr(subExpr), ThrowLoc(throwLoc) {}

  SourceLoc getThrowLoc() const { return ThrowLoc; }

  SourceLoc getStartLoc() const { return ThrowLoc; }
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const {
    return SourceRange(ThrowLoc, getEndLoc());
  }
  
  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *subExpr) { SubExpr = subExpr; }
  
  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Throw;
  }
};

/// DiscardStmt - Consumes a noncopyable value and performs memberwise
/// destruction of unconsumed fields, without invoking its deinit. Only
/// supported form is "discard self".
class DiscardStmt : public Stmt {
  Expr *SubExpr;
  SourceLoc DiscardLoc;
  AbstractFunctionDecl *InnermostMethod;

public:
  explicit DiscardStmt(SourceLoc discardLoc, Expr *subExpr)
      : Stmt(StmtKind::Discard, /*Implicit=*/false),
        SubExpr(subExpr), DiscardLoc(discardLoc), InnermostMethod(nullptr) {}

  /// Location of the 'discard' keyword.
  SourceLoc getDiscardLoc() const { return DiscardLoc; }

  SourceLoc getStartLoc() const { return DiscardLoc; }
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const {
    return SourceRange(DiscardLoc, getEndLoc());
  }

  Expr *getSubExpr() const { return SubExpr; }
  void setSubExpr(Expr *subExpr) { SubExpr = subExpr; }

  /// Retrieves the saved innermost method / accessor decl in which this Stmt
  /// resides. Corresponds to \c DeclContext::getInnermostMethodContext.
  /// Must be saved with \c setInnermostMethodContext during typechecking.
  AbstractFunctionDecl *getInnermostMethodContext() { return InnermostMethod; }
  void setInnermostMethodContext(AbstractFunctionDecl *afd) {
    assert(afd); // shouldn't be clearing this
    InnermostMethod = afd;
  }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::Discard;
  }
};

/// PoundAssertStmt - Asserts that a condition is true, at compile time.
class PoundAssertStmt : public Stmt {
  SourceRange Range;
  Expr *Condition;
  StringRef Message;

 public:
  PoundAssertStmt(SourceRange Range, Expr *condition, StringRef message)
      : Stmt(StmtKind::PoundAssert, /*Implicit=*/false),
        Range(Range),
        Condition(condition),
        Message(message) {}

  SourceRange getSourceRange() const { return Range; }

  Expr *getCondition() const { return Condition; }
  StringRef getMessage() const { return Message; }

  void setCondition(Expr *condition) { Condition = condition; }

  static bool classof(const Stmt *S) {
    return S->getKind() == StmtKind::PoundAssert;
  }
};

SourceLoc extractNearestSourceLoc(const Stmt *stmt);

inline void simple_display(llvm::raw_ostream &out, const Stmt *S) {
  if (S)
    out << Stmt::getKindName(S->getKind());
  else
    out << "(null)";
}

} // end namespace swift

#endif // SWIFT_AST_STMT_H
