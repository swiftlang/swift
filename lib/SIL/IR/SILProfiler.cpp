//===--- SILProfiler.cpp - Instrumentation based profiling ----------------===//
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

#include "swift/SIL/SILProfiler.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/ProfileData/Coverage/CoverageMapping.h"
#include "llvm/ProfileData/Coverage/CoverageMappingWriter.h"
#include "llvm/ProfileData/InstrProf.h"

#include <forward_list>

#define DEBUG_TYPE "SILProfiler"

using namespace swift;

/// Check whether a root AST node should be profiled.
static bool shouldProfile(ASTNode N, SILDeclRef Constant) {
  // Do not profile AST nodes with invalid source locations.
  if (N.getStartLoc().isInvalid() || N.getEndLoc().isInvalid()) {
    LLVM_DEBUG(llvm::dbgs()
               << "Skipping ASTNode: invalid start/end locations\n");
    return false;
  }
  if (!Constant) {
    // We should only ever have a null SILDeclRef for top-level code, which is
    // always user-written, and should always be profiled.
    // FIXME: Once top-level code is unified under a single SILProfiler, this
    // case can be eliminated.
    assert(isa<TopLevelCodeDecl>(N.get<Decl *>()));
    return true;
  }

  // Do not profile AST nodes in unavailable contexts.
  auto *DC = Constant.getInnermostDeclContext();
  if (auto *D = DC->getInnermostDeclarationDeclContext()) {
    if (D->isSemanticallyUnavailable()) {
      LLVM_DEBUG(llvm::dbgs() << "Skipping ASTNode: unavailable context\n");
      return false;
    }
  }

  // Do not profile code that hasn't been written by the user.
  if (!Constant.hasUserWrittenCode()) {
    LLVM_DEBUG(llvm::dbgs() << "Skipping ASTNode: no user-written code\n");
    return false;
  }

  return true;
}

/// Get the DeclContext for the decl referenced by \p forDecl.
DeclContext *getProfilerContextForDecl(ASTNode N, SILDeclRef forDecl) {
  if (auto *D = N.dyn_cast<Decl *>())
    if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D))
      return TLCD;
  assert(!forDecl.isNull() && "Expected a nonnull SILDeclRef");
  if (auto *ACE = forDecl.getAbstractClosureExpr())
    return ACE;
  return forDecl.getDecl()->getDeclContext();
}

static Stmt *getProfilerStmtForCase(CaseStmt *caseStmt) {
  switch (caseStmt->getParentKind()) {
  case CaseParentKind::Switch:
    return caseStmt;
  case CaseParentKind::DoCatch:
    return caseStmt->getBody();
  }
  llvm_unreachable("invalid parent kind");
}

/// Check that the input AST has at least been type-checked.
LLVM_ATTRIBUTE_UNUSED
static bool hasASTBeenTypeChecked(ASTNode N, SILDeclRef forDecl) {
  DeclContext *DC = getProfilerContextForDecl(N, forDecl);
  SourceFile *SF = DC->getParentSourceFile();
  return !SF || SF->ASTStage >= SourceFile::TypeChecked;
}

/// Check whether a mapped AST node is valid for profiling.
static bool canCreateProfilerForAST(ASTNode N, SILDeclRef forDecl) {
  assert(hasASTBeenTypeChecked(N, forDecl) &&
         "Cannot use this AST for profiling");

  if (auto *D = N.dyn_cast<Decl *>()) {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D))
      return AFD->hasBody();

    if (isa<TopLevelCodeDecl>(D))
      return true;
  } else if (N.get<Expr *>()) {
    if (auto *closure = forDecl.getAbstractClosureExpr())
      return closure->hasBody();
    if (forDecl.isStoredPropertyInitializer() ||
        forDecl.isPropertyWrapperBackingInitializer())
      return true;
  }
  return false;
}

SILProfiler *SILProfiler::create(SILModule &M, ASTNode N, SILDeclRef Ref) {
  // If profiling isn't enabled, don't profile anything.
  const auto &Opts = M.getOptions();
  if (!Opts.GenerateProfile && Opts.UseProfile.empty())
    return nullptr;

  if (!shouldProfile(N, Ref))
    return nullptr;

  if (!canCreateProfilerForAST(N, Ref)) {
    N.dump(llvm::errs());
    llvm_unreachable("Invalid AST node for profiling");
  }

  auto *Buf = M.allocate<SILProfiler>(1);
  auto *SP =
      ::new (Buf) SILProfiler(M, N, Ref, Opts.EmitProfileCoverageMapping);
  SP->assignRegionCounters();
  return SP;
}

namespace {

/// Special logic for handling function visitation.
///
/// To avoid creating duplicate mappings, a function decl is only profiled if
/// it hasn't been reached via recursive walk.
///
/// Apply \p Func if the function can be visited.
template <typename F>
ASTWalker::PreWalkAction
visitFunctionDecl(ASTWalker &Walker, AbstractFunctionDecl *AFD, F Func) {
  if (Walker.Parent.isNull()) {
    Func();
    return ASTWalker::Action::Continue();
  }
  return ASTWalker::Action::SkipChildren();
}

/// Whether to walk the children of a given expression.
ASTWalker::PreWalkResult<Expr *>
shouldWalkIntoExpr(Expr *E, ASTWalker::ParentTy Parent, SILDeclRef Constant) {
  using Action = ASTWalker::Action;

  // Profiling for closures should be handled separately. Do not visit
  // closure expressions twice.
  if (isa<AbstractClosureExpr>(E)) {
    // A non-null parent means we have a closure child, which we will visit
    // separately. Even if the parent is null, don't walk into a closure if the
    // SILDeclRef is not for a closure, as it could be for a property
    // initializer instead.
    if (!Parent.isNull() || !Constant || !Constant.getAbstractClosureExpr())
      return Action::SkipChildren(E);
  }
  return Action::Continue(E);
}

/// Whether to skip visitation of an expression. The children may however still
/// be visited
bool shouldSkipExpr(Expr *E) {
  // Expressions with no location should be skipped.
  return E->getStartLoc().isInvalid() || E->getEndLoc().isInvalid();
}

/// Whether the children of a decl that isn't explicitly handled should be
/// walked.
static bool shouldWalkIntoUnhandledDecl(const Decl *D) {
  // We want to walk into the initializer for a pattern binding decl. This
  // allows us to map LazyInitializerExprs.
  return isa<PatternBindingDecl>(D);
}

/// An ASTWalker that maps ASTNodes to profiling counters.
struct MapRegionCounters : public ASTWalker {
  /// The SIL function being profiled.
  SILDeclRef Constant;

  /// The next counter value to assign.
  unsigned NextCounter = 0;

  /// The map of statements to counters.
  llvm::DenseMap<ASTNode, unsigned> &CounterMap;

  MapRegionCounters(SILDeclRef Constant,
                    llvm::DenseMap<ASTNode, unsigned> &CounterMap)
      : Constant(Constant), CounterMap(CounterMap) {}

  LazyInitializerWalking getLazyInitializerWalkingBehavior() override {
    // We want to walk lazy initializers present in the synthesized getter for
    // a lazy variable.
    return LazyInitializerWalking::InAccessor;
  }

  void mapRegion(ASTNode N) {
    CounterMap[N] = NextCounter;

    LLVM_DEBUG({
      llvm::dbgs() << "Assigned counter #" << NextCounter << " to: ";
      auto *E = N.dyn_cast<Expr *>();
      if (E)
        llvm::dbgs() << Expr::getKindName(E->getKind()) << "\n";
      auto *S = N.dyn_cast<Stmt *>();
      if (S)
        llvm::dbgs() << Stmt::getKindName(S->getKind()) << "\n";
    });

    ++NextCounter;
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      return visitFunctionDecl(*this, AFD, [&] { mapRegion(AFD->getBody()); });
    } else if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      mapRegion(TLCD->getBody());
      return Action::Continue();
    }
    return Action::VisitChildrenIf(shouldWalkIntoUnhandledDecl(D));
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    if (auto *IS = dyn_cast<IfStmt>(S)) {
      mapRegion(IS->getThenStmt());
    } else if (auto *US = dyn_cast<GuardStmt>(S)) {
      mapRegion(US->getBody());
    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      mapRegion(WS->getBody());
    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      mapRegion(RWS->getBody());
    } else if (auto *FES = dyn_cast<ForEachStmt>(S)) {
      mapRegion(FES->getBody());
    } else if (auto *CS = dyn_cast<CaseStmt>(S)) {
      mapRegion(getProfilerStmtForCase(CS));
    }
    return Action::Continue(S);
  }

  PreWalkAction walkToParameterListPre(ParameterList *PL) override {
    // We don't walk into parameter lists. Default arguments should be visited
    // directly.
    // FIXME: We don't yet profile default argument generators at all.
    return Action::SkipChildren();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (shouldSkipExpr(E))
      return shouldWalkIntoExpr(E, Parent, Constant);

    // If AST visitation begins with an expression, the counter map must be
    // empty. Set up a counter for the root.
    if (Parent.isNull()) {
      assert(CounterMap.empty() && "Mapped a region before visiting the root?");
      mapRegion(E);
    }

    if (auto *IE = dyn_cast<TernaryExpr>(E)) {
      mapRegion(IE->getThenExpr());
    }

    if (isa<LazyInitializerExpr>(E))
      mapRegion(E);

    return shouldWalkIntoExpr(E, Parent, Constant);
  }
};

struct CounterExprStorage;
using CounterAllocator = llvm::SpecificBumpPtrAllocator<CounterExprStorage>;

/// A node in an expression tree of counters.
class CounterExpr {
  enum class Kind { Node, Add, Sub, Zero };
  Kind K;
  ASTNode Node;
  const CounterExprStorage *Storage = nullptr;

  CounterExpr(Kind K) : K(K) {
    assert((K == Kind::Zero) && "only valid for Zero");
  }

  CounterExpr(Kind K, ASTNode Node) : K(K), Node(Node) {
    assert(K == Kind::Node && "only valid for Node");
  }

  CounterExpr(Kind K, const CounterExprStorage *Storage)
      : K(K), Storage(Storage) {
    assert((K == Kind::Add || K == Kind::Sub) && "only valid for operators");
  }

public:
  static CounterExpr Leaf(ASTNode Node) {
    return CounterExpr(Kind::Node, Node);
  }
  static CounterExpr Zero() {
    return CounterExpr(Kind::Zero);
  }

  static CounterExpr Add(CounterExpr LHS, CounterExpr RHS,
                         CounterAllocator &Alloc);
  static CounterExpr Sub(CounterExpr LHS, CounterExpr RHS,
                         CounterAllocator &Alloc);

  /// Returns true if this is a Zero node.
  bool isZero() const { return K == Kind::Zero; }

  /// For an addition or subtraction counter, retrieves the LHS counter.
  const CounterExpr &getLHS() const;

  /// For an addition or subtraction counter, retrieves the RHS counter.
  const CounterExpr &getRHS() const;

  /// Returns true if the counter is semantically a Zero node. This considers
  /// the simplified version of the counter that has eliminated redundant
  /// operations.
  bool isSemanticallyZero() const {
    // Run the counter through the counter builder to simplify it, using a dummy
    // mapping of unique counter indices for each node reference. The value of
    // the indices doesn't matter, but we need to ensure that e.g subtraction
    // of a node from itself cancels out.
    llvm::coverage::CounterExpressionBuilder Builder;
    llvm::DenseMap<ASTNode, unsigned> DummyIndices;
    unsigned LastIdx = 0;
    auto Counter = expand(Builder, [&](auto Node) {
      if (!DummyIndices.count(Node)) {
        DummyIndices[Node] = LastIdx;
        LastIdx += 1;
      }
      return DummyIndices[Node];
    });
    return Counter.isZero();
  }

  /// Expand this node into an llvm::coverage::Counter.
  ///
  /// Updates \c Builder with any expressions that are needed to represent this
  /// counter.
  llvm::coverage::Counter
  expand(llvm::coverage::CounterExpressionBuilder &Builder,
         llvm::function_ref<unsigned(ASTNode)> GetCounterIdx) const {
    switch (K) {
    case Kind::Zero:
      return llvm::coverage::Counter::getZero();
    case Kind::Node:
      return llvm::coverage::Counter::getCounter(GetCounterIdx(Node));
    case Kind::Add:
      return Builder.add(getLHS().expand(Builder, GetCounterIdx),
                         getRHS().expand(Builder, GetCounterIdx));
    case Kind::Sub:
      return Builder.subtract(getLHS().expand(Builder, GetCounterIdx),
                              getRHS().expand(Builder, GetCounterIdx));
    }

    llvm_unreachable("Unhandled Kind in switch.");
  }

  /// Expand this node into an llvm::coverage::Counter.
  ///
  /// Updates \c Builder with any expressions that are needed to represent this
  /// counter.
  llvm::coverage::Counter
  expand(llvm::coverage::CounterExpressionBuilder &Builder,
         const llvm::DenseMap<ASTNode, unsigned> &Counters) const {
    return expand(Builder, [&](auto Node) {
      auto Result = Counters.find(Node);
      assert(Result != Counters.end() && "Counter not found");
      return Result->second;
    });
  }

  void print(raw_ostream &OS) const {
    switch (K) {
    case Kind::Zero:
      OS << "zero";
      return;
    case Kind::Node:
      OS << "node(" << Node.getOpaqueValue() << ")";
      return;
    case Kind::Add:
    case Kind::Sub:
      getLHS().print(OS);
      OS << ' ' << ((K == Kind::Add) ? '+' : '-') << ' ';
      getRHS().print(OS);
      return;
    }
    llvm_unreachable("Unhandled Kind in switch.");
  }

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const { print(llvm::errs()); }
#endif
};

struct CounterExprStorage {
  CounterExpr LHS;
  CounterExpr RHS;
};

inline CounterExpr CounterExpr::Add(CounterExpr LHS, CounterExpr RHS,
                                    CounterAllocator &Alloc) {
  auto *Storage = Alloc.Allocate();
  Storage->LHS = LHS;
  Storage->RHS = RHS;
  return CounterExpr(Kind::Add, Storage);
}
inline CounterExpr CounterExpr::Sub(CounterExpr LHS, CounterExpr RHS,
                                    CounterAllocator &Alloc) {
  auto *Storage = Alloc.Allocate();
  Storage->LHS = LHS;
  Storage->RHS = RHS;
  return CounterExpr(Kind::Sub, Storage);
}

inline const CounterExpr &CounterExpr::getLHS() const {
  assert(Storage && "Counter does not have an LHS");
  return Storage->LHS;
}

inline const CounterExpr &CounterExpr::getRHS() const {
  assert(Storage && "Counter does not have an RHS");
  return Storage->RHS;
}

/// A region of source code that can be mapped to a counter.
class SourceMappingRegion {
  ASTNode Node;

  /// The counter for an incomplete region. Note we do not store counters
  /// for nodes, as we need to be able to fix them up after popping the regions.
  Optional<CounterExpr> Counter;

  /// The region's starting location.
  Optional<SourceLoc> StartLoc;

  /// The region's ending location.
  Optional<SourceLoc> EndLoc;

public:
  SourceMappingRegion(ASTNode Node, Optional<CounterExpr> Counter,
                      Optional<SourceLoc> StartLoc, Optional<SourceLoc> EndLoc)
      : Node(Node), Counter(std::move(Counter)), StartLoc(StartLoc),
        EndLoc(EndLoc) {
    assert((!StartLoc || StartLoc->isValid()) &&
           "Expected start location to be valid");
    assert((!EndLoc || EndLoc->isValid()) &&
           "Expected end location to be valid");
  }

  SourceMappingRegion(SourceMappingRegion &&Region) = default;
  SourceMappingRegion &operator=(SourceMappingRegion &&RHS) = default;

  ASTNode getNode() const { return Node; }

  CounterExpr
  getCounter(const llvm::DenseMap<ASTNode, CounterExpr> &NodeCounters) const {
    if (Counter)
      return *Counter;

    auto Iter = NodeCounters.find(Node);
    assert(Iter != NodeCounters.end() && "Must have counter for node");
    return Iter->second;
  }

  bool hasStartLoc() const { return StartLoc.hasValue(); }

  void setStartLoc(SourceLoc Loc) {
    assert(Loc.isValid());
    StartLoc = Loc;
  }

  const SourceLoc &getStartLoc() const {
    assert(StartLoc && "Region has no start location");
    return *StartLoc;
  }

  bool hasEndLoc() const { return EndLoc.hasValue(); }

  void setEndLoc(SourceLoc Loc) {
    assert(Loc.isValid());
    EndLoc = Loc;
  }

  const SourceLoc &getEndLoc() const {
    assert(EndLoc && "Region has no end location");
    return *EndLoc;
  }

  void print(llvm::raw_ostream &OS, const SourceManager &SM) const {
    OS << "[";
    if (hasStartLoc())
      getStartLoc().print(OS, SM);
    else
      OS << "?";
    OS << ", ";
    if (hasEndLoc())
      getEndLoc().print(OS, SM);
    else
      OS << "?";
    OS << "]";
  }
};

/// An ASTWalker that maps ASTNodes to profiling counters.
///
/// TODO: We ought to be able to leverage the CounterExprs from the
/// CoverageMapping walker to recompute the correct counter information
/// for this walker.
struct PGOMapping : public ASTWalker {
  /// The SIL function being profiled.
  SILDeclRef Constant;

  /// The counter indices for AST nodes.
  const llvm::DenseMap<ASTNode, unsigned> &CounterMap;

  /// The loaded counter data.
  const llvm::InstrProfRecord &LoadedCounts;

  /// The output map of statements to counters.
  llvm::DenseMap<ASTNode, ProfileCounter> &LoadedCounterMap;
  llvm::DenseMap<ASTNode, ASTNode> &CondToParentMap;

  PGOMapping(SILDeclRef Constant,
             const llvm::DenseMap<ASTNode, unsigned> &CounterMap,
             const llvm::InstrProfRecord &LoadedCounts,
             llvm::DenseMap<ASTNode, ProfileCounter> &LoadedCounterMap,
             llvm::DenseMap<ASTNode, ASTNode> &RegionCondToParentMap)
      : Constant(Constant), CounterMap(CounterMap), LoadedCounts(LoadedCounts),
        LoadedCounterMap(LoadedCounterMap),
        CondToParentMap(RegionCondToParentMap) {}

  /// Retrieve the counter index for a leaf node.
  unsigned getCounterIndex(ASTNode Node) const {
    auto result = CounterMap.find(Node);
    assert(result != CounterMap.end() && "Unmapped node?");
    return result->second;
  }

  unsigned getParentCounter() const {
    if (Parent.isNull())
      return 0;
    else if (Parent.getKind() == ASTWalker::ParentKind::Decl) {
      auto it = CounterMap.find(Parent.getAsDecl());
      return (it != CounterMap.end()) ? it->getSecond() : 0;
    } else if (Parent.getKind() == ASTWalker::ParentKind::Stmt) {
      auto it = CounterMap.find(Parent.getAsStmt());
      return (it != CounterMap.end()) ? it->getSecond() : 0;
    } else if (Parent.getKind() == ASTWalker::ParentKind::Expr) {
      auto it = CounterMap.find(Parent.getAsExpr());
      return (it != CounterMap.end()) ? it->getSecond() : 0;
    }
    return 0;
  }

  ProfileCounter subtract(ProfileCounter L, ProfileCounter R) {
    if (!L.hasValue() || !R.hasValue()) {
      return L;
    }
    uint64_t LV = L.getValue();
    uint64_t RV = R.getValue();
    assert(LV >= RV && "Invalid counter subtraction");
    return LV - RV;
  }

  /// Load the execution count corresponding to \p Node from a profile, if one
  /// is available.
  ProfileCounter loadExecutionCount(ASTNode Node) {
    if (!Node)
      return ProfileCounter();

    auto CounterIt = CounterMap.find(Node);
    assert(CounterIt != CounterMap.end() &&
           "region does not have an associated counter");

    unsigned CounterIndexForFunc = CounterIt->second;
    return LoadedCounts.Counts[CounterIndexForFunc];
  }

  /// Record the execution count for a leaf node.
  void setKnownExecutionCount(ASTNode Node) {
    LoadedCounterMap[Node] = loadExecutionCount(Node);
  }

  /// Record a computed execution count for a node.
  void setExecutionCount(ASTNode Node, ProfileCounter count) {
    LoadedCounterMap[Node] = count;
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      return visitFunctionDecl(*this, AFD, [&] {
        setKnownExecutionCount(AFD->getBody());
      });
    }
    if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      setKnownExecutionCount(TLCD->getBody());
      return Action::Continue();
    }
    return Action::VisitChildrenIf(shouldWalkIntoUnhandledDecl(D));
  }

  LazyInitializerWalking getLazyInitializerWalkingBehavior() override {
    // We want to walk lazy initializers present in the synthesized getter for
    // a lazy variable.
    return LazyInitializerWalking::InAccessor;
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    unsigned parent = getParentCounter();
    auto parentCount = LoadedCounts.Counts[parent];
    if (auto *IS = dyn_cast<IfStmt>(S)) {
      auto thenStmt = IS->getThenStmt();
      auto thenCount = loadExecutionCount(thenStmt);
      setExecutionCount(thenStmt, thenCount);
      if (auto elseStmt = IS->getElseStmt()) {
        auto count = parentCount;
        if (!parent) {
          auto thenVal = thenCount.getValue();
          for (auto pCount = getCounterIndex(thenStmt); pCount > 0; --pCount) {
            auto cCount = LoadedCounts.Counts[pCount];
            if (cCount > thenVal) {
              count = cCount;
              break;
            }
          }
        }
        setExecutionCount(elseStmt, subtract(count, thenCount));
        auto Cond = IS->getCond();
        for (const auto &elt : Cond) {
          if (elt.getKind() ==
              StmtConditionElement::ConditionKind::CK_PatternBinding) {
            CondToParentMap[elt.getInitializer()] = IS;
          }
        }
      }
    } else if (auto *GS = dyn_cast<GuardStmt>(S)) {
      auto guardBody = GS->getBody();
      auto guardCount = loadExecutionCount(guardBody);
      setExecutionCount(guardBody, guardCount);
      setExecutionCount(GS, subtract(parentCount, guardCount));
    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      setKnownExecutionCount(WS->getBody());
      setExecutionCount(WS, parentCount);
    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      setKnownExecutionCount(RWS->getBody());
      setExecutionCount(RWS, parentCount);
    } else if (auto *FES = dyn_cast<ForEachStmt>(S)) {
      setKnownExecutionCount(FES->getBody());
      setExecutionCount(FES, parentCount);
    } else if (auto *CS = dyn_cast<CaseStmt>(S)) {
      setKnownExecutionCount(getProfilerStmtForCase(CS));
    }
    return Action::Continue(S);
  }

  PreWalkAction walkToParameterListPre(ParameterList *PL) override {
    // We don't walk into parameter lists. Default arguments should be visited
    // directly.
    // FIXME: We don't yet profile default argument generators at all.
    return Action::SkipChildren();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (shouldSkipExpr(E))
      return shouldWalkIntoExpr(E, Parent, Constant);

    unsigned parent = getParentCounter();

    if (Parent.isNull())
      setKnownExecutionCount(E);

    if (auto *IE = dyn_cast<TernaryExpr>(E)) {
      auto thenExpr = IE->getThenExpr();
      auto thenCount = loadExecutionCount(thenExpr);
      setExecutionCount(thenExpr, thenCount);
      auto elseExpr = IE->getElseExpr();
      assert(elseExpr && "An if-expr must have an else subexpression");
      auto count = LoadedCounts.Counts[parent];
      if (!parent) {
        auto thenVal = thenCount.getValue();
        for (auto pCount = getCounterIndex(thenExpr); pCount > 0; --pCount) {
          auto cCount = LoadedCounts.Counts[pCount];
          if (cCount > thenVal) {
            count = cCount;
            break;
          }
        }
      }
      setExecutionCount(elseExpr, subtract(count, thenCount));
    }
    if (isa<LazyInitializerExpr>(E))
      setKnownExecutionCount(E);

    return shouldWalkIntoExpr(E, Parent, Constant);
  }
};

/// Produce coverage mapping information for a function. This involves taking
/// the counters computed by MapRegionCounters, and annotating the source with
/// regions that are defined in terms of those counters.
struct CoverageMapping : public ASTWalker {
private:
  const SourceManager &SM;

  /// The SIL function being profiled.
  SILDeclRef Constant;

  /// Allocator for counter expressions.
  CounterAllocator CounterAlloc;

  /// The map of statements to counter expressions.
  llvm::DenseMap<ASTNode, CounterExpr> CounterMap;

  /// The source mapping regions for this function.
  std::vector<SourceMappingRegion> SourceRegions;

  /// A stack of currently live regions.
  std::vector<SourceMappingRegion> RegionStack;

  /// A stack of active repeat-while loops.
  std::vector<RepeatWhileStmt *> RepeatWhileStack;

  /// A stack of active do-catch statements.
  std::vector<DoCatchStmt *> DoCatchStack;

  Optional<CounterExpr> ExitCounter;

  Stmt *ImplicitTopLevelBody = nullptr;

  /// Return true if \c Node has an associated counter.
  bool hasCounter(ASTNode Node) { return CounterMap.count(Node); }

  /// Return the region counter for \c Node.
  ///
  /// This should only be called on statements that have a dedicated counter.
  CounterExpr getCounter(ASTNode Node) {
    auto Iter = CounterMap.find(Node);
    assert(Iter != CounterMap.end() && "No counter found");
    return Iter->second;
  }

  /// Create a counter expression for \c Node and add it to the map.
  void assignCounter(ASTNode Node, CounterExpr Expr) {
    assert(Node && "Assigning counter expression to non-existent AST node");
    auto Res = CounterMap.insert({Node, Expr});

    // Overwrite an existing assignment.
    if (!Res.second)
      Res.first->second = std::move(Expr);
  }

  /// Create a counter expression referencing \c Node's own counter.
  CounterExpr assignCounter(ASTNode Node) {
    auto Counter = CounterExpr::Leaf(Node);
    assignCounter(Node, Counter);
    return Counter;
  }

  /// Add \c Expr to \c Node's counter.
  void addToCounter(ASTNode Node, CounterExpr Expr) {
    auto Counter = getCounter(Node);
    if (Counter.isZero()) {
      Counter = std::move(Expr);
    } else {
      Counter = CounterExpr::Add(Counter, std::move(Expr), CounterAlloc);
    }
    assignCounter(Node, Counter);
  }

  /// Subtract \c Expr from \c Node's counter.
  void subtractFromCounter(ASTNode Node, CounterExpr Expr) {
    auto Counter = getCounter(Node);
    assert(!Counter.isZero() && "Cannot create a negative counter");
    assignCounter(Node,
                  CounterExpr::Sub(Counter, std::move(Expr), CounterAlloc));
  }

  /// Return the current region's counter.
  CounterExpr getCurrentCounter() { return getRegion().getCounter(CounterMap); }

  /// Get the counter from the end of the most recent scope.
  CounterExpr getExitCounter() {
    assert(ExitCounter && "no exit counter available");
    return *ExitCounter;
  }

  /// Set the exit count so we can leave the scope related to \c Node
  ///
  /// Returns the delta of the count on entering \c Node and exiting, or null if
  /// there was no change.
  Optional<CounterExpr> setExitCount(ASTNode Node) {
    ExitCounter = getCurrentCounter();
    if (hasCounter(Node) && getRegion().getNode() != Node)
      return CounterExpr::Sub(getCounter(Node), *ExitCounter, CounterAlloc);
    return None;
  }

  /// Adjust the count for control flow when exiting a scope.
  void adjustForNonLocalExits(ASTNode Scope, Optional<CounterExpr> ControlFlowAdjust) {
    if (Parent.getAsDecl())
      return;

    Optional<CounterExpr> JumpsToLabel;
    Stmt *ParentStmt = Parent.getAsStmt();
    if (ParentStmt) {
      if (isa<DoCatchStmt>(ParentStmt))
        return;
      auto caseStmt = dyn_cast_or_null<CaseStmt>(ParentStmt);
      if (caseStmt && caseStmt->getParentKind() == CaseParentKind::DoCatch)
        return;
      if (auto *LS = dyn_cast<LabeledStmt>(ParentStmt))
        JumpsToLabel = getCounter(LS);
    }

    if (!ControlFlowAdjust && !JumpsToLabel)
      return;

    auto Count = getCurrentCounter();
    // Add the counts from jumps directly to the label (such as breaks)
    if (JumpsToLabel)
      Count = CounterExpr::Add(Count, *JumpsToLabel, CounterAlloc);
    // Now apply any adjustments for control flow.
    if (ControlFlowAdjust)
      Count = CounterExpr::Sub(Count, *ControlFlowAdjust, CounterAlloc);

    replaceCount(Count, getEndLoc(Scope));
  }

  /// Push a region covering \c Node onto the stack.
  void pushRegion(ASTNode Node) {
    // Note we don't store counters for nodes, as we need to be able to fix
    // them up later.
    RegionStack.emplace_back(Node, /*Counter*/ None, Node.getStartLoc(),
                             getEndLoc(Node));
    LLVM_DEBUG({
      llvm::dbgs() << "Pushed region: ";
      RegionStack.back().print(llvm::dbgs(), SM);
      llvm::dbgs() << "\n";
    });
  }

  /// Replace the current region at \p Start with a new counter. If \p Start is
  /// \c None, or the counter is semantically zero, an 'incomplete' region is
  /// formed, which is not recorded unless followed by additional AST nodes.
  void replaceCount(CounterExpr Counter, Optional<SourceLoc> Start) {
    // If the counter is semantically zero, form an 'incomplete' region with
    // no starting location. This prevents forming unreachable regions unless
    // there is a following statement or expression to extend the region.
    if (Start && Counter.isSemanticallyZero())
      Start = None;

    RegionStack.emplace_back(ASTNode(), Counter, Start, None);
  }

  /// Get the location for the end of the last token in \c Node.
  SourceLoc getEndLoc(ASTNode Node) {
    return Lexer::getLocForEndOfToken(SM, Node.getEndLoc());
  }

  /// Pop regions from the stack into the function's list of regions.
  ///
  /// Adds all regions from \c ParentNode to the top of the stack to the
  /// function's \c SourceRegions.
  void popRegions(ASTNode ParentNode) {
    auto I = RegionStack.begin(), E = RegionStack.end();
    while (I != E &&
           I->getNode().getOpaqueValue() != ParentNode.getOpaqueValue())
      ++I;
    assert(I != E && "parent not in stack");
    auto ParentIt = I;
    SourceLoc EndLoc = ParentIt->getEndLoc();

    unsigned FirstPoppedIndex = SourceRegions.size();
    (void)FirstPoppedIndex;
    SourceRegions.push_back(std::move(*I++));
    for (; I != E; ++I) {
      if (!I->hasStartLoc())
        continue;
      if (!I->hasEndLoc())
        I->setEndLoc(EndLoc);
      SourceRegions.push_back(std::move(*I));
    }

    LLVM_DEBUG({
      for (unsigned Idx = FirstPoppedIndex; Idx < SourceRegions.size(); ++Idx) {
        llvm::dbgs() << "Popped region: ";
        SourceRegions[Idx].print(llvm::dbgs(), SM);
        llvm::dbgs() << "\n";
      }
    });

    RegionStack.erase(ParentIt, E);
  }

  /// Return the currently active region.
  SourceMappingRegion &getRegion() {
    assert(!RegionStack.empty() && "statement has no region");
    return RegionStack.back();
  }

  /// Ensure that \c S is included in the current region.
  void extendRegion(ASTNode S) {
    SourceMappingRegion &Region = getRegion();
    SourceLoc StartLoc = S.getStartLoc();
    if (!Region.hasStartLoc())
      Region.setStartLoc(StartLoc);
  }

  /// Mark \c S as a terminator, starting a zero region.
  void terminateRegion(ASTNode S) {
    SourceMappingRegion &Region = getRegion();
    if (!Region.hasEndLoc())
      Region.setEndLoc(getEndLoc(S));
    replaceCount(CounterExpr::Zero(), /*Start*/ None);
  }

  Expr *getConditionNode(StmtCondition SC) {
    assert(!SC.empty() && "Empty condition");
    return SC.front().getBooleanOrNull();
  }

public:
  CoverageMapping(const SourceManager &SM, SILDeclRef Constant)
      : SM(SM), Constant(Constant) {}

  LazyInitializerWalking getLazyInitializerWalkingBehavior() override {
    // We want to walk lazy initializers present in the synthesized getter for
    // a lazy variable.
    return LazyInitializerWalking::InAccessor;
  }

  /// Generate the coverage counter mapping regions from collected
  /// source regions.
  SILCoverageMap *emitSourceRegions(
      SILModule &M, StringRef Name, StringRef PGOFuncName, uint64_t Hash,
      llvm::DenseMap<ASTNode, unsigned> &CounterIndices, StringRef Filename) {
    if (SourceRegions.empty())
      return nullptr;

    llvm::coverage::CounterExpressionBuilder Builder;
    std::vector<SILCoverageMap::MappedRegion> Regions;
    for (const auto &Region : SourceRegions) {
      assert(Region.hasStartLoc() && "invalid region");
      assert(Region.hasEndLoc() && "incomplete region");

      auto Start = SM.getLineAndColumnInBuffer(Region.getStartLoc());
      auto End = SM.getLineAndColumnInBuffer(Region.getEndLoc());
      assert(Start.first <= End.first && "region start and end out of order");

      auto Counter = Region.getCounter(CounterMap);
      Regions.emplace_back(Start.first, Start.second, End.first, End.second,
                           Counter.expand(Builder, CounterIndices));
    }
    return SILCoverageMap::create(M, Filename, Name, PGOFuncName, Hash, Regions,
                                  Builder.getExpressions());
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      return visitFunctionDecl(*this, AFD, [&] {
        assignCounter(AFD->getBody());
      });
    } else if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      assignCounter(TLCD->getBody());
      ImplicitTopLevelBody = TLCD->getBody();
      return Action::Continue();
    }
    return Action::VisitChildrenIf(shouldWalkIntoUnhandledDecl(D));
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    if (isa<TopLevelCodeDecl>(D))
      ImplicitTopLevelBody = nullptr;
    return Action::Continue();
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    if (S->isImplicit() && S != ImplicitTopLevelBody)
      return Action::Continue(S);

    // If we're in an 'incomplete' region, update it to include this node. This
    // ensures we only create the region if needed.
    if (!RegionStack.empty())
      extendRegion(S);

    if (auto *BS = dyn_cast<BraceStmt>(S)) {
      if (hasCounter(BS))
        pushRegion(BS);

    } else if (auto *IS = dyn_cast<IfStmt>(S)) {
      if (auto *Cond = getConditionNode(IS->getCond()))
        assignCounter(Cond, getCurrentCounter());

      // The counter for the if statement itself tracks the number of jumps to
      // it by break statements.
      assignCounter(IS, CounterExpr::Zero());

      // We emit a counter for the then block, and define the else block in
      // terms of it.
      auto ThenCounter = assignCounter(IS->getThenStmt());
      if (IS->getElseStmt()) {
        auto ElseCounter =
            CounterExpr::Sub(getCurrentCounter(), ThenCounter, CounterAlloc);
        assignCounter(IS->getElseStmt(), ElseCounter);
      }
    } else if (auto *GS = dyn_cast<GuardStmt>(S)) {
      assignCounter(GS, CounterExpr::Zero());
      assignCounter(GS->getBody());

    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      // The counter for the while statement itself tracks the number of jumps
      // to it by break and continue statements.
      assignCounter(WS, CounterExpr::Zero());

      if (auto *E = getConditionNode(WS->getCond()))
        assignCounter(E, getCurrentCounter());
      assignCounter(WS->getBody());

    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      // The counter for the while statement itself tracks the number of jumps
      // to it by break and continue statements.
      assignCounter(RWS, CounterExpr::Zero());

      auto BodyCounter = assignCounter(RWS->getBody());
      assignCounter(RWS->getCond(), BodyCounter);
      RepeatWhileStack.push_back(RWS);

    } else if (auto *FES = dyn_cast<ForEachStmt>(S)) {
      // The counter for the for statement itself tracks the number of jumps
      // to it by break and continue statements.
      assignCounter(FES, CounterExpr::Zero());
      assignCounter(FES->getBody());

    } else if (auto *SS = dyn_cast<SwitchStmt>(S)) {
      // The counter for the switch statement itself tracks the number of jumps
      // to it by break statements, including the implicit breaks at the end of
      // cases.
      assignCounter(SS, CounterExpr::Zero());

      assignCounter(SS->getSubjectExpr(), getCurrentCounter());

      // Assign counters for cases so they're available for fallthrough.
      for (CaseStmt *Case : SS->getCases())
        assignCounter(Case);

    } else if (auto caseStmt = dyn_cast<CaseStmt>(S)) {
      if (caseStmt->getParentKind() == CaseParentKind::Switch)
        pushRegion(S);
    } else if (auto *DS = dyn_cast<DoStmt>(S)) {
      // The counter for the do statement itself tracks the number of jumps
      // to it by break statements.
      assignCounter(DS, CounterExpr::Zero());

      assignCounter(DS->getBody(), getCurrentCounter());

    } else if (auto *DCS = dyn_cast<DoCatchStmt>(S)) {
      // The do-catch body is visited the same number of times as its parent.
      assignCounter(DCS->getBody(), getCurrentCounter());

      for (CaseStmt *Catch : DCS->getCatches())
        assignCounter(Catch->getBody());

      // Initialize the exit count of the do-catch to the entry count, then
      // subtract off non-local exits as they are visited.
      assignCounter(DCS, getCurrentCounter());
      DoCatchStack.push_back(DCS);
    }
    return Action::Continue(S);
  }

  PostWalkResult<Stmt *> walkToStmtPost(Stmt *S) override {
    if (S->isImplicit() && S != ImplicitTopLevelBody)
      return Action::Continue(S);

    if (isa<BraceStmt>(S)) {
      if (hasCounter(S)) {
        auto Adjust = setExitCount(S);
        popRegions(S);
        adjustForNonLocalExits(S, Adjust);
      }

    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      // Update the condition with the backedge count.
      if (auto *E = getConditionNode(WS->getCond()))
        addToCounter(E, getExitCounter());

    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      assert(RepeatWhileStack.back() == RWS && "Malformed repeat-while stack");
      (void)RWS;
      RepeatWhileStack.pop_back();

    } else if (auto *CS = dyn_cast<ContinueStmt>(S)) {
      // Continues create extra backedges, add them to the appropriate counters.
      if (!isa<RepeatWhileStmt>(CS->getTarget()))
        addToCounter(CS->getTarget(), getCurrentCounter());
      if (auto *WS = dyn_cast<WhileStmt>(CS->getTarget())) {
        if (auto *E = getConditionNode(WS->getCond()))
          addToCounter(E, getCurrentCounter());
      }
      terminateRegion(S);

    } else if (auto *BS = dyn_cast<BreakStmt>(S)) {
      // When we break from a loop, we need to adjust the exit count.
      Stmt *BreakTarget = BS->getTarget();
      if (auto *RWS = dyn_cast<RepeatWhileStmt>(BreakTarget)) {
        subtractFromCounter(RWS->getCond(), getCurrentCounter());
      } else {
        // Update the exit counter for the target.
        addToCounter(BS->getTarget(), getCurrentCounter());
      }

      // The break also affects the exit counts of active do-catch statements.
      for (auto *DCS : DoCatchStack)
        subtractFromCounter(DCS, getCurrentCounter());

      terminateRegion(S);

    } else if (auto *FS = dyn_cast<FallthroughStmt>(S)) {
      addToCounter(FS->getFallthroughDest(), getCurrentCounter());
      terminateRegion(S);

    } else if (isa<SwitchStmt>(S)) {
      replaceCount(getCounter(S), getEndLoc(S));

    } else if (auto caseStmt = dyn_cast<CaseStmt>(S)) {
      if (caseStmt->getParentKind() == CaseParentKind::Switch) {
        // The end of a case block is an implicit break, update the exit
        // counter to reflect this.
        addToCounter(caseStmt->getParentStmt(), getCurrentCounter());
        popRegions(S);
      }
    } else if (auto *DCS = dyn_cast<DoCatchStmt>(S)) {
      assert(DoCatchStack.back() == DCS && "Malformed do-catch stack");
      DoCatchStack.pop_back();
      replaceCount(getCounter(S), getEndLoc(S));

    } else if (isa<ReturnStmt>(S) || isa<FailStmt>(S) || isa<ThrowStmt>(S)) {
      // When we return, adjust loop condition counts and do-catch exit counts
      // to reflect the early exit.
      if (isa<ReturnStmt>(S) || isa<FailStmt>(S)) {
        for (auto *RWS : RepeatWhileStack)
          subtractFromCounter(RWS->getCond(), getCurrentCounter());
        for (auto *DCS : DoCatchStack)
          subtractFromCounter(DCS, getCurrentCounter());
      }

      terminateRegion(S);
    }
    return Action::Continue(S);
  }

  PreWalkAction walkToParameterListPre(ParameterList *PL) override {
    // We don't walk into parameter lists. Default arguments should be visited
    // directly.
    // FIXME: We don't yet generate coverage for default argument generators at
    // all. This is inconsistent with property initializers, which are
    // effectively default values too. Seems like coverage doesn't offer much
    // benefit in these cases, as they're unlikely to have side effects, and
    // the values can be exercized explicitly, but we should probably at least
    // have a consistent behavior for both no matter what we choose here.
    return Action::SkipChildren();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (shouldSkipExpr(E))
      return shouldWalkIntoExpr(E, Parent, Constant);

    // If we're in an 'incomplete' region, update it to include this node. This
    // ensures we only create the region if needed.
    if (!RegionStack.empty())
      extendRegion(E);

    // If AST visitation begins with an expression, the region stack must be
    // empty. Set up a region for the root.
    if (Parent.isNull()) {
      assert(RegionStack.empty() &&
             "Mapped a region before visiting the root?");
      assignCounter(E);
    }

    if (isa<LazyInitializerExpr>(E))
      assignCounter(E);

    if (hasCounter(E))
      pushRegion(E);

    assert(!RegionStack.empty() && "Must be within a region");

    if (auto *IE = dyn_cast<TernaryExpr>(E)) {
      auto ThenCounter = assignCounter(IE->getThenExpr());
      auto ElseCounter =
          CounterExpr::Sub(getCurrentCounter(), ThenCounter, CounterAlloc);
      assignCounter(IE->getElseExpr(), ElseCounter);
    }
    auto WalkResult = shouldWalkIntoExpr(E, Parent, Constant);
    if (WalkResult.Action.Action == PreWalkAction::SkipChildren) {
      // We need to manually pop the region here as the ASTWalker won't call
      // the post-visitation.
      // FIXME: The ASTWalker should do a post-visit.
      if (hasCounter(E))
        popRegions(E);
    }
    return WalkResult;
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    if (shouldSkipExpr(E))
      return Action::Continue(E);

    if (hasCounter(E))
      popRegions(E);

    return Action::Continue(E);
  }
};

} // end anonymous namespace

static llvm::GlobalValue::LinkageTypes
getEquivalentPGOLinkage(FormalLinkage Linkage) {
  switch (Linkage) {
  case FormalLinkage::PublicUnique:
  case FormalLinkage::PublicNonUnique:
    return llvm::GlobalValue::ExternalLinkage;

  case FormalLinkage::HiddenUnique:
  case FormalLinkage::Private:
    return llvm::GlobalValue::PrivateLinkage;
  }

  llvm_unreachable("Unhandled FormalLinkage in switch.");
}

static StringRef getCurrentFileName(ASTNode N, SILDeclRef forDecl) {
  DeclContext *Ctx = getProfilerContextForDecl(N, forDecl);
  if (auto *ParentFile = Ctx->getParentSourceFile())
    return ParentFile->getFilename();
  return {};
}

void SILProfiler::assignRegionCounters() {
  const auto &SM = M.getASTContext().SourceMgr;

  CurrentFileName = getCurrentFileName(Root, forDecl);

  MapRegionCounters Mapper(forDecl, RegionCounterMap);

  std::string CurrentFuncName;
  FormalLinkage CurrentFuncLinkage;
  if (auto *D = Root.dyn_cast<Decl *>()) {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      CurrentFuncName = forDecl.mangle();
      CurrentFuncLinkage = getDeclLinkage(AFD);
    } else {
      auto *TLCD = cast<TopLevelCodeDecl>(D);
      llvm::raw_string_ostream OS{CurrentFuncName};
      OS << "__tlcd_";
      TLCD->getStartLoc().printLineAndColumn(OS, SM);
      CurrentFuncLinkage = FormalLinkage::HiddenUnique;
    }
  } else {
    CurrentFuncName = forDecl.mangle();
    CurrentFuncLinkage = FormalLinkage::HiddenUnique;
  }

  PGOFuncName = llvm::getPGOFuncName(
      CurrentFuncName, getEquivalentPGOLinkage(CurrentFuncLinkage),
      CurrentFileName);

  assert((!CurrentFuncName.empty() && !PGOFuncName.empty()) &&
         "Expected covered region to be named");

  LLVM_DEBUG(llvm::dbgs() << "Assigning counters to: " << CurrentFuncName
                          << "\n");
  Root.walk(Mapper);

  NumRegionCounters = Mapper.NextCounter;
  // TODO: Mapper needs to calculate a function hash as it goes.
  PGOFuncHash = 0x0;

  if (EmitCoverageMapping) {
    CoverageMapping Coverage(SM, forDecl);
    Root.walk(Coverage);
    CovMap =
        Coverage.emitSourceRegions(M, CurrentFuncName, PGOFuncName, PGOFuncHash,
                                   RegionCounterMap, CurrentFileName);
  }

  if (llvm::IndexedInstrProfReader *IPR = M.getPGOReader()) {
    auto LoadedCounts = IPR->getInstrProfRecord(PGOFuncName, PGOFuncHash);
    if (auto E = LoadedCounts.takeError()) {
      llvm::handleAllErrors(std::move(E), [](const llvm::InstrProfError &Err) {
        Err.log(llvm::dbgs());
        return;
      });
      llvm::dbgs() << PGOFuncName << "\n";
      return;
    }
    PGOMapping pgoMapper(forDecl, RegionCounterMap, LoadedCounts.get(),
                         RegionLoadedCounterMap, RegionCondToParentMap);
    Root.walk(pgoMapper);
  }
}

ProfileCounter SILProfiler::getExecutionCount(ASTNode Node) {
  if (!Node || !M.getPGOReader() || !hasRegionCounters()) {
    return ProfileCounter();
  }
  auto it = RegionLoadedCounterMap.find(Node);
  if (it == RegionLoadedCounterMap.end()) {
    return ProfileCounter();
  }
  return it->getSecond();
}

Optional<ASTNode> SILProfiler::getPGOParent(ASTNode Node) {
  if (!Node || !M.getPGOReader() || !hasRegionCounters()) {
    return None;
  }
  auto it = RegionCondToParentMap.find(Node);
  if (it == RegionCondToParentMap.end()) {
    return None;
  }
  return it->getSecond();
}
