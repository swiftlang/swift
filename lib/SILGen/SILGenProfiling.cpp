//===--- SILGenProfiling.cpp - Instrumentation based profiling ------------===//
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

#include "SILGenProfiling.h"
#include "SILGen.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTWalker.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Parse/Lexer.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/ProfileData/CoverageMapping.h"
#include "llvm/ProfileData/CoverageMappingWriter.h"

#include <forward_list>

using namespace swift;
using namespace Lowering;

ProfilerRAII::ProfilerRAII(SILGenModule &SGM, AbstractFunctionDecl *D)
    : SGM(SGM) {
  const auto &Opts = SGM.M.getOptions();
  if (!Opts.GenerateProfile)
    return;
  SGM.Profiler =
      llvm::make_unique<SILGenProfiling>(SGM, Opts.EmitProfileCoverageMapping);
  SGM.Profiler->assignRegionCounters(D);
}

ProfilerRAII::~ProfilerRAII() { SGM.Profiler = nullptr; }

namespace {

/// An ASTWalker that maps ASTNodes to profiling counters.
struct MapRegionCounters : public ASTWalker {
  /// The next counter value to assign.
  unsigned NextCounter;

  /// The map of statements to counters.
  llvm::DenseMap<ASTNode, unsigned> &CounterMap;

  MapRegionCounters(llvm::DenseMap<ASTNode, unsigned> &CounterMap)
      : NextCounter(0), CounterMap(CounterMap) {}

  bool walkToDeclPre(Decl *D) override {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D))
      CounterMap[AFD->getBody()] = NextCounter++;
    return true;
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (auto *IS = dyn_cast<IfStmt>(S)) {
      CounterMap[IS->getThenStmt()] = NextCounter++;
    } else if (auto *US = dyn_cast<GuardStmt>(S)) {
      CounterMap[US->getBody()] = NextCounter++;
    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      CounterMap[WS->getBody()] = NextCounter++;
    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      CounterMap[RWS->getBody()] = NextCounter++;
    } else if (auto *FS = dyn_cast<ForStmt>(S)) {
      CounterMap[FS->getBody()] = NextCounter++;
    } else if (auto *FES = dyn_cast<ForEachStmt>(S)) {
      CounterMap[FES->getBody()] = NextCounter++;
    } else if (auto *SS = dyn_cast<SwitchStmt>(S)) {
      CounterMap[SS] = NextCounter++;
    } else if (auto *CS = dyn_cast<CaseStmt>(S)) {
      CounterMap[CS] = NextCounter++;
    } else if (auto *DCS = dyn_cast<DoCatchStmt>(S)) {
      CounterMap[DCS] = NextCounter++;
    } else if (auto *CS = dyn_cast<CatchStmt>(S)) {
      CounterMap[CS->getBody()] = NextCounter++;
    }
    return {true, S};
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    switch (E->getKind()) {
    default:
      break;
    case ExprKind::AutoClosure:
    case ExprKind::Closure:
    case ExprKind::If:
      CounterMap[E] = NextCounter++;
      break;
    }
    return {true, E};
  }
};

/// A node in an expression tree of counters.
class CounterExpr {
  enum class Kind { Node, Add, Sub, Zero, Ref };
  Kind K;
  ASTNode Node;
  const CounterExpr *LHS;
  const CounterExpr *RHS;

  CounterExpr(Kind K) : K(K) {
    assert((K == Kind::Zero) && "only valid for Zero");
  }

  CounterExpr(Kind K, ASTNode Node) : K(K), Node(Node) {
    assert(K == Kind::Node && "only valid for Node");
  }

  CounterExpr(Kind K, const CounterExpr &LHS)
      : K(K), LHS(&LHS) {
    assert((K == Kind::Ref) && "only valid for Ref");
  }

  CounterExpr(Kind K, const CounterExpr &LHS, const CounterExpr &RHS)
      : K(K), LHS(&LHS), RHS(&RHS) {
    assert((K == Kind::Add || K == Kind::Sub) && "only valid for operators");
  }

public:
  // Move only.
  CounterExpr(const CounterExpr &) = delete;
  void operator=(const CounterExpr &) = delete;
  CounterExpr(CounterExpr &&Other) = default;
  CounterExpr &operator=(CounterExpr &&RHS) = default;

  static CounterExpr Leaf(ASTNode Node) {
    return CounterExpr(Kind::Node, Node);
  }
  static CounterExpr Add(const CounterExpr &LHS, const CounterExpr &RHS) {
    return CounterExpr(Kind::Add, LHS, RHS);
  }
  static CounterExpr Sub(const CounterExpr &LHS, const CounterExpr &RHS) {
    return CounterExpr(Kind::Sub, LHS, RHS);
  }
  static CounterExpr Zero() { return CounterExpr(Kind::Zero); }
  static CounterExpr Ref(const CounterExpr &LHS) {
    return CounterExpr(Kind::Ref, LHS);
  }

  /// Return the referenced node, or null if this is not a Ref type.
  const CounterExpr *getReferencedNode() const {
    return K == Kind::Ref ? LHS : nullptr;
  }

  /// Returns true if this is a Zero node.
  bool isZero() const { return K == Kind::Zero; }

  /// Expand this node into an llvm::coverage::Counter.
  ///
  /// Updates \c Builder with any expressions that are needed to represent this
  /// counter.
  llvm::coverage::Counter
  expand(llvm::coverage::CounterExpressionBuilder &Builder,
         llvm::DenseMap<ASTNode, unsigned> &Counters) const {
    switch (K) {
    case Kind::Zero:
      return llvm::coverage::Counter::getZero();
    case Kind::Node:
      return llvm::coverage::Counter::getCounter(Counters[Node]);
    case Kind::Add:
      return Builder.add(LHS->expand(Builder, Counters),
                         RHS->expand(Builder, Counters));
    case Kind::Sub:
      return Builder.subtract(LHS->expand(Builder, Counters),
                              RHS->expand(Builder, Counters));
    case Kind::Ref:
      return LHS->expand(Builder, Counters);
    }
  }
};

/// \brief A region of source code that can be mapped to a counter.
class SourceMappingRegion {
  ASTNode Node;

  CounterExpr *Count;

  /// \brief The region's starting location.
  Optional<SourceLoc> StartLoc;

  /// \brief The region's ending location.
  Optional<SourceLoc> EndLoc;

public:
  SourceMappingRegion(ASTNode Node, CounterExpr &Count,
                      Optional<SourceLoc> StartLoc, Optional<SourceLoc> EndLoc)
      : Node(Node), Count(&Count), StartLoc(StartLoc), EndLoc(EndLoc) {}

  SourceMappingRegion(SourceMappingRegion &&Region) = default;
  SourceMappingRegion &operator=(SourceMappingRegion &&RHS) = default;

  ASTNode getNode() const { return Node; }

  CounterExpr &getCounter() const { return *Count; }

  bool hasStartLoc() const { return StartLoc.hasValue(); }

  void setStartLoc(SourceLoc Loc) { StartLoc = Loc; }

  const SourceLoc &getStartLoc() const {
    assert(StartLoc && "Region has no start location");
    return *StartLoc;
  }

  bool hasEndLoc() const { return EndLoc.hasValue(); }

  void setEndLoc(SourceLoc Loc) { EndLoc = Loc; }

  const SourceLoc &getEndLoc() const {
    assert(EndLoc && "Region has no end location");
    return *EndLoc;
  }
};

struct CoverageMapping : public ASTWalker {
private:
  const SourceManager &SM;

  /// \brief Storage for counter expressions.
  std::forward_list<CounterExpr> Exprs;

  /// \brief The map of statements to counter expressions.
  llvm::DenseMap<ASTNode, CounterExpr *> CounterMap;

  /// \brief The source mapping regions for this function.
  std::vector<SourceMappingRegion> SourceRegions;

  /// \brief A stack of currently live regions.
  std::vector<SourceMappingRegion> RegionStack;

  CounterExpr *ExitCounter;

  /// \brief Return true if \c Node has an associated counter.
  bool hasCounter(ASTNode Node) { return CounterMap.count(Node); }

  /// \brief Return the region counter for \c Node.
  ///
  /// This should only be called on statements that have a dedicated counter.
  CounterExpr &getCounter(ASTNode Node) {
    assert(CounterMap.count(Node) && "No counter found");
    return *CounterMap[Node];
  }

  /// \brief Create a counter expression.
  CounterExpr &createCounter(CounterExpr &&Expr) {
    Exprs.push_front(std::move(Expr));
    return Exprs.front();
  }

  /// \brief Create a counter expression for \c Node and add it to the map.
  CounterExpr &assignCounter(ASTNode Node, CounterExpr &&Expr) {
    CounterExpr &Result = createCounter(std::move(Expr));
    CounterMap[Node] = &Result;
    return Result;
  }

  /// \brief Create a counter expression referencing \c Node's own counter.
  CounterExpr &assignCounter(ASTNode Node) {
    return assignCounter(Node, CounterExpr::Leaf(Node));
  }

  /// \brief Add \c Expr to \c Node's counter.
  void addToCounter(ASTNode Node, CounterExpr &Expr) {
    CounterExpr &Counter = getCounter(Node);
    if (const CounterExpr *ReferencedCounter = Counter.getReferencedNode())
      Counter = CounterExpr::Add(*ReferencedCounter, Expr);
    else if (Counter.isZero())
      Counter = CounterExpr::Ref(Expr);
    else
      Counter = CounterExpr::Add(createCounter(std::move(Counter)), Expr);
  }

  /// \brief Return the current region's counter.
  CounterExpr &getCurrentCounter() { return getRegion().getCounter(); }

  /// \brief Get the counter from the end of the most recent scope.
  CounterExpr &getExitCounter() {
    assert(ExitCounter && "no exit counter available");
    return *ExitCounter;
  }

  /// \brief Set the exit count so we can leave the scope related to \c Node
  ///
  /// Returns the delta of the count on entering \c Node and exiting, or null if
  /// there was no change.
  CounterExpr *setExitCount(ASTNode Node) {
    ExitCounter = &getCurrentCounter();
    if (hasCounter(Node) && ExitCounter != &getCounter(Node))
      return &createCounter(CounterExpr::Sub(getCounter(Node), *ExitCounter));
    return nullptr;
  }

  /// \brief Adjust the count for control flow when exiting a scope.
  void adjustForNonLocalExits(ASTNode Scope, CounterExpr *ControlFlowAdjust) {
    if (Parent.getAsDecl())
      return;

    CounterExpr *JumpsToLabel = nullptr;
    Stmt *ParentStmt = Parent.getAsStmt();
    if (ParentStmt) {
      if (isa<DoCatchStmt>(ParentStmt) || isa<CatchStmt>(ParentStmt))
        return;
      if (auto *LS = dyn_cast<LabeledStmt>(ParentStmt))
        JumpsToLabel = &getCounter(LS);
    }

    if (!ControlFlowAdjust && !JumpsToLabel)
      return;

    CounterExpr *Count = &getCurrentCounter();
    // Add the counts from jumps directly to the label (such as breaks)
    if (JumpsToLabel)
      Count = &createCounter(CounterExpr::Add(*Count, *JumpsToLabel));
    // Now apply any adjustments for control flow.
    if (ControlFlowAdjust)
      Count = &createCounter(CounterExpr::Sub(*Count, *ControlFlowAdjust));

    //RegionStack.emplace_back(ASTNode(), *Count, getEndLoc(Scope), None);
    RegionStack.emplace_back(ASTNode(), *Count, getEndLoc(Scope), None);
  }

  /// \brief Push a region covering \c Node onto the stack.
  void pushRegion(ASTNode Node) {
    RegionStack.emplace_back(Node, getCounter(Node), Node.getStartLoc(),
                             getEndLoc(Node));
  }

  /// \brief Replace the current region's count by pushing an incomplete region.
  void replaceCount(CounterExpr &&Expr, Optional<SourceLoc> Start = None) {
    CounterExpr &Counter = createCounter(std::move(Expr));
    RegionStack.emplace_back(ASTNode(), Counter, Start, None);
  }

  /// \brief Get the location for the end of the last token in \c Node.
  SourceLoc getEndLoc(ASTNode Node) {
    return Lexer::getLocForEndOfToken(SM, Node.getEndLoc());
  }

  /// \brief Pop regions from the stack into the function's list of regions.
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

    SourceRegions.push_back(std::move(*I++));
    for (; I != E; ++I) {
      if (!I->hasStartLoc())
        continue;
      if (!I->hasEndLoc())
        I->setEndLoc(EndLoc);
      SourceRegions.push_back(std::move(*I));
    }

    RegionStack.erase(ParentIt, E);
  }

  /// \brief Return the currently active region.
  SourceMappingRegion &getRegion() {
    assert(!RegionStack.empty() && "statement has no region");
    return RegionStack.back();
  }

  /// \brief Ensure that \c S is included in the current region.
  void extendRegion(ASTNode S) {
    SourceMappingRegion &Region = getRegion();
    SourceLoc StartLoc = S.getStartLoc();
    if (!Region.hasStartLoc())
      Region.setStartLoc(StartLoc);
  }

  /// \brief Mark \c S as a terminator, starting a zero region.
  void terminateRegion(ASTNode S) {
    SourceMappingRegion &Region = getRegion();
    if (!Region.hasEndLoc())
      Region.setEndLoc(getEndLoc(S));
    replaceCount(CounterExpr::Zero());
  }

  Expr *getConditionNode(StmtCondition SC) {
    assert(!SC.empty() && "Empty condition");
    return SC.front().getBooleanOrNull();
  }

public:
  CoverageMapping(const SourceManager &SM) : SM(SM) {}

  /// \brief Generate the coverage counter mapping regions from collected
  /// source regions.
  SILCoverageMap *
  emitSourceRegions(SILModule &M, StringRef Name, uint64_t Hash,
                    llvm::DenseMap<ASTNode, unsigned> &CounterIndices) {
    if (SourceRegions.empty())
      return nullptr;
    StringRef Filename = SM.getIdentifierForBuffer(
        SM.findBufferContainingLoc(SourceRegions.front().getStartLoc()));

    llvm::coverage::CounterExpressionBuilder Builder;
    std::vector<SILCoverageMap::MappedRegion> Regions;
    for (const auto &Region : SourceRegions) {
      assert(Region.hasStartLoc() && "invalid region");
      assert(Region.hasEndLoc() && "incomplete region");

      auto Start = SM.getLineAndColumn(Region.getStartLoc());
      auto End = SM.getLineAndColumn(Region.getEndLoc());
      assert(Start.first <= End.first && "region start and end out of order");

      Regions.emplace_back(Start.first, Start.second, End.first, End.second,
                           Region.getCounter().expand(Builder, CounterIndices));
    }
    return SILCoverageMap::create(M, Filename, Name, Hash, Regions,
                                  Builder.getExpressions());
  }

  bool walkToDeclPre(Decl *D) override {
    if (D->isImplicit())
      return false;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D))
      assignCounter(AFD->getBody());
    return true;
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (S->isImplicit())
      return {true, S};

    if (!RegionStack.empty())
      extendRegion(S);

    if (auto *BS = dyn_cast<BraceStmt>(S)) {
      if (hasCounter(BS))
        pushRegion(BS);

    } else if (auto *IS = dyn_cast<IfStmt>(S)) {
      assignCounter(IS, CounterExpr::Zero());
      CounterExpr &ThenCounter = assignCounter(IS->getThenStmt());
      assignCounter(IS->getElseStmt(),
                    CounterExpr::Sub(getCurrentCounter(), ThenCounter));
    } else if (auto *US = dyn_cast<GuardStmt>(S)) {
      assignCounter(US, CounterExpr::Zero());
      CounterExpr &BodyCounter = assignCounter(US->getBody());
      assignCounter(US->getBody(),
                    CounterExpr::Sub(getCurrentCounter(), BodyCounter));

    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      assignCounter(WS, CounterExpr::Zero());
      if (auto *E = getConditionNode(WS->getCond()))
        assignCounter(E, CounterExpr::Ref(getCurrentCounter()));
      assignCounter(WS->getBody());

    } else if (auto *RWS = dyn_cast<RepeatWhileStmt>(S)) {
      assignCounter(RWS, CounterExpr::Zero());
      CounterExpr &BodyCounter = assignCounter(RWS->getBody());
      assignCounter(RWS->getCond(), CounterExpr::Ref(BodyCounter));

    } else if (auto *FS = dyn_cast<ForStmt>(S)) {
      assignCounter(FS, CounterExpr::Zero());
      if (Expr *E = FS->getCond().getPtrOrNull())
        assignCounter(E, CounterExpr::Ref(getCurrentCounter()));
      if (Expr *E = FS->getIncrement().getPtrOrNull())
        assignCounter(E, CounterExpr::Zero());
      assignCounter(FS->getBody());

    } else if (auto *FES = dyn_cast<ForEachStmt>(S)) {
      assignCounter(FES, CounterExpr::Zero());
      assignCounter(FES->getBody());

    } else if (auto *SS = dyn_cast<SwitchStmt>(S)) {
      assignCounter(SS);
      // Assign counters for cases so they're available for fallthrough.
      for (CaseStmt *Case : SS->getCases())
        assignCounter(Case);

    } else if (isa<CaseStmt>(S)) {
      pushRegion(S);

    } else if (auto *DCS = dyn_cast<DoCatchStmt>(S)) {
      assignCounter(DCS->getBody(), CounterExpr::Ref(getCurrentCounter()));
      assignCounter(DCS);

    } else if (auto *CS = dyn_cast<CatchStmt>(S)) {
      assignCounter(CS->getBody());
    }
    return {true, S};
  }

  Stmt *walkToStmtPost(Stmt *S) override {
    if (S->isImplicit())
      return S;

    if (isa<BraceStmt>(S)) {
      if (hasCounter(S)) {
        CounterExpr *Adjust = setExitCount(S);
        popRegions(S);
        adjustForNonLocalExits(S, Adjust);
      }

    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      // Update the condition with the backedge count.
      if (auto *E = getConditionNode(WS->getCond()))
        addToCounter(E, getExitCounter());

    } else if (auto *FS = dyn_cast<ForStmt>(S)) {
      // Both the condition and the increment are reached through the backedge.
      if (Expr *E = FS->getCond().getPtrOrNull())
        addToCounter(E, getExitCounter());
      if (Expr *E = FS->getIncrement().getPtrOrNull())
        addToCounter(E, getExitCounter());

    } else if (auto *CS = dyn_cast<ContinueStmt>(S)) {
      // Continues create extra backedges, add them to the appropriate counters.
      addToCounter(CS->getTarget(), getCurrentCounter());
      if (auto *WS = dyn_cast<WhileStmt>(CS->getTarget())) {
        if (auto *E = getConditionNode(WS->getCond()))
          addToCounter(E, getCurrentCounter());
      } else if (auto *FS = dyn_cast<ForStmt>(CS->getTarget()))
        if (Expr *E = FS->getCond().getPtrOrNull())
          addToCounter(E, getCurrentCounter());
      terminateRegion(S);

    } else if (auto *BS = dyn_cast<BreakStmt>(S)) {
      // When we break from a loop, we need to adjust the exit count.
      if (!isa<SwitchStmt>(BS->getTarget()))
        addToCounter(BS->getTarget(), getCurrentCounter());
      terminateRegion(S);

    } else if (auto *FS = dyn_cast<FallthroughStmt>(S)) {
      addToCounter(FS->getFallthroughDest(), getCurrentCounter());
      terminateRegion(S);

    } else if (isa<SwitchStmt>(S)) {
      replaceCount(CounterExpr::Ref(getCounter(S)), getEndLoc(S));

    } else if (isa<CaseStmt>(S)) {
      popRegions(S);

    } else if (isa<DoCatchStmt>(S)) {
      replaceCount(CounterExpr::Ref(getCounter(S)), getEndLoc(S));

    } else if (isa<ReturnStmt>(S) || isa<FailStmt>(S)) {
      terminateRegion(S);
    }
    return S;
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (!RegionStack.empty())
      extendRegion(E);

    switch (E->getKind()) {
    default:
      break;
    case ExprKind::AutoClosure:
      // Autoclosures look strange if there isn't a region, since it looks like
      // control flow starts partway through an expression. For now we skip
      // these so we don't get odd behaviour in default arguments and the like,
      // but in the future we should consider creating appropriate regions for
      // those expressions.
      if (RegionStack.empty())
        break;
      SWIFT_FALLTHROUGH;
    case ExprKind::Closure:
      assignCounter(E);
      break;
    }

    if (hasCounter(E))
      pushRegion(E);
    return {true, E};
  }

  Expr *walkToExprPost(Expr *E) override {
    if (hasCounter(E))
      popRegions(E);

    if (isa<ThrowExpr>(E))
      terminateRegion(E);

    return E;
  }

};

} // end anonymous namespace

void SILGenProfiling::assignRegionCounters(AbstractFunctionDecl *Root) {
  SmallString<128> NameBuffer;
  SILDeclRef(Root).mangle(NameBuffer);
  CurrentFuncName = NameBuffer.str();

  MapRegionCounters Mapper(RegionCounterMap);
  Root->walk(Mapper);

  NumRegionCounters = Mapper.NextCounter;
  // TODO: Mapper needs to calculate a function hash as it goes.
  FunctionHash = 0x0;

  if (EmitCoverageMapping) {
    CoverageMapping Coverage(SGM.M.getASTContext().SourceMgr);
    Root->walk(Coverage);
    Coverage.emitSourceRegions(SGM.M, CurrentFuncName, FunctionHash,
                               RegionCounterMap);
  }
}

static SILLocation getLocation(ASTNode Node) {
  if (Expr *E = Node.dyn_cast<Expr *>())
    return E;
  else if (Stmt *S = Node.dyn_cast<Stmt *>())
    return S;
  else if (Decl *D = Node.dyn_cast<Decl *>())
    return D;
  else
    llvm_unreachable("unsupported ASTNode");
}

void SILGenProfiling::emitCounterIncrement(SILBuilder &Builder, ASTNode Node) {
  auto &C = Builder.getASTContext();

  auto CounterIt = RegionCounterMap.find(Node);
  assert(CounterIt != RegionCounterMap.end() &&
         "cannot increment non-existent counter");

  auto Int32Ty = SGM.Types.getLoweredType(BuiltinIntegerType::get(32, C));
  auto Int64Ty = SGM.Types.getLoweredType(BuiltinIntegerType::get(64, C));

  SILLocation Loc = getLocation(Node);
  SILValue Args[] = {
      // TODO: In C++ we give this string linkage that matches the functions, so
      // that it's uniqued appropriately across TUs.
      Builder.createStringLiteral(Loc, StringRef(CurrentFuncName),
                                  StringLiteralInst::Encoding::UTF8),
      Builder.createIntegerLiteral(Loc, Int64Ty, FunctionHash),
      Builder.createIntegerLiteral(Loc, Int32Ty, NumRegionCounters),
      // TODO: Should we take care to emit only one copy of each of the above
      // three literals per function?
      Builder.createIntegerLiteral(Loc, Int32Ty, CounterIt->second)};
  Builder.createBuiltin(Loc, C.getIdentifier("int_instrprof_increment"),
                        SGM.Types.getEmptyTupleType(), {}, Args);
}
