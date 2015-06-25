//===--- PlaygroundTransform.cpp - Playground Transform -------------------===//
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
//  This file implements the playground transform for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"
#include "swift/Subsystems.h"
#include "TypeChecker.h"

#include <forward_list>
#include <random>

using namespace swift;

//===----------------------------------------------------------------------===//
// performPlaygroundTransform
//===----------------------------------------------------------------------===//

namespace {

class Instrumenter {
private:
  std::mt19937_64 &RNG;
  ASTContext &Context;
  DeclContext *TypeCheckDC;
  unsigned TmpNameIndex = 0;
  bool HighPerformance;

  struct BracePair {
  public:
    SourceRange BraceRange;
    enum class TargetKinds {
      None = 0,
      Break,
      Return,
      Fallthrough
    };
    TargetKinds TargetKind = TargetKinds::None;

    BracePair(const SourceRange &BR) : 
      BraceRange(BR) { }
  };

  typedef std::forward_list<BracePair> BracePairStack;

  BracePairStack BracePairs;
  class BracePairPusher {
    BracePairStack &BracePairs;
    bool Valid = false;
  public:
    BracePairPusher(BracePairStack &BPS, const SourceRange &BR) :
      BracePairs(BPS) {
      BracePairs.push_front(BracePair(BR));
      Valid = true;
    }
    ~BracePairPusher() {
      if (isValid()) {
        Valid = false;
        BracePairs.pop_front();
      }
    }
    void invalidate() {
      if (isValid()) {
        Valid = false;
        BracePairs.pop_front();
      }
    }
    bool isValid() {
      return Valid;
    }
  };

  class TargetKindSetter {
    BracePairStack &BracePairs;
  public:
    TargetKindSetter(BracePairStack &BPS, BracePair::TargetKinds Kind) :
      BracePairs(BPS) {
      assert(!BracePairs.empty());
      assert(BracePairs.front().TargetKind == BracePair::TargetKinds::None);
      BracePairs.front().TargetKind = Kind;
    }
    ~TargetKindSetter() {
      BracePairs.front().TargetKind = BracePair::TargetKinds::None;
    }
  };

  typedef SmallVector<swift::ASTNode, 3> ElementVector;

  // Before a "return," "continue" or similar statement, emit pops of
  // all the braces up to its target.
  size_t escapeToTarget(BracePair::TargetKinds TargetKind,
                        ElementVector &Elements, size_t EI) {
    if (HighPerformance)
      return EI;

    for (const BracePair &BP : BracePairs) {
      if (BP.TargetKind == TargetKind) {
        break;
      }
      Elements.insert(Elements.begin() + EI,
                      buildScopeExit(BP.BraceRange));
      ++EI;
    }
    return EI;
  }
    
  class ClosureFinder : public ASTWalker {
  private:
    Instrumenter &I;
  public:
    ClosureFinder (Instrumenter &Inst) : I(Inst) { }
    virtual std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) {
      if (isa<BraceStmt>(S)) {
        return { false, S }; // don't walk into brace statements; we
                             // need to respect nesting!
      } else {
        return { true, S };
      }
    }
    virtual std::pair<bool, Expr*> walkToExprPre(Expr *E) {
      if (ClosureExpr *CE = dyn_cast<ClosureExpr>(E)) {
        BraceStmt *B = CE->getBody();
        if (B) {
          BraceStmt *NB = I.transformBraceStmt(B);
          CE->setBody(NB, false);
          // just with the entry and exit logging this is going to
          // be more than a single expression!
        }
      }
      return { true, E };
    }
  };
    
  ClosureFinder CF;

public:
  Instrumenter (ASTContext &C, DeclContext *DC, std::mt19937_64 &RNG,
                bool HP) :
    RNG(RNG), Context(C), TypeCheckDC(DC), HighPerformance(HP), CF(*this) { }
    
  Stmt *transformStmt(Stmt *S) { 
    switch (S->getKind()) {
    default:
      return S;
    case StmtKind::Brace:
      return transformBraceStmt(cast<BraceStmt>(S));
    case StmtKind::If:
      return transformIfStmt(cast<IfStmt>(S));
    case StmtKind::Guard:
      return transformGuardStmt(cast<GuardStmt>(S));
    case StmtKind::While: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformWhileStmt(cast<WhileStmt>(S));
      }
    case StmtKind::RepeatWhile: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformRepeatWhileStmt(cast<RepeatWhileStmt>(S));
      }
    case StmtKind::For: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformForStmt(cast<ForStmt>(S));
      }
    case StmtKind::ForEach: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformForEachStmt(cast<ForEachStmt>(S));
      }
    case StmtKind::Switch: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Fallthrough);
        return transformSwitchStmt(cast<SwitchStmt>(S));
      }
    case StmtKind::Do:
      return transformDoStmt(llvm::cast<DoStmt>(S));
    case StmtKind::DoCatch:
      return transformDoCatchStmt(cast<DoCatchStmt>(S));
    }
  }
    
  // transform*() return their input if it's unmodified,
  // or a modified copy of their input otherwise.
  IfStmt *transformIfStmt(IfStmt *IS) {
    if (Stmt *TS = IS->getThenStmt()) {
      Stmt *NTS = transformStmt(TS);
      if (NTS != TS) {
        IS->setThenStmt(NTS);
      }
    }

    if (Stmt *ES = IS->getElseStmt()) {
      Stmt *NES = transformStmt(ES);
      if (NES != ES) {
        IS->setElseStmt(NES);
      }
    }

    return IS;
  }
  
  GuardStmt *transformGuardStmt(GuardStmt *GS) {
    if (Stmt *BS = GS->getBody())
      GS->setBody(transformStmt(BS));
    return GS;
  }

  WhileStmt *transformWhileStmt(WhileStmt *WS) {
    if (Stmt *B = WS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        WS->setBody(NB);
      }
    }
      
    return WS;
  }

  RepeatWhileStmt *transformRepeatWhileStmt(RepeatWhileStmt *RWS) {
    if (Stmt *B = RWS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        RWS->setBody(NB);
      }
    }
      
    return RWS;
  }

  ForStmt *transformForStmt(ForStmt *FS) {
    if (Stmt *B = FS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        FS->setBody(NB);
      }
    }
      
    return FS;
  }

  ForEachStmt *transformForEachStmt(ForEachStmt *FES) {
    if (BraceStmt *B = FES->getBody()) {
      BraceStmt *NB = transformBraceStmt(B);
      if (NB != B) {
        FES->setBody(NB);
      }
    }
      
    return FES;
  }

  SwitchStmt *transformSwitchStmt(SwitchStmt *SS) {
    for (CaseStmt *CS : SS->getCases()) {
      if (Stmt *S = CS->getBody()) {
        if (auto *B = dyn_cast<BraceStmt>(S)) {
          BraceStmt *NB = transformBraceStmt(B);
          if (NB != B) {
            CS->setBody(NB);
          }
        }
      }
    }
      
    return SS;
  }

  DoStmt *transformDoStmt(DoStmt *DS) {
    if (BraceStmt *B = dyn_cast_or_null<BraceStmt>(DS->getBody())) {
      BraceStmt *NB = transformBraceStmt(B);
      if (NB != B) {
        DS->setBody(NB);
      }
    }
    return DS;
  }

  DoCatchStmt *transformDoCatchStmt(DoCatchStmt *DCS) {
    if (BraceStmt *B = dyn_cast_or_null<BraceStmt>(DCS->getBody())) {
      BraceStmt *NB = transformBraceStmt(B);
      if (NB != B) {
        DCS->setBody(NB);
      }
    }
    for (CatchStmt *C : DCS->getCatches()) {
      if (BraceStmt *CB = dyn_cast_or_null<BraceStmt>(C->getBody())) {
        BraceStmt *NCB = transformBraceStmt(CB);
        if (NCB != CB) {
          C->setBody(NCB);
        }
      }
    }
    return DCS;
  }
  
  Decl *transformDecl(Decl *D) {
    if (D->isImplicit())
      return D;
    if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      if (BraceStmt *B = FD->getBody()) {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Return);
        BraceStmt *NB = transformBraceStmt(B);
        if (NB != B) {
          FD->setBody(NB);
        }
      }
    } else if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
      for (Decl *Member : NTD->getMembers()) {
        transformDecl(Member);
      }
    }
    
    return D;
  }

  std::pair<Expr *, ValueDecl *> digForVariable(Expr *E) {
    switch (E->getKind()) {
    default:
      if (auto *ICE = dyn_cast<ImplicitConversionExpr>(E))
        return digForVariable(ICE->getSubExpr());
      return std::make_pair(nullptr, nullptr);
    case ExprKind::DeclRef:
      return std::make_pair(E, cast<DeclRefExpr>(E)->getDecl());
    case ExprKind::MemberRef:
      return std::make_pair(
        E, cast<MemberRefExpr>(E)->getMember().getDecl());
    case ExprKind::Load:
      return digForVariable(cast<LoadExpr>(E)->getSubExpr());
    case ExprKind::ForceValue:
      return digForVariable(cast<ForceValueExpr>(E)->getSubExpr());
    case ExprKind::InOut:
      return digForVariable(cast<InOutExpr>(E)->getSubExpr());
    }
  }
  
  std::string digForName(Expr *E) {
    Expr *RE = nullptr;
    ValueDecl *VD = nullptr;
    std::tie(RE, VD) = digForVariable(E);
    if (VD) {
      return VD->getName().str();
    } else {
      return std::string("");
    }
  }
  
  static DeclRefExpr *digForInoutDeclRef(Expr *E) {
    if (auto inout = dyn_cast<InOutExpr>(E)) {
      return dyn_cast<DeclRefExpr>(
                   inout->getSubExpr()->getSemanticsProvidingExpr());

    // Drill through tuple shuffles, ignoring non-default-argument inouts.
    } else if (auto shuffle = dyn_cast<TupleShuffleExpr>(E)) {
      return digForInoutDeclRef(shuffle->getSubExpr());

    // Try to find a unique inout argument in a tuple.
    } else if (auto tuple = dyn_cast<TupleExpr>(E)) {
      DeclRefExpr *uniqueRef = nullptr;
      for (Expr *elt : tuple->getElements()) {
        if (auto ref = digForInoutDeclRef(elt)) {
          // If we already have a reference, it's not unique.
          if (uniqueRef) return nullptr;
          uniqueRef = ref;
        }
      }
      return uniqueRef;

    // Look through parens.
    } else {
      auto subExpr = E->getSemanticsProvidingExpr();
      return (E == subExpr ? nullptr : digForInoutDeclRef(subExpr));
    }
  }

  BraceStmt *transformBraceStmt(BraceStmt *BS, bool TopLevel = false) {
    ArrayRef<ASTNode> OriginalElements = BS->getElements();
    typedef SmallVector<swift::ASTNode, 3> ElementVector;
    ElementVector Elements(OriginalElements.begin(),
                           OriginalElements.end());

    SourceRange SR = BS->getSourceRange();
    BracePairPusher BPP(BracePairs, SR);

    for (size_t EI = 0;
         EI != Elements.size();
         ++EI) {
      swift::ASTNode &Element = Elements[EI];
      if (Expr *E = Element.dyn_cast<Expr*>()) {
        E->walk(CF);
        if (AssignExpr *AE = dyn_cast<AssignExpr>(E)) {
          if (auto *MRE = dyn_cast<MemberRefExpr>(AE->getDest())) {
            // an assignment to a property of an object counts as a mutation of
            // that object
            Expr *Base_RE = nullptr;
            ValueDecl *BaseVD = nullptr;
            std::tie(Base_RE, BaseVD) = digForVariable(MRE->getBase());
            
            if (Base_RE) {
              Expr *Log = logDeclOrMemberRef(Base_RE);
              if (Log) {
                Elements.insert(Elements.begin() + (EI + 1), Log);
                ++EI;
              }
            }
          } else {
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(AE->getSrc());
            DeclRefExpr *DRE =
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        AccessSemantics::Ordinary,
                                        AE->getSrc()->getType());
            AssignExpr *NAE = new (Context) AssignExpr(AE->getDest(),
                                                       SourceLoc(),
                                                       DRE,
                                                       true); // implicit
            NAE->setType(Context.TheEmptyTupleType);
            AE->setImplicit(true);
            std::string Name = digForName(AE->getDest());
            
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        AccessSemantics::Ordinary,
                                        AE->getSrc()->getType()),
              AE->getSrc()->getSourceRange(), Name.c_str());
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            Elements.insert(Elements.begin() + (EI + 3), NAE);
            EI += 3;
          }
        }
        else if (ApplyExpr *AE = dyn_cast<ApplyExpr>(E)) {
          bool Handled = false;
          if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(AE->getFn())) {
            auto *FnD = dyn_cast<AbstractFunctionDecl>(DRE->getDecl());
            if (FnD && FnD->getModuleContext() == Context.TheStdlibModule) {
              StringRef FnName = FnD->getNameStr();
              if (FnName.equals("print") || FnName.equals("debugPrint")) {
                const bool isDebugPrint = FnName.equals("debugPrint");
                Expr *object = nullptr;
                Expr *appendNewline = nullptr;
                if (ParenExpr *PE = dyn_cast<ParenExpr>(AE->getArg())) {
                  object = PE->getSubExpr();
                  appendNewline =
                    new (Context) BooleanLiteralExpr(true, SourceLoc(), true);
                  typeCheckContextExpr(Context, TypeCheckDC, appendNewline);
                } else if (TupleExpr *TE = dyn_cast<TupleExpr>(AE->getArg())) {
                  if (TE->getNumElements() == 2) {
                    object = TE->getElement(0);
                    appendNewline = TE->getElement(1);
                  }
                }
                if (object && appendNewline) {
                  std::pair<PatternBindingDecl *, VarDecl *> objectPV =
                    buildPatternAndVariable(object);
                  std::pair<PatternBindingDecl *, VarDecl *> appendNewlinePV =
                    buildPatternAndVariable(appendNewline);
                  Expr *Log = logPrint(isDebugPrint,
                                       objectPV.second, appendNewlinePV.second,
                                       AE->getSourceRange());
                  Elements[EI] = objectPV.first;
                  Elements.insert(Elements.begin() + (EI + 1),
                                  objectPV.second);
                  Elements.insert(Elements.begin() + (EI + 2),
                                  appendNewlinePV.first);
                  Elements.insert(Elements.begin() + (EI + 3),
                                  appendNewlinePV.second);
                  Elements.insert(Elements.begin() + (EI + 4),
                                  Log);
                  EI += 4;
                }
                Handled = true;
              }
            }
          }
          if (!Handled &&
              AE->getType()->getCanonicalType() ==
              Context.TheEmptyTupleType) {
            if (auto *DSCE = dyn_cast<DotSyntaxCallExpr>(AE->getFn())) {
              Expr *TargetExpr = DSCE->getArg();
              Expr *Target_RE = nullptr;
              ValueDecl *TargetVD = nullptr;

              std::tie(Target_RE, TargetVD) = digForVariable(TargetExpr);

              if (TargetVD) {
                Expr *Log = logDeclOrMemberRef(Target_RE);
                if (Log) {
                  Elements.insert(Elements.begin() + (EI + 1), Log);
                  ++EI;
                }
              }
            } else if (DeclRefExpr *DRE = digForInoutDeclRef(AE->getArg())) {
              Expr *Log = logDeclOrMemberRef(DRE);
              if (Log) {
                Elements.insert(Elements.begin() + (EI + 1), Log);
                ++EI;
              }
            }
            Handled = true;
          }
          if (!Handled) {
            // do the same as for all other expressions
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(E);
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        AccessSemantics::Ordinary,
                                        E->getType()),
              E->getSourceRange(), "");
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            EI += 2;
          }
        }
        else {
          if (E->getType()->getCanonicalType() !=
              Context.TheEmptyTupleType) {
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(E);
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        AccessSemantics::Ordinary,
                                        E->getType()),
              E->getSourceRange(), "");
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            EI += 2;
          }
        }
      } else if (Stmt *S = Element.dyn_cast<Stmt*>()) {
        S->walk(CF);
        if (ReturnStmt *RS = dyn_cast<ReturnStmt>(S)) {
          if (RS->hasResult()) {
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(RS->getResult());
            DeclRefExpr *DRE =
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        AccessSemantics::Ordinary,
                                        RS->getResult()->getType());
            ReturnStmt *NRS = new (Context) ReturnStmt(SourceLoc(),
                                                       DRE,
                                                       true); // implicit
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        AccessSemantics::Ordinary,
                                        RS->getResult()->getType()),
              RS->getResult()->getSourceRange(), "");
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            Elements.insert(Elements.begin() + (EI + 3), NRS);
            EI += 3;
          }
          EI = escapeToTarget(BracePair::TargetKinds::Return, Elements, EI);
        } else {
          if (isa<BreakStmt>(S) || isa<ContinueStmt>(S)) {
            EI = escapeToTarget(BracePair::TargetKinds::Break, Elements, EI);
          } else if (isa<FallthroughStmt>(S)) {
            EI = escapeToTarget(BracePair::TargetKinds::Fallthrough, Elements,
                                EI);
          }
          Stmt *NS = transformStmt(S);
          if (NS != S) {
            Elements[EI] = NS;
          }
        }
      } else if (Decl *D = Element.dyn_cast<Decl*>()) {
        D->walk(CF);
        if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
          if (VarDecl *VD = PBD->getSingleVar()) {
            if (VD->getParentInitializer()) {
              if (Expr *Log = logVarDecl(VD)) {
                Elements.insert(Elements.begin() + (EI + 1), Log);
                ++EI;
              }
            }
          }
        } else {
          transformDecl(D);
        }
      }
    }

    if (!TopLevel && !HighPerformance) {
      Elements.insert(Elements.begin(), buildScopeEntry(BS->getSourceRange()));
      Elements.insert(Elements.end(), buildScopeExit(BS->getSourceRange()));
    }

    // Remove null elements from the list.
    // FIXME: This is a band-aid used to work around the fact that the
    // above code can introduce null elements into the vector. The
    // right fix is to avoid doing that above.
    Elements.erase(std::remove_if(Elements.begin(), Elements.end(), 
                                  [](ASTNode node) {
                                    return node.isNull();
                                  }),
                   Elements.end());
    
    return swift::BraceStmt::create(Context, BS->getLBraceLoc(),
                                    Context.AllocateCopy(Elements),
                                    BS->getRBraceLoc());
  }

  // log*() functions return a newly-created log expression to be inserted
  // after or instead of the expression they're looking at.  Only call this
  // if the variable has an initializer.
  Expr *logVarDecl(VarDecl *VD) {
    if (isa<ConstructorDecl>(TypeCheckDC) && VD->getNameStr().equals("self")) {
      // Don't log "self" in a constructor
      return nullptr;
    }

    return buildLoggerCall(
      new (Context) DeclRefExpr(ConcreteDeclRef(VD),
                                SourceLoc(),
                                true, // implicit
                                AccessSemantics::Ordinary,
                                Type()),
      VD->getSourceRange(), VD->getName().str().str().c_str());
  }

  Expr *logDeclOrMemberRef(Expr *RE) {
    if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(RE)) {
      VarDecl *VD = cast<VarDecl>(DRE->getDecl());
  
      if (isa<ConstructorDecl>(TypeCheckDC) && VD->getNameStr().equals("self")){
        // Don't log "self" in a constructor
        return nullptr;
      }
  
      return buildLoggerCall(
        new (Context) DeclRefExpr(ConcreteDeclRef(VD),
                                  SourceLoc(),
                                  true, // implicit
                                  AccessSemantics::Ordinary,
                                  Type()),
        DRE->getSourceRange(), VD->getName().str().str().c_str());
    } else if (MemberRefExpr *MRE = dyn_cast<MemberRefExpr>(RE)) {
      Expr *B = MRE->getBase();
      ConcreteDeclRef M = MRE->getMember();

      if (isa<ConstructorDecl>(TypeCheckDC) &&
          !digForName(B).compare("self")) {
        // Don't log attributes of "self" in a constructor
        return nullptr;
      }
  
      return buildLoggerCall(
        new (Context) MemberRefExpr(B,
                                    SourceLoc(),
                                    M,
                                    SourceRange(),
                                    true, // implicit
                                    AccessSemantics::Ordinary),
        MRE->getSourceRange(), M.getDecl()->getName().str().str().c_str());
    } else {
      return nullptr;
    }
  }

  Expr *logPrint(bool isDebugPrint, VarDecl *object, VarDecl *appendNewline,
                 SourceRange SR) {
    const char *LoggerName = isDebugPrint ? "$builtin_debugPrint" :
                                            "$builtin_print";
    DeclRefExpr *object_DRE = 
      new (Context) DeclRefExpr(ConcreteDeclRef(object),
                                SourceLoc(),
                                true, // implicit
                                AccessSemantics::Ordinary,
                                Type());
    DeclRefExpr *appendNewline_DRE = 
      new (Context) DeclRefExpr(ConcreteDeclRef(appendNewline),
                                SourceLoc(),
                                true, // implicit
                                AccessSemantics::Ordinary,
                                Type());
    Expr *Args[] = { object_DRE, appendNewline_DRE };
    return buildLoggerCallWithArgs(LoggerName, Args, SR);
  }

  std::pair<PatternBindingDecl*, VarDecl*>
    buildPatternAndVariable(Expr *InitExpr) {
    char NameBuf[11] = { 0 };
    snprintf(NameBuf, 11, "tmp%u", TmpNameIndex);
    TmpNameIndex++;
        
    Expr *MaybeLoadInitExpr = nullptr;
    
    if (LValueType *LVT =
          dyn_cast<LValueType>(InitExpr->getType().getPointer())) {
      MaybeLoadInitExpr = new (Context) LoadExpr (InitExpr,
                                                  LVT->getObjectType());
    }
    else {
      MaybeLoadInitExpr = InitExpr;
    }

    VarDecl *VD = new (Context) VarDecl(false, // static
                                        true, // let
                                        SourceLoc(),
                                        Context.getIdentifier(NameBuf),
                                        MaybeLoadInitExpr->getType(),
                                        TypeCheckDC);

    VD->setImplicit();

    NamedPattern *NP = new (Context) NamedPattern(VD, /*implicit*/true);
    PatternBindingDecl *PBD =
      PatternBindingDecl::create(Context, SourceLoc(), StaticSpellingKind::None,
                                 SourceLoc(), NP, MaybeLoadInitExpr,
                                 TypeCheckDC);
    PBD->setImplicit();

    return std::make_pair(PBD, VD);
  }

  Expr *buildLoggerCall(Expr *E, SourceRange SR, const char *Name) {
    assert(Name);
    std::string *NameInContext = Context.AllocateObjectCopy(std::string(Name));
    
    Expr *NameExpr = new (Context) StringLiteralExpr(NameInContext->c_str(),
                                                     SourceRange());
    NameExpr->setImplicit(true);

    const size_t buf_size = 11;
    char * const id_buf = (char*)Context.Allocate(buf_size, 1);
    std::uniform_int_distribution<unsigned int> Distribution(0, 0x7fffffffu);
    const unsigned int id_num = Distribution(RNG);
    ::snprintf(id_buf, buf_size, "%u", id_num);
    Expr *IDExpr = new (Context) IntegerLiteralExpr(id_buf, 
                                                    SR.End, true);
    
    Expr *LoggerArgExprs[] = {
        E,
        NameExpr,
        IDExpr
      };

    return buildLoggerCallWithArgs("$builtin_log_with_id", 
                                   MutableArrayRef<Expr *>(LoggerArgExprs),
                                   SR);
  }

  Expr *buildScopeEntry(SourceRange SR) {
    return buildScopeCall(SR, false);
  }

  Expr *buildScopeExit(SourceRange SR) {
    return buildScopeCall(SR, true);
  }

  Expr *buildScopeCall(SourceRange SR, bool IsExit) {
    const char *LoggerName = IsExit ? "$builtin_log_scope_exit"
                                    : "$builtin_log_scope_entry";

    return buildLoggerCallWithArgs(LoggerName,
                                   MutableArrayRef<Expr *>(),
                                   SR);
  }

  Expr *buildLoggerCallWithArgs(const char *LoggerName,
                                MutableArrayRef<Expr *> Args,
                                SourceRange SR) {
    Expr *LoggerArgs = nullptr;

    if (Args.size() == 1) {
      LoggerArgs = new (Context) ParenExpr(SourceLoc(),
                                           Args[0],
                                           SourceLoc(),
                                           false);
    } else {
      LoggerArgs = TupleExpr::createImplicit(Context, Args, { });
    }

    UnresolvedDeclRefExpr *LoggerRef =
      new (Context) UnresolvedDeclRefExpr(
        Context.getIdentifier(LoggerName),
        DeclRefKind::Ordinary,
        SR.End);

    LoggerRef->setImplicit(true);

    Expr *LoggerCall = new (Context) CallExpr(LoggerRef, LoggerArgs, true,
                                              Type());

    std::pair<unsigned, unsigned> StartLC =
      Context.SourceMgr.getLineAndColumn(SR.Start);

    std::pair<unsigned, unsigned> EndLC =
      Context.SourceMgr.getLineAndColumn(
        Lexer::getLocForEndOfToken(Context.SourceMgr, SR.End));

    const size_t buf_size = 8;

    char *start_line_buf = (char*)Context.Allocate(buf_size, 1);
    char *end_line_buf = (char*)Context.Allocate(buf_size, 1);
    char *start_column_buf = (char*)Context.Allocate(buf_size, 1);
    char *end_column_buf = (char*)Context.Allocate(buf_size, 1);

    ::snprintf(start_line_buf, buf_size, "%d", StartLC.first);
    ::snprintf(start_column_buf, buf_size, "%d", StartLC.second);
    ::snprintf(end_line_buf, buf_size, "%d", EndLC.first);
    ::snprintf(end_column_buf, buf_size, "%d", EndLC.second);

    Expr *StartLine = new (Context) IntegerLiteralExpr(start_line_buf, 
                                                       SR.End, true);
    Expr *EndLine = new (Context) IntegerLiteralExpr(end_line_buf,
                                                     SR.End, true);
    Expr *StartColumn = new (Context) IntegerLiteralExpr(start_column_buf, 
                                                         SR.End, true);
    Expr *EndColumn = new (Context) IntegerLiteralExpr(end_column_buf, 
                                                       SR.End, true);

    Expr *SendDataArgExprs[] = {
        LoggerCall,
        StartLine,
        EndLine,
        StartColumn,
        EndColumn
      };

    TupleExpr *SendDataArgs = TupleExpr::createImplicit(Context, 
                                                        SendDataArgExprs, { });
    UnresolvedDeclRefExpr *SendDataRef = 
      new (Context) UnresolvedDeclRefExpr(
        Context.getIdentifier("$builtin_send_data"),
        DeclRefKind::Ordinary,
        SourceLoc());

    SendDataRef->setImplicit(true);

    Expr *SendDataCall = new (Context) CallExpr(SendDataRef, SendDataArgs, true,
                                                Type());

    if (!typeCheckContextExpr(Context, TypeCheckDC, SendDataCall)) {
      return nullptr;
    }

    return SendDataCall;
  }
};

} // end anonymous namespace

void swift::performPlaygroundTransform(SourceFile &SF,
                                       bool HighPerformance) {
  class ExpressionFinder : public ASTWalker {
  private:
    std::mt19937_64 RNG;
    bool HighPerformance;
  public:
    ExpressionFinder(bool HP) : HighPerformance(HP) { }

    virtual bool walkToDeclPre(Decl *D) {
      if (AbstractFunctionDecl *FD = dyn_cast<AbstractFunctionDecl>(D)) {
        if (!FD->isImplicit()) {
          if (BraceStmt *Body = FD->getBody()) {
            ASTContext &ctx = FD->getASTContext();
            Instrumenter I(ctx, FD, RNG, HighPerformance);
            BraceStmt *NewBody = I.transformBraceStmt(Body);
            if (NewBody != Body) {
              FD->setBody(NewBody);
              TypeChecker(ctx).checkFunctionErrorHandling(FD);
            }
            return false;
          }
        }
      } else if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        if (!TLCD->isImplicit()) {
          if (BraceStmt *Body = TLCD->getBody()) {
            ASTContext &ctx = static_cast<Decl*>(TLCD)->getASTContext();
            Instrumenter I(ctx, TLCD, RNG, HighPerformance);
            BraceStmt *NewBody = I.transformBraceStmt(Body, true);
            if (NewBody != Body) {
              TLCD->setBody(NewBody);
              TypeChecker(ctx).checkTopLevelErrorHandling(TLCD);
            }
            return false;
          }
        }
      }
      return true;
    }
  };

  ExpressionFinder EF(HighPerformance);
  for (Decl* D : SF.Decls) {
    D->walk(EF);
  }
}
