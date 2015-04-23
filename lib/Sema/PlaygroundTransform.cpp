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

  typedef llvm::SmallVector<swift::ASTNode, 3> ElementVector;

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
      if (llvm::dyn_cast<BraceStmt>(S)) {
        return { false, S }; // don't walk into brace statements; we
                             // need to respect nesting!
      } else {
        return { true, S };
      }
    }
    virtual std::pair<bool, Expr*> walkToExprPre(Expr *E) {
      if (ClosureExpr *CE = llvm::dyn_cast<ClosureExpr>(E)) {
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
      return transformBraceStmt(llvm::cast<BraceStmt>(S));
    case StmtKind::If:
      return transformIfStmt(llvm::cast<IfStmt>(S));
    case StmtKind::While: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformWhileStmt(llvm::cast<WhileStmt>(S));
      }
    case StmtKind::RepeatWhile: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformRepeatWhileStmt(llvm::cast<RepeatWhileStmt>(S));
      }
    case StmtKind::For: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformForStmt(llvm::cast<ForStmt>(S));
      }
    case StmtKind::ForEach: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Break);
        return transformForEachStmt(llvm::cast<ForEachStmt>(S));
      }
    case StmtKind::Switch: {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Fallthrough);
        return transformSwitchStmt(llvm::cast<SwitchStmt>(S));
      }
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
        if (BraceStmt *B = llvm::dyn_cast<BraceStmt>(S)) {
          BraceStmt *NB = transformBraceStmt(B);
          if (NB != B) {
            CS->setBody(NB);
          }
        }
      }
    }
      
    return SS;
  }
  
  Decl *transformDecl(Decl *D) {
    if (D->isImplicit())
      return D;
    if (FuncDecl *FD = llvm::dyn_cast<FuncDecl>(D)) {
      if (BraceStmt *B = FD->getBody()) {
        TargetKindSetter TKS(BracePairs, BracePair::TargetKinds::Return);
        BraceStmt *NB = transformBraceStmt(B);
        if (NB != B) {
          FD->setBody(NB);
        }
      }
    } else if (NominalTypeDecl *NTD = llvm::dyn_cast<NominalTypeDecl>(D)) {
      for (Decl *Member : NTD->getMembers()) {
        transformDecl(Member);
      }
    }
    
    return D;
  }

  std::pair<Expr *, ValueDecl *> digForVariable(Expr *E) {
    switch (E->getKind()) {
    default:
      if (ImplicitConversionExpr *ICE =
          llvm::dyn_cast<ImplicitConversionExpr>(E)) {
        return digForVariable(ICE->getSubExpr());
      }
      return std::make_pair(nullptr, nullptr);
    case ExprKind::DeclRef:
      return std::make_pair(E, llvm::cast<DeclRefExpr>(E)->getDecl());
    case ExprKind::MemberRef:
      return std::make_pair(
        E, llvm::cast<MemberRefExpr>(E)->getMember().getDecl());
    case ExprKind::Load:
      return digForVariable(llvm::cast<LoadExpr>(E)->getSubExpr());
    case ExprKind::ForceValue:
      return digForVariable(llvm::cast<ForceValueExpr>(E)->getSubExpr());
    case ExprKind::InOut:
      return digForVariable( llvm::cast<InOutExpr>(E)->getSubExpr());
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
  
  DeclRefExpr *digForInoutDeclRef(Expr *E) {
    if (ScalarToTupleExpr *STE = llvm::dyn_cast<ScalarToTupleExpr>(E)) {
      if (InOutExpr *IOE = llvm::dyn_cast<InOutExpr>(STE->getSubExpr())) {
        return llvm::dyn_cast<DeclRefExpr>(IOE->getSubExpr());
      }
    } else if (TupleExpr *TE = llvm::dyn_cast<TupleExpr>(E)) {
      DeclRefExpr *DRE = nullptr;
      for (Expr *Element : TE->getElements()) {
        if (InOutExpr *IOE = llvm::dyn_cast<InOutExpr>(Element)) {
          if (DeclRefExpr *NDRE =
              llvm::dyn_cast<DeclRefExpr>(IOE->getSubExpr())) {
            if (DRE) {
              return nullptr;
            }
            else {
              DRE = NDRE;
            }
          }
        }
      }
      return DRE;
    }
    return nullptr;
  }

  BraceStmt *transformBraceStmt(BraceStmt *BS, bool TopLevel = false) {
    llvm::ArrayRef<ASTNode> OriginalElements = BS->getElements();
    typedef llvm::SmallVector<swift::ASTNode, 3> ElementVector;
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
        if (AssignExpr *AE = llvm::dyn_cast<AssignExpr>(E)) {
          if (MemberRefExpr *MRE =
              llvm::dyn_cast<MemberRefExpr>(AE->getDest())) {
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
        else if (ApplyExpr *AE = llvm::dyn_cast<ApplyExpr>(E)) {
          bool Handled = false;
          if (DeclRefExpr *DRE = llvm::dyn_cast<DeclRefExpr>(AE->getFn())) {
            AbstractFunctionDecl *FnD =
              llvm::dyn_cast<AbstractFunctionDecl>(DRE->getDecl());
            ParenExpr *PE = llvm::dyn_cast<ParenExpr>(AE->getArg());
            if (FnD && PE &&
                FnD->getModuleContext() == Context.TheStdlibModule) {
              StringRef FnName = FnD->getNameStr();
              enum class CallType {
                NotAPrint = 0,
                Print,
                Println
              } CT = CallType::NotAPrint;
              if (FnName.equals("print")) {
                CT = CallType::Print;
              } else if (FnName.equals("println")) {
                CT = CallType::Println;
              }
              if (CT != CallType::NotAPrint) {
                Expr *S = PE->getSubExpr();
                std::pair<PatternBindingDecl *, VarDecl *> PV =
                  buildPatternAndVariable(S);
                Expr *Log = logPrint(PV.second, AE->getSourceRange(),
                                     (CT == CallType::Println));
                Elements[EI] = PV.first;
                Elements.insert(Elements.begin() + (EI + 1), PV.second);
                Elements.insert(Elements.begin() + (EI + 2), Log);
                EI += 2;
                Handled = true;
              }
            }
          }
          if (!Handled &&
              AE->getType()->getCanonicalType() ==
              Context.TheEmptyTupleType) {
            if (DotSyntaxCallExpr *DSCE =
                llvm::dyn_cast<DotSyntaxCallExpr>(AE->getFn())) {
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
        if (ReturnStmt *RS = llvm::dyn_cast<ReturnStmt>(S)) {
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
          if (llvm::isa<BreakStmt>(S) ||
              llvm::isa<ContinueStmt>(S)) {
            EI = escapeToTarget(BracePair::TargetKinds::Break, Elements, EI);
          } else if (llvm::isa<FallthroughStmt>(S)) {
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
        if (auto *PBD = llvm::dyn_cast<PatternBindingDecl>(D)) {
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
    if (llvm::dyn_cast<ConstructorDecl>(TypeCheckDC) &&
        VD->getNameStr().equals("self")) {
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
    if (DeclRefExpr *DRE = llvm::dyn_cast<DeclRefExpr>(RE)) {
      VarDecl *VD = llvm::cast<VarDecl>(DRE->getDecl());
  
      if (llvm::dyn_cast<ConstructorDecl>(TypeCheckDC) &&
          VD->getNameStr().equals("self")) {
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
    } else if (MemberRefExpr *MRE = llvm::dyn_cast<MemberRefExpr>(RE)) {
      Expr *B = MRE->getBase();
      ConcreteDeclRef M = MRE->getMember();

      if (llvm::dyn_cast<ConstructorDecl>(TypeCheckDC) &&
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

  Expr *logPrint(VarDecl *VD, SourceRange SR, bool IsPrintln) {
    const char *LoggerName = IsPrintln ? "$builtin_println" : "$builtin_print";
    DeclRefExpr *DRE = 
      new (Context) DeclRefExpr(ConcreteDeclRef(VD),
                                SourceLoc(),
                                true, // implicit
                                AccessSemantics::Ordinary,
                                Type());
    Expr *Args[] = { DRE };
    return buildLoggerCallWithArgs(LoggerName, Args, SR);
  }

  std::pair<PatternBindingDecl*, VarDecl*>
    buildPatternAndVariable(Expr *InitExpr) {
    char NameBuf[11] = { 0 };
    snprintf(NameBuf, 11, "tmp%u", TmpNameIndex);
    TmpNameIndex++;
        
    Expr *MaybeLoadInitExpr = nullptr;
    
    if (LValueType *LVT =
        llvm::dyn_cast<LValueType>(InitExpr->getType().getPointer())) {
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

    if (!typeCheckCompletionContextExpr(Context, TypeCheckDC, SendDataCall)) {
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
      if (AbstractFunctionDecl *FD = llvm::dyn_cast<AbstractFunctionDecl>(D)) {
        if (!FD->isImplicit()) {
          if (BraceStmt *Body = FD->getBody()) {
            Instrumenter I(FD->getASTContext(), FD, RNG, HighPerformance);
            BraceStmt *NewBody = I.transformBraceStmt(Body);
            if (NewBody != Body) {
              FD->setBody(NewBody);
            }
            return false;
          }
        }
      } else if (TopLevelCodeDecl *TLCD = llvm::dyn_cast<TopLevelCodeDecl>(D)) {
        if (!TLCD->isImplicit()) {
          if (BraceStmt *Body = TLCD->getBody()) {
            Instrumenter I(((Decl*)TLCD)->getASTContext(), TLCD, RNG, HighPerformance);
            BraceStmt *NewBody = I.transformBraceStmt(Body, true);
            if (NewBody != Body) {
              TLCD->setBody(NewBody);
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
