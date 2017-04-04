//===--- TypeCheckStmt.cpp - Type Checking for Statements -----------------===//
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
// This file implements semantic analysis for statements.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "MiscDiagnostics.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/LocalContext.h"
#include "swift/Syntax/TokenKinds.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Timer.h"

#include <numeric>
#include <forward_list>

using namespace swift;

#define DEBUG_TYPE "TypeCheckStmt"

namespace {
  class ContextualizeClosures : public ASTWalker {
    DeclContext *ParentDC;
  public:
    unsigned NextDiscriminator = 0;

    ContextualizeClosures(DeclContext *parent,
                          unsigned nextDiscriminator = 0)
      : ParentDC(parent), NextDiscriminator(nextDiscriminator) {}

    /// Change the context we're contextualizing to.  This is
    /// basically only reasonable when processing all the different
    /// top-level code declarations.
    void setContext(TopLevelCodeDecl *parent) {
      ParentDC = parent;
    }

    bool hasAutoClosures() const {
      return NextDiscriminator != 0;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      // Autoclosures need to be numbered and potentially reparented.
      // Reparenting is required with:
      //   - nested autoclosures, because the inner autoclosure will be
      //     parented to the outer context, not the outer autoclosure
      //   - non-local initializers
      if (auto CE = dyn_cast<AutoClosureExpr>(E)) {
        // FIXME: Work around an apparent reentrancy problem with the REPL.
        // I don't understand what's going on here well enough to fix the
        // underlying issue. -Joe
        if (CE->getParent() == ParentDC
            && CE->getDiscriminator() != AutoClosureExpr::InvalidDiscriminator)
          return { false, E };
        
        assert(CE->getDiscriminator() == AutoClosureExpr::InvalidDiscriminator);
        CE->setDiscriminator(NextDiscriminator++);
        CE->setParent(ParentDC);

        // Recurse into the autoclosure body using the same sequence,
        // but parenting to the autoclosure instead of the outer closure.
        auto oldParentDC = ParentDC;
        ParentDC = CE;
        CE->getBody()->walk(*this);
        ParentDC = oldParentDC;
        return { false, E };
      } 

      // Capture lists need to be reparented to enclosing autoclosures.
      if (auto CapE = dyn_cast<CaptureListExpr>(E)) {
        if (isa<AutoClosureExpr>(ParentDC)) {
          for (auto &Cap : CapE->getCaptureList()) {
            Cap.Init->setDeclContext(ParentDC);
            Cap.Var->setDeclContext(ParentDC);
          }
        }
      }

      // Explicit closures start their own sequence.
      if (auto CE = dyn_cast<ClosureExpr>(E)) {
        // In the repl, the parent top-level context may have been re-written.
        if (CE->getParent() != ParentDC) {
          if ((CE->getParent()->getContextKind() !=
                    ParentDC->getContextKind()) ||
              ParentDC->getContextKind() != DeclContextKind::TopLevelCodeDecl) {
            // If a closure is nested within an auto closure, we'll need to update
            // its parent to the auto closure parent.
            assert(ParentDC->getContextKind() ==
                   DeclContextKind::AbstractClosureExpr &&
                   "Incorrect parent decl context for closure");
            CE->setParent(ParentDC);
          }
        }

        // If the closure has a single expression body, we need to walk into it
        // with a new sequence.  Otherwise, it'll have been separately
        // type-checked.
        if (CE->hasSingleExpressionBody())
          CE->getBody()->walk(ContextualizeClosures(CE));

        // In neither case do we need to continue the *current* walk.
        return { false, E };
      }

      return { true, E };
    }

    /// We don't want to recurse into most local declarations.
    bool walkToDeclPre(Decl *D) override {
      // But we do want to walk into the initializers of local
      // variables.
      return isa<PatternBindingDecl>(D);
    }
  };

  /// Used for debugging which parts of the code are taking a long time to
  /// compile.
  class FunctionBodyTimer {
    AnyFunctionRef Function;
    llvm::TimeRecord StartTime = llvm::TimeRecord::getCurrentTime();
    unsigned WarnLimit;
    bool ShouldDump;

  public:
    FunctionBodyTimer(AnyFunctionRef Fn, bool shouldDump,
                      unsigned warnLimit)
        : Function(Fn), WarnLimit(warnLimit), ShouldDump(shouldDump) {}

    ~FunctionBodyTimer() {
      llvm::TimeRecord endTime = llvm::TimeRecord::getCurrentTime(false);

      auto elapsed = endTime.getProcessTime() - StartTime.getProcessTime();
      unsigned elapsedMS = static_cast<unsigned>(elapsed * 1000);

      ASTContext &ctx = Function.getAsDeclContext()->getASTContext();

      if (ShouldDump) {
        // Round up to the nearest 100th of a millisecond.
        llvm::errs() << llvm::format("%0.2f", ceil(elapsed * 100000) / 100) << "ms\t";
        Function.getLoc().print(llvm::errs(), ctx.SourceMgr);

        if (auto *AFD = Function.getAbstractFunctionDecl()) {
          llvm::errs() << "\t";
          AFD->print(llvm::errs(), PrintOptions());
        } else {
          llvm::errs() << "\t(closure)";
        }
        llvm::errs() << "\n";
      }

      if (WarnLimit != 0 && elapsedMS >= WarnLimit) {
        if (auto *AFD = Function.getAbstractFunctionDecl()) {
          DeclName name = AFD->getFullName();
          if (!name) {
            if (auto *method = dyn_cast<FuncDecl>(AFD)) {
              name = method->getAccessorStorageDecl()->getFullName();
            }
          }
          ctx.Diags.diagnose(AFD, diag::debug_long_function_body,
                             AFD->getDescriptiveKind(), name,
                             elapsedMS, WarnLimit);
        } else {
          ctx.Diags.diagnose(Function.getLoc(), diag::debug_long_closure_body,
                             elapsedMS, WarnLimit);
        }
      }
    }
  };
} // end anonymous namespace

static void setAutoClosureDiscriminators(DeclContext *DC, Stmt *S) {
  S->walk(ContextualizeClosures(DC));
}

bool TypeChecker::contextualizeInitializer(Initializer *DC, Expr *E) {
  ContextualizeClosures CC(DC);
  E->walk(CC);
  return CC.hasAutoClosures();
}

void TypeChecker::contextualizeTopLevelCode(TopLevelContext &TLC,
                                            ArrayRef<Decl*> topLevel) {
  unsigned nextDiscriminator = TLC.NextAutoClosureDiscriminator;
  ContextualizeClosures CC(nullptr, nextDiscriminator);
  for (auto decl : topLevel) {
    auto topLevelCode = dyn_cast<TopLevelCodeDecl>(decl);
    if (!topLevelCode) continue;
    CC.setContext(topLevelCode);
    topLevelCode->getBody()->walk(CC);
  }
  assert(nextDiscriminator == TLC.NextAutoClosureDiscriminator &&
         "reentrant/concurrent invocation of contextualizeTopLevelCode?");
  TLC.NextAutoClosureDiscriminator = CC.NextDiscriminator;
}

/// Emits an error with a fixit for the case of unnecessary cast over a
/// OptionSet value. The primary motivation is to help with SDK changes.
/// Example:
/// \code
///   func supported() -> MyMask {
///     return Int(MyMask.Bingo.rawValue)
///   }
/// \endcode
static void tryDiagnoseUnnecessaryCastOverOptionSet(ASTContext &Ctx,
                                                    Expr *E,
                                                    Type ResultType,
                                                    ModuleDecl *module) {
  auto *NTD = ResultType->getAnyNominal();
  if (!NTD)
    return;
  auto optionSetType = dyn_cast<ProtocolDecl>(Ctx.getOptionSetDecl());
  SmallVector<ProtocolConformance *, 4> conformances;
  if (!(optionSetType &&
        NTD->lookupConformance(module, optionSetType, conformances)))
    return;

  CallExpr *CE = dyn_cast<CallExpr>(E);
  if (!CE)
    return;
  if (!isa<ConstructorRefCallExpr>(CE->getFn()))
    return;
  ParenExpr *ParenE = dyn_cast<ParenExpr>(CE->getArg());
  if (!ParenE)
    return;
  MemberRefExpr *ME = dyn_cast<MemberRefExpr>(ParenE->getSubExpr());
  if (!ME)
    return;
  ValueDecl *VD = ME->getMember().getDecl();
  if (!VD || VD->getName() != Ctx.Id_rawValue)
    return;
  MemberRefExpr *BME = dyn_cast<MemberRefExpr>(ME->getBase());
  if (!BME)
    return;
  if (!BME->getType()->isEqual(ResultType))
    return;

  Ctx.Diags.diagnose(E->getLoc(), diag::unnecessary_cast_over_optionset,
                     ResultType)
    .highlight(E->getSourceRange())
    .fixItRemoveChars(E->getLoc(), ME->getStartLoc())
    .fixItRemove(SourceRange(ME->getDotLoc(), E->getEndLoc()));
}

namespace {
class StmtChecker : public StmtVisitor<StmtChecker, Stmt*> {
public:
  TypeChecker &TC;

  /// \brief This is the current function or closure being checked.
  /// This is null for top level code.
  Optional<AnyFunctionRef> TheFunc;
  
  /// DC - This is the current DeclContext.
  DeclContext *DC;

  // Scope information for control flow statements
  // (break, continue, fallthrough).

  /// The level of loop nesting. 'break' and 'continue' are valid only in scopes
  /// where this is greater than one.
  SmallVector<LabeledStmt*, 2> ActiveLabeledStmts;

  /// The level of 'switch' nesting. 'fallthrough' is valid only in scopes where
  /// this is greater than one.
  unsigned SwitchLevel = 0;
  /// The destination block for a 'fallthrough' statement. Null if the switch
  /// scope depth is zero or if we are checking the final 'case' of the current
  /// switch.
  CaseStmt /*nullable*/ *FallthroughDest = nullptr;

  SourceLoc EndTypeCheckLoc;

  /// Used to check for discarded expression values: in the REPL top-level
  /// expressions are not discarded.
  bool IsREPL;

  /// The SpaceEngine encapsulates an algorithm for computing the exhaustiveness
  /// of a switch statement using an algebra of spaces described by Fengyun Liu
  /// and an algorithm for computing warnings for pattern matching by
  //  Luc Maranget.
  ///
  /// The main algorithm centers around the computation of the difference and
  /// the intersection of the "Spaces" given in each case, which reduces the
  /// definition of exhaustiveness to checking if the difference of the space
  /// 'S' of the user's written patterns and the space 'T' of the pattern
  /// condition is empty.
  struct SpaceEngine {
    enum class SpaceKind : uint8_t {
      Empty           = 1 << 0,
      Type            = 1 << 1,
      Constructor     = 1 << 2,
      Disjunct        = 1 << 3,
      BooleanConstant = 1 << 5,
    };

    /// A data structure for conveniently pattern-matching on the kinds of
    /// two spaces.
    struct PairSwitch {
    public:
      constexpr PairSwitch(SpaceKind pair1, SpaceKind pair2)
        : Data(static_cast<uint8_t>(pair1) | (static_cast<uint8_t>(pair2) << 8))
        {}

      constexpr bool operator==(const PairSwitch other) const {
        return Data == other.Data;
      }

      constexpr operator int() const {
        return Data;
      }

    private:
      uint16_t Data;

      PairSwitch (const PairSwitch&) = delete;
      PairSwitch& operator= (const PairSwitch&) = delete;
    };

#define PAIRCASE(XS, YS) case PairSwitch(XS, YS)

    class Space final {
    private:
      SpaceKind Kind;
      Type Ty;
      Identifier Head;
      std::forward_list<Space> Spaces;
      bool Val;

    public:
      explicit Space(Type T)
        : Kind(SpaceKind::Type), Ty(T), Head(Identifier()),
          Spaces({}), Val(false) {}
      explicit Space(Type T, Identifier H, SmallVectorImpl<Space> &SP)
        : Kind(SpaceKind::Constructor), Ty(T), Head(H),
          Spaces(SP.begin(), SP.end()), Val(false) {}
      explicit Space(SmallVectorImpl<Space> &SP)
        : Kind(SpaceKind::Disjunct), Ty(Type()), Head(Identifier()),
          Spaces(SP.begin(), SP.end()), Val(false) {}
      explicit Space()
        : Kind(SpaceKind::Empty), Ty(Type()), Head(Identifier()),
          Spaces({}), Val(false) {}
      explicit Space(bool C)
        : Kind(SpaceKind::BooleanConstant), Ty(Type()), Head(Identifier()),
          Spaces({}), Val(C) {}

      SpaceKind getKind() const { return Kind; }

      void dump() const LLVM_ATTRIBUTE_USED;

      bool isEmpty() const { return getKind() == SpaceKind::Empty; }

      Type getType() const {
        assert((getKind() == SpaceKind::Type
                || getKind() == SpaceKind::Constructor)
               && "Wrong kind of space tried to access space type");
        return Ty;
      }

      Identifier getHead() const {
        assert(getKind() == SpaceKind::Constructor
               && "Wrong kind of space tried to access head");
        return Head;
      }

      const std::forward_list<Space> &getSpaces() const {
        assert((getKind() == SpaceKind::Constructor
                || getKind() == SpaceKind::Disjunct)
               && "Wrong kind of space tried to access subspace list");
        return Spaces;
      }

      bool getBoolValue() const {
        assert(getKind() == SpaceKind::BooleanConstant
                && "Wrong kind of space tried to access bool value");
        return Val;
      }

      // An optimization that computes if the difference of this space and
      // another space is empty.
      bool isSubspace(const Space &other) const {
        if (this->isEmpty()) {
          return true;
        }

        if (other.isEmpty()) {
          return false;
        }

        switch (PairSwitch(getKind(), other.getKind())) {
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Empty):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Type):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Constructor):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::BooleanConstant): {
          // (S1 | ... | Sn) <= S iff (S1 <= S) && ... && (Sn <= S)
          for (auto &space : this->getSpaces()) {
            if (!space.isSubspace(other)) {
              return false;
            }
          }
          return true;
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Type): {
          // Optimization: Are the types equal?  If so, the space is covered.
          if (this->getType()->isEqual(other.getType())) {
            return true;
          }

          // (_ : Ty1) <= (_ : Ty2) iff D(Ty1) == D(Ty2)
          if (canDecompose(this->getType())) {
            SmallVector<Space, 4> disjuncts;
            decompose(this->getType(), disjuncts);
            Space or1Space(disjuncts);
            if (or1Space.isSubspace(other)) {
              return true;
            }
          }

          if (canDecompose(other.getType())) {
            SmallVector<Space, 4> disjuncts;
            decompose(other.getType(), disjuncts);
            Space or2Space(disjuncts);
            return this->isSubspace(or2Space);
          }

          return true;
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Disjunct): {
          // (_ : Ty1) <= (S1 | ... | Sn) iff (S1 <= S) || ... || (Sn <= S)
          for (auto &dis : other.getSpaces()) {
            if (this->isSubspace(dis)) {
              return true;
            }
          }

          // (_ : Ty1) <= (S1 | ... | Sn) iff D(Ty1) <= (S1 | ... | Sn)
          if (!canDecompose(this->getType())) {
            return false;
          }
          SmallVector<Space, 4> disjuncts;
          decompose(this->getType(), disjuncts);
          Space or1Space(disjuncts);
          return or1Space.isSubspace(other);
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Constructor): {
          // (_ : Ty1) <= H(p1 | ... | pn) iff D(Ty1) <= H(p1 | ... | pn)
          SmallVector<Space, 4> disjuncts;
          decompose(this->getType(), disjuncts);
          Space or1Space(disjuncts);
          return or1Space.isSubspace(other);
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Type):
          return other.getType()->isBool();
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Constructor): {
          // Optimization: If the constructor heads don't match, subspace is
          // impossible.
          if (this->Head.compare(other.Head) != 0) {
            return false;
          }

          // Special Case: A constructor pattern may include the head but not
          // the payload patterns.  In that case the space is covered.
          if (other.getSpaces().empty()) {
            return true;
          }

          // H(a1, ..., an) <= H(b1, ..., bn) iff a1 <= b1 && ... && an <= bn
          auto i = this->getSpaces().begin();
          auto j = other.getSpaces().begin();
          for (; i != this->getSpaces().end() && j != other.getSpaces().end();
               ++i, ++j) {
            if (!(*i).isSubspace(*j)) {
              return false;
            }
          }
          return true;
        }
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Type):
          // Typechecking guaranteed this constructor is a subspace of the type.
          return true;
        PAIRCASE(SpaceKind::Constructor, SpaceKind::Disjunct):
          /// H(p1, ..., pn) <= (S1 | ... | Sn) iff (this - other) == [EMPTY]
          return this->minus(other).simplify().isEmpty();

        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Disjunct): {
          // Bool <= (S1 | ... | Sn) iff Bool <= S1 || ... || Bool <= Sn
          for (auto &param : other.getSpaces()) {
            if (this->isSubspace(param)) {
              return true;
            }
          }
          return false;
        }

        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::BooleanConstant):
          return this->getBoolValue() == other.getBoolValue();

        PAIRCASE (SpaceKind::Empty, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Constructor, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Type, SpaceKind::BooleanConstant):
            return false;

        default:
          llvm_unreachable("Uncovered pair found while computing subspaces?");
        }
      }

      // Returns the intersection of this space with another.  The intersection
      // is the largest shared subspace occupied by both arguments.
      Space intersect(const Space &other) const {
        // The intersection of an empty space is empty.
        if (this->isEmpty() || other.isEmpty()) {
          return Space();
        }

        llvm::function_ref<Space(SmallVectorImpl<Space> &)> examineDecomp
          = [&](SmallVectorImpl<Space> &decomposition) -> Space {
          if (decomposition.empty()) {
            return Space();
          } else if (decomposition.size() == 1) {
            return decomposition.front();
          }
          Space ds(decomposition);
          return ds;
        };

        switch (PairSwitch(getKind(), other.getKind())) {
          PAIRCASE (SpaceKind::Empty, SpaceKind::Disjunct):
          PAIRCASE (SpaceKind::Type, SpaceKind::Disjunct):
          PAIRCASE (SpaceKind::Constructor, SpaceKind::Disjunct):
          PAIRCASE (SpaceKind::Disjunct, SpaceKind::Disjunct):
          PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Disjunct): {
            // S & (S1 || ... || Sn) iff (S & S1) && ... && (S & Sn)
            SmallVector<Space, 4> intersectedCases;
            std::transform(other.getSpaces().begin(), other.getSpaces().end(),
                           std::back_inserter(intersectedCases),
                           [&](const Space &s) {
              return this->intersect(s);
            });
            // Optimization: Remove all empty spaces.
            SmallVector<Space, 4> filteredCases;
            std::copy_if(intersectedCases.begin(), intersectedCases.end(),
                         std::back_inserter(filteredCases),
                         [&](const Space &s) {
              return !s.isEmpty();
            });
            return examineDecomp(filteredCases);
          }

          PAIRCASE (SpaceKind::Disjunct, SpaceKind::Empty):
          PAIRCASE (SpaceKind::Disjunct, SpaceKind::Type):
          PAIRCASE (SpaceKind::Disjunct, SpaceKind::Constructor):
          PAIRCASE (SpaceKind::Disjunct, SpaceKind::BooleanConstant): {
            // (S1 || ... || Sn) & S iff (S & S1) && ... && (S & Sn)
            SmallVector<Space, 4> intersectedCases;
            std::transform(this->getSpaces().begin(), this->getSpaces().end(),
                           std::back_inserter(intersectedCases),
                           [&](const Space &s) {
              return s.intersect(other);
            });
            // Optimization: Remove all empty spaces.
            SmallVector<Space, 4> filteredCases;
            std::copy_if(intersectedCases.begin(), intersectedCases.end(),
                         std::back_inserter(filteredCases),
                         [&](const Space &s) {
              return !s.isEmpty();
            });
            return examineDecomp(filteredCases);
          }
          PAIRCASE (SpaceKind::Type, SpaceKind::Type): {
            // Optimization: The intersection of equal types is that type.
            if (this->getType()->isEqual(other.getType())) {
              return other;
            } else if (canDecompose(this->getType())) {
              SmallVector<Space, 4> spaces;
              decompose(this->getType(), spaces);
              auto decomposition = examineDecomp(spaces);
              return decomposition.intersect(other);
            } else if (canDecompose(other.getType())) {
              SmallVector<Space, 4> spaces;
              decompose(other.getType(), spaces);
              auto disjunctSp = examineDecomp(spaces);
              return this->intersect(disjunctSp);
            } else {
              return other;
            }
          }
          PAIRCASE (SpaceKind::Type, SpaceKind::Constructor): {
            if (canDecompose(this->getType())) {
              SmallVector<Space, 4> spaces;
              decompose(this->getType(), spaces);
              auto decomposition = examineDecomp(spaces);
              return decomposition.intersect(other);
            } else if (canDecompose(other.getType())) {
              SmallVector<Space, 4> spaces;
              decompose(other.getType(), spaces);
              auto disjunctSp = examineDecomp(spaces);
              return this->intersect(disjunctSp);
            } else {
              return other;
            }
          }
          PAIRCASE (SpaceKind::Constructor, SpaceKind::Type):
            return *this;
            
          PAIRCASE (SpaceKind::Constructor, SpaceKind::Constructor): {
            // Optimization: If the heads don't match, the intersection of
            // the constructor spaces is empty.
            if (this->getHead().compare(other.Head) != 0) {
              return Space();
            }

            // Special Case: A constructor pattern may include the head but not
            // the payload patterns.  In that case, the intersection is the
            // whole original space.
            if (other.getSpaces().empty()) {
              return *this;
            }

            SmallVector<Space, 4> paramSpace;
            auto i = this->getSpaces().begin();
            auto j = other.getSpaces().begin();
            for (; i != this->getSpaces().end() && j != other.getSpaces().end();
                 ++i, ++j) {
              auto intersection = (*i).intersect(*j);
              if (intersection.simplify().isEmpty()) {
                return Space();
              }
              paramSpace.push_back(intersection);
            }
            return examineDecomp(paramSpace);
          }

          PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::BooleanConstant):
            return this->getBoolValue() == other.getBoolValue() ? *this : Space();

          PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Type): {
            if (other.getType()->isBool()) {
              return this->getKind() == SpaceKind::BooleanConstant ? *this : Space();
            }

            if (canDecompose(other.getType())) {
              SmallVector<Space, 4> spaces;
              decompose(other.getType(), spaces);
              auto disjunctSp = examineDecomp(spaces);
              return this->intersect(disjunctSp);
            }
            return Space();
          }
          PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Empty):
          PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Constructor):
            return Space();

          PAIRCASE (SpaceKind::Type, SpaceKind::BooleanConstant): {
            // if (isSubType(tp2, tp1)) b
            if (canDecompose(this->getType())) {
              SmallVector<Space, 4> spaces;
              decompose(this->getType(), spaces);
              auto disjunctSp = examineDecomp(spaces);
              return disjunctSp.intersect(other);
            } else {
              return Space();
            }
          }

          PAIRCASE (SpaceKind::Empty, SpaceKind::BooleanConstant):
          PAIRCASE (SpaceKind::Constructor, SpaceKind::BooleanConstant):
            return Space();

          default:
            llvm_unreachable("Uncovered pair found while computing intersect?");
        }
      }

      // Returns the result of subtracting the other space from this space.  The
      // result is empty if the other space completely covers this space, or
      // non-empty if there were any uncovered cases.  The difference of spaces
      // is the smallest uncovered set of cases.
      Space minus(const Space &other) const {
        if (this->isEmpty()) {
          return Space();
        }

        if (other.isEmpty()) {
          return *this;
        }

        llvm::function_ref<Space(SmallVectorImpl<Space> &)> examineDecomp
          = [&](SmallVectorImpl<Space> &decomposition) -> Space {
          if (decomposition.empty()) {
            return Space();
          } else if (decomposition.size() == 1) {
            return decomposition.front();
          }
          return Space(decomposition);
        };

        switch (PairSwitch(this->getKind(), other.getKind())) {
        PAIRCASE (SpaceKind::Type, SpaceKind::Type): {
          // Optimization: Are the types equal?  If so, the space is covered.
          if (this->getType()->isEqual(other.getType())) {
            return Space();
          } else if (canDecompose(this->getType())) {
            SmallVector<Space, 4> spaces;
            this->decompose(this->getType(), spaces);
            return examineDecomp(spaces).intersect(other);
          } else if (canDecompose(other.getType())) {
            SmallVector<Space, 4> spaces;
            this->decompose(other.getType(), spaces);
            auto decomp = examineDecomp(spaces);
            return this->intersect(decomp);
          }
          return Space();
        }
        PAIRCASE (SpaceKind::Type, SpaceKind::Constructor): {
          if (canDecompose(this->getType())) {
            SmallVector<Space, 4> spaces;
            this->decompose(this->getType(), spaces);
            auto decomp = examineDecomp(spaces);
            return decomp.minus(other);
          } else {
            return *this;
          }
        }
        PAIRCASE (SpaceKind::Empty, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Type, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Disjunct):
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Disjunct): {
          return std::accumulate(other.getSpaces().begin(),
                                 other.getSpaces().end(),
                                 *this,
                                 [](const Space &left, const Space &right){
            return left.minus(right);
          });
        }

        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Empty):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Type):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::Constructor):
        PAIRCASE (SpaceKind::Disjunct, SpaceKind::BooleanConstant): {
          SmallVector<Space, 4> smallSpaces;
          std::transform(this->getSpaces().begin(), this->getSpaces().end(),
                         std::back_inserter(smallSpaces),
                         [&](const Space &first){
            return first.minus(other);
          });
          return examineDecomp(smallSpaces);
        }
        PAIRCASE (SpaceKind::Constructor, SpaceKind::Type):
          return Space();

        PAIRCASE (SpaceKind::Constructor, SpaceKind::Constructor): {
          // Optimization: If the heads of the constructors don't match then
          // the two are disjoint and their difference is the first space.
          if (this->Head.compare(other.Head) != 0) {
            return *this;
          }

          // Special Case: A constructor pattern may include the head but not
          // the payload patterns.  In that case, because the heads match, it
          // covers the whole space.
          if (other.getSpaces().empty()) {
            return Space();
          }

          SmallVector<Space, 4> constrSpaces;
          bool foundBad = false;
          auto i = this->getSpaces().begin();
          auto j = other.getSpaces().begin();
          for (auto idx = 0;
               i != this->getSpaces().end() && j != other.getSpaces().end();
               ++i, ++j, ++idx) {
            auto &s1 = *i;
            auto &s2 = *j;
            // If the intersection of each subspace is ever empty then the
            // two spaces are disjoint and their difference is the first space.
            if (s1.intersect(s2).simplify().isEmpty()) {
              return *this;
            }

            // If one constructor parameter doens't cover the other then we've
            // got to report the uncovered cases in a user-friendly way.
            if (!s1.isSubspace(s2)) {
              foundBad = true;
            }
            // Copy the params and replace the parameter at each index with the
            // difference of the two spaces.  This unpacks one constructor head
            // into each parameter.
            SmallVector<Space, 4> copyParams(this->getSpaces().begin(),
                                             this->getSpaces().end());
            copyParams[idx] = s1.minus(s2);
            Space CS(this->getType(), this->Head, copyParams);
            constrSpaces.push_back(CS);
          }
          if (foundBad) {
            return examineDecomp(constrSpaces);
          }
          return Space();
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::BooleanConstant): {
          // The difference of boolean constants depends on their values.
          if (this->getBoolValue() == other.getBoolValue()) {
            return Space();
          } else {
            return *this;
          }
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Type): {
          if (other.getType()->isBool()) {
            return (getKind() == SpaceKind::BooleanConstant)  ? Space() : *this;
          }

          if (canDecompose(other.getType())) {
            SmallVector<Space, 4> spaces;
            this->decompose(other.getType(), spaces);
            auto disjunctSp = examineDecomp(spaces);
            return this->minus(disjunctSp);
          }
          return *this;
        }
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Empty):
        PAIRCASE (SpaceKind::BooleanConstant, SpaceKind::Constructor):
          return *this;

        PAIRCASE (SpaceKind::Type, SpaceKind::BooleanConstant): {
          if (canDecompose(this->getType())) {
            SmallVector<Space, 4> spaces;
            this->decompose(this->getType(), spaces);
            auto orSpace = examineDecomp(spaces);
            return orSpace.minus(other);
          } else {
            return *this;
          }
        }

        PAIRCASE (SpaceKind::Empty, SpaceKind::BooleanConstant):
        PAIRCASE (SpaceKind::Constructor, SpaceKind::BooleanConstant):
          return Space();
        default:
          llvm_unreachable("Uncovered pair found while computing difference?");
        }
      }

      void show(llvm::raw_ostream &buffer, bool normalize = true) const {
        switch (getKind()) {
        case SpaceKind::Empty:
          buffer << "[EMPTY]";
          break;
        case SpaceKind::Disjunct: {
          if (normalize) {
            return simplify().show(buffer, false);
          } else {
            buffer << "DISJOIN(";
            for (auto &sp : Spaces) {
              buffer << "\n";
              sp.show(buffer, normalize);
              buffer << " |";
            }
            buffer << ")";
          }
        }
          break;
        case SpaceKind::BooleanConstant:
          buffer << ((Val) ? "true" : "false");
          break;
        case SpaceKind::Constructor: {
          if (!Head.empty()) {
            buffer << ".";
            buffer << Head.str();
          }

          if (Spaces.empty()) {
            return;
          }

          buffer << "(";
          bool first = true;
          for (auto &param : Spaces) {
            if (!first) {
              buffer << ", ";
            }
            if (normalize) {
              param.simplify().show(buffer, normalize);
            } else {
              param.show(buffer);
            }
            if (first) {
              first = false;
            }
          }
          buffer << ")";
        }
          break;
        case SpaceKind::Type:
          if (!normalize) {
            Ty->print(buffer);
          }
          buffer << "_";
          break;
        }
      }

      // For optimization, attempt to simplify a space by removing any empty
      // cases and unpacking empty or singular disjunctions where possible.
      Space simplify() const {
        switch (getKind()) {
        case SpaceKind::Constructor: {
          // If a constructor has no spaces it is an enum without a payload and
          // cannot be optimized further.
          if (getSpaces().empty()) {
            return *this;
          }

          // Simplify each component subspace.  If, after simplification, any
          // subspace contains an empty, then the whole space is empty.
          SmallVector<Space, 4> simplifiedSpaces;
          std::transform(getSpaces().begin(), getSpaces().end(),
                         std::back_inserter(simplifiedSpaces),
                         [](const Space &el) {
            return el.simplify();
          });
          for (auto &el : simplifiedSpaces) {
            if (el.isEmpty()) {
              return Space();
            }
          }
          return Space(Ty, Head, simplifiedSpaces);
        }
        case SpaceKind::Type: {
          // If the decomposition of a space is empty, the space is empty.
          if (canDecompose(Ty)) {
            SmallVector<Space, 4> ss;
            decompose(Ty, ss);
            if (ss.empty()) {
              return Space();
            }
            return *this;
          } else {
            return *this;
          }
        }
        case SpaceKind::Disjunct: {
          // Simplify each disjunct.
          SmallVector<Space, 4> simplifiedSpaces;
          std::transform(Spaces.begin(), Spaces.end(),
                         std::back_inserter(simplifiedSpaces),
                         [](const Space &el){
            return el.simplify();
          });
          // If the disjunct is singular, unpack it into its component.
          if (simplifiedSpaces.size() == 1) {
            return simplifiedSpaces.front();
          }

          // Otherwise, remove any empties.
          SmallVector<Space, 4> compatifiedSpaces;
          std::copy_if(simplifiedSpaces.begin(), simplifiedSpaces.end(),
                       std::back_inserter(compatifiedSpaces),
                       [&](const Space &el) {
            return !el.isEmpty();
          });
          // If the disjunct was all empty, the space is empty.
          if (compatifiedSpaces.empty()) {
            return Space();
          }
          // Else if the disjunct is singular, unpack it into its component.
          if (compatifiedSpaces.size() == 1) {
            return compatifiedSpaces.front();
          }
          return Space(compatifiedSpaces);
        }
        default:
          return *this;
        }
      }

    private:
      // Decompose a type into its component spaces.
      void decompose(Type tp, SmallVectorImpl<Space> &arr) const {
        assert(canDecompose(tp) && "Non-decomposable type?");

        if (tp->isBool()) {
          arr.push_back(Space(true));
          arr.push_back(Space(false));
        } else if (auto *E = tp->getEnumOrBoundGenericEnum()) {
          // Look into each case of the enum and decompose it in turn.
          auto children = E->getAllElements();
          std::transform(children.begin(), children.end(),
                         std::back_inserter(arr), [&](EnumElementDecl *eed) {
            auto eedTy = tp->getCanonicalType()
                           ->getTypeOfMember(E->getModuleContext(), eed,
                                             eed->getArgumentInterfaceType());
            SmallVector<Space, 4> constElemSpaces;
            if (eedTy) {
              if (auto *TTy = eedTy->getAs<TupleType>()) {
                // Decompose the payload tuple into its component type spaces.
                std::transform(TTy->getElements().begin(),
                               TTy->getElements().end(),
                               std::back_inserter(constElemSpaces),
                               [&](TupleTypeElt ty){
                                 return Space(ty.getType());
                               });
              } else if (auto *TTy = dyn_cast<ParenType>(eedTy.getPointer())) {
                constElemSpaces.push_back(Space(TTy->getUnderlyingType()));
              }
            }
            return Space(tp, eed->getName(), constElemSpaces);
          });
        } else if (auto *TTy = tp->castTo<TupleType>()) {
          // Decompose each of the elements into its component type space.
          SmallVector<Space, 4> constElemSpaces;
          std::transform(TTy->getElements().begin(), TTy->getElements().end(),
                         std::back_inserter(constElemSpaces),
                         [&](TupleTypeElt ty){
            return Space(ty.getType());
          });
          // Create an empty constructor head for the tuple space.
          arr.push_back(Space(tp, Identifier(), constElemSpaces));
        } else {
          llvm_unreachable("Can't decompose type?");
        }
      }

      static bool canDecompose(Type tp) {
        return tp->is<TupleType>() || tp->isBool()
            || tp->getEnumOrBoundGenericEnum();
      }
    };

    StmtChecker &S;
    SwitchStmt *Switch;

    SpaceEngine(StmtChecker &SC, SwitchStmt *SS) : S(SC), Switch(SS) {}

    void checkExhaustiveness() {
      ASTContext &ctx = S.TC.Context;
      SmallVector<Space, 4> spaces;
      for (unsigned i = 0, e = Switch->getCases().size(); i < e; ++i) {
        auto *caseBlock = Switch->getCases()[i];
        for (auto &caseItem : caseBlock->getCaseLabelItems()) {
          auto projection = projectPattern(ctx, caseItem.getPattern());
          spaces.push_back(projection);

          // Space is trivially covered with a default clause.
          if (caseItem.isDefault()) {
            return;
          }
        }
      }

      Space totalSpace(Switch->getSubjectExpr()->getType());
      Space coveredSpace(spaces);
      auto uncovered = totalSpace.minus(coveredSpace).simplify();
      if (uncovered.isEmpty()) {
        return;
      }

      // If the entire space is left uncovered, we just need a default.
      if (uncovered.getKind() == SpaceKind::Type) {
        diagnoseMissingCases(ctx, Switch, /*justNeedsDefault*/ true, Space());
        return;
      }

      // If the space isn't a disjunct then make it one.
      if (uncovered.getKind() != SpaceKind::Disjunct) {
        SmallVector<Space, 1> spaces = { uncovered };
        uncovered = Space(spaces);
      }

      diagnoseMissingCases(ctx, Switch, /*justNeedsDefault*/ false, uncovered);
    }

    static void diagnoseMissingCases(ASTContext &Ctx, const SwitchStmt *Switch,
                                     bool JustNeedsDefault,
                                     SpaceEngine::Space uncovered) {
      bool Empty = Switch->getCases().empty();
      SourceLoc StartLoc = Switch->getStartLoc();
      SourceLoc EndLoc = Switch->getEndLoc();
      StringRef Placeholder = getCodePlaceholder();
      llvm::SmallString<128> Buffer;
      llvm::raw_svector_ostream OS(Buffer);

      bool InEditor = Ctx.LangOpts.DiagnosticsEditorMode;

      if (JustNeedsDefault) {
        OS << tok::kw_default << ": " << Placeholder << "\n";
        if (Empty) {
          Ctx.Diags.diagnose(StartLoc, diag::empty_switch_stmt)
             .fixItInsert(EndLoc, Buffer.str());
        } else {
          auto Diag = Ctx.Diags.diagnose(StartLoc, diag::non_exhaustive_switch,
                                         InEditor, uncovered.isEmpty(), "");
          Diag.fixItInsert(EndLoc, Buffer.str());
        }
        return;
      }

      // If there's nothing else to diagnose, bail.
      if (uncovered.isEmpty()) return;

      // If editing is enabled, emit a formatted error of the form:
      //
      // switch must be exhaustive, do you want to add missing cases?
      //     (.none, .some(_)): <#code#>
      //     (.some(_), .none): <#code#>
      //
      // else:
      //
      // switch must be exhaustive, consider adding missing cases:
      //
      //     (.none, .some(_))
      //     (.some(_), .none)
      Buffer.clear();
      OS << "\n";
      for (auto &uncoveredSpace : uncovered.getSpaces()) {
        SmallVector<Space, 4> flats;
        flatten(uncoveredSpace, flats);
        if (flats.empty()) {
          flats.append({ uncoveredSpace });
        }
        for (size_t i = 0; i < flats.size(); ++i) {
          auto &FlatSpace = flats[i];
          if (InEditor) {
            OS << tok::kw_case << " ";
          }
          FlatSpace.show(OS);
          if (InEditor) {
            OS << ": " << Placeholder;
          }
          OS << "\n";
        }
      }

      if (InEditor) {
        auto Diag = Ctx.Diags.diagnose(StartLoc, diag::non_exhaustive_switch,
                                       InEditor, false, Buffer.str());
        Diag.fixItInsert(EndLoc, Buffer.str());
      } else {
        Ctx.Diags.diagnose(StartLoc, diag::non_exhaustive_switch,
                           InEditor, false, Buffer.str());
      }
    }

  private:
    // Recursively unpacks a space of disjunctions or constructor parameters
    // into its component parts such that the resulting array of flattened
    // spaces contains no further disjunctions.  If there were no disjunctions
    // in the starting space, the original space is already flat and the
    // returned array of spaces will be empty.
    static void flatten(const Space space, SmallVectorImpl<Space> &flats) {
      switch (space.getKind()) {
      case SpaceKind::Constructor: {
        size_t i = 0;
        for (auto &param : space.getSpaces()) {
          // We're only interested in recursively unpacking constructors and
          // disjunctions (booleans are considered constructors).
          if (param.getKind() != SpaceKind::Constructor
              || param.getKind() != SpaceKind::Disjunct
              || param.getKind() != SpaceKind::BooleanConstant)
            continue;

          SmallVector<Space, 4> flattenedParams;
          flatten(param, flattenedParams);
          for (auto &space : flattenedParams) {
            SmallVector<Space, 4> row(space.getSpaces().begin(),
                                      space.getSpaces().end());
            row[i] = space;
            Space cs(space.getType(), space.getHead(), row);
            flats.push_back(cs);
          }
          ++i;
        }
      }
        break;
      case SpaceKind::Disjunct: {
        auto begin = space.getSpaces().begin();
        auto end = space.getSpaces().end();
        for (; begin != end; ++begin) {
          flatten(*begin, flats);
        }
      }
        break;
      default:
        flats.push_back(space);
        break;
      }
    }

    // Recursively project a pattern into a Space.
    static Space projectPattern(ASTContext &Ctx, const Pattern *item) {
      switch (item->getKind()) {
      case swift::PatternKind::Any:
      case swift::PatternKind::Named:
        return Space(item->getType());
      case swift::PatternKind::Bool: {
        auto *BP = cast<BoolPattern>(item);
        return Space(BP->getValue());
      }
      case swift::PatternKind::Is:
      case swift::PatternKind::Expr:
        return Space();
      case swift::PatternKind::Var: {
        auto *VP = cast<VarPattern>(item);
        return projectPattern(Ctx, VP->getSubPattern());
      }
      case swift::PatternKind::Paren: {
        auto *PP = cast<ParenPattern>(item);
        return projectPattern(Ctx, PP->getSubPattern());
      }
      case swift::PatternKind::OptionalSome: {
        auto *OSP = cast<OptionalSomePattern>(item);
        SmallVector<Space, 1> payload = {
          projectPattern(Ctx, OSP->getSubPattern())
        };
        return Space(item->getType(), Ctx.getIdentifier("some"), payload);
      }
      case swift::PatternKind::EnumElement: {
        auto *VP = cast<EnumElementPattern>(item);
        SmallVector<Space, 4> conArgSpace;
        if (auto *SP = VP->getSubPattern()) {
          if (auto *TP = dyn_cast<TuplePattern>(SP)) {
            std::transform(TP->getElements().begin(), TP->getElements().end(),
                           std::back_inserter(conArgSpace),
                           [&](TuplePatternElt pate) {
              return projectPattern(Ctx, pate.getPattern());
            });
          } else {
            auto *PP = dyn_cast<ParenPattern>(SP);
            conArgSpace.push_back(projectPattern(Ctx, PP->getSubPattern()));
          }
        }
        return Space(item->getType(), VP->getName(), conArgSpace);
      }
      case swift::PatternKind::Tuple: {
        auto *TP = cast<TuplePattern>(item);
        SmallVector<Space, 4> conArgSpace;
        std::transform(TP->getElements().begin(), TP->getElements().end(),
                       std::back_inserter(conArgSpace),
                       [&](TuplePatternElt pate) {
          return projectPattern(Ctx, pate.getPattern());
        });
        return Space(item->getType(), Identifier(), conArgSpace);
      }
      case swift::PatternKind::Typed:
        llvm_unreachable("Typed pattern in switch?");
      }
    }
  };

  struct AddLabeledStmt {
    StmtChecker &SC;
    AddLabeledStmt(StmtChecker &SC, LabeledStmt *LS) : SC(SC) {
      // Verify that we don't have label shadowing.
      if (!LS->getLabelInfo().Name.empty())
        for (auto PrevLS : SC.ActiveLabeledStmts) {
          if (PrevLS->getLabelInfo().Name == LS->getLabelInfo().Name) {
            SC.TC.diagnose(LS->getLabelInfo().Loc,
                        diag::label_shadowed, LS->getLabelInfo().Name);
            SC.TC.diagnose(PrevLS->getLabelInfo().Loc,
                           diag::invalid_redecl_prev, 
                           PrevLS->getLabelInfo().Name);
          }
        }

      // In any case, remember that we're in this labeled statement so that
      // break and continue are aware of it.
      SC.ActiveLabeledStmts.push_back(LS);
    }
    ~AddLabeledStmt() {
      SC.ActiveLabeledStmts.pop_back();
    }
  };
  
  struct AddSwitchNest {
    StmtChecker &SC;
    CaseStmt *OuterFallthroughDest;
    AddSwitchNest(StmtChecker &SC) : SC(SC),
        OuterFallthroughDest(SC.FallthroughDest) {
      ++SC.SwitchLevel;
    }
    
    ~AddSwitchNest() {
      --SC.SwitchLevel;
      SC.FallthroughDest = OuterFallthroughDest;
    }
  };

  StmtChecker(TypeChecker &TC, AbstractFunctionDecl *AFD)
    : TC(TC), TheFunc(AFD), DC(AFD), IsREPL(false) { }

  StmtChecker(TypeChecker &TC, ClosureExpr *TheClosure)
    : TC(TC), TheFunc(TheClosure), DC(TheClosure), IsREPL(false) { }

  StmtChecker(TypeChecker &TC, DeclContext *DC)
    : TC(TC), TheFunc(), DC(DC), IsREPL(false) {
    if (const SourceFile *SF = DC->getParentSourceFile())
      if (SF->Kind == SourceFileKind::REPL)
        IsREPL = true;
  }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//
  
  bool isInDefer() const {
    if (!TheFunc.hasValue()) return false;
    auto *FD = dyn_cast_or_null<FuncDecl>
      (TheFunc.getValue().getAbstractFunctionDecl());
    return FD && FD->isDeferBody();
  }
  
  template<typename StmtTy>
  bool typeCheckStmt(StmtTy *&S) {
    StmtTy *S2 = cast_or_null<StmtTy>(visit(S));
    if (S2 == nullptr)
      return true;
    S = S2;
    performStmtDiagnostics(TC, S);
    return false;
  }

  /// Type-check an entire function body.
  bool typeCheckBody(BraceStmt *&S) {
    typeCheckStmt(S);
    setAutoClosureDiscriminators(DC, S);
    return false;
  }
  
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  Stmt *visitBraceStmt(BraceStmt *BS);

  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (!TheFunc.hasValue()) {
      TC.diagnose(RS->getReturnLoc(), diag::return_invalid_outside_func);
      return nullptr;
    }
    
    // If the return is in a defer, then it isn't valid either.
    if (isInDefer()) {
      TC.diagnose(RS->getReturnLoc(), diag::jump_out_of_defer, "return");
      return nullptr;
    }

    Type ResultTy = TheFunc->getBodyResultType();
    if (!ResultTy || ResultTy->hasError())
      return nullptr;

    if (!RS->hasResult()) {
      if (!ResultTy->isVoid())
        TC.diagnose(RS->getReturnLoc(), diag::return_expr_missing);
      return RS;
    }

    Expr *E = RS->getResult();

    // In an initializer, the only expression allowed is "nil", which indicates
    // failure from a failable initializer.
    if (auto ctor = dyn_cast_or_null<ConstructorDecl>(
                      TheFunc->getAbstractFunctionDecl())) {
      // The only valid return expression in an initializer is the literal
      // 'nil'.
      auto nilExpr = dyn_cast<NilLiteralExpr>(E->getSemanticsProvidingExpr());
      if (!nilExpr) {
        TC.diagnose(RS->getReturnLoc(), diag::return_init_non_nil)
          .highlight(E->getSourceRange());
        RS->setResult(nullptr);
        return RS;
      }

      // "return nil" is only permitted in a failable initializer.
      if (ctor->getFailability() == OTK_None) {
        TC.diagnose(RS->getReturnLoc(), diag::return_non_failable_init)
          .highlight(E->getSourceRange());
        TC.diagnose(ctor->getLoc(), diag::make_init_failable,
                    ctor->getFullName())
          .fixItInsertAfter(ctor->getLoc(), "?");
        RS->setResult(nullptr);
        return RS;
      }

      // Replace the "return nil" with a new 'fail' statement.
      return new (TC.Context) FailStmt(RS->getReturnLoc(), nilExpr->getLoc(),
                                       RS->isImplicit());
    }

    auto hadTypeError = TC.typeCheckExpression(E, DC,
                                               TypeLoc::withoutLoc(ResultTy),
                                               CTP_ReturnStmt);
    RS->setResult(E);
    
    if (hadTypeError) {
      tryDiagnoseUnnecessaryCastOverOptionSet(TC.Context, E, ResultTy,
                                              DC->getParentModule());
    }
    if (auto DRE = dyn_cast<DeclRefExpr>(E))
      if (auto FD = dyn_cast<FuncDecl>(DRE->getDecl()))
        TC.addEscapingFunctionAsReturnValue(FD, RS);
    return RS;
  }
  
  Stmt *visitThrowStmt(ThrowStmt *TS) {
    // Coerce the operand to the exception type.
    auto E = TS->getSubExpr();

    Type exnType = TC.getExceptionType(DC, TS->getThrowLoc());
    if (!exnType) return TS;
    
    TC.typeCheckExpression(E, DC, TypeLoc::withoutLoc(exnType), CTP_ThrowStmt);
    TS->setSubExpr(E);
    
    return TS;
  }
    
  Stmt *visitDeferStmt(DeferStmt *DS) {
    TC.typeCheckDecl(DS->getTempDecl(), /*isFirstPass*/false);

    Expr *theCall = DS->getCallExpr();
    TC.typeCheckExpression(theCall, DC);
    DS->setCallExpr(theCall);
    
    return DS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    StmtCondition C = IS->getCond();
    TC.typeCheckStmtCondition(C, DC, diag::if_always_true);
    IS->setCond(C);

    AddLabeledStmt ifNest(*this, IS);

    Stmt *S = IS->getThenStmt();
    typeCheckStmt(S);
    IS->setThenStmt(S);

    if ((S = IS->getElseStmt())) {
      typeCheckStmt(S);
      IS->setElseStmt(S);
    }
    
    return IS;
  }
  
  Stmt *visitGuardStmt(GuardStmt *GS) {
    StmtCondition C = GS->getCond();
    TC.typeCheckStmtCondition(C, DC, diag::guard_always_succeeds);
    GS->setCond(C);
    
    AddLabeledStmt ifNest(*this, GS);
    
    Stmt *S = GS->getBody();
    typeCheckStmt(S);
    GS->setBody(S);
    return GS;
  }

  Stmt *visitIfConfigStmt(IfConfigStmt *ICS) {
    
    // Active members are attached to the enclosing declaration, so there's no
    // need to walk anything within.
    
    return ICS;
  }

  Stmt *visitDoStmt(DoStmt *DS) {
    AddLabeledStmt loopNest(*this, DS);
    Stmt *S = DS->getBody();
    typeCheckStmt(S);
    DS->setBody(S);
    return DS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    StmtCondition C = WS->getCond();
    TC.typeCheckStmtCondition(C, DC, diag::while_always_true);
    WS->setCond(C);

    AddLabeledStmt loopNest(*this, WS);
    Stmt *S = WS->getBody();
    typeCheckStmt(S);
    WS->setBody(S);
    
    return WS;
  }
  Stmt *visitRepeatWhileStmt(RepeatWhileStmt *RWS) {
    {
      AddLabeledStmt loopNest(*this, RWS);
      Stmt *S = RWS->getBody();
      typeCheckStmt(S);
      RWS->setBody(S);
    }
    
    Expr *E = RWS->getCond();
    TC.typeCheckCondition(E, DC);
    RWS->setCond(E);
    return RWS;
  }
  Stmt *visitForStmt(ForStmt *FS) {
    // Type check any var decls in the initializer.
    for (auto D : FS->getInitializerVarDecls())
      TC.typeCheckDecl(D, /*isFirstPass*/false);

    if (auto *Initializer = FS->getInitializer().getPtrOrNull()) {
      TC.typeCheckExpression(Initializer, DC, TypeLoc(), CTP_Unused,
                             TypeCheckExprFlags::IsDiscarded);
      FS->setInitializer(Initializer);
      TC.checkIgnoredExpr(Initializer);
    }

    if (auto *Cond = FS->getCond().getPtrOrNull()) {
      TC.typeCheckCondition(Cond, DC);
      FS->setCond(Cond);
    }

    if (auto *Increment = FS->getIncrement().getPtrOrNull()) {
      TC.typeCheckExpression(Increment, DC, TypeLoc(), CTP_Unused,
                             TypeCheckExprFlags::IsDiscarded);
      FS->setIncrement(Increment);
      TC.checkIgnoredExpr(Increment);
    }

    AddLabeledStmt loopNest(*this, FS);
    Stmt *S = FS->getBody();
    typeCheckStmt(S);
    FS->setBody(S);
    
    return FS;
  }
  
  Stmt *visitForEachStmt(ForEachStmt *S) {
    TypeResolutionOptions options;
    options |= TR_AllowUnspecifiedTypes;
    options |= TR_AllowUnboundGenerics;
    options |= TR_InExpression;
    
    if (auto *P = TC.resolvePattern(S->getPattern(), DC,
                                    /*isStmtCondition*/false)) {
      S->setPattern(P);
    } else {
      S->getPattern()->setType(ErrorType::get(TC.Context));
      return nullptr;
    }
  
    if (TC.typeCheckPattern(S->getPattern(), DC, options)) {
      // FIXME: Handle errors better.
      S->getPattern()->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    if (TC.typeCheckForEachBinding(DC, S))
      return nullptr;

    if (auto *Where = S->getWhere()) {
      if (TC.typeCheckCondition(Where, DC))
        return nullptr;
      S->setWhere(Where);
    }


    // Retrieve the 'Sequence' protocol.
    ProtocolDecl *sequenceProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::Sequence);
    if (!sequenceProto) {
      return nullptr;
    }

    // Retrieve the 'Iterator' protocol.
    ProtocolDecl *generatorProto
      = TC.getProtocol(S->getForLoc(), KnownProtocolKind::IteratorProtocol);
    if (!generatorProto) {
      return nullptr;
    }
    
    // If the sequence is an implicitly unwrapped optional, force it.
    Expr *sequence = S->getSequence();
    if (auto objectTy
          = sequence->getType()->getImplicitlyUnwrappedOptionalObjectType()) {
      sequence = new (TC.Context) ForceValueExpr(sequence,
                                                 sequence->getEndLoc());
      sequence->setType(objectTy);
      sequence->setImplicit();
      S->setSequence(sequence);
    }

    // Invoke iterator() to get an iterator from the sequence.
    Type generatorTy;
    VarDecl *generator;
    {
      Type sequenceType = sequence->getType();
      auto conformance =
        TC.conformsToProtocol(sequenceType, sequenceProto, DC,
                              ConformanceCheckFlags::InExpression,
                              sequence->getLoc());
      if (!conformance)
        return nullptr;
      
      generatorTy = TC.getWitnessType(sequenceType, sequenceProto,
                                      *conformance,
                                      TC.Context.Id_Iterator,
                                      diag::sequence_protocol_broken);
      
      Expr *getIterator
        = TC.callWitness(sequence, DC, sequenceProto, *conformance,
                         TC.Context.Id_makeIterator,
                         {}, diag::sequence_protocol_broken);
      if (!getIterator) return nullptr;

      // Create a local variable to capture the generator.
      std::string name;
      if (auto np = dyn_cast_or_null<NamedPattern>(S->getPattern()))
        name = "$"+np->getBoundName().str().str();
      name += "$generator";
      generator = new (TC.Context)
        VarDecl(/*IsStatic*/false, /*IsLet*/false, /*IsCaptureList*/false,
                S->getInLoc(), TC.Context.getIdentifier(name), generatorTy, DC);
      generator->setInterfaceType(DC->mapTypeOutOfContext(generatorTy));
      generator->setImplicit();

      // Create a pattern binding to initialize the generator.
      auto genPat = new (TC.Context) NamedPattern(generator);
      genPat->setImplicit();
      auto genBinding =
          PatternBindingDecl::create(TC.Context, SourceLoc(),
                                     StaticSpellingKind::None,
                                     S->getForLoc(), genPat, getIterator, DC);
      genBinding->setImplicit();
      S->setIterator(genBinding);
    }

    // Working with generators requires Optional.
    if (TC.requireOptionalIntrinsics(S->getForLoc()))
      return nullptr;

    // Gather the witnesses from the Iterator protocol conformance, which
    // we'll use to drive the loop.
    // FIXME: Would like to customize the diagnostic emitted in
    // conformsToProtocol().
    auto genConformance =
      TC.conformsToProtocol(generatorTy, generatorProto, DC,
                            ConformanceCheckFlags::InExpression,
                            sequence->getLoc());
    if (!genConformance)
      return nullptr;
    
    Type elementTy = TC.getWitnessType(generatorTy, generatorProto,
                                       *genConformance, TC.Context.Id_Element,
                                       diag::iterator_protocol_broken);
    if (!elementTy)
      return nullptr;
    
    // Compute the expression that advances the generator.
    Expr *iteratorNext
      = TC.callWitness(TC.buildCheckedRefExpr(generator, DC,
                                              DeclNameLoc(S->getInLoc()),
                                              /*implicit*/true),
                       DC, generatorProto, *genConformance,
                       TC.Context.Id_next, {}, diag::iterator_protocol_broken);
    if (!iteratorNext) return nullptr;
    // Check that next() produces an Optional<T> value.
    if (iteratorNext->getType()->getAnyNominal()
          != TC.Context.getOptionalDecl()) {
      TC.diagnose(S->getForLoc(), diag::iterator_protocol_broken);
      return nullptr;
    }

    // Convert that Optional<T> value to Optional<Element>.
    auto optPatternType = OptionalType::get(S->getPattern()->getType());
    if (!optPatternType->isEqual(iteratorNext->getType()) &&
        TC.convertToType(iteratorNext, optPatternType, DC, S->getPattern())) {
      return nullptr;
    }

    S->setIteratorNext(iteratorNext);
    
    // Type-check the body of the loop.
    AddLabeledStmt loopNest(*this, S);
    BraceStmt *Body = S->getBody();
    typeCheckStmt(Body);
    S->setBody(Body);
    
    return S;
  }

  Stmt *visitBreakStmt(BreakStmt *S) {
    LabeledStmt *Target = nullptr;
    // Pick the nearest break target that matches the specified name.
    if (S->getTargetName().empty()) {
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        // 'break' with no label looks through non-loop structures
        // except 'switch'.
        if (!(*I)->requiresLabelOnJump()) {
          Target = *I;
          break;
        }
      }

    } else {
      // Scan inside out until we find something with the right label.
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        if (S->getTargetName() == (*I)->getLabelInfo().Name) {
          Target = *I;
          break;
        }
      }
    }
    
    if (!Target) {
      // If we're in a defer, produce a tailored diagnostic.
      if (isInDefer()) {
        TC.diagnose(S->getLoc(), diag::jump_out_of_defer, "break");
        return nullptr;
      }
      
      auto diagid = diag::break_outside_loop;

      // If someone is using an unlabeled break inside of an 'if' or 'do'
      // statement, produce a more specific error.
      if (S->getTargetName().empty() &&
          std::any_of(ActiveLabeledStmts.rbegin(),
                      ActiveLabeledStmts.rend(),
                      [&](Stmt *S) -> bool {
                        return isa<IfStmt>(S) || isa<DoStmt>(S);
                      })) {
        diagid = diag::unlabeled_break_outside_loop;
      }

      TC.diagnose(S->getLoc(), diagid);
      return nullptr;
    }
    S->setTarget(Target);
    return S;
  }

  Stmt *visitContinueStmt(ContinueStmt *S) {
    LabeledStmt *Target = nullptr;
    // Scan to see if we are in any non-switch labeled statements (loops).  Scan
    // inside out.
    if (S->getTargetName().empty()) {
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        // 'continue' with no label ignores non-loop structures.
        if (!(*I)->requiresLabelOnJump() &&
            (*I)->isPossibleContinueTarget()) {
          Target = *I;
          break;
        }
      }
    } else {
      // Scan inside out until we find something with the right label.
      for (auto I = ActiveLabeledStmts.rbegin(), E = ActiveLabeledStmts.rend();
           I != E; ++I) {
        if (S->getTargetName() == (*I)->getLabelInfo().Name) {
          Target = *I;
          break;
        }
      }
    }

    if (!Target) {
      // If we're in a defer, produce a tailored diagnostic.
      if (isInDefer()) {
        TC.diagnose(S->getLoc(), diag::jump_out_of_defer, "break");
        return nullptr;
      }

      TC.diagnose(S->getLoc(), diag::continue_outside_loop);
      return nullptr;
    }

    // Continue cannot be used to repeat switches, use fallthrough instead.
    if (!Target->isPossibleContinueTarget()) {
      TC.diagnose(S->getLoc(), diag::continue_not_in_this_stmt,
                  isa<SwitchStmt>(Target) ? "switch" : "if");
      return nullptr;
    }

    S->setTarget(Target);
    return S;
  }
  
  Stmt *visitFallthroughStmt(FallthroughStmt *S) {
    if (!SwitchLevel) {
      TC.diagnose(S->getLoc(), diag::fallthrough_outside_switch);
      return nullptr;
    }
    if (!FallthroughDest) {
      TC.diagnose(S->getLoc(), diag::fallthrough_from_last_case);
      return nullptr;
    }
    if (FallthroughDest->hasBoundDecls())
      TC.diagnose(S->getLoc(), diag::fallthrough_into_case_with_var_binding);
    S->setFallthroughDest(FallthroughDest);
    return S;
  }
  
  Stmt *visitSwitchStmt(SwitchStmt *S) {
    bool hadError = false;

    // Type-check the subject expression.
    Expr *subjectExpr = S->getSubjectExpr();
    hadError |= TC.typeCheckExpression(subjectExpr, DC);
    if (Expr *newSubjectExpr = TC.coerceToMaterializable(subjectExpr))
      subjectExpr = newSubjectExpr;
    S->setSubjectExpr(subjectExpr);
    Type subjectType = S->getSubjectExpr()->getType();

    // Type-check the case blocks.
    AddSwitchNest switchNest(*this);
    AddLabeledStmt labelNest(*this, S);

    // Reject switch statements with empty blocks.
    if (S->getCases().empty())
      SpaceEngine::diagnoseMissingCases(TC.Context, S, /*justNeedsDefault*/ true,
                                        SpaceEngine::Space());
    for (unsigned i = 0, e = S->getCases().size(); i < e; ++i) {
      auto *caseBlock = S->getCases()[i];
      // Fallthrough transfers control to the next case block. In the
      // final case block, it is invalid.
      FallthroughDest = i+1 == e ? nullptr : S->getCases()[i+1];

      for (auto &labelItem : caseBlock->getMutableCaseLabelItems()) {
        // Resolve the pattern in the label.
        Pattern *pattern = labelItem.getPattern();
        if (auto *newPattern = TC.resolvePattern(pattern, DC,
                                                 /*isStmtCondition*/false)) {
          pattern = newPattern;
          // Coerce the pattern to the subject's type.
          if (TC.coercePatternToType(pattern, DC, subjectType,
                                     TR_InExpression)) {
            hadError = true;

            // If that failed, mark any variables binding pieces of the pattern
            // as invalid to silence follow-on errors.
            pattern->forEachVariable([&](VarDecl *VD) {
              VD->markInvalid();
            });
          }
          labelItem.setPattern(pattern);
          
          // For each variable in the pattern, make sure its type is identical to what it
          // was in the first label item's pattern.
          auto firstPattern = caseBlock->getCaseLabelItems()[0].getPattern();
          if (pattern != firstPattern) {
            SmallVector<VarDecl *, 4> Vars;
            firstPattern->collectVariables(Vars);
            pattern->forEachVariable([&](VarDecl *VD) {
              if (!VD->hasName())
                return;
              for (auto expected : Vars) {
                if (expected->hasName() && expected->getName() == VD->getName()) {
                  if (!VD->getType()->isEqual(expected->getType())) {
                    TC.diagnose(VD->getLoc(), diag::type_mismatch_multiple_pattern_list,
                                VD->getType(), expected->getType());
                    VD->markInvalid();
                    expected->markInvalid();
                  }
                  return;
                }
              }
            });
          }
        }

        // Check the guard expression, if present.
        if (auto *guard = labelItem.getGuardExpr()) {
          hadError |= TC.typeCheckCondition(guard, DC);
          labelItem.setGuardExpr(guard);
        }
      }
      
      // Type-check the body statements.
      Stmt *body = caseBlock->getBody();
      hadError |= typeCheckStmt(body);
      caseBlock->setBody(body);
    }

    if (!(S->isImplicit() || hadError)) {
      SpaceEngine(*this, S).checkExhaustiveness();
    }

    return S;
  }

  Stmt *visitCaseStmt(CaseStmt *S) {
    // Cases are handled in visitSwitchStmt.
    llvm_unreachable("case stmt outside of switch?!");
  }

  Stmt *visitCatchStmt(CatchStmt *S) {
    // Catches are handled in visitDoCatchStmt.
    llvm_unreachable("catch stmt outside of do-catch?!");
  }

  void checkCatchStmt(CatchStmt *S) {
    // Check the catch pattern.
    TC.typeCheckCatchPattern(S, DC);

    // Check the guard expression, if present.
    if (Expr *guard = S->getGuardExpr()) {
      TC.typeCheckCondition(guard, DC);
      S->setGuardExpr(guard);
    }
      
    // Type-check the clause body.
    Stmt *body = S->getBody();
    typeCheckStmt(body);
    S->setBody(body);
  }

  Stmt *visitDoCatchStmt(DoCatchStmt *S) {
    // The labels are in scope for both the 'do' and all of the catch
    // clauses.  This allows the user to break out of (or restart) the
    // entire construct.
    AddLabeledStmt loopNest(*this, S);

    // Type-check the 'do' body.  Type failures in here will generally
    // not cause type failures in the 'catch' clauses.
    Stmt *newBody = S->getBody();
    typeCheckStmt(newBody);
    S->setBody(newBody);

    // Check all the catch clauses independently.
    for (auto clause : S->getCatches()) {
      checkCatchStmt(clause);
    }
    
    return S;
  }

  Stmt *visitFailStmt(FailStmt *S) {
    // These are created as part of type-checking "return" in an initializer.
    // There is nothing more to do.
    return S;
  }

};

void StmtChecker::SpaceEngine::Space::dump() const {
  SmallString<128> buf;
  llvm::raw_svector_ostream os(buf);
  this->show(os, /*normalize*/false);
  llvm::errs() << buf.str();
  llvm::errs() << "\n";
}

} // end anonymous namespace

bool TypeChecker::typeCheckCatchPattern(CatchStmt *S, DeclContext *DC) {
  // Grab the standard exception type.
  Type exnType = getExceptionType(DC, S->getCatchLoc());

  Pattern *pattern = S->getErrorPattern();
  if (Pattern *newPattern = resolvePattern(pattern, DC,
                                           /*isStmtCondition*/false)) {
    pattern = newPattern;

    // Coerce the pattern to the exception type.
    if (!exnType ||
        coercePatternToType(pattern, DC, exnType, TR_InExpression)) {
      // If that failed, be sure to give the variables error types
      // before we type-check the guard.  (This will probably kill
      // most of the type-checking, but maybe not.)
      pattern->forEachVariable([&](VarDecl *var) {
        var->markInvalid();
      });
    }

    S->setErrorPattern(pattern);
  }
  return false;
}

static bool isDiscardableType(Type type) {
  return (type->hasError() ||
          type->isUninhabited() ||
          type->lookThroughAllAnyOptionalTypes()->isVoid());
}

static void diagnoseIgnoredLiteral(TypeChecker &TC, LiteralExpr *LE) {
  const auto getLiteralDescription = [](LiteralExpr *LE) -> StringRef {
    switch (LE->getKind()) {
    case ExprKind::IntegerLiteral: return "integer";
    case ExprKind::FloatLiteral: return "floating-point";
    case ExprKind::BooleanLiteral: return "boolean";
    case ExprKind::StringLiteral: return "string";
    case ExprKind::InterpolatedStringLiteral: return "string";
    case ExprKind::MagicIdentifierLiteral:
      switch (cast<MagicIdentifierLiteralExpr>(LE)->getKind()) {
      case MagicIdentifierLiteralExpr::Kind::File: return "#file";
      case MagicIdentifierLiteralExpr::Kind::Line: return "#line";
      case MagicIdentifierLiteralExpr::Kind::Column: return "#column";
      case MagicIdentifierLiteralExpr::Kind::Function: return "#function";
      case MagicIdentifierLiteralExpr::Kind::DSOHandle: return "#dsohandle";
      }
    case ExprKind::NilLiteral: return "nil";
    case ExprKind::ObjectLiteral: return "object";

    // Define an unreachable case for all non-literal expressions.
    // This way, if a new literal is added in the future, the compiler
    // will warn that a case is missing from this switch.
#define LITERAL_EXPR(Id, Parent)
#define EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
      llvm_unreachable("Not a literal expression");
    }

    llvm_unreachable("Unhandled ExprKind in switch.");
  };

  TC.diagnose(LE->getLoc(), diag::expression_unused_literal,
              getLiteralDescription(LE))
    .highlight(LE->getSourceRange());
}

void TypeChecker::checkIgnoredExpr(Expr *E) {
  // For parity with C, several places in the grammar accept multiple
  // comma-separated expressions and then bind them together as an implicit
  // tuple.  Break these apart and check them separately.
  if (E->isImplicit() && isa<TupleExpr>(E)) {
    for (auto Elt : cast<TupleExpr>(E)->getElements()) {
      checkIgnoredExpr(Elt);
    }
    return;
  }

  // Complain about l-values that are neither loaded nor stored.
  if (E->getType()->isLValueType()) {
    diagnose(E->getLoc(), diag::expression_unused_lvalue)
      .highlight(E->getSourceRange());
    return;
  }

  // Drill through noop expressions we don't care about.
  auto valueE = E;
  while (1) {
    valueE = valueE->getValueProvidingExpr();
    
    if (auto *OEE = dyn_cast<OpenExistentialExpr>(valueE))
      valueE = OEE->getSubExpr();
    else if (auto *CRCE = dyn_cast<CovariantReturnConversionExpr>(valueE))
      valueE = CRCE->getSubExpr();
    else if (auto *EE = dyn_cast<ErasureExpr>(valueE))
      valueE = EE->getSubExpr();
    else
      break;
  }
  
  // Complain about functions that aren't called.
  // TODO: What about tuples which contain functions by-value that are
  // dead?
  if (E->getType()->is<AnyFunctionType>()) {
    bool isDiscardable = false;
    if (auto *Fn = dyn_cast<ApplyExpr>(E)) {
      if (auto *declRef = dyn_cast<DeclRefExpr>(Fn->getFn())) {
        if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(declRef->getDecl())) {
          if (funcDecl->getAttrs().hasAttribute<DiscardableResultAttr>()) {
            isDiscardable = true;
          }
        }
      }
    }

    if (!isDiscardable) {
      diagnose(E->getLoc(), diag::expression_unused_function)
          .highlight(E->getSourceRange());
      return;
    }
  }

  // If the result of this expression is of type "Never" or "()"
  // (the latter potentially wrapped in optionals) then it is
  // safe to ignore.
  if (isDiscardableType(valueE->getType()))
    return;
  
  // Complain about '#selector'.
  if (auto *ObjCSE = dyn_cast<ObjCSelectorExpr>(valueE)) {
    diagnose(ObjCSE->getLoc(), diag::expression_unused_selector_result)
      .highlight(E->getSourceRange());
    return;
  }

  // Complain about '#keyPath'.
  if (isa<KeyPathExpr>(valueE)) {
    diagnose(valueE->getLoc(), diag::expression_unused_keypath_result)
      .highlight(E->getSourceRange());
    return;
  }
    
  // Always complain about 'try?'.
  if (auto *OTE = dyn_cast<OptionalTryExpr>(valueE)) {
    diagnose(OTE->getTryLoc(), diag::expression_unused_optional_try)
      .highlight(E->getSourceRange());
    return;
  }

  // If we have an OptionalEvaluationExpr at the top level, then someone is
  // "optional chaining" and ignoring the result.  Produce a diagnostic if it
  // doesn't make sense to ignore it.
  if (auto *OEE = dyn_cast<OptionalEvaluationExpr>(valueE)) {
    if (auto *IIO = dyn_cast<InjectIntoOptionalExpr>(OEE->getSubExpr()))
      return checkIgnoredExpr(IIO->getSubExpr());
    if (auto *C = dyn_cast<CallExpr>(OEE->getSubExpr()))
      return checkIgnoredExpr(C);
  }

  if (auto *LE = dyn_cast<LiteralExpr>(valueE)) {
    diagnoseIgnoredLiteral(*this, LE);
    return;
  }

  // Check if we have a call to a function not marked with
  // '@discardableResult'.
  if (auto call = dyn_cast<ApplyExpr>(valueE)) {
    // Dig through all levels of calls.
    Expr *fn = call->getFn();
    while (true) {
      fn = fn->getSemanticsProvidingExpr();
      if (auto applyFn = dyn_cast<ApplyExpr>(fn)) {
        fn = applyFn->getFn();
      } else if (auto FVE = dyn_cast<ForceValueExpr>(fn)) {
        fn = FVE->getSubExpr();
      } else if (auto dotSyntaxRef = dyn_cast<DotSyntaxBaseIgnoredExpr>(fn)) {
        fn = dotSyntaxRef->getRHS();
      } else {
        break;
      }
    }

    // Find the callee.
    AbstractFunctionDecl *callee = nullptr;
    if (auto declRef = dyn_cast<DeclRefExpr>(fn))
      callee = dyn_cast<AbstractFunctionDecl>(declRef->getDecl());
    else if (auto ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(fn))
      callee = ctorRef->getDecl();
    else if (auto memberRef = dyn_cast<MemberRefExpr>(fn))
      callee = dyn_cast<AbstractFunctionDecl>(memberRef->getMember().getDecl());
    else if (auto dynMemberRef = dyn_cast<DynamicMemberRefExpr>(fn))
      callee = dyn_cast<AbstractFunctionDecl>(
                 dynMemberRef->getMember().getDecl());
    
    // If the callee explicitly allows its result to be ignored, then don't
    // complain.
    if (callee && callee->getAttrs().getAttribute<DiscardableResultAttr>())
      return;

    // Otherwise, complain.  Start with more specific diagnostics.

    // Diagnose unused literals that were translated to implicit
    // constructor calls during CSApply / ExprRewriter::convertLiteral.
    if (call->isImplicit()) {
      Expr *arg = call->getArg();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(arg))
        if (TE->getNumElements() == 1)
          arg = TE->getElement(0);

      if (LiteralExpr *LE = dyn_cast<LiteralExpr>(arg)) {
        diagnoseIgnoredLiteral(*this, LE);
        return;
      }
    }

    // Other unused constructor calls.
    if (callee && isa<ConstructorDecl>(callee) && !call->isImplicit()) {
      diagnose(fn->getLoc(), diag::expression_unused_init_result,
               callee->getDeclContext()->getDeclaredTypeOfContext())
        .highlight(call->getArg()->getSourceRange());
      return;
    }
    
    SourceRange SR1 = call->getArg()->getSourceRange(), SR2;
    if (auto *BO = dyn_cast<BinaryExpr>(call)) {
      SR1 = BO->getArg()->getElement(0)->getSourceRange();
      SR2 = BO->getArg()->getElement(1)->getSourceRange();
    }
    
    // Otherwise, produce a generic diagnostic.
    if (callee) {
      auto diagID = diag::expression_unused_result_call;
      if (callee->getFullName().isOperator())
        diagID = diag::expression_unused_result_operator;
      
      diagnose(fn->getLoc(), diagID, callee->getFullName())
        .highlight(SR1).highlight(SR2);
    } else
      diagnose(fn->getLoc(), diag::expression_unused_result_unknown,
               valueE->getType())
        .highlight(SR1).highlight(SR2);

    return;
  }

  // Produce a generic diagnostic.
  diagnose(valueE->getLoc(), diag::expression_unused_result, valueE->getType())
    .highlight(valueE->getSourceRange());
}

Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  const SourceManager &SM = TC.Context.SourceMgr;
  for (auto &elem : BS->getElements()) {
    if (Expr *SubExpr = elem.dyn_cast<Expr*>()) {
      SourceLoc Loc = SubExpr->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      // Type check the expression.
      TypeCheckExprOptions options = TypeCheckExprFlags::IsExprStmt;
      bool isDiscarded = !(IsREPL && isa<TopLevelCodeDecl>(DC))
        && !TC.Context.LangOpts.Playground
        && !TC.Context.LangOpts.DebuggerSupport;
      if (isDiscarded)
        options |= TypeCheckExprFlags::IsDiscarded;

      bool hadTypeError = TC.typeCheckExpression(SubExpr, DC, TypeLoc(),
                                                 CTP_Unused, options);

      // If a closure expression is unused, the user might have intended
      // to write "do { ... }".
      auto *CE = dyn_cast<ClosureExpr>(SubExpr);
      if (CE || isa<CaptureListExpr>(SubExpr)) {
        TC.diagnose(SubExpr->getLoc(), diag::expression_unused_closure);
        
        if (CE && CE->hasAnonymousClosureVars() &&
            CE->getParameters()->size() == 0) {
          TC.diagnose(CE->getStartLoc(), diag::brace_stmt_suggest_do)
            .fixItInsert(CE->getStartLoc(), "do ");
        }
      } else if (isDiscarded && !hadTypeError)
        TC.checkIgnoredExpr(SubExpr);

      elem = SubExpr;
      continue;
    }

    if (Stmt *SubStmt = elem.dyn_cast<Stmt*>()) {
      SourceLoc Loc = SubStmt->getStartLoc();
      if (EndTypeCheckLoc.isValid() &&
          (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
        break;

      typeCheckStmt(SubStmt);
      elem = SubStmt;
      continue;
    }

    Decl *SubDecl = elem.get<Decl *>();
    SourceLoc Loc = SubDecl->getStartLoc();
    if (EndTypeCheckLoc.isValid() &&
        (Loc == EndTypeCheckLoc || SM.isBeforeInBuffer(EndTypeCheckLoc, Loc)))
      break;

    TC.typeCheckDecl(SubDecl, /*isFirstPass*/false);
  }
  
  return BS;
}

/// Check the default arguments that occur within this pattern.
static void checkDefaultArguments(TypeChecker &tc,
                                  ParameterList *params,
                                  unsigned &nextArgIndex,
                                  AbstractFunctionDecl *func) {
  // In Swift 4 mode, default argument bodies are inlined into the
  // caller.
  auto expansion = func->getResilienceExpansion();
  if (!tc.Context.isSwiftVersion3() &&
      func->getEffectiveAccess() == Accessibility::Public)
    expansion = ResilienceExpansion::Minimal;

  for (auto &param : *params) {
    ++nextArgIndex;
    if (!param->getDefaultValue() || !param->hasType() ||
        param->getType()->hasError())
      continue;
    
    Expr *e = param->getDefaultValue();
    auto initContext = param->getDefaultArgumentInitContext();

    cast<DefaultArgumentInitializer>(initContext)
        ->changeResilienceExpansion(expansion);

    // Type-check the initializer, then flag that we did so.
    bool hadError = tc.typeCheckExpression(e, initContext,
                                           TypeLoc::withoutLoc(param->getType()),
                                           CTP_DefaultParameter);
    if (!hadError) {
      param->setDefaultValue(e);
    } else {
      param->setDefaultValue(nullptr);
    }

    tc.checkInitializerErrorHandling(initContext, e);

    // Walk the checked initializer and contextualize any closures
    // we saw there.
    (void)tc.contextualizeInitializer(initContext, e);
  }
}

bool TypeChecker::typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                                     SourceLoc EndTypeCheckLoc) {
  validateDecl(AFD);

  if (!AFD->getBody())
    return false;

  if (auto *FD = dyn_cast<FuncDecl>(AFD))
    return typeCheckFunctionBodyUntil(FD, EndTypeCheckLoc);

  if (auto *CD = dyn_cast<ConstructorDecl>(AFD))
    return typeCheckConstructorBodyUntil(CD, EndTypeCheckLoc);

  auto *DD = cast<DestructorDecl>(AFD);
  return typeCheckDestructorBodyUntil(DD, EndTypeCheckLoc);
}

bool TypeChecker::typeCheckAbstractFunctionBody(AbstractFunctionDecl *AFD) {
  if (!AFD->getBody())
    return false;

  SWIFT_FUNC_STAT;

  Optional<FunctionBodyTimer> timer;
  if (DebugTimeFunctionBodies || WarnLongFunctionBodies)
    timer.emplace(AFD, DebugTimeFunctionBodies, WarnLongFunctionBodies);

  if (typeCheckAbstractFunctionBodyUntil(AFD, SourceLoc()))
    return true;
  
  performAbstractFuncDeclDiagnostics(*this, AFD);
  return false;
}

// Type check a function body (defined with the func keyword) that is either a
// named function or an anonymous func expression.
bool TypeChecker::typeCheckFunctionBodyUntil(FuncDecl *FD,
                                             SourceLoc EndTypeCheckLoc) {
  // Check the default argument definitions.
  unsigned nextArgIndex = 0;
  for (auto paramList : FD->getParameterLists())
    checkDefaultArguments(*this, paramList, nextArgIndex, FD);

  // Clang imported inline functions do not have a Swift body to
  // typecheck.
  if (FD->getClangDecl())
    return false;

  BraceStmt *BS = FD->getBody();
  assert(BS && "Should have a body");

  StmtChecker SC(*this, static_cast<AbstractFunctionDecl *>(FD));
  SC.EndTypeCheckLoc = EndTypeCheckLoc;
  bool HadError = SC.typeCheckBody(BS);

  FD->setBody(BS);
  return HadError;
}

Expr* TypeChecker::constructCallToSuperInit(ConstructorDecl *ctor,
                                            ClassDecl *ClDecl) {
  Expr *superRef = new (Context) SuperRefExpr(ctor->getImplicitSelfDecl(),
                                              SourceLoc(), /*Implicit=*/true);
  Expr *r = new (Context) UnresolvedDotExpr(superRef, SourceLoc(),
                                            Context.Id_init, DeclNameLoc(),
                                            /*Implicit=*/true);
  r = CallExpr::createImplicit(Context, r, { }, { });

  if (ctor->hasThrows())
    r = new (Context) TryExpr(SourceLoc(), r, Type(), /*implicit=*/true);

  if (typeCheckExpression(r, ctor, TypeLoc(), CTP_Unused,
                          TypeCheckExprFlags::IsDiscarded | 
                          TypeCheckExprFlags::SuppressDiagnostics))
    return nullptr;
  
  return r;
}

/// Check a super.init call.
///
/// \returns true if an error occurred.
static bool checkSuperInit(TypeChecker &tc, ConstructorDecl *fromCtor, 
                           ApplyExpr *apply, bool implicitlyGenerated) {
  // Make sure we are referring to a designated initializer.
  auto otherCtorRef = dyn_cast<OtherConstructorDeclRefExpr>(
                        apply->getSemanticFn());
  if (!otherCtorRef)
    return false;
  
  auto ctor = otherCtorRef->getDecl();
  if (!ctor->isDesignatedInit()) {
    if (!implicitlyGenerated) {
      auto selfTy = fromCtor->getDeclContext()->getSelfInterfaceType();
      if (auto classTy = selfTy->getClassOrBoundGenericClass()) {
        assert(classTy->getSuperclass());
        tc.diagnose(apply->getArg()->getLoc(), diag::chain_convenience_init,
                    classTy->getSuperclass());
        tc.diagnose(ctor, diag::convenience_init_here);
      }
    }
    return true;
  }
  
  // For an implicitly generated super.init() call, make sure there's
  // only one designated initializer.
  if (implicitlyGenerated) {
    auto superclassTy = ctor->getDeclContext()->getDeclaredInterfaceType();
    auto lookupOptions = defaultConstructorLookupOptions;
    lookupOptions |= NameLookupFlags::KnownPrivate;

    // If a constructor is only visible as a witness for a protocol
    // requirement, it must be an invalid override. Also, protocol
    // extensions cannot yet define designated initializers.
    lookupOptions -= NameLookupFlags::ProtocolMembers;
    lookupOptions -= NameLookupFlags::PerformConformanceCheck;

    for (auto member : tc.lookupConstructors(fromCtor, superclassTy,
                                             lookupOptions)) {
      auto superclassCtor = dyn_cast<ConstructorDecl>(member.Decl);
      if (!superclassCtor || !superclassCtor->isDesignatedInit() ||
          superclassCtor == ctor)
        continue;
      
      // Found another designated initializer in the superclass. Don't add the
      // super.init() call.
      return true;
    }
  }
  
  return false;
}

static bool isKnownEndOfConstructor(ASTNode N) {
  auto *S = N.dyn_cast<Stmt*>();
  if (!S) return false;

  return isa<ReturnStmt>(S) || isa<FailStmt>(S);
}

bool TypeChecker::typeCheckConstructorBodyUntil(ConstructorDecl *ctor,
                                                SourceLoc EndTypeCheckLoc) {
  // Check the default argument definitions.
  unsigned nextArgIndex = 0;
  for (auto paramList : ctor->getParameterLists())
    checkDefaultArguments(*this, paramList, nextArgIndex, ctor);

  BraceStmt *body = ctor->getBody();
  if (!body)
    return true;

  // For constructors, we make sure that the body ends with a "return" stmt,
  // which we either implicitly synthesize, or the user can write.  This
  // simplifies SILGen.
  if (body->getNumElements() == 0 ||
      !isKnownEndOfConstructor(body->getElements().back())) {
    SmallVector<ASTNode, 8> Elts(body->getElements().begin(),
                                 body->getElements().end());
    Elts.push_back(new (Context) ReturnStmt(body->getRBraceLoc(),
                                            /*value*/nullptr,
                                            /*implicit*/true));
    body = BraceStmt::create(Context, body->getLBraceLoc(), Elts,
                             body->getRBraceLoc(), body->isImplicit());
    ctor->setBody(body);
  }
  
  // Type-check the body.
  StmtChecker SC(*this, static_cast<AbstractFunctionDecl *>(ctor));
  SC.EndTypeCheckLoc = EndTypeCheckLoc;
  bool HadError = SC.typeCheckBody(body);

  if (ctor->isInvalid())
    return HadError;

  // Determine whether we need to introduce a super.init call.
  auto nominalDecl = ctor->getDeclContext()
    ->getAsNominalTypeOrNominalTypeExtensionContext();

  // Error case.
  if (nominalDecl == nullptr)
    return HadError;

  ClassDecl *ClassD = dyn_cast<ClassDecl>(nominalDecl);
  bool wantSuperInitCall = false;
  if (ClassD) {
    bool isDelegating = false;
    ApplyExpr *initExpr = nullptr;
    switch (ctor->getDelegatingOrChainedInitKind(&Diags, &initExpr)) {
    case ConstructorDecl::BodyInitKind::Delegating:
      isDelegating = true;
      wantSuperInitCall = false;
      break;

    case ConstructorDecl::BodyInitKind::Chained:
      checkSuperInit(*this, ctor, initExpr, false);

      /// A convenience initializer cannot chain to a superclass constructor.
      if (ctor->isConvenienceInit()) {
        diagnose(initExpr->getLoc(), diag::delegating_convenience_super_init,
                 ctor->getDeclContext()->getDeclaredInterfaceType());
      }

      LLVM_FALLTHROUGH;

    case ConstructorDecl::BodyInitKind::None:
      wantSuperInitCall = false;
      break;

    case ConstructorDecl::BodyInitKind::ImplicitChained:
      wantSuperInitCall = true;
      break;
    }

    // A class designated initializer must never be delegating.
    if (ctor->isDesignatedInit() && isDelegating) {
      diagnose(ctor->getLoc(),
               diag::delegating_designated_init,
               ctor->getDeclContext()->getDeclaredInterfaceType())
        .fixItInsert(ctor->getLoc(), "convenience ");
      diagnose(initExpr->getLoc(), diag::delegation_here);
      ctor->setInitKind(CtorInitializerKind::Convenience);
    }
  }

  diagnoseResilientConstructor(ctor);

  // If we want a super.init call...
  if (wantSuperInitCall) {
    assert(ClassD != nullptr);

    // Find a default initializer in the superclass.
    if (Expr *SuperInitCall = constructCallToSuperInit(ctor, ClassD)) {
      // If the initializer we found is a designated initializer, we're okay.
      class FindOtherConstructorRef : public ASTWalker {
      public:
        ApplyExpr *Found = nullptr;

        std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
          if (auto apply = dyn_cast<ApplyExpr>(E)) {
            if (isa<OtherConstructorDeclRefExpr>(apply->getSemanticFn())) {
              Found = apply;
              return { false, E };
            }
          }

          return { Found == nullptr, E };
        }
      };

      FindOtherConstructorRef finder;
      SuperInitCall->walk(finder);
      if (!checkSuperInit(*this, ctor, finder.Found, true)) {
        // Store the super.init expression within the constructor declaration
        // to be emitted during SILGen.
        ctor->setSuperInitCall(SuperInitCall);
      }
    }
  }

  return HadError;
}

bool TypeChecker::typeCheckDestructorBodyUntil(DestructorDecl *DD,
                                               SourceLoc EndTypeCheckLoc) {
  StmtChecker SC(*this, static_cast<AbstractFunctionDecl *>(DD));
  SC.EndTypeCheckLoc = EndTypeCheckLoc;
  BraceStmt *Body = DD->getBody();
  if (!Body)
    return false;

  bool HadError = SC.typeCheckBody(Body);

  DD->setBody(Body);
  return HadError;
}

void TypeChecker::typeCheckClosureBody(ClosureExpr *closure) {
  BraceStmt *body = closure->getBody();

  Optional<FunctionBodyTimer> timer;
  if (DebugTimeFunctionBodies || WarnLongFunctionBodies)
    timer.emplace(closure, DebugTimeFunctionBodies, WarnLongFunctionBodies);

  StmtChecker(*this, closure).typeCheckBody(body);
  if (body) {
    closure->setBody(body, closure->hasSingleExpressionBody());
  }
}

void TypeChecker::typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
  // We intentionally use typeCheckStmt instead of typeCheckBody here
  // because we want to contextualize all the TopLevelCode
  // declarations simultaneously.
  BraceStmt *Body = TLCD->getBody();
  StmtChecker(*this, TLCD).typeCheckStmt(Body);
  TLCD->setBody(Body);
  checkTopLevelErrorHandling(TLCD);
  performTopLevelDeclDiagnostics(*this, TLCD);
}
