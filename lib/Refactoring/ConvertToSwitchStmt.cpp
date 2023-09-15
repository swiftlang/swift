//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "RefactoringActions.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"

using namespace swift::refactoring;

bool RefactoringActionConvertToSwitchStmt::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {

  class ConditionalChecker : public ASTWalker {
  public:
    bool ParamsUseSameVars = true;
    bool ConditionUseOnlyAllowedFunctions = false;
    StringRef ExpectName;

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
      if (E->getKind() != ExprKind::DeclRef)
        return Action::Continue(E);
      auto D = dyn_cast<DeclRefExpr>(E)->getDecl();
      if (D->getKind() == DeclKind::Var || D->getKind() == DeclKind::Param)
        ParamsUseSameVars = checkName(dyn_cast<VarDecl>(D));
      if (D->getKind() == DeclKind::Func)
        ConditionUseOnlyAllowedFunctions = checkName(dyn_cast<FuncDecl>(D));
      if (allCheckPassed())
        return Action::Continue(E);

      return Action::Stop();
    }

    bool allCheckPassed() {
      return ParamsUseSameVars && ConditionUseOnlyAllowedFunctions;
    }

  private:
    bool checkName(VarDecl *VD) {
      auto Name = VD->getName().str();
      if (ExpectName.empty())
        ExpectName = Name;
      return Name == ExpectName;
    }

    bool checkName(FuncDecl *FD) {
      const auto Name = FD->getBaseIdentifier().str();
      return Name == "~=" || Name == "==" || Name == "__derived_enum_equals" ||
             Name == "__derived_struct_equals" || Name == "||" || Name == "...";
    }
  };

  class SwitchConvertable {
  public:
    SwitchConvertable(const ResolvedRangeInfo &Info) : Info(Info) {}

    bool isApplicable() {
      if (Info.Kind != RangeKind::SingleStatement)
        return false;
      if (!findIfStmt())
        return false;
      return checkEachCondition();
    }

  private:
    const ResolvedRangeInfo &Info;
    IfStmt *If = nullptr;
    ConditionalChecker checker;

    bool findIfStmt() {
      if (Info.ContainedNodes.size() != 1)
        return false;
      if (auto S = Info.ContainedNodes.front().dyn_cast<Stmt *>())
        If = dyn_cast<IfStmt>(S);
      return If != nullptr;
    }

    bool checkEachCondition() {
      checker = ConditionalChecker();
      do {
        if (!checkEachElement())
          return false;
      } while ((If = dyn_cast_or_null<IfStmt>(If->getElseStmt())));
      return true;
    }

    bool checkEachElement() {
      bool result = true;
      auto ConditionalList = If->getCond();
      for (auto Element : ConditionalList) {
        result &= check(Element);
      }
      return result;
    }

    bool check(StmtConditionElement ConditionElement) {
      if (ConditionElement.getKind() == StmtConditionElement::CK_Availability)
        return false;
      if (ConditionElement.getKind() == StmtConditionElement::CK_PatternBinding)
        checker.ConditionUseOnlyAllowedFunctions = true;
      ConditionElement.walk(checker);
      return checker.allCheckPassed();
    }
  };
  return SwitchConvertable(Info).isApplicable();
}

bool RefactoringActionConvertToSwitchStmt::performChange() {

  class VarNameFinder : public ASTWalker {
  public:
    std::string VarName;

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
      if (E->getKind() != ExprKind::DeclRef)
        return Action::Continue(E);
      auto D = dyn_cast<DeclRefExpr>(E)->getDecl();
      if (D->getKind() != DeclKind::Var && D->getKind() != DeclKind::Param)
        return Action::Continue(E);
      VarName = dyn_cast<VarDecl>(D)->getName().str().str();
      return Action::Stop();
    }
  };

  class ConditionalPatternFinder : public ASTWalker {
  public:
    ConditionalPatternFinder(SourceManager &SM) : SM(SM) {}

    SmallString<64> ConditionalPattern = SmallString<64>();

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
      auto *BE = dyn_cast<BinaryExpr>(E);
      if (!BE)
        return Action::Continue(E);
      if (isFunctionNameAllowed(BE))
        appendPattern(BE->getLHS(), BE->getRHS());
      return Action::Continue(E);
    }

    PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
      ConditionalPattern.append(
          Lexer::getCharSourceRangeFromSourceRange(SM, P->getSourceRange())
              .str());
      if (P->getKind() == PatternKind::OptionalSome)
        ConditionalPattern.append("?");
      return Action::Stop();
    }

  private:
    SourceManager &SM;

    bool isFunctionNameAllowed(BinaryExpr *E) {
      Expr *Fn = E->getFn();
      if (auto DotSyntaxCall = dyn_cast_or_null<DotSyntaxCallExpr>(Fn)) {
        Fn = DotSyntaxCall->getFn();
      }
      DeclRefExpr *DeclRef = dyn_cast_or_null<DeclRefExpr>(Fn);
      if (!DeclRef) {
        return false;
      }
      auto FunctionDeclaration = dyn_cast_or_null<FuncDecl>(DeclRef->getDecl());
      if (!FunctionDeclaration) {
        return false;
      }
      auto &ASTCtx = FunctionDeclaration->getASTContext();
      const auto FunctionName = FunctionDeclaration->getBaseIdentifier();
      return FunctionName == ASTCtx.Id_MatchOperator ||
             FunctionName == ASTCtx.Id_EqualsOperator ||
             FunctionName == ASTCtx.Id_derived_enum_equals ||
             FunctionName == ASTCtx.Id_derived_struct_equals;
    }

    void appendPattern(Expr *LHS, Expr *RHS) {
      auto *PatternArgument = RHS;
      if (PatternArgument->getKind() == ExprKind::DeclRef)
        PatternArgument = LHS;
      if (ConditionalPattern.size() > 0)
        ConditionalPattern.append(", ");
      ConditionalPattern.append(Lexer::getCharSourceRangeFromSourceRange(
                                    SM, PatternArgument->getSourceRange())
                                    .str());
    }
  };

  class ConverterToSwitch {
  public:
    ConverterToSwitch(const ResolvedRangeInfo &Info, SourceManager &SM)
        : Info(Info), SM(SM) {}

    void performConvert(SmallString<64> &Out) {
      If = findIf();
      OptionalLabel = If->getLabelInfo().Name.str().str();
      ControlExpression = findControlExpression();
      findPatternsAndBodies(PatternsAndBodies);
      DefaultStatements = findDefaultStatements();
      makeSwitchStatement(Out);
    }

  private:
    const ResolvedRangeInfo &Info;
    SourceManager &SM;

    IfStmt *If;
    IfStmt *PreviousIf;

    std::string OptionalLabel;
    std::string ControlExpression;
    SmallVector<std::pair<std::string, std::string>, 16> PatternsAndBodies;
    std::string DefaultStatements;

    IfStmt *findIf() {
      auto S = Info.ContainedNodes[0].dyn_cast<Stmt *>();
      return dyn_cast<IfStmt>(S);
    }

    std::string findControlExpression() {
      auto ConditionElement = If->getCond().front();
      auto Finder = VarNameFinder();
      ConditionElement.walk(Finder);
      return Finder.VarName;
    }

    void findPatternsAndBodies(
        SmallVectorImpl<std::pair<std::string, std::string>> &Out) {
      do {
        auto pattern = findPattern();
        auto body = findBodyStatements();
        Out.push_back(std::make_pair(pattern, body));
        PreviousIf = If;
      } while ((If = dyn_cast_or_null<IfStmt>(If->getElseStmt())));
    }

    std::string findPattern() {
      auto ConditionElement = If->getCond().front();
      auto Finder = ConditionalPatternFinder(SM);
      ConditionElement.walk(Finder);
      return Finder.ConditionalPattern.str().str();
    }

    std::string findBodyStatements() {
      return findBodyWithoutBraces(If->getThenStmt());
    }

    std::string findDefaultStatements() {
      auto ElseBody = dyn_cast_or_null<BraceStmt>(PreviousIf->getElseStmt());
      if (!ElseBody)
        return getTokenText(tok::kw_break).str();
      return findBodyWithoutBraces(ElseBody);
    }

    std::string findBodyWithoutBraces(Stmt *body) {
      auto BS = dyn_cast<BraceStmt>(body);
      if (!BS)
        return Lexer::getCharSourceRangeFromSourceRange(SM,
                                                        body->getSourceRange())
            .str()
            .str();
      if (BS->getElements().empty())
        return getTokenText(tok::kw_break).str();
      SourceRange BodyRange = BS->getElements().front().getSourceRange();
      BodyRange.widen(BS->getElements().back().getSourceRange());
      return Lexer::getCharSourceRangeFromSourceRange(SM, BodyRange)
          .str()
          .str();
    }

    void makeSwitchStatement(SmallString<64> &Out) {
      StringRef Space = " ";
      StringRef NewLine = "\n";
      llvm::raw_svector_ostream OS(Out);
      if (OptionalLabel.size() > 0)
        OS << OptionalLabel << ":" << Space;
      OS << tok::kw_switch << Space << ControlExpression << Space
         << tok::l_brace << NewLine;
      for (auto &pair : PatternsAndBodies) {
        OS << tok::kw_case << Space << pair.first << tok::colon << NewLine;
        OS << pair.second << NewLine;
      }
      OS << tok::kw_default << tok::colon << NewLine;
      OS << DefaultStatements << NewLine;
      OS << tok::r_brace;
    }
  };

  SmallString<64> result;
  ConverterToSwitch(RangeInfo, SM).performConvert(result);
  EditConsumer.accept(SM, RangeInfo.ContentRange, result.str());
  return false;
}
