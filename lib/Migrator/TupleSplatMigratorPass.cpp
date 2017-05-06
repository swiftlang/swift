//===--- TupleSplatMigratorPass.cpp ---------------------------------------===//
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

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/Migrator/ASTMigratorPass.h"
#include "swift/Parse/Lexer.h"

using namespace swift;
using namespace swift::migrator;

namespace {

struct TupleSplatMigratorPass : public ASTMigratorPass,
  public SourceEntityWalker {

      /// Migrates code that compiles fine in Swift 3 but breaks in Swift 4 due to
  /// changes in how the typechecker handles tuple arguments.
  void handleTupleArgumentMismatches(const CallExpr *E) {
    if (!SF->getASTContext().LangOpts.isSwiftVersion3())
      return;
    if (E->isImplicit())
      return;

    // Handles such kind of cases:
    // \code
    //   func test(_: ()) {}
    //   test()
    // \endcode
    // This compiles fine in Swift 3 but Swift 4 complains with
    //   error: missing argument for parameter #1 in call
    //
    // It will fix the code to "test(())".
    //
    auto handleCallsToEmptyTuple = [&](const CallExpr *E) -> bool {
      auto fnTy = E->getFn()->getType()->getAs<FunctionType>();
      if (!fnTy)
        return false;
      auto parenT = dyn_cast<ParenType>(fnTy->getInput().getPointer());
      if (!parenT)
        return false;
      auto inp = dyn_cast<TupleType>(parenT->getUnderlyingType().getPointer());
      if (!inp)
        return false;
      if (inp->getNumElements() != 0)
        return false;
      auto argTupleT = dyn_cast<TupleType>(E->getArg()->getType().getPointer());
      if (!argTupleT)
        return false;
      if (argTupleT->getNumElements() != 0)
        return false;
      Editor.insertWrap("(", E->getArg()->getSourceRange(), ")");
      return true;
    };

    // Handles such kind of cases:
    // \code
    //   func test(_: ((Int, Int)) -> ()) {}
    //   test({ (x,y) in })
    // \endcode
    // This compiles fine in Swift 3 but Swift 4 complains with
    //   error: cannot convert value of type '(_, _) -> ()' to expected
    //   argument type '((Int, Int)) -> ()'
    //
    // It will fix the code to "test({ let (x,y) = $0; })".
    //
    auto handleTupleMapToClosureArgs = [&](const CallExpr *E) -> bool {
      auto fnTy = E->getFn()->getType()->getAs<FunctionType>();
      if (!fnTy)
        return false;
      auto fnTy2 = fnTy->getInput()->getAs<FunctionType>();
      if (!fnTy2)
        return false;
      auto parenT = dyn_cast<ParenType>(fnTy2->getInput().getPointer());
      if (!parenT)
        return false;
      auto tupleInFn = dyn_cast<TupleType>(parenT->getUnderlyingType().getPointer());
      if (!tupleInFn)
        return false;
      if (!E->getArg())
        return false;
      auto argE = E->getArg()->getSemanticsProvidingExpr();
      while (auto *ICE = dyn_cast<ImplicitConversionExpr>(argE))
        argE = ICE->getSubExpr();
      argE = argE->getSemanticsProvidingExpr();
      auto closureE = dyn_cast<ClosureExpr>(argE);
      if (!closureE)
        return false;
      if (closureE->getInLoc().isInvalid())
        return false;
      auto paramList = closureE->getParameters();
      if (!paramList ||
          paramList->getLParenLoc().isInvalid() || paramList->getRParenLoc().isInvalid())
        return false;
      if (paramList->size() != tupleInFn->getNumElements())
        return false;
      if (paramList->size() == 0)
        return false;

      auto hasParamListWithNoTypes = [&]() {
        if (closureE->hasExplicitResultType())
          return false;
        for (auto *param : *paramList) {
          auto tyLoc = param->getTypeLoc();
          if (!tyLoc.isNull())
            return false;
        }
        return true;
      };

      if (hasParamListWithNoTypes()) {
        // Simpler form depending on type inference.
        // Change "(x, y) in " to "let (x, y) = $0;".

        Editor.insert(paramList->getLParenLoc(), "let ");
        for (auto *param : *paramList) {
          // If the argument list is like "(_ x, _ y)", remove the underscores.
          if (param->getArgumentNameLoc().isValid()) {
            Editor.remove(CharSourceRange(SM, param->getArgumentNameLoc(),
                                          param->getNameLoc()));
          }
          // If the argument list has type annotations, remove them.
          auto tyLoc = param->getTypeLoc();
          if (!tyLoc.isNull() && !tyLoc.getSourceRange().isInvalid()) {
            auto nameRange = CharSourceRange(param->getNameLoc(),
                                             param->getNameStr().size());
            auto tyRange = Lexer::getCharSourceRangeFromSourceRange(SM,
                                                        tyLoc.getSourceRange());
            Editor.remove(CharSourceRange(SM, nameRange.getEnd(),
                                          tyRange.getEnd()));
          }
        }

        Editor.replaceToken(closureE->getInLoc(), "= $0;");
        return true;
      }

      // Includes types in the closure signature. The following will do a
      // more complicated edit than the above:
      //   (x: Int, y: Int) -> Int in
      // to
      //   (__val:(Int, Int)) -> Int in let (x,y) = __val;

      std::string paramListText;
      {
        llvm::raw_string_ostream OS(paramListText);
        OS << "(__val:(";
        for (size_t i = 0, e = paramList->size(); i != e; ++i) {
          if (i != 0)
            OS << ", ";
          auto param = paramList->get(i);
          auto tyLoc = param->getTypeLoc();
          if (!tyLoc.isNull() && !tyLoc.getSourceRange().isInvalid()) {
            OS << SM.extractText(
              Lexer::getCharSourceRangeFromSourceRange(SM,
                                                       tyLoc.getSourceRange()));
          } else {
            param->getType().print(OS);
          }
        }
        OS << "))";
      }
      std::string varBindText;
      {
        llvm::raw_string_ostream OS(varBindText);
        OS << " let (";
        for (size_t i = 0, e = paramList->size(); i != e; ++i) {
          if (i != 0)
            OS << ",";
          auto param = paramList->get(i);
          OS << param->getNameStr();
        }
        OS << ") = __val; ";
      }

      Editor.replace(paramList->getSourceRange(), paramListText);
      Editor.insertAfterToken(closureE->getInLoc(), varBindText);
      return true;
    };

    if (handleCallsToEmptyTuple(E))
      return;
    if (handleTupleMapToClosureArgs(E))
      return;
  }

  bool walkToExprPre(Expr *E) override {
    if (auto *CE = dyn_cast<CallExpr>(E)) {
      handleTupleArgumentMismatches(CE);
    }
    return true;
  }
public:

  TupleSplatMigratorPass(EditorAdapter &Editor,
                         SourceFile *SF,
                         const MigratorOptions &Opts)
    : ASTMigratorPass(Editor, SF, Opts) {}
};

} // end anonymous namespace

void migrator::runTupleSplatMigratorPass(EditorAdapter &Editor,
                                         SourceFile *SF,
                                         const MigratorOptions &Opts) {
  TupleSplatMigratorPass { Editor, SF, Opts }.walk(SF);
}
