//===--- OptionalTryMigratorPass.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Migrator/ASTMigratorPass.h"
#include "swift/Parse/Lexer.h"

using namespace swift;
using namespace swift::migrator;

namespace {
  
  class OptionalTryMigratorPass: public ASTMigratorPass,
  public SourceEntityWalker {
    
    std::vector<DeclContext *> ConditionStack;
    
    void handleIfAndGuard(const LabeledConditionalStmt *IF) {
      // Look for cases like this:
      //   if let optX = try? somethingOptional(),
      //      let x = optX { }
      
      NamedPattern *tempVarNamePattern = nullptr;
      SourceLoc endOfPrecedingElement;
      
      for (auto conditionElement : IF->getCond()) {
        if (conditionElement.getKind() != StmtConditionElement::CK_PatternBinding) {
          continue;
        }
        
        const auto pattern = dyn_cast<OptionalSomePattern>(conditionElement.getPattern());
        if (!pattern) {
          // We are only concerned with patterns that are unwrapping optionals.
          continue;
        }
        
        const auto initializer = conditionElement.getInitializer();
        
        if (tempVarNamePattern && initializer->getKind() == ExprKind::DeclRef) {
          const auto declExpr = static_cast<DeclRefExpr *>(initializer);
          
          if (const auto rhsVarDecl = dyn_cast<VarDecl>(declExpr->getDecl())) {
            if (rhsVarDecl->getName() == tempVarNamePattern->getBoundName()) {
              
              // We found an unwrap binding of our previous 'try?' variable
              auto thisVarPattern = dyn_cast<VarPattern>(pattern->getSubPattern());
              if (!thisVarPattern) { continue; }
              
              // Use the final unwrapped variable name for the original 'try?' expression
              auto tempVarRange = Lexer::getCharSourceRangeFromSourceRange(SM, tempVarNamePattern->getSourceRange());
              Editor.replace(tempVarRange, thisVarPattern->getBoundName().str());
              
              // Remove this condition element that only served to unwrap the try.
              auto rangeToRemove = SourceRange(endOfPrecedingElement, conditionElement.getEndLoc());
              auto charRangeToRemove = Lexer::getCharSourceRangeFromSourceRange(SM, rangeToRemove);
              Editor.remove(charRangeToRemove);
              
              // We've made our change. Clear out the tempVarNamePattern to allow for
              // finding another pattern in this 'if'
              tempVarNamePattern = nullptr;
            }
          }
        }
        else if (const auto optTryExpr = dyn_cast<OptionalTryExpr>(initializer)) {
          if (!optTryExpr->getSubExpr()->getType()->getOptionalObjectType()) {
            continue;
          }
          // This is a `try?` wrapping an optional. It's behavior has changed in Swift 5
          const auto VP = dyn_cast<VarPattern>(pattern->getSubPattern());
          const auto NP = dyn_cast<NamedPattern>(VP->getSubPattern());
          if (!NP) { continue; }
          
          tempVarNamePattern = NP;
        }
        endOfPrecedingElement = conditionElement.getEndLoc().getAdvancedLoc(1);
      }
    }
    
    bool walkToStmtPre(Stmt *S) override {
      if (const auto *IF = dyn_cast<LabeledConditionalStmt>(S)) {
        handleIfAndGuard(IF);
      }
      return true;
    }
    
  public:
    OptionalTryMigratorPass(EditorAdapter &Editor,
                       SourceFile *SF,
                       const MigratorOptions &Opts)
    : ASTMigratorPass(Editor, SF, Opts) {}
  };
  
} // end anonymous namespace

void migrator::runOptionalTryMigratorPass(EditorAdapter &Editor,
                                     SourceFile *SF,
                                     const MigratorOptions &Opts) {
  OptionalTryMigratorPass { Editor, SF, Opts }.walk(SF);
}
