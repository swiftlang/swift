//===--- SyntacticMigratorPass.cpp ----------------------------------------===//
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

#include "swift/AST/USRGeneration.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/Utils.h"
#include "swift/Index/Utils.h"
#include "swift/Migrator/EditorAdapter.h"
#include "swift/Migrator/FixitApplyDiagnosticConsumer.h"
#include "swift/Migrator/Migrator.h"
#include "swift/Migrator/RewriteBufferEditsReceiver.h"
#include "swift/Migrator/SyntacticMigratorPass.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Edit/EditedSource.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/Support/FileSystem.h"
#include "swift/IDE/APIDigesterData.h"

using namespace swift;
using namespace swift::migrator;
using namespace swift::ide;
using namespace swift::ide::api;

struct FoundResult {
  SourceRange TokenRange;
  bool Optional; // Range has a trailing ? or ! included
  bool Suffixable; // No need to wrap parens when adding optionality
  bool isValid() const { return TokenRange.isValid(); }
};

class ChildIndexFinder : public TypeReprVisitor<ChildIndexFinder, FoundResult> {
  ArrayRef<uint8_t> ChildIndices;

public:
  ChildIndexFinder(ArrayRef<uint8_t> ChildIndices) :
    ChildIndices(ChildIndices) {}

  FoundResult findChild(AbstractFunctionDecl *Parent) {
    auto NextIndex = consumeNext();
    if (!NextIndex) {
      if (auto Func = dyn_cast<FuncDecl>(Parent))
        return findChild(Func->getBodyResultTypeLoc());
      if (auto Init = dyn_cast<ConstructorDecl>(Parent)) {
        SourceLoc End = Init->getFailabilityLoc();
        bool Optional = End.isValid();
        if (!Optional)
          End = Init->getNameLoc();
        return {SourceRange(Init->getNameLoc(), End), Optional, /*suffixable=*/true};
      }
      return {SourceRange(), false, false};
    }

    for (auto *Params: Parent->getParameterLists()) {
      for (auto *Param: Params->getArray()) {
        if (Param->isImplicit())
          continue;
        if (!--NextIndex) {
          return findChild(Param->getTypeLoc());
        }
      }
    }
    llvm_unreachable("child index out of bounds");
  }

private:
  bool hasNextIndex() const {
    return !ChildIndices.empty();
  }

  unsigned consumeNext() {
    unsigned Next = ChildIndices.front();
    ChildIndices = ChildIndices.drop_front();
    return Next;
  }

  bool isUserTypeAlias(TypeRepr *T) const {
    if (auto Ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
      if (auto Bound = Ident->getBoundDecl()) {
        return isa<TypeAliasDecl>(Bound) &&
          !Bound->getModuleContext()->isSystemModule();
      }
    }
    return false;
  }

  FoundResult findChild(TypeLoc Loc) {
    if (!Loc.hasLocation())
      return {SourceRange(), false, false};
    return visit(Loc.getTypeRepr());
  }

public:

  template<typename T>
  FoundResult handleParent(TypeRepr *Parent, const ArrayRef<T> Children,
                           bool Optional = false, bool Suffixable = true) {
    if (!hasNextIndex())
      return {Parent->getSourceRange(), Optional, Suffixable};
    auto NextIndex = consumeNext();
    if (isUserTypeAlias(Parent))
      return {SourceRange(), false, false};
    assert(NextIndex < Children.size());
    TypeRepr *Child = Children[NextIndex];
    return visit(Child);
  }

  FoundResult handleParent(TypeRepr *Parent, TypeRepr *FirstChild,
                           TypeRepr *SecondChild, bool Optional = false,
                           bool Suffixable = true) {
    TypeRepr *Children[] = {FirstChild, SecondChild};
    return handleParent(Parent, llvm::makeArrayRef(Children), Optional,
                        Suffixable);
  }

  FoundResult handleParent(TypeRepr *Parent, TypeRepr *Base,
                           bool Optional = false, bool Suffixable = true) {
    return handleParent(Parent, llvm::makeArrayRef(Base), Optional, Suffixable);
  }

  FoundResult visitTypeRepr(TypeRepr *T) {
    llvm_unreachable("unexpected typerepr");
  }

  FoundResult visitErrorTypeRepr(ErrorTypeRepr *T) {
    return {SourceRange(), false, false};
  }

  FoundResult visitAttributedTypeRepr(AttributedTypeRepr *T) {
    return visit(T->getTypeRepr());
  }

  FoundResult visitInOutTypeRepr(InOutTypeRepr *T) {
    return visit(T->getBase());
  }

  FoundResult visitArrayTypeRepr(ArrayTypeRepr *T) {
    return handleParent(T, T->getBase());
  }

  FoundResult visitDictionaryTypeRepr(DictionaryTypeRepr *T) {
    return handleParent(T, T->getKey(), T->getValue());
  }

  FoundResult visitTupleTypeRepr(TupleTypeRepr *T) {
    // Single element TupleTypeReprs may be arbitrarily nested so don't count
    // as their own index level
    if (T->getNumElements() == 1)
      return visit(T->getElement(0));
    return handleParent(T, T->getElements());
  }

  FoundResult visitFunctionTypeRepr(FunctionTypeRepr *T) {
    return handleParent(T, T->getResultTypeRepr(), T->getArgsTypeRepr(),
                       /*Optional=*/false, /*Suffixable=*/false);
  }

  FoundResult visitCompositionTypeRepr(CompositionTypeRepr *T) {
    return handleParent(T, T->getTypes(), /*Optional=*/false,
                        /*Suffixable=*/false);
  }

  FoundResult visitSimpleIdentTypeRepr(SimpleIdentTypeRepr *T) {
    return handleParent(T, ArrayRef<TypeRepr*>());
  }

  FoundResult visitGenericIdentTypeRepr(GenericIdentTypeRepr *T) {
    return handleParent(T, T->getGenericArgs());
  }

  FoundResult visitCompoundIdentTypeRepr(CompoundIdentTypeRepr *T) {
    return visit(T->Components.back());
  }

  FoundResult visitOptionalTypeRepr(OptionalTypeRepr *T) {
    return handleParent(T, T->getBase(), /*Optional=*/true);
  }

  FoundResult visitImplicitlyUnwrappedOptionalTypeRepr(ImplicitlyUnwrappedOptionalTypeRepr *T) {
    return handleParent(T, T->getBase(), /*Optional=*/true);
  }

  FoundResult visitProtocolTypeRepr(ProtocolTypeRepr *T) {
    return handleParent(T, T->getBase());
  }

  FoundResult visitMetatypeTypeRepr(MetatypeTypeRepr *T) {
    return handleParent(T, T->getBase());
  }

  FoundResult visitFixedTypeRepr(FixedTypeRepr *T) {
    return handleParent(T, ArrayRef<TypeRepr*>());
  }
};

struct SyntacticMigratorPass::Implementation : public SourceEntityWalker {
  SourceFile *SF;
  const StringRef FileName;
  unsigned BufferId;
  SourceManager &SM;
  EditorAdapter &Editor;
  const MigratorOptions &Opts;

  APIDiffItemStore DiffStore;

  std::vector<APIDiffItem*> getRelatedDiffItems(ValueDecl *VD) {
    std::vector<APIDiffItem*> results;
    auto addDiffItems = [&](ValueDecl *VD) {
      llvm::SmallString<64> Buffer;
      llvm::raw_svector_ostream OS(Buffer);
      if (swift::ide::printDeclUSR(VD, OS))
        return;
      auto Items = DiffStore.getDiffItems(Buffer.str());
      results.insert(results.end(), Items.begin(), Items.end());
    };

    addDiffItems(VD);
    for (auto *Overridden: getOverriddenDecls(VD, /*IncludeProtocolReqs=*/true,
                                              /*Transitive=*/true)) {
      addDiffItems(Overridden);
    }
    return results;
  }

  DeclNameViewer getFuncRename(ValueDecl *VD, bool &IgnoreBase) {
    for (auto *Item: getRelatedDiffItems(VD)) {
      if (auto *CI = dyn_cast<CommonDiffItem>(Item)) {
        if (CI->isRename()) {
          IgnoreBase = true;
          switch(CI->NodeKind) {
          case SDKNodeKind::Function:
            IgnoreBase = false;
            LLVM_FALLTHROUGH;
          case SDKNodeKind::Constructor:
            return DeclNameViewer(CI->getNewName());
          default:
            return DeclNameViewer();
          }
        }
      }
    }
    return DeclNameViewer();
  }

  bool isSimpleReplacement(APIDiffItem *Item, std::string &Text) {
    if (auto *MD = dyn_cast<TypeMemberDiffItem>(Item)) {
      // We need to pull the self if self index is set.
      if (MD->selfIndex.hasValue())
        return false;
      Text = (llvm::Twine(MD->newTypeName) + "." + MD->newPrintedName).str();
      return true;
    }

    // Simple rename.
    if (auto CI = dyn_cast<CommonDiffItem>(Item)) {
      if (CI->NodeKind == SDKNodeKind::Var && CI->isRename()) {
        Text = CI->getNewName();
        return true;
      }
    }
    return false;
  }

  Implementation(SourceFile *SF, EditorAdapter &Editor,
                 const MigratorOptions &Opts) :
    SF(SF), FileName(SF->getFilename()), BufferId(SF->getBufferID().getValue()),
      SM(SF->getASTContext().SourceMgr), Editor(Editor), Opts(Opts) {}

  void run() {
    if (Opts.APIDigesterDataStorePath.empty())
      return;
    DiffStore.addStorePath(Opts.APIDigesterDataStorePath);
    DiffStore.printIncomingUsr(Opts.DumpUsr);
    walk(SF);
  }

  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                          Type T, ReferenceMetaData Data) override {
    for (auto *Item: getRelatedDiffItems(D)) {
      std::string RepText;
      if (isSimpleReplacement(Item, RepText)) {
        Editor.replace(Range, RepText);
        return true;
      }
    }
    return true;
  }

  struct ReferenceCollector : public SourceEntityWalker {
    ValueDecl *Target;
    CharSourceRange Result;
    ReferenceCollector(ValueDecl* Target) : Target(Target) {}
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                            Type T, ReferenceMetaData Data) override {
      if (D == Target) {
        Result = Range;
        return false;
      }
      return true;
    }
  };

  void handleFuncRename(ValueDecl *FD, Expr* FuncRefContainer, Expr *Arg) {
    bool IgnoreBase = false;
    if (auto View = getFuncRename(FD, IgnoreBase)) {
      if (!IgnoreBase) {
        ReferenceCollector Walker(FD);
        Walker.walk(FuncRefContainer);
        Editor.replace(Walker.Result, View.base());
      }
      unsigned Idx = 0;
      for (auto LR :getCallArgLabelRanges(SM, Arg,
                                          LabelRangeEndAt::LabelNameOnly)) {
        if (Idx < View.argSize()) {
          auto Label = View.args()[Idx++];

          // FIXME: We update only when args are consistently valid.
          if (Label != "_" && LR.getByteLength())
            Editor.replace(LR, Label);
        }
      }
    }
  }

  void handleFunctionCallToPropertyChange(ValueDecl *FD, Expr* FuncRefContainer,
                                          Expr *Arg) {
    for (auto *Item : getRelatedDiffItems(FD)) {
      if (auto *CD = dyn_cast<CommonDiffItem>(Item)) {
        switch (CD->DiffKind) {
        case NodeAnnotation::GetterToProperty: {
          // Remove "()"
          Editor.remove(Lexer::getCharSourceRangeFromSourceRange(SM,
                                                        Arg->getSourceRange()));
          return;
        }
        case NodeAnnotation::SetterToProperty: {
          ReferenceCollector Walker(FD);
          Walker.walk(FuncRefContainer);
          auto ReplaceRange = CharSourceRange(SM, Walker.Result.getStart(),
                                          Arg->getStartLoc().getAdvancedLoc(1));

          // Replace "x.getY(" with "x.Y =".
          Editor.replace(ReplaceRange, (llvm::Twine(Walker.Result.str().
                                                   substr(3)) + " = ").str());
          // Remove ")"
          Editor.remove(CharSourceRange(SM, Arg->getEndLoc(), Arg->getEndLoc().
                                        getAdvancedLoc(1)));
          return;
        }
        default:
          break;
        }
      }
    }
  }

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
    //   error: cannot convert value of type '(_, _) -> ()' to expected argument type '((Int, Int)) -> ()'
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

      auto Fn = CE->getFn();
      auto Args = CE->getArg();
      switch (Fn->getKind()) {
      case ExprKind::DeclRef: {
        if (auto FD = Fn->getReferencedDecl().getDecl())
          handleFuncRename(FD, Fn, Args);
        break;
      }
      case ExprKind::DotSyntaxCall: {
        auto DSC = cast<DotSyntaxCallExpr>(Fn);
        if (auto FD = DSC->getFn()->getReferencedDecl().getDecl()) {
          handleFuncRename(FD, DSC->getFn(), Args);
          handleFunctionCallToPropertyChange(FD, DSC->getFn(), Args);
        }
        break;
      }
      case ExprKind::ConstructorRefCall: {
        auto CCE = cast<ConstructorRefCallExpr>(Fn);
        if (auto FD = CCE->getFn()->getReferencedDecl().getDecl())
          handleFuncRename(FD, CCE->getFn(), Args);
        break;
      }
      default:
        break;
      }
    }
    return true;
  }

  void handleFuncDeclRename(AbstractFunctionDecl *AFD,
                            CharSourceRange NameRange) {
    bool IgnoreBase = false;
    if (auto View = getFuncRename(AFD, IgnoreBase)) {
      if (!IgnoreBase)
        Editor.replace(NameRange, View.base());
      unsigned Index = 0;
      for (auto PL : AFD->getParameterLists()) {
        for (auto *PD : *PL) {
          if (Index == View.argSize())
            break;
          // Self parameter should not be updated.
          if (PD->isSelfParameter())
            continue;
          StringRef NewArg = View.args()[Index++];
          auto ArgLoc = PD->getArgumentNameLoc();

          // If the argument name is not specified, add the argument name before
          // the paramter name.
          if (ArgLoc.isInvalid())
            Editor.insertBefore(PD->getNameLoc(),
                                (llvm::Twine(NewArg) + " ").str());
          else
            // Otherwise, replace the argument name directly.
            Editor.replaceToken(ArgLoc, NewArg);
        }
      }
    }
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (D->isImplicit())
      return true;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      handleFuncDeclRename(AFD, Range);
      for (auto *Item: getRelatedDiffItems(AFD)) {
        if (auto *DiffItem = dyn_cast<CommonDiffItem>(Item)) {
          if (!DiffItem->isTypeChange())
            continue;

          ChildIndexFinder Finder(DiffItem->getChildIndices());
          auto Result = Finder.findChild(AFD);
          if (!Result.isValid())
            return false;

          switch (DiffItem->DiffKind) {
          case ide::api::NodeAnnotation::WrapOptional:
            if (Result.Suffixable) {
              Editor.insertAfterToken(Result.TokenRange.End, "?");
            } else {
              Editor.insertWrap("(", Result.TokenRange, ")?");
            }
            break;
          case ide::api::NodeAnnotation::WrapImplicitOptional:
            if (Result.Suffixable) {
              Editor.insertAfterToken(Result.TokenRange.End, "!");
            } else {
              Editor.insertWrap("(", Result.TokenRange, (")!"));
            }
            break;
          case ide::api::NodeAnnotation::UnwrapOptional:
            if (Result.Optional)
              Editor.remove(Result.TokenRange.End);
            break;
          case ide::api::NodeAnnotation::ImplicitOptionalToOptional:
            if (Result.Optional)
              Editor.replace(Result.TokenRange.End, "?");
            break;
          case ide::api::NodeAnnotation::TypeRewritten:
            Editor.replace(Result.TokenRange, DiffItem->RightComment);
            break;
          default:
            break;
          }
        }
      }
    }
    return true;
  }
};

SyntacticMigratorPass::
SyntacticMigratorPass(EditorAdapter &Editor, SourceFile *SF,
  const MigratorOptions &Opts) : Impl(*new Implementation(SF, Editor, Opts)) {}

SyntacticMigratorPass::~SyntacticMigratorPass() { delete &Impl; }

void SyntacticMigratorPass::run() { Impl.run(); }

const clang::edit::Commit &SyntacticMigratorPass::getEdits() const {
  return Impl.Editor.getEdits();
}
