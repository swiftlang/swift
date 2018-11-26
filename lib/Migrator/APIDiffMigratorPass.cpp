//===--- APIDiffMigratorPass.cpp ------------------------------------------===//
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
#include "swift/Basic/StringExtras.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/Utils.h"
#include "swift/Index/Utils.h"
#include "swift/Migrator/ASTMigratorPass.h"
#include "swift/Migrator/EditorAdapter.h"
#include "swift/Migrator/FixitApplyDiagnosticConsumer.h"
#include "swift/Migrator/Migrator.h"
#include "swift/Migrator/RewriteBufferEditsReceiver.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Edit/EditedSource.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/Support/FileSystem.h"
#include "swift/IDE/APIDigesterData.h"
#include "swift/Basic/Defer.h"

using namespace swift;
using namespace swift::migrator;
using namespace swift::ide;
using namespace swift::ide::api;

namespace {

struct FoundResult {
  SourceRange TokenRange;
  bool Optional; // Range includes a trailing ? or !, e.g. [SomeType!]
  bool Suffixable; // No need to wrap parens when adding optionality
  bool Suffixed; // Range is followed by a trailing ? or !, e.g. [SomeType]!
  bool isValid() const { return TokenRange.isValid(); }
};

class ChildIndexFinder : public TypeReprVisitor<ChildIndexFinder, FoundResult> {
  ArrayRef<uint8_t> ChildIndices;
  bool ParentIsOptional;

public:
  ChildIndexFinder(ArrayRef<uint8_t> ChildIndices) :
    ChildIndices(ChildIndices) {}

  FoundResult findChild(AbstractFunctionDecl *Parent) {
    ParentIsOptional = false;
    auto NextIndex = consumeNext();
    if (!NextIndex) {
      if (auto Func = dyn_cast<FuncDecl>(Parent))
        return findChild(Func->getBodyResultTypeLoc());
      if (auto Init = dyn_cast<ConstructorDecl>(Parent)) {
        SourceLoc End = Init->getFailabilityLoc();
        bool Optional = End.isValid();
        if (!Optional)
          End = Init->getNameLoc();
        return {SourceRange(Init->getNameLoc(), End), Optional,
          /*suffixable=*/true, /*suffixed=*/false};
      }
      return {SourceRange(), false, false, false};
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
      return {SourceRange(), false, false, false};
    return visit(Loc.getTypeRepr());
  }

public:

  template<typename T>
  FoundResult handleParent(TypeRepr *Parent, const ArrayRef<T> Children,
                           bool Optional = false, bool Suffixable = true) {
    if (!hasNextIndex())
      return {
        Parent->getSourceRange(),
        Optional, Suffixable, /*Suffixed=*/ParentIsOptional
      };
    auto NextIndex = consumeNext();
    if (isUserTypeAlias(Parent))
      return {SourceRange(), false, false, false};
    assert(NextIndex < Children.size());
    TypeRepr *Child = Children[NextIndex];
    ParentIsOptional = Optional;
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
    return {SourceRange(), false, false, false};
  }

  FoundResult visitAttributedTypeRepr(AttributedTypeRepr *T) {
    return visit(T->getTypeRepr());
  }

  FoundResult visitInOutTypeRepr(InOutTypeRepr *T) {
    return visit(T->getBase());
  }

  FoundResult visitSharedTypeRepr(SharedTypeRepr *T) {
    return visit(T->getBase());
  }

  FoundResult visitOwnedTypeRepr(OwnedTypeRepr *T) {
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
    if (T->getNumElements() == 1) {
      ParentIsOptional = false;
      return visit(T->getElementType(0));
    }
    llvm::SmallVector<TypeRepr *, 8> Children;
    T->getElementTypes(Children);
    return handleParent(T, ArrayRef<TypeRepr *>(Children));
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
    return visit(T->getComponents().back());
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

static ValueDecl* getReferencedDecl(Expr *E) {
  if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
    return DRE->getDecl();
  } else if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
    return MRE->getMember().getDecl();
  } else if (auto OtherCtorE = dyn_cast<OtherConstructorDeclRefExpr>(E)) {
    return OtherCtorE->getDecl();
  } else {
    return nullptr;
  }
}

struct APIDiffMigratorPass : public ASTMigratorPass, public SourceEntityWalker {

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
          case SDKNodeKind::DeclFunction:
            IgnoreBase = false;
            LLVM_FALLTHROUGH;
          case SDKNodeKind::DeclConstructor:
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
      if (MD->Subkind == TypeMemberDiffItemSubKind::SimpleReplacement) {
        Text = (llvm::Twine(MD->newTypeName) + "." + MD->getNewName().base()).
          str();
        return true;
      }
    }

    // Simple rename.
    if (auto CI = dyn_cast<CommonDiffItem>(Item)) {
      if (CI->NodeKind == SDKNodeKind::DeclVar && CI->isRename()) {
        Text = CI->getNewName();
        return true;
      }
    }
    return false;
  }

  std::set<std::string> InsertedFunctions;
  SourceLoc FileEndLoc;
  APIDiffMigratorPass(EditorAdapter &Editor, SourceFile *SF,
                      const MigratorOptions &Opts):
    ASTMigratorPass(Editor, SF, Opts),
    FileEndLoc(SM.getRangeForBuffer(BufferID).getEnd()) {}

  void run() {
    if (Opts.APIDigesterDataStorePaths.empty())
      return;
    for (auto Path : Opts.APIDigesterDataStorePaths)
      DiffStore.addStorePath(Path);
    DiffStore.printIncomingUsr(Opts.DumpUsr);
    walk(SF);
  }

  bool updateStringRepresentableDeclRef(APIDiffItem *Diff,
      CharSourceRange Range) {
    auto *CD = dyn_cast<CommonDiffItem>(Diff);
    if (!CD)
      return false;
    if (CD->NodeKind != SDKNodeKind::DeclVar)
      return false;
    if (!CD->isStringRepresentableChange())
      return false;
    switch(CD->DiffKind) {
    case NodeAnnotation::SimpleStringRepresentableUpdate:
      Editor.insert(Range.getEnd(), ".rawValue");
      return true;
    default:
      return false;
    }
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
      if (updateStringRepresentableDeclRef(Item, Range)) {
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

  void emitRenameLabelChanges(Expr *Arg, DeclNameViewer NewName,
                              llvm::ArrayRef<unsigned> IgnoreArgIndex) {
    unsigned Idx = 0;
    auto Ranges = getCallArgLabelRanges(SM, Arg,
                                        LabelRangeEndAt::LabelNameOnly);
    llvm::SmallVector<uint8_t, 2> ToRemoveIndices;
    for (unsigned I = 0; I < Ranges.size(); I ++) {
      if (std::any_of(IgnoreArgIndex.begin(), IgnoreArgIndex.end(),
                      [I](unsigned Ig) { return Ig == I; }))
        continue;
      auto LR = Ranges[I];
      if (Idx < NewName.argSize()) {
        auto Label = NewName.args()[Idx++];

        if (!Label.empty()) {
          if (LR.getByteLength())
            Editor.replace(LR, Label);
          else
            Editor.insert(LR.getStart(), (llvm::Twine(Label) + ": ").str());
        } else if (LR.getByteLength()){
          // New label is "_" however the old label is explicit.
          ToRemoveIndices.push_back(I);
        }
      }
    }
    if (!ToRemoveIndices.empty()) {
      auto Ranges = getCallArgLabelRanges(SM, Arg,
                                         LabelRangeEndAt::BeforeElemStart);
      for (auto I : ToRemoveIndices) {
        Editor.remove(Ranges[I]);
      }
    }
  }

  void handleFuncRename(ValueDecl *FD, Expr* FuncRefContainer, Expr *Arg) {
    bool IgnoreBase = false;
    if (auto View = getFuncRename(FD, IgnoreBase)) {
      if (!IgnoreBase) {
        ReferenceCollector Walker(FD);
        Walker.walk(FuncRefContainer);
        Editor.replace(Walker.Result, View.base());
      }
      emitRenameLabelChanges(Arg, View, {});
    }
  }

  bool handleQualifiedReplacement(Expr* Call) {
    auto handleDecl = [&](ValueDecl *VD, SourceRange ToReplace) {
      for (auto *I: getRelatedDiffItems(VD)) {
        if (auto *Item = dyn_cast<TypeMemberDiffItem>(I)) {
          if (Item->Subkind == TypeMemberDiffItemSubKind::QualifiedReplacement) {
            Editor.replace(ToReplace, (llvm::Twine(Item->newTypeName) + "." +
              Item->getNewName().base()).str());
            return true;
          }
        }
      }
      return false;
    };
    if (auto *DSC = dyn_cast<DotSyntaxCallExpr>(Call)) {
      if (auto FD = DSC->getFn()->getReferencedDecl().getDecl()) {
        if (handleDecl(FD, Call->getSourceRange()))
          return true;
      }
    } else if (auto MRE = dyn_cast<MemberRefExpr>(Call)) {
      if (handleDecl(MRE->getReferencedDecl().getDecl(), MRE->getSourceRange()))
        return true;
    }
    return false;
  }

  bool handleSpecialCases(ValueDecl *FD, CallExpr* Call, Expr *Arg) {
    SpecialCaseDiffItem *Item = nullptr;
    for (auto *I: getRelatedDiffItems(FD)) {
      Item = dyn_cast<SpecialCaseDiffItem>(I);
      if (Item)
        break;
    }
    if (!Item)
      return false;
    std::vector<CallArgInfo> AllArgs =
      getCallArgInfo(SM, Arg, LabelRangeEndAt::LabelNameOnly);
    switch(Item->caseId) {
    case SpecialCaseId::NSOpenGLSetOption: {
      // swift 3.2:
      // NSOpenGLSetOption(NSOpenGLGOFormatCacheSize, 5)
      // swift 4:
      // NSOpenGLGOFormatCacheSize.globalValue = 5
      CallArgInfo &FirstArg = AllArgs[0];
      CallArgInfo &SecondArg = AllArgs[1];
      Editor.remove(CharSourceRange(SM, Call->getStartLoc(),
                                    FirstArg.ArgExp->getStartLoc()));
      Editor.replace(CharSourceRange(SM, Lexer::getLocForEndOfToken(SM,
        FirstArg.ArgExp->getEndLoc()), SecondArg.LabelRange.getStart()),
                     ".globalValue = ");
      Editor.remove(Call->getEndLoc());
      return true;
    }
    case SpecialCaseId::NSOpenGLGetOption: {
      // swift 3.2:
      // NSOpenGLGetOption(NSOpenGLGOFormatCacheSize, &cacheSize)
      // swift 4:
      // cacheSize = NSOpenGLGOFormatCacheSize.globalValue
      CallArgInfo &FirstArg = AllArgs[0];
      CallArgInfo &SecondArg = AllArgs[1];

      StringRef SecondArgContent = SecondArg.getEntireCharRange(SM).str();
      if (SecondArgContent[0] == '&')
        SecondArgContent = SecondArgContent.substr(1);

      Editor.replace(CharSourceRange(SM, Call->getStartLoc(),
                                     FirstArg.ArgExp->getStartLoc()),
                     (llvm::Twine(SecondArgContent) + " = ").str());
      Editor.replace(CharSourceRange(SM, FirstArg.getEntireCharRange(SM).getEnd(),
                                     Lexer::getLocForEndOfToken(SM, Call->getEndLoc())),
                     ".globalValue");
      return true;
    }
    case SpecialCaseId::StaticAbsToSwiftAbs:
      // swift 3:
      // CGFloat.abs(1.0)
      // Float.abs(1.0)
      // Double.abs(1.0)
      // Float80.abs(1.0)
      //
      // swift 4:
      // Swift.abs(1.0)
      // Swift.abs(1.0)
      // Swift.abs(1.0)
      // Swift.abs(1.0)
      Editor.replace(Call->getFn()->getSourceRange(), "Swift.abs");
      return true;
    case SpecialCaseId::ToUIntMax:
      if (const auto *DotCall = dyn_cast<DotSyntaxCallExpr>(Call->getFn())) {
        Editor.insert(DotCall->getStartLoc(), "UInt64(");
        Editor.replace({ DotCall->getDotLoc(), Call->getEndLoc() }, ")");
        return true;
      }
      return false;
    case SpecialCaseId::ToIntMax:
      if (const auto *DotCall = dyn_cast<DotSyntaxCallExpr>(Call->getFn())) {
        Editor.insert(DotCall->getStartLoc(), "Int64(");
        Editor.replace({ DotCall->getDotLoc(), Call->getEndLoc() }, ")");
        return true;
      }
      return false;
    case SpecialCaseId::NSOpenGLGetVersion: {
      if (const auto *Tuple = dyn_cast<TupleExpr>(Arg)) {
        if (Tuple->getNumElements() != 2) {
          return false;
        }

        auto extractArg = [](const Expr *Arg) -> const DeclRefExpr * {
          while (const auto *ICE = dyn_cast<ImplicitConversionExpr>(Arg)) {
            Arg = ICE->getSubExpr();
          }
          if (const auto *IOE = dyn_cast<InOutExpr>(Arg)) {
            return dyn_cast<DeclRefExpr>(IOE->getSubExpr());
          }
          return nullptr;
        };

        const auto *Arg0 = extractArg(Tuple->getElement(0));
        const auto *Arg1 = extractArg(Tuple->getElement(1));

        if (!(Arg0 && Arg1)) {
          return false;
        }
        SmallString<256> Scratch;
        llvm::raw_svector_ostream OS(Scratch);
        auto StartLoc = Call->getStartLoc();
        Editor.insert(StartLoc, "(");
        Editor.insert(StartLoc,
          SM.extractText(Lexer::getCharSourceRangeFromSourceRange(SM,
            Arg0->getSourceRange())));
        Editor.insert(StartLoc, ", ");
        Editor.insert(StartLoc,
          SM.extractText(Lexer::getCharSourceRangeFromSourceRange(SM,
            Arg1->getSourceRange())));
        Editor.insert(StartLoc, ") = ");
        Editor.replace(Call->getSourceRange(), "NSOpenGLContext.openGLVersion");
        return true;
      }
      return false;
    }
    }
  }

  bool handleTypeHoist(ValueDecl *FD, CallExpr* Call, Expr *Arg) {
    TypeMemberDiffItem *Item = nullptr;
    for (auto *I: getRelatedDiffItems(FD)) {
      Item = dyn_cast<TypeMemberDiffItem>(I);
      if (Item)
        break;
    }
    if (!Item)
      return false;
    if (Item->Subkind == TypeMemberDiffItemSubKind::SimpleReplacement ||
        Item->Subkind == TypeMemberDiffItemSubKind::QualifiedReplacement)
      return false;

    if (Item->Subkind == TypeMemberDiffItemSubKind::GlobalFuncToStaticProperty) {
      Editor.replace(Call->getSourceRange(), (llvm::Twine(Item->newTypeName) +
        "." + Item->getNewName().base()).str());
      return true;
    }
    if (*Item->selfIndex)
      return false;
    std::vector<CallArgInfo> AllArgs =
      getCallArgInfo(SM, Arg, LabelRangeEndAt::LabelNameOnly);
    if (!AllArgs.size())
      return false;
    assert(*Item->selfIndex == 0 && "we cannot handle otherwise");
    DeclNameViewer NewName = Item->getNewName();
    llvm::SmallVector<unsigned, 2> IgnoredArgIndices;
    IgnoredArgIndices.push_back(*Item->selfIndex);
    if (auto RI = Item->removedIndex)
      IgnoredArgIndices.push_back(*RI);
    emitRenameLabelChanges(Arg, NewName, IgnoredArgIndices);
    auto *SelfExpr = AllArgs[0].ArgExp;
    if (auto *IOE = dyn_cast<InOutExpr>(SelfExpr))
      SelfExpr = IOE->getSubExpr();
    const bool NeedParen = !SelfExpr->canAppendPostfixExpression();

    // Remove the global function name: "Foo(a, b..." to "a, b..."
    Editor.remove(CharSourceRange(SM, Call->getStartLoc(),
                                  SelfExpr->getStartLoc()));
    if (NeedParen)
      Editor.insert(SelfExpr->getStartLoc(), "(");
    std::string MemberFuncBase;
    if (Item->Subkind == TypeMemberDiffItemSubKind::HoistSelfAndUseProperty)
      MemberFuncBase = (llvm::Twine(NeedParen ? ")." : ".") + Item->getNewName().
        base()).str();
    else
      MemberFuncBase = (llvm::Twine(NeedParen ? ")." : ".") + Item->getNewName().
        base() + "(").str();

    if (AllArgs.size() > 1) {
      Editor.replace(CharSourceRange(SM, Lexer::getLocForEndOfToken(SM,
        SelfExpr->getEndLoc()), AllArgs[1].LabelRange.getStart()),
                     MemberFuncBase);
    } else {
      Editor.insert(Lexer::getLocForEndOfToken(SM, SelfExpr->getEndLoc()),
                    MemberFuncBase);
    }

    switch (Item->Subkind) {
    case TypeMemberDiffItemSubKind::GlobalFuncToStaticProperty:
    case TypeMemberDiffItemSubKind::SimpleReplacement:
    case TypeMemberDiffItemSubKind::QualifiedReplacement:
      llvm_unreachable("should be handled elsewhere");
    case TypeMemberDiffItemSubKind::HoistSelfOnly:
      // we are done here.
      return true;
    case TypeMemberDiffItemSubKind::HoistSelfAndRemoveParam: {
      unsigned RI = *Item->removedIndex;
      CallArgInfo &ToRemove = AllArgs[RI];
      if (AllArgs.size() == RI + 1) {
        Editor.remove(ToRemove.getEntireCharRange(SM));
      } else {
        CallArgInfo &AfterToRemove = AllArgs[RI + 1];
        Editor.remove(CharSourceRange(SM, ToRemove.LabelRange.getStart(),
                                      AfterToRemove.LabelRange.getStart()));
      }
      return true;
    }
    case TypeMemberDiffItemSubKind::HoistSelfAndUseProperty:
      // Remove ).
      Editor.remove(Arg->getEndLoc());
      return true;
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
          auto Replacement = (llvm::Twine(Walker.Result.str()
                                          .substr(3)) + " = ").str();
          SmallString<64> Scratch;
          Editor.replace(ReplaceRange,
                         camel_case::toLowercaseInitialisms(Replacement, Scratch));

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

  bool wrapAttributeReference(Expr* Reference, Expr* WrapperTarget,
                              bool FromString) {
    auto *RD = getReferencedDecl(Reference);
    if (!RD)
      return false;
    for (auto *Item: getRelatedDiffItems(RD)) {
      if (auto *CI = dyn_cast<CommonDiffItem>(Item)) {
        if (CI->isStringRepresentableChange() &&
            CI->NodeKind == SDKNodeKind::DeclVar) {
          SmallString<256> Buffer;
          auto Func = insertHelperFunction(CI->DiffKind, CI->RightComment,
                                           Buffer, FromString);
          Editor.insert(WrapperTarget->getStartLoc(), (Twine(Func) + "(").str());
          Editor.insertAfterToken(WrapperTarget->getEndLoc(), ")");
          return true;
        }
      }
    }
    return false;
  }

  bool handleAssignDestMigration(Expr *E) {
    auto *ASE = dyn_cast<AssignExpr>(E);
    if (!ASE || !ASE->getDest() || !ASE->getSrc())
      return false;
    return wrapAttributeReference(ASE->getDest(), ASE->getSrc(), true);
  }

  bool handleAttributeReference(Expr *E) {
    return wrapAttributeReference(E, E, false);
  }

  StringRef insertHelperFunction(NodeAnnotation Anno, StringRef NewType,
                                 SmallString<256> &Buffer, bool FromString) {
    llvm::raw_svector_ostream OS(Buffer);
    OS << "\n";
    OS << "// Helper function inserted by Swift 4.2 migrator.\n";
    OS << "fileprivate func ";
    unsigned FuncNameStart = Buffer.size();
    OS << (FromString ? "convertTo" : "convertFrom");
    SmallVector<std::string, 8> Segs;
    StringRef guard = "\tguard let input = input else { return nil }\n";
    switch(Anno) {
    case NodeAnnotation::OptionalArrayMemberUpdate:
      Segs = {"Optional", "Array", "[String]?"};
      Segs.push_back((Twine("[") + NewType +"]?").str());
      Segs.push_back((Twine(guard) + "\treturn input.map { key in " + NewType +"(key) }").str());
      Segs.push_back((Twine(guard) + "\treturn input.map { key in key.rawValue }").str());
      break;
    case NodeAnnotation::OptionalDictionaryKeyUpdate:
      Segs = {"Optional", "Dictionary", "[String: Any]?"};
      Segs.push_back((Twine("[") + NewType +": Any]?").str());
      Segs.push_back((Twine(guard) +
                      "\treturn Dictionary(uniqueKeysWithValues: input.map"
                      " { key, value in (" + NewType + "(rawValue: key), value)})").str());
      Segs.push_back((Twine(guard) +
                      "\treturn Dictionary(uniqueKeysWithValues: input.map"
                      " {key, value in (key.rawValue, value)})").str());
      break;
    case NodeAnnotation::ArrayMemberUpdate:
      Segs = {"", "Array", "[String]"};
      Segs.push_back((Twine("[") + NewType +"]").str());
      Segs.push_back((Twine("\treturn input.map { key in ") + NewType +"(key) }").str());
      Segs.push_back("\treturn input.map { key in key.rawValue }");
      break;
    case NodeAnnotation::DictionaryKeyUpdate:
      Segs = {"", "Dictionary", "[String: Any]"};
      Segs.push_back((Twine("[") + NewType +": Any]").str());
      Segs.push_back((Twine("\treturn Dictionary(uniqueKeysWithValues: input.map"
        " { key, value in (") + NewType + "(rawValue: key), value)})").str());
      Segs.push_back("\treturn Dictionary(uniqueKeysWithValues: input.map"
                     " {key, value in (key.rawValue, value)})");
      break;
    case NodeAnnotation::SimpleStringRepresentableUpdate:
      Segs = {"", "", "String"};
      Segs.push_back(NewType);
      Segs.push_back((Twine("\treturn ") + NewType + "(rawValue: input)").str());
      Segs.push_back("\treturn input.rawValue");
      break;
    case NodeAnnotation::SimpleOptionalStringRepresentableUpdate:
      Segs = {"Optional", "", "String?"};
      Segs.push_back((Twine(NewType) +"?").str());
      Segs.push_back((Twine(guard) + "\treturn " + NewType + "(rawValue: input)").str());
      Segs.push_back((Twine(guard) + "\treturn input.rawValue").str());
      break;
    default:
      llvm_unreachable("shouldn't handle this key.");
    }
    assert(Segs.size() == 6);
    OS << Segs[0];
    SmallVector<StringRef, 4> Parts;
    NewType.split(Parts, '.');
    for (auto P: Parts)
      OS << P;
    OS << Segs[1];
    auto FuncName = Buffer.str().substr(FuncNameStart);
    if (!InsertedFunctions.count(FuncName)) {
      if (FromString) {
        OS << "(_ input: " << Segs[2] << ") -> " << Segs[3] << " {\n";
        OS << Segs[4] << "\n}\n";
      } else {
        OS << "(_ input: " << Segs[3] << ") -> " << Segs[2] << " {\n";
        OS << Segs[5] << "\n}\n";
      }
      Editor.insert(FileEndLoc, OS.str());
      InsertedFunctions.insert(FuncName);
    }
    return FuncName;
  }

  void handleStringRepresentableArg(ValueDecl *FD, Expr *Arg, Expr *Call) {
    Editor.disableCache();
    SWIFT_DEFER { Editor.enableCache(); };
    NodeAnnotation Kind;
    StringRef NewAttributeType;
    uint8_t ArgIdx;
    for (auto Item: getRelatedDiffItems(FD)) {
      if (auto *CI = dyn_cast<CommonDiffItem>(Item)) {
        if (CI->isStringRepresentableChange()) {
          Kind = CI->DiffKind;
          NewAttributeType = CI->RightComment;
          assert(CI->getChildIndices().size() == 1);
          ArgIdx = CI->getChildIndices().front();
          break;
        }
      }
    }
    if (NewAttributeType.empty())
      return;
    SmallString<256> Buffer;
    auto FuncName = insertHelperFunction(Kind, NewAttributeType, Buffer,
                                         /*FromString*/ArgIdx);
    if (ArgIdx) {
      ArgIdx --;
      auto AllArgs = getCallArgInfo(SM, Arg, LabelRangeEndAt::LabelNameOnly);
      if (AllArgs.size() <= ArgIdx)
        return;
      auto Exp = AllArgs[ArgIdx].ArgExp;
      Editor.insert(Exp->getStartLoc(), (Twine(FuncName) + "(").str());
      Editor.insertAfterToken(Exp->getEndLoc(), ")");
    } else {
      Editor.insert(Call->getStartLoc(), (Twine(FuncName) + "(").str());
      Editor.insertAfterToken(Call->getEndLoc(), ")");
    }
  }

  bool walkToExprPre(Expr *E) override {
    if (handleQualifiedReplacement(E))
      return false;
    if (handleAssignDestMigration(E) || handleAttributeReference(E))
      return false;
    if (auto *CE = dyn_cast<CallExpr>(E)) {
      auto Fn = CE->getFn();
      auto Args = CE->getArg();
      switch (Fn->getKind()) {
      case ExprKind::DeclRef: {
        if (auto FD = Fn->getReferencedDecl().getDecl()) {
          handleFuncRename(FD, Fn, Args);
          handleTypeHoist(FD, CE, Args);
          handleSpecialCases(FD, CE, Args);
          handleStringRepresentableArg(FD, Args, CE);
        }
        break;
      }
      case ExprKind::DotSyntaxCall: {
        auto DSC = cast<DotSyntaxCallExpr>(Fn);
        if (auto FD = DSC->getFn()->getReferencedDecl().getDecl()) {
          handleFuncRename(FD, DSC->getFn(), Args);
          handleFunctionCallToPropertyChange(FD, DSC->getFn(), Args);
          handleSpecialCases(FD, CE, Args);
          handleStringRepresentableArg(FD, Args, CE);
        }
        break;
      }
      case ExprKind::ConstructorRefCall: {
        auto CCE = cast<ConstructorRefCallExpr>(Fn);
        if (auto FD = CCE->getFn()->getReferencedDecl().getDecl()) {
          auto *CE = CCE->getFn();
          handleFuncRename(FD, CE, Args);
          handleStringRepresentableArg(FD, Args, CE);
        }
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

          // Represent empty label with underscore.
          if (NewArg.empty())
            NewArg = "_";

          // If the argument name is not specified, add the argument name before
          // the parameter name.
          if (ArgLoc.isInvalid())
            Editor.insertBefore(PD->getNameLoc(),
                                (llvm::Twine(NewArg) + " ").str());
          else {
            // Otherwise, replace the argument name directly.
            Editor.replaceToken(ArgLoc, NewArg);
          }
        }
      }
    }
  }

  bool typeReplacementMayNeedParens(StringRef Replacement) const {
    return Replacement.contains('&') || Replacement.contains("->");
  }

  void handleOverridingTypeChange(AbstractFunctionDecl *AFD,
                                  CommonDiffItem *DiffItem) {
    assert(AFD);
    assert(DiffItem->isTypeChange());
    ChildIndexFinder Finder(DiffItem->getChildIndices());
    auto Result = Finder.findChild(AFD);
    if (!Result.isValid())
      return;

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
        if (Result.Suffixed && typeReplacementMayNeedParens(DiffItem->RightComment)) {
          Editor.insertBefore(Result.TokenRange.Start, "(");
          Editor.insertAfterToken(Result.TokenRange.End, ")");
        }
        break;
      default:
        break;
    }
  }

  void handleOverridingPropertyChange(AbstractFunctionDecl *AFD,
                                      CommonDiffItem *DiffItem) {
    assert(AFD);
    assert(DiffItem->isToPropertyChange());
    auto FD = dyn_cast<FuncDecl>(AFD);
    if (!FD)
      return;

    switch (DiffItem->DiffKind) {
    case NodeAnnotation::GetterToProperty: {
      auto FuncLoc = FD->getFuncLoc();
      auto ReturnTyLoc = FD->getBodyResultTypeLoc().getSourceRange().Start;
      auto NameLoc = FD->getNameLoc();
      if (FuncLoc.isInvalid() || ReturnTyLoc.isInvalid() || NameLoc.isInvalid())
        break;

      // Replace "func" with "var"
      Editor.replaceToken(FuncLoc, "var");

      // Replace "() -> " with ": "
      Editor.replace(CharSourceRange(SM, Lexer::getLocForEndOfToken(SM, NameLoc),
        ReturnTyLoc), ": ");

      break;
    }
    case NodeAnnotation::SetterToProperty: {
      // FIXME: we should migrate this case too.
      break;
    }
    default:
      llvm_unreachable("should not be handled here.");
    }
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (D->isImplicit())
      return true;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      handleFuncDeclRename(AFD, Range);
      for (auto *Item: getRelatedDiffItems(AFD)) {
        if (auto *DiffItem = dyn_cast<CommonDiffItem>(Item)) {
          if (DiffItem->isTypeChange())
            handleOverridingTypeChange(AFD, DiffItem);
          else if (DiffItem->isToPropertyChange())
            handleOverridingPropertyChange(AFD, DiffItem);
        }
      }
    }
    return true;
  }
};

} // end anonymous namespace

void migrator::runAPIDiffMigratorPass(EditorAdapter &Editor,
                                      SourceFile *SF,
                                      const MigratorOptions &Opts) {
  APIDiffMigratorPass { Editor, SF, Opts }.run();
}
