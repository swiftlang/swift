#include "swift/IDE/CodeCompletion.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Sema/Lookup.h"
#include <algorithm>
#include <string>

using namespace swift;
using namespace code_completion;

std::string swift::code_completion::removeCodeCompletionTokens(
    StringRef Input, StringRef TokenName, unsigned *CompletionOffset) {
  assert(TokenName.size() >= 1);

  *CompletionOffset = ~0U;

  std::string CleanFile;
  CleanFile.reserve(Input.size());
  const std::string Token = std::string("#^") + TokenName.str() + "^#";

  for (const char *Ptr = Input.begin(), *End = Input.end();
       Ptr != End; ++Ptr) {
    const char C = *Ptr;
    if (C == '#' && Ptr <= End - Token.size() &&
        StringRef(Ptr, Token.size()) == Token) {
      Ptr += Token.size() - 1;
      *CompletionOffset = CleanFile.size() + 1;
      continue;
    }
    if (C == '#' && Ptr <= End - 2 && Ptr[1] == '^') {
      do {
        Ptr++;
      } while(*Ptr != '#');
      continue;
    }
    CleanFile += C;
  }
  return CleanFile;
}

CodeCompletionString::CodeCompletionString(ArrayRef<Chunk> Chunks) {
  Chunk *TailChunks = reinterpret_cast<Chunk *>(this + 1);
  std::copy(Chunks.begin(), Chunks.end(), TailChunks);
  NumChunks = Chunks.size();
}

void CodeCompletionString::print(raw_ostream &OS) const {
  unsigned PrevNestingLevel = 0;
  for (auto C : getChunks()) {
    if (C.getNestingLevel() < PrevNestingLevel) {
      OS << "#}";
    }
    switch (C.getKind()) {
    case Chunk::ChunkKind::Text:
    case Chunk::ChunkKind::LeftParen:
    case Chunk::ChunkKind::RightParen:
    case Chunk::ChunkKind::LeftBracket:
    case Chunk::ChunkKind::RightBracket:
    case Chunk::ChunkKind::Dot:
    case Chunk::ChunkKind::Comma:
    case Chunk::ChunkKind::CallParameterName:
    case Chunk::ChunkKind::CallParameterColon:
    case Chunk::ChunkKind::CallParameterType:
      OS << C.getText();
      break;
    case Chunk::ChunkKind::OptionalBegin:
    case Chunk::ChunkKind::CallParameterBegin:
      OS << "{#";
      break;
    case Chunk::ChunkKind::TypeAnnotation:
      OS << "[#";
      OS << C.getText();
      OS << "#]";
    }
    PrevNestingLevel = C.getNestingLevel();
  }
  while (PrevNestingLevel > 0) {
    OS << "#}";
    PrevNestingLevel--;
  }
}

void CodeCompletionString::dump() const {
  print(llvm::errs());
}

void CodeCompletionResult::print(raw_ostream &OS) const {
  switch (Kind) {
  case ResultKind::SwiftDeclaration:
    OS << "SwiftDecl: ";
    break;
  case ResultKind::ClangDeclaration:
    OS << "ClangDecl: ";
    break;
  case ResultKind::Keyword:
    OS << "Keyword: ";
    break;
  case ResultKind::Pattern:
    OS << "Pattern: ";
    break;
  }
  CompletionString->print(OS);
}

void CodeCompletionResult::dump() const {
  print(llvm::errs());
}

void CodeCompletionResultBuilder::addChunkWithText(
    CodeCompletionString::Chunk::ChunkKind Kind, StringRef Text) {
  Chunks.push_back(CodeCompletionString::Chunk::createWithText(
      Kind, CurrentNestingLevel, Context.copyString(Text)));
}

CodeCompletionResult *CodeCompletionResultBuilder::takeResult() {
  void *Mem = Context.Allocator
      .Allocate(sizeof(CodeCompletionResult) +
                    Chunks.size() * sizeof(CodeCompletionString::Chunk),
                llvm::alignOf<CodeCompletionString>());
  switch (Kind) {
  case CodeCompletionResult::ResultKind::SwiftDeclaration:
    return new (Context.Allocator)
        CodeCompletionResult(new (Mem) CodeCompletionString(Chunks),
                             AssociatedDecl.get<const Decl *>());
  case CodeCompletionResult::ResultKind::ClangDeclaration:
    return new (Context.Allocator)
        CodeCompletionResult(new (Mem) CodeCompletionString(Chunks),
                             AssociatedDecl.get<const clang::Decl *>());
  case CodeCompletionResult::ResultKind::Keyword:
  case CodeCompletionResult::ResultKind::Pattern:
    return new (Context.Allocator)
        CodeCompletionResult(Kind, new (Mem) CodeCompletionString(Chunks));
  }
}

void CodeCompletionResultBuilder::finishResult() {
  Context.CurrentCompletionResults.push_back(takeResult());
}

StringRef CodeCompletionContext::copyString(StringRef String) {
  char *Mem = Allocator.Allocate<char>(String.size());
  std::copy(String.begin(), String.end(), Mem);
  return StringRef(Mem, String.size());
}

ArrayRef<CodeCompletionResult *> CodeCompletionContext::takeResults() {
  const size_t Count = CurrentCompletionResults.size();
  CodeCompletionResult **Results =
      Allocator.Allocate<CodeCompletionResult *>(Count);
  std::copy(CurrentCompletionResults.begin(), CurrentCompletionResults.end(),
            Results);
  CurrentCompletionResults.clear();
  return llvm::makeArrayRef(Results, Count);
}

namespace {
class CodeCompletionCallbacksImpl : public CodeCompletionCallbacks {
  CodeCompletionContext &CompletionContext;
  CodeCompletionConsumer &Consumer;
  TranslationUnit *const TU;

  template<typename ExprType>
  bool typecheckExpr(ExprType *&E) {
    assert(E && "should have an expression");

    DEBUG(llvm::dbgs() << "\nparsed:\n";
          E->print(llvm::dbgs());
          llvm::dbgs() << "\n");

    Expr *AsExpr = E;
    if (!typeCheckCompletionContextExpr(TU, AsExpr))
      return false;
    E = cast<ExprType>(AsExpr);

    DEBUG(llvm::dbgs() << "\type checked:\n";
          E->print(llvm::dbgs());
          llvm::dbgs() << "\n");
    return true;
  }

public:
  CodeCompletionCallbacksImpl(Parser &P,
                              CodeCompletionContext &CompletionContext,
                              CodeCompletionConsumer &Consumer)
      : CodeCompletionCallbacks(P), CompletionContext(CompletionContext),
        Consumer(Consumer), TU(P.TU) {
  }

  void completeExpr() override;
  void completeDotExpr(Expr *E) override;
  void completePostfixExprBeginning() override;
  void completePostfixExpr(Expr *E) override;
  void completeExprSuper(SuperRefExpr *SRE) override;
  void completeExprSuperDot(SuperRefExpr *SRE) override;
};
} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeExpr() {
  Parser::ParserPositionRAII RestorePosition(P);
  P.restoreParserPosition(ExprBeginPosition);

  // FIXME: implement fallback code completion.

  Consumer.handleResults(CompletionContext.takeResults());
}

namespace {
/// Build completions by doing visible decl lookup from a context.
class CompletionLookup : swift::VisibleDeclConsumer,
                         clang::VisibleDeclConsumer
{
  CodeCompletionContext &CompletionContext;
  ASTContext &SwiftContext;
  const DeclContext *CurrDeclContext;

  enum class LookupKind {
    ValueExpr,
    DeclContext
  };

  LookupKind Kind;
  Type ExprType;
  bool HaveDot = false;
  bool IsSuperRefExpr = false;

  /// \brief True if we are code completing inside a static method.
  bool InsideStaticMethod = false;

  /// \brief DeclContext of the inner method of the code completion point.
  const DeclContext *CurrMethodDC = nullptr;

  bool needDot() const {
    return Kind == LookupKind::ValueExpr && !HaveDot;
  }

public:
  CompletionLookup(CodeCompletionContext &CompletionContext,
                   ASTContext &SwiftContext,
                   const DeclContext *CurrDeclContext)
      : CompletionContext(CompletionContext), SwiftContext(SwiftContext),
        CurrDeclContext(CurrDeclContext) {
    // Determine if we are doing code completion inside a static method.
    if (CurrDeclContext->isLocalContext()) {
      const DeclContext *FunctionDC = CurrDeclContext;
      while (FunctionDC->isLocalContext()) {
        const DeclContext *Parent = FunctionDC->getParent();
        if (!Parent->isLocalContext())
          break;
        FunctionDC = Parent;
      }
      if (auto *FE = dyn_cast<FuncExpr>(FunctionDC)) {
        auto *FD = FE->getDecl();
        if (FD->getDeclContext()->isTypeContext()) {
          CurrMethodDC = FunctionDC;
          InsideStaticMethod = FD->isStatic();
        }
      }
    }
  }

  void setHaveDot() {
    HaveDot = true;
  }

  void setIsSuperRefExpr() {
    IsSuperRefExpr = true;
  }

  void addTypeAnnotation(CodeCompletionResultBuilder &Builder, Type T) {
    if (T->isVoid())
      Builder.addTypeAnnotation("Void");
    else
      Builder.addTypeAnnotation(T.getString());
  }

  void addSwiftVarDeclRef(const VarDecl *VD) {
    StringRef Name = VD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    if (InsideStaticMethod &&
        VD->getDeclContext() == CurrMethodDC->getParent()) {
      // Can not refer to an instance variable from a static function.
      // FIXME: this should be probably handled by name lookup.
      // FIXME: static variables.
      return;
    }

    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(VD);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(Name);
    addTypeAnnotation(Builder, VD->getType());
  }

  void addPatternParameters(CodeCompletionResultBuilder &Builder,
                            const Pattern *P) {
    if (auto *TP = dyn_cast<TuplePattern>(P)) {
      bool NeedComma = false;
      for (auto TupleElt : TP->getFields()) {
        if (NeedComma)
          Builder.addComma(", ");
        StringRef BoundNameStr;
        Identifier BoundName = TupleElt.getPattern()->getBoundName();
        if (BoundName.get())
          BoundNameStr = BoundName.str();
        Builder.addCallParameter(BoundNameStr,
                                 TupleElt.getPattern()->getType().getString());
        NeedComma = true;
      }
      return;
    }
    auto *PP = cast<ParenPattern>(P);
    Builder.addCallParameter("", PP->getSubPattern()->getType().getString());
  }

  void addSwiftFunctionCall(const AnyFunctionType *AFT) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Pattern);
    Builder.addLeftParen();
    bool NeedComma = false;
    for (auto TupleElt : AFT->getInput()->castTo<TupleType>()->getFields()) {
      if (NeedComma)
        Builder.addComma(", ");
      Builder.addCallParameter(TupleElt.getName().str(),
                               TupleElt.getType().getString());
      NeedComma = true;
    }
    Builder.addRightParen();
    addTypeAnnotation(Builder, AFT->getResult());
  }

  void addSwiftMethodCall(const FuncDecl *FD) {
    StringRef Name = FD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    if (InsideStaticMethod && !FD->isStatic() &&
        FD->getDeclContext() == CurrMethodDC->getParent()) {
      // If we got an instance function while code completing in a static
      // function, then don't display the result.
      // FIXME: referencing an instance function from a static function is
      // allowed by the language, but the compiler crashes on this right now.
      // rdar://14432081
      return;
    }

    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(FD);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(Name);
    Builder.addLeftParen();
    auto *FE = FD->getBody();
    auto Patterns = FE->getArgParamPatterns();
    unsigned FirstIndex = 0;
    if (Patterns[0]->isImplicit())
      FirstIndex = 1;
    addPatternParameters(Builder, Patterns[FirstIndex]);
    Builder.addRightParen();
    // FIXME: Pattern should pretty-print itself.
    llvm::SmallString<32> TypeStr;
    for (unsigned i = FirstIndex + 1, e = Patterns.size(); i != e; ++i) {
      TypeStr += "(";
      for (auto TupleElt : cast<TuplePattern>(Patterns[i])->getFields()) {
        TypeStr += TupleElt.getPattern()->getBoundName().str();
        TypeStr += ": ";
        TypeStr += TupleElt.getPattern()->getType().getString();
      }
      TypeStr += ") -> ";
    }
    Type ResultType = FE->getResultType(SwiftContext);
    if (ResultType->isVoid())
      TypeStr += "Void";
    else
      TypeStr += ResultType.getString();
    Builder.addTypeAnnotation(TypeStr);

    // TODO: skip arguments with default parameters?
  }

  void addSwiftConstructorCall(const ConstructorDecl *CD) {
    assert(!HaveDot && "can not add a constructor call after a dot");
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(CD);
    if (IsSuperRefExpr && isa<ConstructorDecl>(CurrDeclContext)) {
      Builder.addTextChunk("constructor");
    }
    Builder.addLeftParen();
    addPatternParameters(Builder, CD->getArguments());
    Builder.addRightParen();
    addTypeAnnotation(Builder, CD->getResultType());
  }

  void addSwiftSubscriptCall(const SubscriptDecl *SD) {
    assert(!HaveDot && "can not add a subscript after a dot");
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(SD);
    Builder.addLeftBracket();
    addPatternParameters(Builder, SD->getIndices());
    Builder.addRightBracket();
    addTypeAnnotation(Builder, SD->getElementType());
  }

  void addSwiftNominalTypeRef(const NominalTypeDecl *NTD) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(NTD);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(NTD->getName().str());
    addTypeAnnotation(Builder,
                      MetaTypeType::get(NTD->getDeclaredType(), SwiftContext));
  }

  void addSwiftTypeAliasRef(const TypeAliasDecl *TAD) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(TAD);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(TAD->getName().str());
    Type TypeAnnotation;
    if (TAD->hasUnderlyingType())
      TypeAnnotation = MetaTypeType::get(TAD->getUnderlyingType(),
                                         SwiftContext);
    else
      TypeAnnotation = MetaTypeType::get(TAD->getDeclaredType(), SwiftContext);
    addTypeAnnotation(Builder, TypeAnnotation);
  }

  void addClangDecl(const clang::NamedDecl *ND) {
    // FIXME: Eventually, we'll import the Clang Decl and format it as a Swift
    // declaration.  Right now we don't do this because of performance reasons.
    StringRef Name = ND->getName();
    if (Name.empty())
      return;
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::ClangDeclaration);
    Builder.setAssociatedClangDecl(ND);
    Builder.addTextChunk(Name);
  }

  void addKeyword(StringRef Name, Type TypeAnnotation) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Keyword);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.isNull())
      addTypeAnnotation(Builder, TypeAnnotation);
  }

  // Implement swift::VisibleDeclConsumer
  void foundDecl(ValueDecl *D) override {
    switch (Kind) {
    case LookupKind::ValueExpr:
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        // Swift does not have class variables.
        // FIXME: add code completion results when class variables are added.
        if (ExprType->is<MetaTypeType>())
          return;
        addSwiftVarDeclRef(VD);
        return;
      }

      if (auto *FD = dyn_cast<FuncDecl>(D)) {
        // We can not call getters or setters.  We use VarDecls and
        // SubscriptDecls to produce completions that refer to getters and
        // setters.
        if (FD->isGetterOrSetter())
          return;

        // Don't complete class methods of instances; don't complete instance
        // methods of metatypes.
        if (FD->isStatic() != ExprType->is<MetaTypeType>())
          return;
        addSwiftMethodCall(FD);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addSwiftNominalTypeRef(NTD);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addSwiftTypeAliasRef(TAD);
        return;
      }

      if (HaveDot)
        return;

      // FIXME: super.constructor
      if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
        if (!ExprType->is<MetaTypeType>())
          return;
        if (IsSuperRefExpr && !isa<ConstructorDecl>(CurrDeclContext))
          return;
        addSwiftConstructorCall(CD);
        return;
      }

      if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
        if (ExprType->is<MetaTypeType>())
          return;
        addSwiftSubscriptCall(SD);
        return;
      }
      return;

    case LookupKind::DeclContext:
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        addSwiftVarDeclRef(VD);
        return;
      }

      if (auto *FD = dyn_cast<FuncDecl>(D)) {
        // We can not call operators with a postfix parenthesis syntax.
        if (FD->isBinaryOperator() || FD->isUnaryOperator())
          return;

        // We can not call getters or setters.  We use VarDecls and
        // SubscriptDecls to produce completions that refer to getters and
        // setters.
        if (FD->isGetterOrSetter())
          return;

        addSwiftMethodCall(FD);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addSwiftNominalTypeRef(NTD);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addSwiftTypeAliasRef(TAD);
        return;
      }

      return;
    }
  }

  // Implement clang::VisibleDeclConsumer
  void FoundDecl(clang::NamedDecl *ND, clang::NamedDecl *Hiding,
                 clang::DeclContext *Ctx,
                 bool InBaseClass) override {
    addClangDecl(ND);
  }

  void getValueExprCompletions(Type ExprType) {
    Kind = LookupKind::ValueExpr;
    this->ExprType = ExprType;
    bool Done = false;
    if (auto AFT = ExprType->getAs<AnyFunctionType>()) {
      addSwiftFunctionCall(AFT);
      Done = true;
    }
    if (auto LVT = ExprType->getAs<LValueType>()) {
      if (auto AFT = LVT->getObjectType()->getAs<AnyFunctionType>()) {
        addSwiftFunctionCall(AFT);
        Done = true;
      }
    }
    if (!Done) {
      lookupVisibleDecls(*this, ExprType, CurrDeclContext,
                         /*IsTypeLookup=*/ExprType->is<MetaTypeType>());
    }
    // Add the special qualified keyword 'metatype' so that, for example,
    // 'Int.metatype' can be completed.
    // Use the canonical type as a type annotation because looking at the
    // '.metatype' in the IDE is a way to understand what type the expression
    // has.
    addKeyword(
        "metatype",
        MetaTypeType::get(ExprType, SwiftContext)->getCanonicalType());
  }

  void getCompletionsInDeclContext(SourceLoc Loc) {
    Kind = LookupKind::DeclContext;
    lookupVisibleDecls(*this, CurrDeclContext, Loc);

    if (auto Importer = SwiftContext.getClangModuleLoader())
      static_cast<ClangImporter&>(*Importer).lookupVisibleDecls(*this);
  }
};

} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeDotExpr(Expr *E) {
  if (!typecheckExpr(E))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.setHaveDot();
  Lookup.getValueExprCompletions(E->getType());

  Consumer.handleResults(CompletionContext.takeResults());
}

void CodeCompletionCallbacksImpl::completePostfixExprBeginning() {
  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  assert(P.Tok.is(tok::code_complete));
  Lookup.getCompletionsInDeclContext(P.Tok.getLoc());

  Consumer.handleResults(CompletionContext.takeResults());
}

void CodeCompletionCallbacksImpl::completePostfixExpr(Expr *E) {
  if (!typecheckExpr(E))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.getValueExprCompletions(E->getType());

  Consumer.handleResults(CompletionContext.takeResults());
}

void CodeCompletionCallbacksImpl::completeExprSuper(SuperRefExpr *SRE) {
  if (!typecheckExpr(SRE))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.setIsSuperRefExpr();
  Lookup.getValueExprCompletions(SRE->getType());

  Consumer.handleResults(CompletionContext.takeResults());
}

void CodeCompletionCallbacksImpl::completeExprSuperDot(SuperRefExpr *SRE) {
  if (!typecheckExpr(SRE))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.setIsSuperRefExpr();
  Lookup.setHaveDot();
  Lookup.getValueExprCompletions(SRE->getType());

  Consumer.handleResults(CompletionContext.takeResults());
}

void PrintingCodeCompletionConsumer::handleResults(
    ArrayRef<CodeCompletionResult *> Results) {
  OS << "Begin completions\n";
  for (auto Result : Results) {
    Result->print(OS);
    OS << "\n";
  }
  OS << "End completions\n";
}

namespace {
class CodeCompletionCallbacksFactoryImpl
    : public CodeCompletionCallbacksFactory {
  CodeCompletionContext &CompletionContext;
  CodeCompletionConsumer &Consumer;

public:
  CodeCompletionCallbacksFactoryImpl(CodeCompletionContext &CompletionContext,
                                     CodeCompletionConsumer &Consumer)
      : CompletionContext(CompletionContext), Consumer(Consumer) {}

  CodeCompletionCallbacks *createCodeCompletionCallbacks(Parser &P) override {
    return new CodeCompletionCallbacksImpl(P, CompletionContext, Consumer);
  }
};
} // end unnamed namespace

CodeCompletionCallbacksFactory *
swift::code_completion::makeCodeCompletionCallbacksFactory(
    CodeCompletionContext &CompletionContext,
    CodeCompletionConsumer &Consumer) {
  return new CodeCompletionCallbacksFactoryImpl(CompletionContext, Consumer);
}

