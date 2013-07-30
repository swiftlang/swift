#include "swift/IDE/CodeCompletion.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ModuleLoadListener.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
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
      *CompletionOffset = CleanFile.size();
      CleanFile += '\0';
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
    case Chunk::ChunkKind::CallParameterNameAnnotation:
    case Chunk::ChunkKind::CallParameterColon:
    case Chunk::ChunkKind::CallParameterColonAnnotation:
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
  case ResultKind::Declaration:
    OS << "SwiftDecl: ";
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
  case CodeCompletionResult::ResultKind::Declaration:
    return new (Context.Allocator)
        CodeCompletionResult(new (Mem) CodeCompletionString(Chunks),
                             AssociatedDecl);
  case CodeCompletionResult::ResultKind::Keyword:
  case CodeCompletionResult::ResultKind::Pattern:
    return new (Context.Allocator)
        CodeCompletionResult(Kind, new (Mem) CodeCompletionString(Chunks));
  }
}

void CodeCompletionResultBuilder::finishResult() {
  auto *Result = takeResult();
  if (Kind == CodeCompletionResult::ResultKind::Declaration &&
      AssociatedDecl->hasClangNode())
    Context.ClangCompletionResults.push_back(Result);
  else
    Context.CurrentCompletionResults.push_back(Result);
}

StringRef CodeCompletionContext::copyString(StringRef String) {
  char *Mem = Allocator.Allocate<char>(String.size());
  std::copy(String.begin(), String.end(), Mem);
  return StringRef(Mem, String.size());
}

void CodeCompletionContext::clearClangCache() {
  ClangCompletionResults.clear();
}

ArrayRef<CodeCompletionResult *> CodeCompletionContext::takeResults() {
  if (IncludeClangResults) {
    CurrentCompletionResults.reserve(CurrentCompletionResults.size() +
                                     ClangCompletionResults.size());
    // Add Clang results from the cache.
    for (auto R : ClangCompletionResults)
      CurrentCompletionResults.push_back(R);
  }

  // Copy pointers to the results.
  const size_t Count = CurrentCompletionResults.size();
  CodeCompletionResult **Results =
      Allocator.Allocate<CodeCompletionResult *>(Count);
  std::copy(CurrentCompletionResults.begin(), CurrentCompletionResults.end(),
            Results);
  CurrentCompletionResults.clear();
  IncludeClangResults = false;
  return llvm::makeArrayRef(Results, Count);
}

namespace {
class CodeCompletionCallbacksImpl : public CodeCompletionCallbacks,
                                    public ModuleLoadListener {
  CodeCompletionContext &CompletionContext;
  CodeCompletionConsumer &Consumer;
  TranslationUnit *const TU;

  /// \brief Set to true when we have delivered code completion results
  /// to the \c Consumer.
  bool DeliveredResults = false;

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
    P.Context.addModuleLoadListener(*this);
  }

  ~CodeCompletionCallbacksImpl() {
    P.Context.removeModuleLoadListener(*this);
  }

  void completeExpr() override;
  void completeDotExpr(Expr *E) override;
  void completePostfixExprBeginning() override;
  void completePostfixExpr(Expr *E) override;
  void completeExprSuper(SuperRefExpr *SRE) override;
  void completeExprSuperDot(SuperRefExpr *SRE) override;

  void deliverCompletionResults();

  // Implement swift::ModuleLoadListener.
  void loadedModule(ModuleLoader *Loader, Module *M) override;
};
} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeExpr() {
  if (DeliveredResults)
    return;

  Parser::ParserPositionRAII RestorePosition(P);
  P.restoreParserPosition(ExprBeginPosition);

  // FIXME: implement fallback code completion.

  deliverCompletionResults();
}

namespace {
/// Build completions by doing visible decl lookup from a context.
class CompletionLookup : swift::VisibleDeclConsumer
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

    assert(!(InsideStaticMethod &&
            VD->getDeclContext() == CurrMethodDC->getParent()) &&
           "name lookup bug -- can not see an instance variable "
           "in a static function");
    // FIXME: static variables.

    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Declaration);
    Builder.setAssociatedDecl(VD);
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
        Builder.addCallParameter(TupleElt.getPattern()->getBoundName(),
                                 TupleElt.getPattern()->getType().getString());
        NeedComma = true;
      }
      return;
    }
    if (auto *PP = dyn_cast<ParenPattern>(P)) {
      Builder.addCallParameter(PP->getBoundName(), PP->getType().getString());
      return;
    }
    auto *TP = cast<TypedPattern>(P);
    Builder.addCallParameter(TP->getBoundName(), TP->getType().getString());
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
      Builder.addCallParameter(TupleElt.getName(),
                               TupleElt.getType().getString());
      NeedComma = true;
    }
    Builder.addRightParen();
    addTypeAnnotation(Builder, AFT->getResult());
  }

  void addSwiftMethodCall(const FuncDecl *FD) {
    bool IsImlicitlyCurriedInstanceMethod;
    switch (Kind) {
    case LookupKind::ValueExpr:
      IsImlicitlyCurriedInstanceMethod = ExprType->is<MetaTypeType>() &&
                                         !FD->isStatic();
      break;
    case LookupKind::DeclContext:
      IsImlicitlyCurriedInstanceMethod =
          CurrMethodDC &&
          FD->getDeclContext() == CurrMethodDC->getParent() &&
          InsideStaticMethod && !FD->isStatic();
      break;
    }

    StringRef Name = FD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Declaration);
    Builder.setAssociatedDecl(FD);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(Name);
    Builder.addLeftParen();
    auto *FE = FD->getBody();
    auto Patterns = FE->getArgParamPatterns();
    unsigned FirstIndex = 0;
    if (!IsImlicitlyCurriedInstanceMethod && FE->getImplicitThisDecl())
      FirstIndex = 1;
    addPatternParameters(Builder, Patterns[FirstIndex]);
    Builder.addRightParen();
    // FIXME: Pattern should pretty-print itself.
    llvm::SmallString<32> TypeStr;
    for (unsigned i = FirstIndex + 1, e = Patterns.size(); i != e; ++i) {
      TypeStr += "(";
      bool NeedComma = false;
      if (auto *PP = dyn_cast<ParenPattern>(Patterns[i])) {
        TypeStr += PP->getType().getString();
      } else {
        auto *TP = cast<TuplePattern>(Patterns[i]);
        for (auto TupleElt : TP->getFields()) {
          if (NeedComma)
            TypeStr += ", ";
          Identifier BoundName = TupleElt.getPattern()->getBoundName();
          if (!BoundName.empty()) {
            TypeStr += BoundName.str();
            TypeStr += ": ";
          }
          TypeStr += TupleElt.getPattern()->getType().getString();
          NeedComma = true;
        }
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
        CodeCompletionResult::ResultKind::Declaration);
    Builder.setAssociatedDecl(CD);
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
        CodeCompletionResult::ResultKind::Declaration);
    Builder.setAssociatedDecl(SD);
    Builder.addLeftBracket();
    addPatternParameters(Builder, SD->getIndices());
    Builder.addRightBracket();
    addTypeAnnotation(Builder, SD->getElementType());
  }

  void addSwiftNominalTypeRef(const NominalTypeDecl *NTD) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Declaration);
    Builder.setAssociatedDecl(NTD);
    if (needDot())
      Builder.addDot();
    Builder.addTextChunk(NTD->getName().str());
    addTypeAnnotation(Builder,
                      MetaTypeType::get(NTD->getDeclaredType(), SwiftContext));
  }

  void addSwiftTypeAliasRef(const TypeAliasDecl *TAD) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Declaration);
    Builder.setAssociatedDecl(TAD);
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
        assert(!ExprType->is<MetaTypeType>() && "name lookup bug");
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
      lookupVisibleDecls(*this, ExprType, CurrDeclContext);
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

    if (!CompletionContext.haveClangResults())
      if (auto Importer = SwiftContext.getClangModuleLoader())
        static_cast<ClangImporter&>(*Importer).lookupVisibleDecls(*this);

    CompletionContext.includeUnqualifiedClangResults();
  }
};

} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeDotExpr(Expr *E) {
  if (!typecheckExpr(E))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.setHaveDot();
  Lookup.getValueExprCompletions(E->getType());

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::completePostfixExprBeginning() {
  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  assert(P.Tok.is(tok::code_complete));
  Lookup.getCompletionsInDeclContext(P.Tok.getLoc());

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::completePostfixExpr(Expr *E) {
  if (!typecheckExpr(E))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.getValueExprCompletions(E->getType());

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::completeExprSuper(SuperRefExpr *SRE) {
  if (!typecheckExpr(SRE))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.setIsSuperRefExpr();
  Lookup.getValueExprCompletions(SRE->getType());

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::completeExprSuperDot(SuperRefExpr *SRE) {
  if (!typecheckExpr(SRE))
    return;

  CompletionLookup Lookup(CompletionContext, TU->Ctx, P.CurDeclContext);
  Lookup.setIsSuperRefExpr();
  Lookup.setHaveDot();
  Lookup.getValueExprCompletions(SRE->getType());

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::deliverCompletionResults() {
  ArrayRef<CodeCompletionResult *> Results = CompletionContext.takeResults();
  if (!Results.empty()) {
    Consumer.handleResults(Results);
    DeliveredResults = true;
  }
}

void CodeCompletionCallbacksImpl::loadedModule(ModuleLoader *Loader,
                                               Module *M) {
  CompletionContext.clearClangCache();
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

