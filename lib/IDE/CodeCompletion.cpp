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

std::string CodeCompletionString::getAsString() const {
  std::string Result;
  unsigned PrevNestingLevel = 0;
  for (auto C : getChunks()) {
    if (C.getNestingLevel() < PrevNestingLevel) {
      Result += "#}";
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
      Result += C.getText();
      break;
    case Chunk::ChunkKind::OptionalBegin:
    case Chunk::ChunkKind::CallParameterBegin:
      Result += "{#";
      break;
    case Chunk::ChunkKind::TypeAnnotation:
      Result += "[#";
      Result += C.getText();
      Result += "#]";
    }
    PrevNestingLevel = C.getNestingLevel();
  }
  while (PrevNestingLevel > 0) {
    Result += "#}";
    PrevNestingLevel--;
  }
  return Result;
}

std::string CodeCompletionResult::getAsString() const {
  std::string Result;
  switch (Kind) {
  case ResultKind::SwiftDeclaration:
    Result += "SwiftDecl: ";
    break;
  case ResultKind::ClangDeclaration:
    Result += "ClangDecl: ";
    break;
  case ResultKind::Keyword:
    Result += "Keyword: ";
    break;
  case ResultKind::Pattern:
    Result += "Pattern: ";
    break;
  }
  Result += CompletionString->getAsString();
  return Result;
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

  void typecheckExpr(Expr *&E) {
    assert(E && "should have an expression");

    DEBUG(llvm::dbgs() << "\nparsed:\n";
          E->print(llvm::dbgs());
          llvm::dbgs() << "\n");

    if (!typeCheckCompletionContextExpr(TU, E))
      return;

    DEBUG(llvm::dbgs() << "\type checked:\n";
          E->print(llvm::dbgs());
          llvm::dbgs() << "\n");
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
  void completePostfixExpr(Expr *E) override;
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
  StringRef Prefix;

  enum class LookupKind {
    ValueExpr,
    DeclContext
  };

  LookupKind Kind;
  Type ExprType;
  bool HaveDot = false;

public:
  CompletionLookup(CodeCompletionContext &CompletionContext,
                   ASTContext &SwiftContext, StringRef Prefix)
      : CompletionContext(CompletionContext), SwiftContext(SwiftContext),
        Prefix(Prefix) {
  }

  void setHaveDot() {
    HaveDot = true;
  }

  void addSwiftVarDeclRef(const VarDecl *VD) {
    StringRef Name = VD->getName().get();
    assert(!Name.empty() && "name should not be empty");
    if (!Prefix.empty() && !Name.startswith(Prefix))
      return;

    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(VD);
    if (!HaveDot)
      Builder.addDot();
    Builder.addTextChunk(Name);
    Builder.addTypeAnnotation(VD->getType().getString());
  }

  void addTuplePatternParameters(CodeCompletionResultBuilder &Builder,
                                 const TuplePattern *TP) {
    bool NeedComma = false;
    for (auto TupleElt : TP->getFields()) {
      if (NeedComma)
        Builder.addComma(", ");
      Builder.addCallParameter(TupleElt.getPattern()->getBoundName().str(),
                               TupleElt.getPattern()->getType().getString());
      NeedComma = true;
    }
  }

  void addSwiftMethodCall(const FuncDecl *FD) {
    StringRef Name = FD->getName().get();
    assert(!Name.empty() && "name should not be empty");
    if (!Prefix.empty() && !Name.startswith(Prefix))
      return;

    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(FD);
    if (!HaveDot)
      Builder.addDot();
    Builder.addTextChunk(Name);
    Builder.addLeftParen();
    auto *FE = FD->getBody();
    auto Patterns = FE->getArgParamPatterns();
    unsigned FirstIndex = 0;
    if (Patterns[0]->isImplicit())
      FirstIndex = 1;
    addTuplePatternParameters(Builder, cast<TuplePattern>(Patterns[FirstIndex]));
    Builder.addRightParen();
    // FIXME: Pattern should pretty-print itself.
    llvm::SmallString<32> Type;
    for (unsigned i = FirstIndex + 1, e = Patterns.size(); i != e; ++i) {
      Type += "(";
      for (auto TupleElt : cast<TuplePattern>(Patterns[i])->getFields()) {
        Type += TupleElt.getPattern()->getBoundName().str();
        Type += ": ";
        Type += TupleElt.getPattern()->getType().getString();
      }
      Type += ") -> ";
    }
    Type += FE->getResultType(SwiftContext).getString();
    Builder.addTypeAnnotation(Type);

    // TODO: skip arguments with default parameters?
  }

  void addSwiftSubscriptCall(const SubscriptDecl *SD) {
    assert(!HaveDot && "can not add a subscript after a dot");
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::SwiftDeclaration);
    Builder.setAssociatedSwiftDecl(SD);
    Builder.addLeftBracket();
    addTuplePatternParameters(Builder, cast<TuplePattern>(SD->getIndices()));
    Builder.addRightBracket();
    Builder.addTypeAnnotation(SD->getElementType().getString());
  }

  void addClangDecl(const clang::NamedDecl *ND) {
    StringRef Name = ND->getName();
    if (Name.empty())
      return;
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::ClangDeclaration);
    Builder.setAssociatedClangDecl(ND);
    Builder.addTextChunk(Name);
  }

  void addKeyword(StringRef Name) {
    CodeCompletionResultBuilder Builder(
        CompletionContext,
        CodeCompletionResult::ResultKind::Keyword);
    if (!HaveDot)
      Builder.addDot();
    Builder.addTextChunk(Name);
  }

  // Implement swift::VisibleDeclConsumer
  void foundDecl(ValueDecl *D) override {
    if (Kind == LookupKind::ValueExpr) {
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        // Swift does not have class variables.
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

      if (HaveDot)
        return;

      // FIXME: super.constructor
      if (isa<ConstructorDecl>(D)) {
        if (!ExprType->is<MetaTypeType>())
          return;
        // FIXME: produce a constructor call.
        return;
      }

      if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
        addSwiftSubscriptCall(SD);
        return;
      }
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
    if (ExprType->is<AnyFunctionType>()) {
      // FIXME: produce a call.
    } else {
      lookupVisibleDecls(*this, ExprType);
    }
    // Add the special qualified keyword 'metatype' so that, for example,
    // 'Int.metatype' can be completed.
    addKeyword("metatype");
  }

  void getCompletionsInDeclContext(DeclContext *DC, SourceLoc Loc) {
    Kind = LookupKind::DeclContext;
    lookupVisibleDecls(*this, DC, Loc);

    if (auto Importer = SwiftContext.getClangModuleLoader())
      static_cast<ClangImporter&>(*Importer).lookupVisibleDecls(*this);
  }
};

} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeDotExpr(Expr *E) {
  typecheckExpr(E);

  CompletionLookup Lookup(CompletionContext, TU->Ctx, "");
  Lookup.setHaveDot();
  Lookup.getValueExprCompletions(E->getType());

  Consumer.handleResults(CompletionContext.takeResults());
}

void CodeCompletionCallbacksImpl::completePostfixExpr(Expr *E) {
  typecheckExpr(E);

  CompletionLookup Lookup(CompletionContext, TU->Ctx, "");
  Lookup.getValueExprCompletions(E->getType());

  Consumer.handleResults(CompletionContext.takeResults());
}

void PrintingCodeCompletionConsumer::handleResults(
    ArrayRef<CodeCompletionResult *> Results) {
  OS << "Begin completions\n";
  for (auto Result : Results) {
    OS << Result->getAsString() << "\n";
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

