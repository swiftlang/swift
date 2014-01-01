//===- CodeCompletion.cpp - Code completion implementation ----------------===//
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

#include "swift/IDE/CodeCompletion.h"
#include "swift/Basic/Cache.h"
#include "swift/Basic/ThreadSafeRefCounted.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Optional.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include "CodeCompletionResultBuilder.h"
#include <algorithm>
#include <functional>
#include <string>

using namespace swift;
using namespace ide;

std::string swift::ide::removeCodeCompletionTokens(
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

namespace {
class StmtFinder : public ASTWalker {
  SourceManager &SM;
  SourceLoc Loc;
  StmtKind Kind;
  Stmt *Found = nullptr;

public:
  StmtFinder(SourceManager &SM, SourceLoc Loc, StmtKind Kind)
      : SM(SM), Loc(Loc), Kind(Kind) {}

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (SM.rangeContainsTokenLoc(S->getSourceRange(), Loc))
      return { true, S };
    else
      return { false, S };
  }

  Stmt *walkToStmtPost(Stmt *S) override {
    if (S->getKind() == Kind) {
      Found = S;
      return nullptr;
    }
    return S;
  }

  Stmt *getFoundStmt() const {
    return Found;
  }
};

Stmt *findNearestStmt(const AbstractFunctionDecl *AFD, SourceLoc Loc,
                      StmtKind Kind) {
  auto &SM = AFD->getASTContext().SourceMgr;
  assert(SM.rangeContainsTokenLoc(AFD->getSourceRange(), Loc));
  StmtFinder Finder(SM, Loc, Kind);
  // FIXME(thread-safety): the walker is is mutating the AST.
  const_cast<AbstractFunctionDecl *>(AFD)->walk(Finder);
  return Finder.getFoundStmt();
}
} // unnamed namespace

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
    case Chunk::ChunkKind::LeftAngle:
    case Chunk::ChunkKind::RightAngle:
    case Chunk::ChunkKind::Dot:
    case Chunk::ChunkKind::Comma:
    case Chunk::ChunkKind::ExclamationMark:
    case Chunk::ChunkKind::QuestionMark:
    case Chunk::ChunkKind::CallParameterName:
    case Chunk::ChunkKind::CallParameterColon:
    case Chunk::ChunkKind::CallParameterType:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
      OS << C.getText();
      break;
    case Chunk::ChunkKind::OptionalBegin:
    case Chunk::ChunkKind::CallParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
      OS << "{#";
      break;
    case Chunk::ChunkKind::DynamicLookupMethodCallTail:
      OS << "{#" << C.getText() << "#}";
      break;
    case Chunk::ChunkKind::TypeAnnotation:
      OS << "[#";
      OS << C.getText();
      OS << "#]";
      break;
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

CodeCompletionDeclKind
CodeCompletionResult::getCodeCompletionDeclKind(const Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("not expecting such a declaration result");

  case DeclKind::TypeAlias:
  case DeclKind::AssociatedType:
    return CodeCompletionDeclKind::TypeAlias;
  case DeclKind::GenericTypeParam:
    return CodeCompletionDeclKind::GenericTypeParam;
  case DeclKind::Enum:
    return CodeCompletionDeclKind::Enum;
  case DeclKind::Struct:
    return CodeCompletionDeclKind::Struct;
  case DeclKind::Class:
    return CodeCompletionDeclKind::Class;
  case DeclKind::Protocol:
    return CodeCompletionDeclKind::Protocol;
  case DeclKind::Var: {
    auto DC = D->getDeclContext();
    if (DC->isTypeContext()) {
      if (cast<VarDecl>(D)->isStatic())
        return CodeCompletionDeclKind::StaticVar;
      else
        return CodeCompletionDeclKind::InstanceVar;
    }
    if (DC->isLocalContext())
      return CodeCompletionDeclKind::LocalVar;
    return CodeCompletionDeclKind::GlobalVar;
  }
  case DeclKind::Constructor:
    return CodeCompletionDeclKind::Constructor;
  case DeclKind::Destructor:
    return CodeCompletionDeclKind::Destructor;
  case DeclKind::Func: {
    auto DC = D->getDeclContext();
    auto FD = cast<FuncDecl>(D);
    if (DC->isTypeContext()) {
      if (FD->isStatic())
        return CodeCompletionDeclKind::StaticMethod;
      return CodeCompletionDeclKind::InstanceMethod;
    }
    if (FD->isOperator())
      return CodeCompletionDeclKind::OperatorFunction;
    return CodeCompletionDeclKind::FreeFunction;
  }
  case DeclKind::EnumElement:
    return CodeCompletionDeclKind::EnumElement;
  case DeclKind::Subscript:
    return CodeCompletionDeclKind::Subscript;
  }
  llvm_unreachable("invalid DeclKind");
}

void CodeCompletionResult::print(raw_ostream &OS) const {
  llvm::SmallString<64> Prefix;
  switch (getKind()) {
  case ResultKind::Declaration:
    Prefix.append("Decl");
    switch (getAssociatedDeclKind()) {
    case CodeCompletionDeclKind::Class:
      Prefix.append("[Class]");
      break;
    case CodeCompletionDeclKind::Struct:
      Prefix.append("[Struct]");
      break;
    case CodeCompletionDeclKind::Enum:
      Prefix.append("[Enum]");
      break;
    case CodeCompletionDeclKind::EnumElement:
      Prefix.append("[EnumElement]");
      break;
    case CodeCompletionDeclKind::Protocol:
      Prefix.append("[Protocol]");
      break;
    case CodeCompletionDeclKind::TypeAlias:
      Prefix.append("[TypeAlias]");
      break;
    case CodeCompletionDeclKind::GenericTypeParam:
      Prefix.append("[GenericTypeParam]");
      break;
    case CodeCompletionDeclKind::Constructor:
      Prefix.append("[Constructor]");
      break;
    case CodeCompletionDeclKind::Destructor:
      Prefix.append("[Destructor]");
      break;
    case CodeCompletionDeclKind::Subscript:
      Prefix.append("[Subscript]");
      break;
    case CodeCompletionDeclKind::StaticMethod:
      Prefix.append("[StaticMethod]");
      break;
    case CodeCompletionDeclKind::InstanceMethod:
      Prefix.append("[InstanceMethod]");
      break;
    case CodeCompletionDeclKind::OperatorFunction:
      Prefix.append("[OperatorFunction]");
      break;
    case CodeCompletionDeclKind::FreeFunction:
      Prefix.append("[FreeFunction]");
      break;
    case CodeCompletionDeclKind::StaticVar:
      Prefix.append("[StaticVar]");
      break;
    case CodeCompletionDeclKind::InstanceVar:
      Prefix.append("[InstanceVar]");
      break;
    case CodeCompletionDeclKind::LocalVar:
      Prefix.append("[LocalVar]");
      break;
    case CodeCompletionDeclKind::GlobalVar:
      Prefix.append("[GlobalVar]");
      break;
    }
    break;
  case ResultKind::Keyword:
    Prefix.append("Keyword");
    break;
  case ResultKind::Pattern:
    Prefix.append("Pattern");
    break;
  }
  Prefix.append("/");
  switch (getSemanticContext()) {
  case SemanticContextKind::None:
    Prefix.append("None");
    break;
  case SemanticContextKind::ExpressionSpecific:
    Prefix.append("ExprSpecific");
    break;
  case SemanticContextKind::Local:
    Prefix.append("Local");
    break;
  case SemanticContextKind::CurrentNominal:
    Prefix.append("CurrNominal");
    break;
  case SemanticContextKind::Super:
    Prefix.append("Super");
    break;
  case SemanticContextKind::OutsideNominal:
    Prefix.append("OutNominal");
    break;
  case SemanticContextKind::CurrentModule:
    Prefix.append("CurrModule");
    break;
  case SemanticContextKind::OtherModule:
    Prefix.append("OtherModule");
    break;
  }
  Prefix.append(": ");
  while (Prefix.size() < 36) {
    Prefix.append(" ");
  }
  OS << Prefix;
  CompletionString->print(OS);
}

void CodeCompletionResult::dump() const {
  print(llvm::errs());
}

static StringRef copyString(llvm::BumpPtrAllocator &Allocator,
                            StringRef Str) {
  char *Mem = Allocator.Allocate<char>(Str.size());
  std::copy(Str.begin(), Str.end(), Mem);
  return StringRef(Mem, Str.size());
}

void CodeCompletionResultBuilder::addChunkWithText(
    CodeCompletionString::Chunk::ChunkKind Kind, StringRef Text) {
  addChunkWithTextNoCopy(Kind, copyString(Sink.Allocator, Text));
}

StringRef CodeCompletionContext::copyString(StringRef Str) {
  return ::copyString(CurrentResults.Allocator, Str);
}

CodeCompletionResult *CodeCompletionResultBuilder::takeResult() {
  void *CCSMem = Sink.Allocator
      .Allocate(sizeof(CodeCompletionString) +
                    Chunks.size() * sizeof(CodeCompletionString::Chunk),
                llvm::alignOf<CodeCompletionString>());
  auto *CCS = new (CCSMem) CodeCompletionString(Chunks);

  switch (Kind) {
  case CodeCompletionResult::ResultKind::Declaration:
    return new (Sink.Allocator) CodeCompletionResult(SemanticContext, CCS,
                                                     AssociatedDecl);

  case CodeCompletionResult::ResultKind::Keyword:
  case CodeCompletionResult::ResultKind::Pattern:
    return new (Sink.Allocator) CodeCompletionResult(Kind, SemanticContext,
                                                     CCS);
  }
}

void CodeCompletionResultBuilder::finishResult() {
  Sink.Results.push_back(takeResult());
}

namespace swift {
namespace ide {

struct CodeCompletionCacheImpl {
  /// \brief Cache key.
  struct Key {
    std::string ModuleFilename;
    std::string ModuleName;
    std::vector<std::string> AccessPath;
    bool ResultsHaveLeadingDot;

    friend bool operator==(const Key &LHS, const Key &RHS) {
      return LHS.ModuleFilename == RHS.ModuleFilename &&
             LHS.ModuleName == RHS.ModuleName &&
             LHS.AccessPath == RHS.AccessPath &&
             LHS.ResultsHaveLeadingDot == RHS.ResultsHaveLeadingDot;
    }
  };

  struct Value : public ThreadSafeRefCountedBase<Value> {
    llvm::sys::TimeValue ModuleModificationTime;
    CodeCompletionResultSink Sink;
  };
  using ValueRefCntPtr = llvm::IntrusiveRefCntPtr<Value>;

  sys::Cache<Key, ValueRefCntPtr> TheCache{"swift.libIDE.CodeCompletionCache"};

  void getResults(
      const Key &K, CodeCompletionResultSink &TargetSink, bool OnlyTypes,
      std::function<void(CodeCompletionCacheImpl &, Key)> FillCacheCallback);

  ValueRefCntPtr getResultSinkFor(const Key &K);

  void storeResults(const Key &K, ValueRefCntPtr V);
};

} // namespace ide
} // namespace swift

namespace llvm {
template<>
struct DenseMapInfo<swift::ide::CodeCompletionCacheImpl::Key> {
  using KeyTy = swift::ide::CodeCompletionCacheImpl::Key;
  static inline KeyTy getEmptyKey() {
    return KeyTy{"", "", {}, false};
  }
  static inline KeyTy getTombstoneKey() {
    return KeyTy{"x", "", {}, false};
  }
  static unsigned getHashValue(const KeyTy &Val) {
    size_t H = 0;
    H ^= std::hash<std::string>()(Val.ModuleFilename);
    H ^= std::hash<std::string>()(Val.ModuleName);
    for (auto Piece : Val.AccessPath)
      H ^= std::hash<std::string>()(Piece);
    H ^= std::hash<bool>()(Val.ResultsHaveLeadingDot);
    return static_cast<unsigned>(H);
  }
  static bool isEqual(const KeyTy &LHS, const KeyTy &RHS) {
    return LHS == RHS;
  }
};
} // namespace llvm

namespace swift {
namespace sys {
template<>
struct CacheValueCostInfo<swift::ide::CodeCompletionCacheImpl::Value> {
  static size_t
  getCost(const swift::ide::CodeCompletionCacheImpl::Value &V) {
    return V.Sink.Allocator.getTotalMemory();
  };
};
} // namespace sys
} // namespace swift

void CodeCompletionCacheImpl::getResults(
    const Key &K, CodeCompletionResultSink &TargetSink, bool OnlyTypes,
    std::function<void(CodeCompletionCacheImpl &, Key)> FillCacheCallback) {
  // FIXME(thread-safety): lock the whole AST context.  We might load a module.
  llvm::Optional<ValueRefCntPtr> V = TheCache.get(K);
  if (!V.hasValue()) {
    // No cached results found.  Fill the cache.
    FillCacheCallback(*this, K);
    V = TheCache.get(K);
  } else {
    llvm::sys::fs::file_status ModuleStatus;
    bool IsError = llvm::sys::fs::status(K.ModuleFilename, ModuleStatus);
    if (IsError ||
        V.getValue()->ModuleModificationTime !=
            ModuleStatus.getLastModificationTime()) {
      // Cache is stale.  Update the cache.
      TheCache.remove(K);
      FillCacheCallback(*this, K);
      V = TheCache.get(K);
    }
  }
  assert(V.hasValue());
  auto &SourceSink = V.getValue()->Sink;
  if (OnlyTypes) {
    std::copy_if(SourceSink.Results.begin(), SourceSink.Results.end(),
                 std::back_inserter(TargetSink.Results),
                 [](CodeCompletionResult *R) -> bool {
      if (R->getKind() != CodeCompletionResult::Declaration)
        return false;
      switch(R->getAssociatedDeclKind()) {
      case CodeCompletionDeclKind::Class:
      case CodeCompletionDeclKind::Struct:
      case CodeCompletionDeclKind::Enum:
      case CodeCompletionDeclKind::Protocol:
      case CodeCompletionDeclKind::TypeAlias:
      case CodeCompletionDeclKind::GenericTypeParam:
        return true;
      case CodeCompletionDeclKind::EnumElement:
      case CodeCompletionDeclKind::Constructor:
      case CodeCompletionDeclKind::Destructor:
      case CodeCompletionDeclKind::Subscript:
      case CodeCompletionDeclKind::StaticMethod:
      case CodeCompletionDeclKind::InstanceMethod:
      case CodeCompletionDeclKind::OperatorFunction:
      case CodeCompletionDeclKind::FreeFunction:
      case CodeCompletionDeclKind::StaticVar:
      case CodeCompletionDeclKind::InstanceVar:
      case CodeCompletionDeclKind::LocalVar:
      case CodeCompletionDeclKind::GlobalVar:
        return false;
      }
    });
  } else {
    TargetSink.Results.insert(TargetSink.Results.end(),
                              SourceSink.Results.begin(),
                              SourceSink.Results.end());
  }
}

CodeCompletionCacheImpl::ValueRefCntPtr
CodeCompletionCacheImpl::getResultSinkFor(const Key &K) {
  TheCache.remove(K);
  auto V = ValueRefCntPtr(new Value);
  TheCache.set(K, V);
  return V;
}

void CodeCompletionCacheImpl::storeResults(const Key &K, ValueRefCntPtr V) {
  {
    assert(!K.ModuleFilename.empty());

    llvm::sys::fs::file_status ModuleStatus;
    if (llvm::sys::fs::status(K.ModuleFilename, ModuleStatus)) {
      V->ModuleModificationTime = llvm::sys::TimeValue::now();
    } else {
      V->ModuleModificationTime = ModuleStatus.getLastModificationTime();
    }
  }

  // Remove the cache entry and add it back to refresh the cost value.
  TheCache.remove(K);
  TheCache.set(K, V);
}

CodeCompletionCache::CodeCompletionCache()
    : Impl(new CodeCompletionCacheImpl()) {}

CodeCompletionCache::~CodeCompletionCache() {}

MutableArrayRef<CodeCompletionResult *> CodeCompletionContext::takeResults() {
  // Copy pointers to the results.
  const size_t Count = CurrentResults.Results.size();
  CodeCompletionResult **Results =
      CurrentResults.Allocator.Allocate<CodeCompletionResult *>(Count);
  std::copy(CurrentResults.Results.begin(), CurrentResults.Results.end(),
            Results);
  CurrentResults.Results.clear();
  return MutableArrayRef<CodeCompletionResult *>(Results, Count);
}

static StringRef getFirstTextChunk(CodeCompletionResult *R) {
  for (auto C : R->getCompletionString()->getChunks()) {
    switch (C.getKind()) {
    case CodeCompletionString::Chunk::ChunkKind::Text:
    case CodeCompletionString::Chunk::ChunkKind::LeftParen:
    case CodeCompletionString::Chunk::ChunkKind::RightParen:
    case CodeCompletionString::Chunk::ChunkKind::LeftBracket:
    case CodeCompletionString::Chunk::ChunkKind::RightBracket:
    case CodeCompletionString::Chunk::ChunkKind::LeftAngle:
    case CodeCompletionString::Chunk::ChunkKind::RightAngle:
    case CodeCompletionString::Chunk::ChunkKind::Dot:
    case CodeCompletionString::Chunk::ChunkKind::Comma:
    case CodeCompletionString::Chunk::ChunkKind::ExclamationMark:
    case CodeCompletionString::Chunk::ChunkKind::QuestionMark:
      return C.getText();

    case CodeCompletionString::Chunk::ChunkKind::CallParameterName:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterColon:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterType:
    case CodeCompletionString::Chunk::ChunkKind::OptionalBegin:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
    case CodeCompletionString::Chunk::ChunkKind::DynamicLookupMethodCallTail:
    case CodeCompletionString::Chunk::ChunkKind::TypeAnnotation:
      continue;
    }
  }
  return StringRef();
}

void CodeCompletionContext::sortCompletionResults(
    MutableArrayRef<CodeCompletionResult *> Results) {
  std::sort(Results.begin(), Results.end(),
            [](CodeCompletionResult *LHS, CodeCompletionResult *RHS) {
    StringRef LHSChunk = getFirstTextChunk(LHS);
    StringRef RHSChunk = getFirstTextChunk(RHS);
    int Result = LHSChunk.compare_lower(RHSChunk);
    // If the case insensitive comparison is equal, then secondary sort order
    // should be case sensitive.
    if (Result == 0)
      Result = LHSChunk.compare(RHSChunk);
    return Result < 0;
  });
}

namespace {
class CodeCompletionCallbacksImpl : public CodeCompletionCallbacks {
  CodeCompletionContext &CompletionContext;
  CodeCompletionConsumer &Consumer;

  enum class CompletionKind {
    None,
    DotExpr,
    PostfixExprBeginning,
    PostfixExpr,
    SuperExpr,
    SuperExprDot,
    TypeSimpleBeginning,
    TypeIdentifierWithDot,
    TypeIdentifierWithoutDot,
    CaseStmtBeginning,
    CaseStmtDotPrefix,
  };

  CompletionKind Kind = CompletionKind::None;
  Expr *ParsedExpr = nullptr;
  TypeLoc ParsedTypeLoc;
  DeclContext *CurDeclContext = nullptr;
  Decl *CStyleForLoopIterationVariable = nullptr;

  /// \brief Set to true when we have delivered code completion results
  /// to the \c Consumer.
  bool DeliveredResults = false;

  bool typecheckContextImpl(DeclContext *DC) {
    // Type check the function that contains the expression.
    if (DC->getContextKind() == DeclContextKind::AbstractClosureExpr ||
        DC->getContextKind() == DeclContextKind::AbstractFunctionDecl) {
      SourceLoc EndTypeCheckLoc =
          ParsedExpr ? ParsedExpr->getStartLoc()
                     : P.Context.SourceMgr.getCodeCompletionLoc();
      // Find the nearest outer function.
      DeclContext *DCToTypeCheck = DC;
      while (!DCToTypeCheck->isModuleContext() &&
             !isa<AbstractFunctionDecl>(DCToTypeCheck))
        DCToTypeCheck = DCToTypeCheck->getParent();
      // First, type check the nominal decl that contains the function.
      typecheckContextImpl(DCToTypeCheck->getParent());
      // Then type check the function itself.
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DCToTypeCheck))
        return typeCheckAbstractFunctionBodyUntil(AFD, EndTypeCheckLoc);
      return false;
    }
    if (DC->getContextKind() == DeclContextKind::NominalTypeDecl) {
      auto *NTD = cast<NominalTypeDecl>(DC);
      // First, type check the parent DeclContext.
      typecheckContextImpl(DC->getParent());
      if (NTD->hasType())
        return true;
      return typeCheckCompletionDecl(cast<NominalTypeDecl>(DC));
    }
    if (DC->getContextKind() == DeclContextKind::TopLevelCodeDecl) {
      return typeCheckTopLevelCodeDecl(cast<TopLevelCodeDecl>(DC));
    }
    return true;
  }

  /// \returns true on success, false on failure.
  bool typecheckContext() {
    return typecheckContextImpl(CurDeclContext);
  }

  /// \returns true on success, false on failure.
  bool typecheckDelayedParsedDecl() {
    assert(DelayedParsedDecl && "should have a delayed parsed decl");
    return typeCheckCompletionDecl(DelayedParsedDecl);
  }

  /// \returns true on success, false on failure.
  bool typecheckParsedExpr() {
    assert(ParsedExpr && "should have an expression");

    Expr *TypecheckedExpr = ParsedExpr;
    if (!typeCheckCompletionContextExpr(P.Context, CurDeclContext,
                                        TypecheckedExpr))
      return false;

    if (TypecheckedExpr->getType()->is<ErrorType>())
      return false;

    ParsedExpr = TypecheckedExpr;
    return true;
  }

  /// \returns true on success, false on failure.
  bool typecheckParsedType() {
    assert(ParsedTypeLoc.getTypeRepr() && "should have a TypeRepr");
    return !performTypeLocChecking(P.Context, ParsedTypeLoc, /*SIL*/ false,
                                   CurDeclContext, false);
  }

public:
  CodeCompletionCallbacksImpl(Parser &P,
                              CodeCompletionContext &CompletionContext,
                              CodeCompletionConsumer &Consumer)
      : CodeCompletionCallbacks(P), CompletionContext(CompletionContext),
        Consumer(Consumer) {
  }

  void completeExpr() override;
  void completeDotExpr(Expr *E) override;
  void completePostfixExprBeginning() override;
  void completePostfixExpr(Expr *E) override;
  void completeExprSuper(SuperRefExpr *SRE) override;
  void completeExprSuperDot(SuperRefExpr *SRE) override;

  void completeTypeSimpleBeginning() override;
  void completeTypeIdentifierWithDot(IdentTypeRepr *ITR) override;
  void completeTypeIdentifierWithoutDot(IdentTypeRepr *ITR) override;

  void completeCaseStmtBeginning() override;
  void completeCaseStmtDotPrefix() override;

  void doneParsing() override;

  void deliverCompletionResults();
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
class CompletionLookup : public swift::VisibleDeclConsumer {
  CodeCompletionResultSink &Sink;
  ASTContext &Ctx;
  OwnedResolver TypeResolver;
  const DeclContext *CurrDeclContext;

  enum class LookupKind {
    ValueExpr,
    ValueInDeclContext,
    EnumElement,
    Type,
    TypeInDeclContext,
    ImportFromModule,
  };

  LookupKind Kind;

  /// Type of the user-provided expression for LookupKind::ValueExpr
  /// completions.
  Type ExprType;

  /// User-provided base type for LookupKind::Type completions.
  Type BaseType;

  bool HaveDot = false;
  bool NeedLeadingDot = false;
  bool IsSuperRefExpr = false;
  bool IsDynamicLookup = false;

  /// \brief True if we are code completing inside a static method.
  bool InsideStaticMethod = false;

  /// \brief Innermost method that the code completion point is in.
  const AbstractFunctionDecl *CurrentMethod = nullptr;

  /// \brief Declarations that should get ExpressionSpecific semantic context.
  llvm::SmallSet<const Decl *, 4> ExpressionSpecificDecls;

  using DeducedAssociatedTypes =
      llvm::DenseMap<const AssociatedTypeDecl *, Type>;
  std::map<const NominalTypeDecl *, DeducedAssociatedTypes>
      DeducedAssociatedTypeCache;

public:
  struct RequestedResultsTy {
    const Module *TheModule;
    bool OnlyTypes;
    bool NeedLeadingDot;

    static RequestedResultsTy fromModule(const Module *TheModule) {
      return { TheModule, false, false };
    }

    RequestedResultsTy onlyTypes() const {
      return { TheModule, true, NeedLeadingDot };
    }

    RequestedResultsTy needLeadingDot(bool NeedDot) const {
      return { TheModule, OnlyTypes, NeedDot };
    }

    static RequestedResultsTy toplevelResults() {
      return { nullptr, false, false };
    }
  };

  Optional<RequestedResultsTy> RequestedCachedResults;

public:
  CompletionLookup(CodeCompletionResultSink &Sink,
                   ASTContext &Ctx,
                   const DeclContext *CurrDeclContext)
      : Sink(Sink), Ctx(Ctx),
        TypeResolver(createLazyResolver(Ctx)),
        CurrDeclContext(CurrDeclContext) {
    // Determine if we are doing code completion inside a static method.
    if (CurrDeclContext && CurrDeclContext->isLocalContext()) {
      const DeclContext *FunctionDC = CurrDeclContext;
      while (FunctionDC->isLocalContext()) {
        const DeclContext *Parent = FunctionDC->getParent();
        if (!Parent->isLocalContext())
          break;
        FunctionDC = Parent;
      }
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(FunctionDC)) {
        if (AFD->getExtensionType()) {
          CurrentMethod = AFD;
          if (auto *FD = dyn_cast<FuncDecl>(AFD))
            InsideStaticMethod = FD->isStatic();
        }
      }
    }
  }

  void setHaveDot() {
    HaveDot = true;
  }

  bool needDot() const {
    return NeedLeadingDot;
  }

  void setIsSuperRefExpr() {
    IsSuperRefExpr = true;
  }

  void setIsDynamicLookup() {
    IsDynamicLookup = true;
  }

  void addExpressionSpecificDecl(const Decl *D) {
    ExpressionSpecificDecls.insert(D);
  }

  SemanticContextKind getSemanticContext(const Decl *D,
                                         DeclVisibilityKind Reason) {
    switch (Reason) {
    case DeclVisibilityKind::LocalVariable:
    case DeclVisibilityKind::FunctionParameter:
    case DeclVisibilityKind::GenericParameter:
      if (ExpressionSpecificDecls.count(D))
        return SemanticContextKind::ExpressionSpecific;
      return SemanticContextKind::Local;

    case DeclVisibilityKind::MemberOfCurrentNominal:
      if (IsSuperRefExpr &&
          CurrentMethod && CurrentMethod->getOverriddenDecl() == D)
        return SemanticContextKind::ExpressionSpecific;
      return SemanticContextKind::CurrentNominal;

    case DeclVisibilityKind::MemberOfSuper:
      return SemanticContextKind::Super;

    case DeclVisibilityKind::MemberOfOutsideNominal:
      return SemanticContextKind::OutsideNominal;

    case DeclVisibilityKind::VisibleAtTopLevel:
      if (CurrDeclContext &&
          D->getModuleContext() == CurrDeclContext->getParentModule())
        return SemanticContextKind::CurrentModule;
      else
        return SemanticContextKind::OtherModule;

    case DeclVisibilityKind::DynamicLookup:
      // DynamicLookup results can come from different modules, including the
      // current module, but we always assign them the OtherModule semantic
      // context.  These declarations are uniqued by signature, so it is
      // totally random (determined by the hash function) which of the
      // equivalent declarations (across multiple modules) we will get.
      return SemanticContextKind::OtherModule;
    }
    llvm_unreachable("unhandled kind");
  }

  void addTypeAnnotation(CodeCompletionResultBuilder &Builder, Type T) {
    if (T->isVoid())
      Builder.addTypeAnnotation("Void");
    else
      Builder.addTypeAnnotation(T.getString());
  }

  static bool isBoringBoundGenericType(Type T) {
    BoundGenericType *BGT = T->getAs<BoundGenericType>();
    if (!BGT)
      return false;
    for (Type Arg : BGT->getGenericArgs()) {
      if (!Arg->is<GenericTypeParamType>())
        return false;
    }
    return true;
  }

  Type getTypeOfMember(const ValueDecl *VD) {
    if (ExprType) {
      Type ContextTy = VD->getDeclContext()->getDeclaredTypeOfContext();
      if (ContextTy) {
        Type MaybeNominalType = ExprType->getRValueInstanceType();
        if (ContextTy->getAnyNominal() == MaybeNominalType->getAnyNominal() &&
            !isBoringBoundGenericType(MaybeNominalType))
          return MaybeNominalType->getTypeOfMember(
              CurrDeclContext->getParentModule(), VD, TypeResolver.get());
      }
    }

    return VD->getType();
  }

  const DeducedAssociatedTypes &
  getAssociatedTypeMap(const NominalTypeDecl *NTD) {
    {
      auto It = DeducedAssociatedTypeCache.find(NTD);
      if (It != DeducedAssociatedTypeCache.end())
        return It->second;
    }

    DeducedAssociatedTypes Types;
    auto TopConformances = NTD->getConformances();
    SmallVector<ProtocolConformance *, 8> Worklist(TopConformances.begin(),
                                                   TopConformances.end());
    while (!Worklist.empty()) {
      auto Conformance = Worklist.pop_back_val();
      Conformance->forEachTypeWitness(TypeResolver.get(),
                                      [&](const AssociatedTypeDecl *ATD,
                                          const Substitution &Subst) -> bool {
        Types[ATD] = Subst.Replacement;
        return false;
      });
      for (auto It : Conformance->getInheritedConformances())
        Worklist.push_back(It.second);
    }

    auto ItAndInserted = DeducedAssociatedTypeCache.insert({ NTD, Types });
    assert(ItAndInserted.second == true && "should not be in the map");
    return ItAndInserted.first->second;
  }

  Type getAssociatedTypeType(const AssociatedTypeDecl *ATD) {
    Type BaseTy = BaseType;
    if (!BaseTy)
      BaseTy = ExprType;
    if (BaseTy) {
      BaseTy = BaseTy->getRValueInstanceType();
      if (auto NTD = BaseTy->getAnyNominal()) {
        auto &Types = getAssociatedTypeMap(NTD);
        if (Type T = Types.lookup(ATD))
          return MetatypeType::get(T, Ctx);
      }
    }
    return Type();
  }

  void addVarDeclRef(const VarDecl *VD, DeclVisibilityKind Reason) {
    StringRef Name = VD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    assert(VD->isStatic() ||
           !(InsideStaticMethod &&
            VD->getDeclContext() == CurrentMethod->getDeclContext()) &&
           "name lookup bug -- can not see an instance variable "
           "in a static function");

    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(VD, Reason));
    Builder.setAssociatedDecl(VD);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(Name);

    // Add a type annotation.
    Type VarType = getTypeOfMember(VD);
    if (VD->getName() == Ctx.SelfIdentifier) {
      // Strip @inout from 'self'.  It is useful to show @inout for function
      // parameters.  But for 'self' it is just noise.
      VarType = VarType->getInOutObjectType();
    }
    if (IsDynamicLookup) {
      // Values of properties that were found on a DynamicLookup have
      // Optional<T> type.
      VarType = OptionalType::get(VarType);
    }
    addTypeAnnotation(Builder, VarType);
  }

  void addPatternParameters(CodeCompletionResultBuilder &Builder,
                            const Pattern *P) {
    if (auto *TP = dyn_cast<TuplePattern>(P)) {
      bool NeedComma = false;
      for (auto TupleElt : TP->getFields()) {
        if (NeedComma)
          Builder.addComma();
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

  void addPatternFromTypeImpl(CodeCompletionResultBuilder &Builder, Type T,
                              Identifier Label, bool IsTopLevel) {
    if (auto *TT = T->getAs<TupleType>()) {
      if (!Label.empty()) {
        Builder.addTextChunk(Label.str());
        Builder.addTextChunk(": ");
      }
      Builder.addLeftParen();
      bool NeedComma = false;
      for (auto TupleElt : TT->getFields()) {
        if (NeedComma)
          Builder.addComma();
        addPatternFromTypeImpl(Builder, TupleElt.getType(),
                               TupleElt.getName(), false);
        NeedComma = true;
      }
      Builder.addRightParen();
      return;
    }
    if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
      if (IsTopLevel)
        Builder.addLeftParen();
      Builder.addCallParameter(Identifier(),
                               PT->getUnderlyingType().getString());
      if (IsTopLevel)
        Builder.addRightParen();
      return;
    }

    if (IsTopLevel)
      Builder.addLeftParen();
    Builder.addCallParameter(Label, T.getString());
    if (IsTopLevel)
      Builder.addRightParen();
  }

  void addPatternFromType(CodeCompletionResultBuilder &Builder, Type T) {
    addPatternFromTypeImpl(Builder, T, Identifier(), true);
  }

  void addFunctionCall(const AnyFunctionType *AFT) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Pattern,
        SemanticContextKind::ExpressionSpecific);
    Builder.addLeftParen();
    bool NeedComma = false;
    if (auto *TT = AFT->getInput()->getAs<TupleType>()) {
      for (auto TupleElt : TT->getFields()) {
        if (NeedComma)
          Builder.addComma();
        Builder.addCallParameter(TupleElt.getName(),
                                 TupleElt.getType().getString());
        NeedComma = true;
      }
    } else {
      Type T = AFT->getInput();
      if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
        // Only unwrap the paren shugar, if it exists.
        T = PT->getUnderlyingType();
      }
      Builder.addCallParameter(Identifier(), T->getString());
    }
    Builder.addRightParen();
    addTypeAnnotation(Builder, AFT->getResult());
  }

  void addMethodCall(const FuncDecl *FD, DeclVisibilityKind Reason) {
    bool IsImlicitlyCurriedInstanceMethod;
    switch (Kind) {
    case LookupKind::ValueExpr:
      IsImlicitlyCurriedInstanceMethod = ExprType->is<MetatypeType>() &&
                                         !FD->isStatic();
      break;
    case LookupKind::ValueInDeclContext:
      IsImlicitlyCurriedInstanceMethod =
          CurrentMethod &&
          FD->getDeclContext() == CurrentMethod->getDeclContext() &&
          InsideStaticMethod && !FD->isStatic();
      break;
    case LookupKind::EnumElement:
    case LookupKind::Type:
    case LookupKind::TypeInDeclContext:
      llvm_unreachable("can not have a method call while doing a "
                       "type completion");
    case LookupKind::ImportFromModule:
      IsImlicitlyCurriedInstanceMethod = false;
      break;
    }

    StringRef Name = FD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(FD, Reason));
    Builder.setAssociatedDecl(FD);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(Name);
    if (IsDynamicLookup)
      Builder.addDynamicLookupMethodCallTail();

    auto Patterns = FD->getArgParamPatterns();
    unsigned FirstIndex = 0;
    if (!IsImlicitlyCurriedInstanceMethod && FD->getImplicitSelfDecl())
      FirstIndex = 1;
    Type FunctionType = getTypeOfMember(FD);

    if (FirstIndex != 0)
      FunctionType = FunctionType->castTo<AnyFunctionType>()->getResult();

    Type FirstInputType = FunctionType->castTo<AnyFunctionType>()->getInput();

    if (IsImlicitlyCurriedInstanceMethod) {
      if (auto PT = dyn_cast<ParenType>(FirstInputType.getPointer()))
        FirstInputType = PT->getUnderlyingType();

      Builder.addLeftParen();
      Builder.addCallParameter(Ctx.SelfIdentifier,
                               FirstInputType.getString());
      Builder.addRightParen();
    } else {
      addPatternFromType(Builder, FirstInputType);
    }

    FunctionType = FunctionType->castTo<AnyFunctionType>()->getResult();

    // Build type annotation.
    llvm::SmallString<32> TypeStr;
    {
      llvm::raw_svector_ostream OS(TypeStr);
      for (unsigned i = FirstIndex + 1, e = Patterns.size(); i != e; ++i) {
        FunctionType->castTo<AnyFunctionType>()->getInput()->print(OS);
        FunctionType = FunctionType->castTo<AnyFunctionType>()->getResult();
        OS << " -> ";
      }
      // What's left is the result type.
      Type ResultType = FunctionType;
      if (ResultType->isVoid())
        OS << "Void";
      else
        ResultType.print(OS);
    }
    Builder.addTypeAnnotation(TypeStr);

    // TODO: skip arguments with default parameters?
  }

  void addConstructorCall(const ConstructorDecl *CD,
                          DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(CD, Reason));
    Builder.setAssociatedDecl(CD);
     if (IsSuperRefExpr) {
      assert(isa<ConstructorDecl>(
                 dyn_cast<AbstractFunctionDecl>(CurrDeclContext)) &&
             "can call super.init only inside a constructor");
      if (needDot())
        Builder.addLeadingDot();
      Builder.addTextChunk("init");
    }
    Type ConstructorType =
        getTypeOfMember(CD)->castTo<AnyFunctionType>()->getResult();
    addPatternFromType(
        Builder, ConstructorType->castTo<AnyFunctionType>()->getInput());
    addTypeAnnotation(
        Builder, ConstructorType->castTo<AnyFunctionType>()->getResult());
  }

  void addSubscriptCall(const SubscriptDecl *SD, DeclVisibilityKind Reason) {
    assert(!HaveDot && "can not add a subscript after a dot");
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(SD, Reason));
    Builder.setAssociatedDecl(SD);
    Builder.addLeftBracket();
    addPatternParameters(Builder, SD->getIndices());
    Builder.addRightBracket();

    // Add a type annotation.
    Type T = SD->getElementType();
    if (IsDynamicLookup) {
      // Values of properties that were found on a DynamicLookup have
      // Optional<T> type.
      T = OptionalType::get(T);
    }
    addTypeAnnotation(Builder, T);
  }

  void addNominalTypeRef(const NominalTypeDecl *NTD,
                         DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(NTD, Reason));
    Builder.setAssociatedDecl(NTD);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(NTD->getName().str());
    addTypeAnnotation(Builder,
                      MetatypeType::get(NTD->getDeclaredType(), Ctx));
  }

  void addTypeAliasRef(const TypeAliasDecl *TAD, DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(TAD, Reason));
    Builder.setAssociatedDecl(TAD);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(TAD->getName().str());
    if (TAD->hasUnderlyingType())
      addTypeAnnotation(Builder,
                        MetatypeType::get(TAD->getUnderlyingType(), Ctx));
    else {
      addTypeAnnotation(Builder,
                        MetatypeType::get(TAD->getDeclaredType(), Ctx));
    }
  }

  void addGenericTypeParamRef(const GenericTypeParamDecl *GP,
                              DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(GP, Reason));
    Builder.setAssociatedDecl(GP);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(GP->getName().str());
    addTypeAnnotation(Builder,
                      MetatypeType::get(GP->getDeclaredType(), Ctx));
  }

  void addAssociatedTypeRef(const AssociatedTypeDecl *AT,
                            DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(AT, Reason));
    Builder.setAssociatedDecl(AT);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(AT->getName().str());
    if (Type T = getAssociatedTypeType(AT))
      addTypeAnnotation(Builder, T);
  }

  void addEnumElementRef(const EnumElementDecl *EED,
                         DeclVisibilityKind Reason,
                         bool HasTypeContext) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        HasTypeContext ? SemanticContextKind::ExpressionSpecific
                       : getSemanticContext(EED, Reason));
    Builder.setAssociatedDecl(EED);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(EED->getName().str());
    if (EED->hasArgumentType())
      addPatternFromType(Builder, EED->getArgumentType());
    addTypeAnnotation(Builder, EED->getType());
  }

  void addKeyword(StringRef Name, Type TypeAnnotation) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.isNull())
      addTypeAnnotation(Builder, TypeAnnotation);
  }

  void addKeyword(StringRef Name, StringRef TypeAnnotation) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None);
    if (needDot())
      Builder.addLeadingDot();
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
  }

  // Implement swift::VisibleDeclConsumer
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
    if (!D->hasType())
      TypeResolver->resolveDeclSignature(D);

    switch (Kind) {
    case LookupKind::ValueExpr:
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        addVarDeclRef(VD, Reason);
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

        addMethodCall(FD, Reason);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason);
        return;
      }

      if (auto *AT = dyn_cast<AssociatedTypeDecl>(D)) {
        addAssociatedTypeRef(AT, Reason);
        return;
      }

      if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
        if (ExprType->is<MetatypeType>()) {
          if (HaveDot)
            return;
          addConstructorCall(CD, Reason);
        }
        if (IsSuperRefExpr) {
          if (auto *AFD = dyn_cast<AbstractFunctionDecl>(CurrDeclContext))
            if (!isa<ConstructorDecl>(AFD))
              return;
          addConstructorCall(CD, Reason);
        }
        return;
      }

      if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
        addEnumElementRef(EED, Reason, /*HasTypeContext=*/false);
      }

      if (HaveDot)
        return;

      if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
        if (ExprType->is<MetatypeType>())
          return;
        addSubscriptCall(SD, Reason);
        return;
      }
      return;

    case LookupKind::ValueInDeclContext:
    case LookupKind::ImportFromModule:
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        addVarDeclRef(VD, Reason);
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

        addMethodCall(FD, Reason);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason);
        return;
      }

      if (auto *AT = dyn_cast<AssociatedTypeDecl>(D)) {
        addAssociatedTypeRef(AT, Reason);
        return;
      }

      return;

    case LookupKind::EnumElement:
      if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
        addEnumElementRef(EED, Reason, /*HasTypeContext=*/true);
      }
      return;

    case LookupKind::Type:
    case LookupKind::TypeInDeclContext:
      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason);
        return;
      }

      if (auto *AT = dyn_cast<AssociatedTypeDecl>(D)) {
        addAssociatedTypeRef(AT, Reason);
        return;
      }

      return;
    }
  }

  void getTupleExprCompletions(TupleType *ExprType) {
    unsigned Index = 0;
    for (auto TupleElt : ExprType->getFields()) {
      CodeCompletionResultBuilder Builder(
          Sink,
          CodeCompletionResult::ResultKind::Pattern,
          SemanticContextKind::CurrentNominal);
      if (needDot())
        Builder.addLeadingDot();
      if (TupleElt.hasName()) {
        Builder.addTextChunk(TupleElt.getName().str());
      } else {
        llvm::SmallString<4> IndexStr;
        {
          llvm::raw_svector_ostream OS(IndexStr);
          OS << Index;
        }
        Builder.addTextChunk(IndexStr.str());
      }
      addTypeAnnotation(Builder, TupleElt.getType());
      Index++;
    }
  }

  void tryAddStlibOptionalCompletions(Type ExprType) {
    // If there is a dot, we don't have any special completions for
    // Optional<T>.
    if (!needDot())
      return;

    ExprType = ExprType->getRValueType();
    Type Unwrapped = ExprType->getOptionalObjectType(Ctx);
    if (!Unwrapped)
      return;
    // FIXME: consider types convertible to T?.

    {
      CodeCompletionResultBuilder Builder(
          Sink,
          CodeCompletionResult::ResultKind::Pattern,
          SemanticContextKind::ExpressionSpecific);
      Builder.addExclamationMark();
      addTypeAnnotation(Builder, Unwrapped);
    }
    {
      CodeCompletionResultBuilder Builder(
          Sink,
          CodeCompletionResult::ResultKind::Pattern,
          SemanticContextKind::ExpressionSpecific);
      Builder.addQuestionMark();
      addTypeAnnotation(Builder, Unwrapped);
    }
  }

  void getValueExprCompletions(Type ExprType) {
    Kind = LookupKind::ValueExpr;
    NeedLeadingDot = !HaveDot;
    this->ExprType = ExprType;
    bool Done = false;
    if (auto AFT = ExprType->getAs<AnyFunctionType>()) {
      addFunctionCall(AFT);
      Done = true;
    }
    if (auto LVT = ExprType->getAs<LValueType>()) {
      if (auto AFT = LVT->getObjectType()->getAs<AnyFunctionType>()) {
        addFunctionCall(AFT);
        Done = true;
      }
    }
    if (auto MT = ExprType->getAs<ModuleType>()) {
      Module *M = MT->getModule();
      if (CurrDeclContext->getParentModule() != M) {
        // Only use the cache if it is not the current module.
        RequestedCachedResults = RequestedResultsTy::fromModule(M)
                                                     .needLeadingDot(needDot());
        Done = true;
      }
    }
    if (auto *TT = ExprType->getRValueType()->getAs<TupleType>()) {
      getTupleExprCompletions(TT);
      Done = true;
    }
    tryAddStlibOptionalCompletions(ExprType);
    if (!Done) {
      lookupVisibleMemberDecls(*this, ExprType, CurrDeclContext,
                               TypeResolver.get());
    }
    {
      // Add the special qualified keyword 'metatype' so that, for example,
      // 'Int.metatype' can be completed.
      Type Annotation = ExprType;

      // First, unwrap the outer LValue.  LValueness of the expr is unrelated
      // to the LValueness of the metatype.
      if (auto *LVT = dyn_cast<LValueType>(Annotation.getPointer())) {
        Annotation = LVT->getObjectType();
      }

      Annotation = MetatypeType::get(Annotation, Ctx);

      // Use the canonical type as a type annotation because looking at the
      // '.metatype' in the IDE is a way to understand what type the expression
      // has.
      addKeyword("metatype", Annotation->getCanonicalType());
    }
  }

  void getValueCompletionsInDeclContext(SourceLoc Loc) {
    Kind = LookupKind::ValueInDeclContext;
    NeedLeadingDot = false;
    lookupVisibleDecls(*this, CurrDeclContext, TypeResolver.get(),
                       /*IncludeTopLevel=*/false, Loc);

    // FIXME: The pedantically correct way to find the type is to resolve the
    // swift.StringLiteralType type.
    addKeyword("__FILE__", "String");
    // Same: swift.IntegerLiteralType.
    addKeyword("__LINE__", "Int");
    addKeyword("__COLUMN__", "Int");
    RequestedCachedResults = RequestedResultsTy::toplevelResults();
  }

  void getTypeContextEnumElementCompletions(SourceLoc Loc) {
    llvm::SaveAndRestore<LookupKind> ChangeLookupKind(
        Kind, LookupKind::EnumElement);
    NeedLeadingDot = !HaveDot;

    const DeclContext *FunctionDC = CurrDeclContext;
    const AbstractFunctionDecl *CurrentFunction = nullptr;
    while (FunctionDC->isLocalContext()) {
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(FunctionDC)) {
        CurrentFunction = AFD;
        break;
      }
      FunctionDC = FunctionDC->getParent();
    }
    if (!CurrentFunction)
      return;

    auto *Switch = cast_or_null<SwitchStmt>(
        findNearestStmt(CurrentFunction, Loc, StmtKind::Switch));
    if (!Switch)
      return;
    auto Ty = Switch->getSubjectExpr()->getType();
    if (!Ty)
      return;
    auto *TheEnumDecl = dyn_cast_or_null<EnumDecl>(Ty->getAnyNominal());
    if (!TheEnumDecl)
      return;
    for (auto Element : TheEnumDecl->getAllElements()) {
      foundDecl(Element, DeclVisibilityKind::MemberOfCurrentNominal);
    }
  }

  void getTypeCompletions(Type BaseType) {
    Kind = LookupKind::Type;
    this->BaseType = BaseType;
    NeedLeadingDot = !HaveDot;
    lookupVisibleMemberDecls(*this, MetatypeType::get(BaseType, Ctx),
                             CurrDeclContext, TypeResolver.get());
  }

  void getTypeCompletionsInDeclContext(SourceLoc Loc) {
    Kind = LookupKind::TypeInDeclContext;
    lookupVisibleDecls(*this, CurrDeclContext, TypeResolver.get(),
                       /*IncludeTopLevel=*/false, Loc);

    RequestedCachedResults =
        RequestedResultsTy::toplevelResults().onlyTypes();
  }

  void getToplevelCompletions(bool OnlyTypes) {
    Kind = OnlyTypes ? LookupKind::TypeInDeclContext
                     : LookupKind::ValueInDeclContext;
    NeedLeadingDot = false;
    Module *M = CurrDeclContext->getParentModule();
    M->lookupVisibleDecls({}, *this, NLKind::QualifiedLookup);
  }

  void getModuleImportCompletions(StringRef ModuleName,
                                  const std::vector<std::string> &AccessPath,
                                  bool ResultsHaveLeadingDot) {
    Kind = LookupKind::ImportFromModule;
    NeedLeadingDot = ResultsHaveLeadingDot;

    auto ModulePath =
        std::make_pair(Ctx.getIdentifier(ModuleName), SourceLoc());
    Module *M = Ctx.getModule(llvm::makeArrayRef(ModulePath));
    if (!M)
      return;

    llvm::SmallVector<std::pair<Identifier, SourceLoc>, 1> LookupAccessPath;
    for (auto Piece : AccessPath) {
      LookupAccessPath.push_back(
          std::make_pair(Ctx.getIdentifier(Piece), SourceLoc()));
    }

    using namespace swift::namelookup;
    SmallVector<ValueDecl *, 1> Decls;
    lookupVisibleDeclsInModule(M, LookupAccessPath, Decls,
                               NLKind::QualifiedLookup,
                               ResolutionKind::Overloadable,
                               TypeResolver.get());

    for (auto *VD : Decls) {
      foundDecl(VD, DeclVisibilityKind::VisibleAtTopLevel);
    }
  }
};

} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeDotExpr(Expr *E) {
  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::DotExpr;
  ParsedExpr = E;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completePostfixExprBeginning() {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::PostfixExprBeginning;
  CurDeclContext = P.CurDeclContext;
  CStyleForLoopIterationVariable =
      CodeCompletionCallbacks::CStyleForLoopIterationVariable;
}

void CodeCompletionCallbacksImpl::completePostfixExpr(Expr *E) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::PostfixExpr;
  ParsedExpr = E;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeExprSuper(SuperRefExpr *SRE) {
  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::SuperExpr;
  ParsedExpr = SRE;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeExprSuperDot(SuperRefExpr *SRE) {
  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::SuperExprDot;
  ParsedExpr = SRE;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeSimpleBeginning() {
  Kind = CompletionKind::TypeSimpleBeginning;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeIdentifierWithDot(
    IdentTypeRepr *ITR) {
  if (!ITR) {
    completeTypeSimpleBeginning();
    return;
  }
  Kind = CompletionKind::TypeIdentifierWithDot;
  ParsedTypeLoc = TypeLoc(ITR);
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeIdentifierWithoutDot(
    IdentTypeRepr *ITR) {
  assert(ITR);
  Kind = CompletionKind::TypeIdentifierWithoutDot;
  ParsedTypeLoc = TypeLoc(ITR);
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeCaseStmtBeginning() {
  assert(!InEnumElementRawValue);

  Kind = CompletionKind::CaseStmtBeginning;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeCaseStmtDotPrefix() {
  assert(!InEnumElementRawValue);

  Kind = CompletionKind::CaseStmtDotPrefix;
  CurDeclContext = P.CurDeclContext;
}

static bool isDynamicLookup(Type T) {
  if (auto *PT = T->getRValueType()->getAs<ProtocolType>())
    return PT->getDecl()->isSpecificProtocol(KnownProtocolKind::DynamicLookup);
  return false;
}

void CodeCompletionCallbacksImpl::doneParsing() {
  if (Kind == CompletionKind::None) {
    return;
  }

  if (!typecheckContext())
    return;

  if (DelayedParsedDecl && !typecheckDelayedParsedDecl())
    return;

  if (auto *AFD = dyn_cast_or_null<AbstractFunctionDecl>(DelayedParsedDecl))
    CurDeclContext = AFD;

  if (ParsedExpr && !typecheckParsedExpr())
    return;

  if (!ParsedTypeLoc.isNull() && !typecheckParsedType())
    return;

  CompletionLookup Lookup(CompletionContext.getResultSink(), P.Context,
                          CurDeclContext);

  switch (Kind) {
  case CompletionKind::None:
    llvm_unreachable("should be already handled");
    return;

  case CompletionKind::DotExpr: {
    Lookup.setHaveDot();
    Type ExprType = ParsedExpr->getType();
    if (isDynamicLookup(ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(ExprType);
    break;
  }

  case CompletionKind::PostfixExprBeginning: {
    if (CStyleForLoopIterationVariable)
      Lookup.addExpressionSpecificDecl(CStyleForLoopIterationVariable);
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    Lookup.getValueCompletionsInDeclContext(Loc);
    break;
  }

  case CompletionKind::PostfixExpr: {
    Type ExprType = ParsedExpr->getType();
    if (isDynamicLookup(ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(ExprType);
    break;
  }

  case CompletionKind::SuperExpr: {
    Lookup.setIsSuperRefExpr();
    Lookup.getValueExprCompletions(ParsedExpr->getType());
    break;
  }

  case CompletionKind::SuperExprDot: {
    Lookup.setIsSuperRefExpr();
    Lookup.setHaveDot();
    Lookup.getValueExprCompletions(ParsedExpr->getType());
    break;
  }

  case CompletionKind::TypeSimpleBeginning: {
    Lookup.getTypeCompletionsInDeclContext(
        P.Context.SourceMgr.getCodeCompletionLoc());
    break;
  }

  case CompletionKind::TypeIdentifierWithDot: {
    Lookup.setHaveDot();
    Lookup.getTypeCompletions(ParsedTypeLoc.getType());
    break;
  }

  case CompletionKind::TypeIdentifierWithoutDot: {
    Lookup.getTypeCompletions(ParsedTypeLoc.getType());
    break;
  }

  case CompletionKind::CaseStmtBeginning: {
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    Lookup.getValueCompletionsInDeclContext(Loc);
    Lookup.getTypeContextEnumElementCompletions(Loc);
    break;
  }

  case CompletionKind::CaseStmtDotPrefix: {
    Lookup.setHaveDot();
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    Lookup.getTypeContextEnumElementCompletions(Loc);
    break;
  }
  }

  if (Lookup.RequestedCachedResults) {
    auto &Request = Lookup.RequestedCachedResults.getValue();
    if (Request.TheModule) {
      // Create helpers for result caching.
      auto &SwiftContext = P.Context;
      auto FillCacheCallback =
          [&SwiftContext](CodeCompletionCacheImpl &Cache,
                          const CodeCompletionCacheImpl::Key &K) {
        auto V = Cache.getResultSinkFor(K);
        CompletionLookup Lookup(V->Sink, SwiftContext, nullptr);
        Lookup.getModuleImportCompletions(K.ModuleName, K.AccessPath,
                                          K.ResultsHaveLeadingDot);
        Cache.storeResults(K, V);
      };

      // FIXME: actually check imports.
      StringRef ModuleFilename = Request.TheModule->getModuleFilename();
      assert(!ModuleFilename.empty() && "should have a filename");
      CodeCompletionCacheImpl::Key K{ModuleFilename,
                                     Request.TheModule->Name.str(),
                                     {}, Request.NeedLeadingDot};
      CompletionContext.Cache.Impl->getResults(
          K, CompletionContext.getResultSink(), Request.OnlyTypes,
          FillCacheCallback);
    } else {
      // Add results from current module.
      Lookup.getToplevelCompletions(Request.OnlyTypes);

      // Create helpers for result caching.
      auto &SwiftContext = P.Context;
      auto FillCacheCallback =
          [&SwiftContext](CodeCompletionCacheImpl &Cache,
                          const CodeCompletionCacheImpl::Key &K) {
        auto V = Cache.getResultSinkFor(K);
        CompletionLookup Lookup(V->Sink, SwiftContext, nullptr);
        Lookup.getModuleImportCompletions(K.ModuleName, K.AccessPath,
                                          K.ResultsHaveLeadingDot);
        Cache.storeResults(K, V);
      };

      // Add results for all imported modules.
      auto *SF = CurDeclContext->getParentSourceFile();
      for (std::pair<Module::ImportedModule, bool> Imported :
               SF->getImports()) {
        std::vector<std::string> AccessPath;
        for (auto Piece : Imported.first.first) {
          AccessPath.push_back(Piece.first.str());
        }
        Module *TheModule = Imported.first.second;
        StringRef ModuleFilename = TheModule->getModuleFilename();
        assert(!ModuleFilename.empty() && "should have a filename");
        CodeCompletionCacheImpl::Key K{ModuleFilename,
                                       TheModule->Name.str(),
                                       AccessPath, false};
        CompletionContext.Cache.Impl->getResults(
            K, CompletionContext.getResultSink(), Request.OnlyTypes,
            FillCacheCallback);
      }
    }
    Lookup.RequestedCachedResults.reset();
  }

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::deliverCompletionResults() {
  auto Results = CompletionContext.takeResults();
  if (!Results.empty()) {
    Consumer.handleResults(Results);
    DeliveredResults = true;
  }
}

void PrintingCodeCompletionConsumer::handleResults(
    MutableArrayRef<CodeCompletionResult *> Results) {
  OS << "Begin completions, " << Results.size() << " items\n";
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
swift::ide::makeCodeCompletionCallbacksFactory(
    CodeCompletionContext &CompletionContext,
    CodeCompletionConsumer &Consumer) {
  return new CodeCompletionCallbacksFactoryImpl(CompletionContext, Consumer);
}

