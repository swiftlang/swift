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
#include "swift/IDE/Utils.h"
#include "swift/Basic/Cache.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/ThreadSafeRefCounted.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include "CodeCompletionResultBuilder.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
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
} // unnamed namespace

static Stmt *findNearestStmt(const AbstractFunctionDecl *AFD, SourceLoc Loc,
                             StmtKind Kind) {
  auto &SM = AFD->getASTContext().SourceMgr;
  assert(SM.rangeContainsTokenLoc(AFD->getSourceRange(), Loc));
  StmtFinder Finder(SM, Loc, Kind);
  // FIXME(thread-safety): the walker is is mutating the AST.
  const_cast<AbstractFunctionDecl *>(AFD)->walk(Finder);
  return Finder.getFoundStmt();
}

CodeCompletionString::CodeCompletionString(ArrayRef<Chunk> Chunks) {
  Chunk *TailChunks = reinterpret_cast<Chunk *>(this + 1);
  std::copy(Chunks.begin(), Chunks.end(), TailChunks);
  NumChunks = Chunks.size();
}

void CodeCompletionString::print(raw_ostream &OS) const {
  unsigned PrevNestingLevel = 0;
  for (auto C : getChunks()) {
    bool AnnotatedTextChunk = false;
    if (C.getNestingLevel() < PrevNestingLevel) {
      OS << "#}";
    }
    switch (C.getKind()) {
    case Chunk::ChunkKind::AccessControlKeyword:
    case Chunk::ChunkKind::OverrideKeyword:
    case Chunk::ChunkKind::DeclIntroducer:
    case Chunk::ChunkKind::Text:
    case Chunk::ChunkKind::LeftParen:
    case Chunk::ChunkKind::RightParen:
    case Chunk::ChunkKind::LeftBracket:
    case Chunk::ChunkKind::RightBracket:
    case Chunk::ChunkKind::LeftAngle:
    case Chunk::ChunkKind::RightAngle:
    case Chunk::ChunkKind::Dot:
    case Chunk::ChunkKind::Ellipsis:
    case Chunk::ChunkKind::Comma:
    case Chunk::ChunkKind::ExclamationMark:
    case Chunk::ChunkKind::QuestionMark:
    case Chunk::ChunkKind::Ampersand:
      AnnotatedTextChunk = C.isAnnotation();
      SWIFT_FALLTHROUGH;
    case Chunk::ChunkKind::CallParameterName:
    case Chunk::ChunkKind::CallParameterInternalName:
    case Chunk::ChunkKind::CallParameterColon:
    case Chunk::ChunkKind::CallParameterType:
    case Chunk::ChunkKind::CallParameterClosureType:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
      if (AnnotatedTextChunk)
        OS << "['";
      else if (C.getKind() == Chunk::ChunkKind::CallParameterInternalName)
        OS << "(";
      else if (C.getKind() == Chunk::ChunkKind::CallParameterClosureType)
        OS << "##";
      for (char Ch : C.getText()) {
        if (Ch == '\n')
          OS << "\\n";
        else
          OS << Ch;
      }
      if (AnnotatedTextChunk)
        OS << "']";
      else if (C.getKind() == Chunk::ChunkKind::CallParameterInternalName)
        OS << ")";
      break;
    case Chunk::ChunkKind::OptionalBegin:
    case Chunk::ChunkKind::CallParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
      OS << "{#";
      break;
    case Chunk::ChunkKind::DynamicLookupMethodCallTail:
    case Chunk::ChunkKind::OptionalMethodCallTail:
      OS << C.getText();
      break;
    case Chunk::ChunkKind::TypeAnnotation:
      OS << "[#";
      OS << C.getText();
      OS << "#]";
      break;
    case Chunk::ChunkKind::BraceStmtWithCursor:
      OS << " {|}";
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
  case DeclKind::IfConfig:
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
  case DeclKind::Var:
  case DeclKind::Param: {
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
  if (NotRecommended)
    Prefix.append("/NotRecommended");
  if (NumBytesToErase != 0) {
    Prefix.append("/Erase[");
    Prefix.append(Twine(NumBytesToErase).str());
    Prefix.append("]");
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

static ArrayRef<StringRef> copyStringArray(llvm::BumpPtrAllocator &Allocator,
                                           ArrayRef<StringRef> Arr) {
  StringRef *Buff = Allocator.Allocate<StringRef>(Arr.size());
  std::copy(Arr.begin(), Arr.end(), Buff);
  return llvm::makeArrayRef(Buff, Arr.size());
}

void CodeCompletionResultBuilder::addChunkWithText(
    CodeCompletionString::Chunk::ChunkKind Kind, StringRef Text) {
  addChunkWithTextNoCopy(Kind, copyString(*Sink.Allocator, Text));
}

StringRef CodeCompletionContext::copyString(StringRef Str) {
  return ::copyString(*CurrentResults.Allocator, Str);
}

bool shouldCopyAssociatedUSRForDecl(const ValueDecl *VD) {
  // Avoid trying to generate a USR for some declaration types.
  if (isa<AbstractTypeParamDecl>(VD) && !isa<AssociatedTypeDecl>(VD))
    return false;
  if (isa<ParamDecl>(VD))
    return false;
  if (VD->hasClangNode() && !VD->getClangDecl())
    return false;

  return true;
}

template <typename FnTy>
static void walkValueDeclAndOverriddenDecls(const Decl *D, const FnTy &Fn) {
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    Fn(VD);
    walkOverriddenDecls(VD, Fn);
  }
}

ArrayRef<StringRef> copyAssociatedUSRs(llvm::BumpPtrAllocator &Allocator,
                                       const Decl *D) {
  llvm::SmallVector<StringRef, 4> USRs;
  walkValueDeclAndOverriddenDecls(D, [&](llvm::PointerUnion<const ValueDecl*,
                                                  const clang::NamedDecl*> OD) {
    llvm::SmallString<128> SS;
    bool Ignored = true;
    if (auto *OVD = OD.dyn_cast<const ValueDecl*>()) {
      if (shouldCopyAssociatedUSRForDecl(OVD)) {
        llvm::raw_svector_ostream OS(SS);
        Ignored = printDeclUSR(OVD, OS);
      }
    } else if (auto *OND = OD.dyn_cast<const clang::NamedDecl*>()) {
      Ignored = clang::index::generateUSRForDecl(OND, SS);
    }
    
    if (!Ignored)
      USRs.push_back(copyString(Allocator, SS));
  });

  if (!USRs.empty())
    return copyStringArray(Allocator, USRs);

  return ArrayRef<StringRef>();
}

CodeCompletionResult *CodeCompletionResultBuilder::takeResult() {
  void *CCSMem = Sink.Allocator
      ->Allocate(sizeof(CodeCompletionString) +
                     Chunks.size() * sizeof(CodeCompletionString::Chunk),
                 llvm::alignOf<CodeCompletionString>());
  auto *CCS = new (CCSMem) CodeCompletionString(Chunks);

  switch (Kind) {
  case CodeCompletionResult::ResultKind::Declaration: {
    StringRef BriefComment;
    auto MaybeClangNode = AssociatedDecl->getClangNode();
    if (MaybeClangNode) {
      if (auto *D = MaybeClangNode.getAsDecl()) {
        const auto &ClangContext = D->getASTContext();
        if (const clang::RawComment *RC =
                ClangContext.getRawCommentForAnyRedecl(D))
          BriefComment = RC->getBriefText(ClangContext);
      }
    } else {
      BriefComment = AssociatedDecl->getBriefComment();
    }
    return new (*Sink.Allocator) CodeCompletionResult(
        SemanticContext, NumBytesToErase, CCS, AssociatedDecl,
        /*NotRecommended=*/false,
        copyString(*Sink.Allocator, BriefComment),
        copyAssociatedUSRs(*Sink.Allocator, AssociatedDecl));
  }

  case CodeCompletionResult::ResultKind::Keyword:
  case CodeCompletionResult::ResultKind::Pattern:
    return new (*Sink.Allocator)
        CodeCompletionResult(Kind, SemanticContext, NumBytesToErase, CCS);
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
      const Module *TheModule,
      std::function<ValueRefCntPtr(CodeCompletionCacheImpl &, Key,
                         const Module *)> FillCacheCallback);

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
    return V.Sink.Allocator->getTotalMemory();
  }
};
} // namespace sys
} // namespace swift

void CodeCompletionCacheImpl::getResults(
    const Key &K, CodeCompletionResultSink &TargetSink, bool OnlyTypes,
    const Module *TheModule,
    std::function<ValueRefCntPtr(
        CodeCompletionCacheImpl &, Key, const Module *)> FillCacheCallback) {
  // FIXME(thread-safety): lock the whole AST context.  We might load a module.
  llvm::Optional<ValueRefCntPtr> V = TheCache.get(K);
  if (!V.hasValue()) {
    // No cached results found.  Fill the cache.
    V = FillCacheCallback(*this, K, TheModule);
  } else {
    llvm::sys::fs::file_status ModuleStatus;
    if (llvm::sys::fs::status(K.ModuleFilename, ModuleStatus) ||
        V.getValue()->ModuleModificationTime !=
            ModuleStatus.getLastModificationTime()) {
      // Cache is stale.  Update the cache.
      TheCache.remove(K);
      V = FillCacheCallback(*this, K, TheModule);
    }
  }
  assert(V.hasValue());
  auto &SourceSink = V.getValue()->Sink;

  // We will be adding foreign results (from another sink) into TargetSink.
  // TargetSink should have an owning pointer to the allocator that keeps the
  // results alive.
  TargetSink.ForeignAllocators.push_back(SourceSink.Allocator);

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
      CurrentResults.Allocator->Allocate<CodeCompletionResult *>(Count);
  std::copy(CurrentResults.Results.begin(), CurrentResults.Results.end(),
            Results);
  CurrentResults.Results.clear();
  return MutableArrayRef<CodeCompletionResult *>(Results, Count);
}

Optional<unsigned> CodeCompletionString::getFirstTextChunkIndex() const {
  for (auto i : indices(getChunks())) {
    auto &C = getChunks()[i];
    switch (C.getKind()) {
    case CodeCompletionString::Chunk::ChunkKind::Text:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterName:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterInternalName:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterName:
    case CodeCompletionString::Chunk::ChunkKind::LeftParen:
    case CodeCompletionString::Chunk::ChunkKind::LeftBracket:
      return i;
    case CodeCompletionString::Chunk::ChunkKind::RightParen:
    case CodeCompletionString::Chunk::ChunkKind::RightBracket:
    case CodeCompletionString::Chunk::ChunkKind::LeftAngle:
    case CodeCompletionString::Chunk::ChunkKind::RightAngle:
    case CodeCompletionString::Chunk::ChunkKind::Dot:
    case CodeCompletionString::Chunk::ChunkKind::Ellipsis:
    case CodeCompletionString::Chunk::ChunkKind::Comma:
    case CodeCompletionString::Chunk::ChunkKind::ExclamationMark:
    case CodeCompletionString::Chunk::ChunkKind::QuestionMark:
    case CodeCompletionString::Chunk::ChunkKind::Ampersand:
    case CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword:
    case CodeCompletionString::Chunk::ChunkKind::OverrideKeyword:
    case CodeCompletionString::Chunk::ChunkKind::DeclIntroducer:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterColon:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterType:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterClosureType:
    case CodeCompletionString::Chunk::ChunkKind::OptionalBegin:
    case CodeCompletionString::Chunk::ChunkKind::CallParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin:
    case CodeCompletionString::Chunk::ChunkKind::DynamicLookupMethodCallTail:
    case CodeCompletionString::Chunk::ChunkKind::OptionalMethodCallTail:
    case CodeCompletionString::Chunk::ChunkKind::TypeAnnotation:
      continue;

    case CodeCompletionString::Chunk::ChunkKind::BraceStmtWithCursor:
      llvm_unreachable("should have already extracted the text");
    }
  }
  return None;
}

StringRef CodeCompletionString::getFirstTextChunk() const {
  Optional<unsigned> Idx = getFirstTextChunkIndex();
  if (Idx.hasValue())
    return getChunks()[*Idx].getText();
  return StringRef();
}

void CodeCompletionString::getName(raw_ostream &OS) const {
  auto FirstTextChunk = getFirstTextChunkIndex();
  int TextSize = 0;
  if (FirstTextChunk.hasValue()) {
    for (auto C : getChunks().slice(*FirstTextChunk)) {
      using ChunkKind = CodeCompletionString::Chunk::ChunkKind;
      if (C.getKind() == ChunkKind::BraceStmtWithCursor)
        break;
      if (C.getKind() == ChunkKind::TypeAnnotation)
        continue;
      if (C.hasText() && !C.isAnnotation()) {
        TextSize += C.getText().size();
        OS << C.getText();
      }
    }
  }
  assert((TextSize > 0) &&
         "code completion string should have non-empty name!");
}

void CodeCompletionContext::sortCompletionResults(
    MutableArrayRef<CodeCompletionResult *> Results) {
  std::sort(Results.begin(), Results.end(),
            [](CodeCompletionResult *LHS, CodeCompletionResult *RHS) {
    llvm::SmallString<64> LSS;
    llvm::SmallString<64> RSS;
    {
      llvm::raw_svector_ostream LOS(LSS);
      LHS->getCompletionString()->getName(LOS);
      llvm::raw_svector_ostream ROS(RSS);
      RHS->getCompletionString()->getName(ROS);
    }
    int Result = LSS.compare_lower(RSS);
    // If the case insensitive comparison is equal, then secondary sort order
    // should be case sensitive.
    if (Result == 0)
      Result = LSS.compare(RSS);
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
    PostfixExprParen,
    SuperExpr,
    SuperExprDot,
    TypeSimpleBeginning,
    TypeIdentifierWithDot,
    TypeIdentifierWithoutDot,
    CaseStmtBeginning,
    CaseStmtDotPrefix,
    NominalMemberBeginning,
  };

  CompletionKind Kind = CompletionKind::None;
  Expr *ParsedExpr = nullptr;
  SourceLoc DotLoc;
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
      // Find the nearest containing function or nominal decl.
      DeclContext *DCToTypeCheck = DC;
      while (!DCToTypeCheck->isModuleContext() &&
             !isa<AbstractFunctionDecl>(DCToTypeCheck) &&
             !isa<NominalTypeDecl>(DCToTypeCheck) &&
             !isa<TopLevelCodeDecl>(DCToTypeCheck))
        DCToTypeCheck = DCToTypeCheck->getParent();
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DCToTypeCheck)) {
        // We found a function.  First, type check the nominal decl that
        // contains the function.  Then type check the function itself.
        typecheckContextImpl(DCToTypeCheck->getParent());
        return typeCheckAbstractFunctionBodyUntil(AFD, EndTypeCheckLoc);
      }
      if (isa<NominalTypeDecl>(DCToTypeCheck)) {
        // We found a nominal decl (for example, the closure is used in an
        // initializer of a property).
        return typecheckContextImpl(DCToTypeCheck);
      }
      if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(DCToTypeCheck)) {
        return typeCheckTopLevelCodeDecl(TLCD);
      }
      return false;
    }
    if (auto *NTD = dyn_cast<NominalTypeDecl>(DC)) {
      // First, type check the parent DeclContext.
      typecheckContextImpl(DC->getParent());
      if (NTD->hasType())
        return true;
      return typeCheckCompletionDecl(cast<NominalTypeDecl>(DC));
    }
    if (auto *TLCD = dyn_cast<TopLevelCodeDecl>(DC)) {
      return typeCheckTopLevelCodeDecl(TLCD);
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
  void completeDotExpr(Expr *E, SourceLoc DotLoc) override;
  void completePostfixExprBeginning() override;
  void completePostfixExpr(Expr *E) override;
  void completePostfixExprParen(Expr *E) override;
  void completeExprSuper(SuperRefExpr *SRE) override;
  void completeExprSuperDot(SuperRefExpr *SRE) override;

  void completeTypeSimpleBeginning() override;
  void completeTypeIdentifierWithDot(IdentTypeRepr *ITR) override;
  void completeTypeIdentifierWithoutDot(IdentTypeRepr *ITR) override;

  void completeCaseStmtBeginning() override;
  void completeCaseStmtDotPrefix() override;

  void completeNominalMemberBeginning() override;

  void addKeywords(CodeCompletionResultSink &Sink);

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
class CompletionLookup final : public swift::VisibleDeclConsumer {
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
  SourceLoc DotLoc;
  bool NeedLeadingDot = false;

  bool NeedOptionalUnwrap = false;
  unsigned NumBytesToEraseForOptionalUnwrap = 0;

  bool HaveLParen = false;
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

  Optional<SemanticContextKind> ForcedSemanticContext = None;

public:
  bool FoundFunctionCalls = false;
  bool FoundFunctionsWithoutFirstKeyword = false;

private:
  void foundFunction(const AbstractFunctionDecl *AFD) {
    FoundFunctionCalls = true;
    DeclName Name = AFD->getFullName();
    auto ArgNames = Name.getArgumentNames();
    if (ArgNames.empty())
      return;
    if (ArgNames[0].empty())
      FoundFunctionsWithoutFirstKeyword = true;
  }

  void foundFunction(const AnyFunctionType *AFT) {
    FoundFunctionCalls = true;
    Type In = AFT->getInput();
    if (!In)
      return;
    if (isa<ParenType>(In.getPointer())) {
      FoundFunctionsWithoutFirstKeyword = true;
      return;
    }
    TupleType *InTuple = In->getAs<TupleType>();
    if (!InTuple)
      return;
    auto Fields = InTuple->getFields();
    if (Fields.empty())
      return;
    if (!Fields[0].hasName())
      FoundFunctionsWithoutFirstKeyword = true;
  }

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
    if (CurrDeclContext) {
      CurrentMethod = CurrDeclContext->getInnermostMethodContext();
      if (auto *FD = dyn_cast_or_null<FuncDecl>(CurrentMethod))
        InsideStaticMethod = FD->isStatic();
    }
  }

  void discardTypeResolver() {
    TypeResolver.reset();
  }

  void setHaveDot(SourceLoc DotLoc) {
    HaveDot = true;
    this->DotLoc = DotLoc;
  }

  bool needDot() const {
    return NeedLeadingDot;
  }

  void setHaveLParen(bool Value) {
    HaveLParen = Value;
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
    if (ForcedSemanticContext)
      return *ForcedSemanticContext;

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

    case DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal:
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
      // AnyObject results can come from different modules, including the
      // current module, but we always assign them the OtherModule semantic
      // context.  These declarations are uniqued by signature, so it is
      // totally random (determined by the hash function) which of the
      // equivalent declarations (across multiple modules) we will get.
      return SemanticContextKind::OtherModule;
    }
    llvm_unreachable("unhandled kind");
  }

  void addLeadingDot(CodeCompletionResultBuilder &Builder) {
    if (NeedOptionalUnwrap) {
      Builder.setNumBytesToErase(NumBytesToEraseForOptionalUnwrap);
      Builder.addQuestionMark();
      Builder.addLeadingDot();
      return;
    }
    if (needDot())
      Builder.addLeadingDot();
  }

  void addTypeAnnotation(CodeCompletionResultBuilder &Builder, Type T) {
    T = T->getReferenceStorageReferent();
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
    SmallVector<ProtocolConformance *, 8> Worklist;

    // Collect conformances from the nominal and its superclasses.
    const auto *CurrNominal = NTD;
    while (true) {
      auto Conformances = CurrNominal->getConformances();
      Worklist.append(Conformances.begin(), Conformances.end());
      if (const auto *CD = dyn_cast<ClassDecl>(CurrNominal)) {
        if (CD->hasSuperclass()) {
          CurrNominal = CD->getSuperclass()->getAnyNominal();
          continue;
        }
      }
      break;
    }

    while (!Worklist.empty()) {
      auto Conformance = Worklist.pop_back_val();
      if (!Conformance->isComplete())
        continue;
      Conformance->forEachTypeWitness(TypeResolver.get(),
                                      [&](const AssociatedTypeDecl *ATD,
                                          const Substitution &Subst) -> bool {
        Types[ATD] = Subst.getReplacement();
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
    if (!BaseTy && CurrDeclContext)
      BaseTy = CurrDeclContext->getInnermostTypeContext()
                   ->getDeclaredTypeInContext();
    if (BaseTy) {
      BaseTy = BaseTy->getRValueInstanceType();
      if (auto NTD = BaseTy->getAnyNominal()) {
        auto &Types = getAssociatedTypeMap(NTD);
        if (Type T = Types.lookup(ATD))
          return T;
      }
    }
    return Type();
  }

  void addVarDeclRef(const VarDecl *VD, DeclVisibilityKind Reason) {
    if (!VD->hasName())
      return;

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
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);

    // Add a type annotation.
    Type VarType = getTypeOfMember(VD);
    if (VD->getName() == Ctx.Id_self) {
      // Strip inout from 'self'.  It is useful to show inout for function
      // parameters.  But for 'self' it is just noise.
      VarType = VarType->getInOutObjectType();
    }
    if (IsDynamicLookup || VD->getAttrs().hasAttribute<OptionalAttr>()) {
      // Values of properties that were found on a AnyObject have
      // Optional<T> type.  Same applies to optional members.
      VarType = OptionalType::get(VarType);
    }
    addTypeAnnotation(Builder, VarType);
  }

  void addPatternParameters(CodeCompletionResultBuilder &Builder,
                            const Pattern *P) {
    if (auto *TP = dyn_cast<TuplePattern>(P)) {
      bool NeedComma = false;
      for (unsigned i = 0, end = TP->getNumFields(); i < end; ++i) {
        TuplePatternElt TupleElt = TP->getFields()[i];
        if (NeedComma)
          Builder.addComma();
        NeedComma = true;

        // The last elt must be the vararg if there is one.
        bool IsVarArg = i == end-1 && TP->hasVararg();
        Type EltT = TupleElt.getPattern()->getType();
        if (IsVarArg)
          EltT = TupleTypeElt::getVarargBaseTy(EltT);

        Builder.addCallParameter(TupleElt.getPattern()->getBoundName(),
                                 EltT, IsVarArg);
      }
      return;
    }

    Type PType = P->getType();
    if (auto Parens = dyn_cast<ParenType>(PType.getPointer()))
      PType = Parens->getUnderlyingType();
    Builder.addCallParameter(P->getBoundName(), PType, /*IsVarArg*/false);
  }

  void addPatternFromTypeImpl(CodeCompletionResultBuilder &Builder, Type T,
                              Identifier Label, bool IsTopLevel, bool IsVarArg) {
    if (auto *TT = T->getAs<TupleType>()) {
      if (!Label.empty()) {
        Builder.addTextChunk(Label.str());
        Builder.addTextChunk(": ");
      }
      if (!IsTopLevel || !HaveLParen)
        Builder.addLeftParen();
      else
        Builder.addAnnotatedLeftParen();
      bool NeedComma = false;
      for (auto TupleElt : TT->getFields()) {
        if (NeedComma)
          Builder.addComma();
        Type EltT = TupleElt.isVararg() ? TupleElt.getVarargBaseTy()
                                        : TupleElt.getType();
        addPatternFromTypeImpl(Builder, EltT, TupleElt.getName(), false,
                               TupleElt.isVararg());
        NeedComma = true;
      }
      Builder.addRightParen();
      return;
    }
    if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
      if (IsTopLevel && !HaveLParen)
        Builder.addLeftParen();
      else if (IsTopLevel)
        Builder.addAnnotatedLeftParen();
      Builder.addCallParameter(Identifier(), PT->getUnderlyingType(),
                               /*IsVarArg*/false);
      if (IsTopLevel)
        Builder.addRightParen();
      return;
    }

    if (IsTopLevel && !HaveLParen)
      Builder.addLeftParen();
    else if (IsTopLevel)
      Builder.addAnnotatedLeftParen();

    Builder.addCallParameter(Label, T, IsVarArg);
    if (IsTopLevel)
      Builder.addRightParen();
  }

  void addPatternFromType(CodeCompletionResultBuilder &Builder, Type T) {
    addPatternFromTypeImpl(Builder, T, Identifier(), true, /*isVarArg*/false);
  }

  void addParamPatternFromFunction(CodeCompletionResultBuilder &Builder,
                                   const AnyFunctionType *AFT,
                                   const AbstractFunctionDecl *AFD) {

    const TuplePattern *BodyTuple = nullptr;
    if (AFD) {
      auto BodyPatterns = AFD->getBodyParamPatterns();
      // Skip over the implicit 'self'.
      if (AFD->getImplicitSelfDecl()) {
        BodyPatterns = BodyPatterns.slice(1);
      }

      if (!BodyPatterns.empty())
        BodyTuple = dyn_cast<TuplePattern>(BodyPatterns.front());
    }

    // Do not desugar AFT->getInput(), as we want to treat (_: (a,b)) distinctly
    // from (a,b) for code-completion.
    if (auto *TT = dyn_cast<TupleType>(AFT->getInput().getPointer())) {
      bool NeedComma = false;
      // Iterate over the tuple type fields, corresponding to each parameter.
      for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
        const auto &TupleElt = TT->getFields()[i];
        switch (TupleElt.getDefaultArgKind()) {
        case DefaultArgumentKind::None:
        case DefaultArgumentKind::Normal:
        case DefaultArgumentKind::Inherited:
          break;

        case DefaultArgumentKind::File:
        case DefaultArgumentKind::Line:
        case DefaultArgumentKind::Column:
        case DefaultArgumentKind::Function:
        case DefaultArgumentKind::DSOHandle:
          // Skip parameters that are defaulted to source location or other
          // caller context information.  Users typically don't want to specify
          // these parameters.
          continue;
        }
        auto ParamType = TupleElt.isVararg() ? TupleElt.getVarargBaseTy()
                                             : TupleElt.getType();
        auto Name = TupleElt.getName();

        if (NeedComma)
          Builder.addComma();
        if (BodyTuple) {
          // If we have a local name for the parameter, pass in that as well.
          auto ParamPat = BodyTuple->getFields()[i].getPattern();
          Builder.addCallParameter(Name, ParamPat->getBodyName(), ParamType,
                                   TupleElt.isVararg());
        } else {
          Builder.addCallParameter(Name, ParamType, TupleElt.isVararg());
        }
        NeedComma = true;
      }
    } else {
      // If it's not a tuple, it could be a unary function.
      Type T = AFT->getInput();
      if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
        // Only unwrap the paren sugar, if it exists.
        T = PT->getUnderlyingType();
      }
      if (BodyTuple) {
        auto ParamPat = BodyTuple->getFields().front().getPattern();
        Builder.addCallParameter(Identifier(), ParamPat->getBodyName(), T,
                                 /*IsVarArg*/false);
      } else
        Builder.addCallParameter(Identifier(), T, /*IsVarArg*/false);
    }
  }

  void addFunctionCallPattern(const AnyFunctionType *AFT,
                              const AbstractFunctionDecl *AFD = nullptr) {
    foundFunction(AFT);
    CodeCompletionResultBuilder Builder(
      Sink,
      CodeCompletionResult::ResultKind::Pattern,
      SemanticContextKind::ExpressionSpecific);
    if (!HaveLParen)
      Builder.addLeftParen();
    else
      Builder.addAnnotatedLeftParen();

    addParamPatternFromFunction(Builder, AFT, AFD);

    Builder.addRightParen();
    addTypeAnnotation(Builder, AFT->getResult());
  }

  void addMethodCall(const FuncDecl *FD, DeclVisibilityKind Reason) {
    foundFunction(FD);
    bool IsImplicitlyCurriedInstanceMethod;
    switch (Kind) {
    case LookupKind::ValueExpr:
      IsImplicitlyCurriedInstanceMethod = ExprType->is<AnyMetatypeType>() &&
                                          !FD->isStatic();
      break;
    case LookupKind::ValueInDeclContext:
      IsImplicitlyCurriedInstanceMethod =
          InsideStaticMethod &&
          FD->getDeclContext() == CurrentMethod->getDeclContext() &&
          !FD->isStatic();
      if (!IsImplicitlyCurriedInstanceMethod) {
        if (auto Init = dyn_cast<Initializer>(CurrDeclContext)) {
          IsImplicitlyCurriedInstanceMethod =
              FD->getDeclContext() == Init->getParent() &&
              !FD->isStatic();
        }
      }
      break;
    case LookupKind::EnumElement:
    case LookupKind::Type:
    case LookupKind::TypeInDeclContext:
      llvm_unreachable("can not have a method call while doing a "
                       "type completion");
    case LookupKind::ImportFromModule:
      IsImplicitlyCurriedInstanceMethod = false;
      break;
    }

    StringRef Name = FD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(FD, Reason));
    Builder.setAssociatedDecl(FD);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    if (IsDynamicLookup)
      Builder.addDynamicLookupMethodCallTail();
    else if (FD->getAttrs().hasAttribute<OptionalAttr>())
      Builder.addOptionalMethodCallTail();

    llvm::SmallString<32> TypeStr;

    unsigned FirstIndex = 0;
    if (!IsImplicitlyCurriedInstanceMethod && FD->getImplicitSelfDecl())
      FirstIndex = 1;
    Type FunctionType = getTypeOfMember(FD);

    if (FunctionType->is<ErrorType>()) {
      llvm::raw_svector_ostream OS(TypeStr);
      FunctionType.print(OS);
      Builder.addTypeAnnotation(OS.str());
      return;
    }

    if (FirstIndex != 0)
      FunctionType = FunctionType->castTo<AnyFunctionType>()->getResult();

    Type FirstInputType = FunctionType->castTo<AnyFunctionType>()->getInput();

    if (IsImplicitlyCurriedInstanceMethod) {
      if (auto PT = dyn_cast<ParenType>(FirstInputType.getPointer()))
        FirstInputType = PT->getUnderlyingType();

      Builder.addLeftParen();
      Builder.addCallParameter(Ctx.Id_self, FirstInputType, /*IsVarArg*/false);
      Builder.addRightParen();
    } else {
      Builder.addLeftParen();
      addParamPatternFromFunction(Builder,
                                  FunctionType->castTo<AnyFunctionType>(), FD);
      Builder.addRightParen();
    }

    FunctionType = FunctionType->castTo<AnyFunctionType>()->getResult();

    // Build type annotation.
    {
      llvm::raw_svector_ostream OS(TypeStr);
      for (unsigned i = FirstIndex + 1, e = FD->getBodyParamPatterns().size();
           i != e; ++i) {
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
    foundFunction(CD);
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(CD, Reason));
    Builder.setAssociatedDecl(CD);
     if (IsSuperRefExpr) {
      assert(isa<ConstructorDecl>(CurrDeclContext) &&
             "can call super.init only inside a constructor");
      addLeadingDot(Builder);
      Builder.addTextChunk("init");
    }

    Type MemberType = getTypeOfMember(CD);
    if (MemberType->is<ErrorType>()) {
      addTypeAnnotation(Builder, MemberType);
      return;
    }

    if (!HaveLParen)
      Builder.addLeftParen();
    else
      Builder.addAnnotatedLeftParen();

    Type ConstructorType = MemberType->castTo<AnyFunctionType>()->getResult();
    addParamPatternFromFunction(
        Builder, ConstructorType->castTo<AnyFunctionType>(), CD);

    Builder.addRightParen();
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
      // Values of properties that were found on a AnyObject have
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
    addLeadingDot(Builder);
    Builder.addTextChunk(NTD->getName().str());
    addTypeAnnotation(Builder, NTD->getDeclaredType());
  }

  void addTypeAliasRef(const TypeAliasDecl *TAD, DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(TAD, Reason));
    Builder.setAssociatedDecl(TAD);
    addLeadingDot(Builder);
    Builder.addTextChunk(TAD->getName().str());
    if (TAD->hasUnderlyingType())
      addTypeAnnotation(Builder, TAD->getUnderlyingType());
    else {
      addTypeAnnotation(Builder, TAD->getDeclaredType());
    }
  }

  void addGenericTypeParamRef(const GenericTypeParamDecl *GP,
                              DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(GP, Reason));
    Builder.setAssociatedDecl(GP);
    addLeadingDot(Builder);
    Builder.addTextChunk(GP->getName().str());
    addTypeAnnotation(Builder, GP->getDeclaredType());
  }

  void addAssociatedTypeRef(const AssociatedTypeDecl *AT,
                            DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(AT, Reason));
    Builder.setAssociatedDecl(AT);
    addLeadingDot(Builder);
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
    addLeadingDot(Builder);
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
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.isNull())
      addTypeAnnotation(Builder, TypeAnnotation);
  }

  void addKeyword(StringRef Name, StringRef TypeAnnotation) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
  }

  // Implement swift::VisibleDeclConsumer.
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
    // Hide private stdlib declarations.
    if (D->isPrivateStdlibDecl())
      return;
    if (AvailabilityAttr::isUnavailable(D))
      return;

    if (!D->hasType())
      TypeResolver->resolveDeclSignature(D);
    else if (isa<TypeAliasDecl>(D)) {
      // A TypeAliasDecl might have type set, but not the underlying type.
      TypeResolver->resolveDeclSignature(D);
    }

    switch (Kind) {
    case LookupKind::ValueExpr:
      if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
        if (ExprType->is<AnyMetatypeType>()) {
          if (HaveDot)
            return;
          addConstructorCall(CD, Reason);
        }
        if (IsSuperRefExpr) {
          if (!isa<ConstructorDecl>(CurrDeclContext))
            return;
          addConstructorCall(CD, Reason);
        }
        return;
      }

      if (HaveLParen)
        return;

      if (auto *VD = dyn_cast<VarDecl>(D)) {
        addVarDeclRef(VD, Reason);
        return;
      }

      if (auto *FD = dyn_cast<FuncDecl>(D)) {
        // We can not call operators with a postfix parenthesis syntax.
        if (FD->isBinaryOperator() || FD->isUnaryOperator())
          return;

        // We can not call accessors.  We use VarDecls and SubscriptDecls to
        // produce completions that refer to getters and setters.
        if (FD->isAccessor())
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

      if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
        addEnumElementRef(EED, Reason, /*HasTypeContext=*/false);
      }

      if (HaveDot)
        return;

      if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
        if (ExprType->is<AnyMetatypeType>())
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

        // We can not call accessors.  We use VarDecls and SubscriptDecls to
        // produce completions that refer to getters and setters.
        if (FD->isAccessor())
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
      addLeadingDot(Builder);
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

  bool tryFunctionCallCompletions(Type ExprType, const ValueDecl *VD) {
    ExprType = ExprType->getRValueType();
    if (auto AFT = ExprType->getAs<AnyFunctionType>()) {
      if (auto *AFD = dyn_cast_or_null<AbstractFunctionDecl>(VD)) {
        addFunctionCallPattern(AFT, AFD);
      } else {
        addFunctionCallPattern(AFT);
      }
      return true;
    }
    return false;
  }

  bool tryStdlibOptionalCompletions(Type ExprType) {
    // FIXME: consider types convertible to T?.

    ExprType = ExprType->getRValueType();
    if (Type Unwrapped = ExprType->getOptionalObjectType()) {
      llvm::SaveAndRestore<bool> ChangeNeedOptionalUnwrap(NeedOptionalUnwrap,
                                                          true);
      if (DotLoc.isValid()) {
        NumBytesToEraseForOptionalUnwrap = Ctx.SourceMgr.getByteDistance(
            DotLoc, Ctx.SourceMgr.getCodeCompletionLoc());
      } else {
        NumBytesToEraseForOptionalUnwrap = 0;
      }
      if (NumBytesToEraseForOptionalUnwrap <=
          CodeCompletionResult::MaxNumBytesToErase)
        lookupVisibleMemberDecls(*this, Unwrapped, CurrDeclContext,
                                 TypeResolver.get());
    } else if (Type Unwrapped = ExprType->getImplicitlyUnwrappedOptionalObjectType()) {
      lookupVisibleMemberDecls(*this, Unwrapped, CurrDeclContext,
                               TypeResolver.get());
    } else {
      return false;
    }

    // Ignore the internal members of Optional, like getLogicValue() and
    // getMirror().
    // These are not commonly used and cause noise and confusion when showing
    // among the the members of the underlying type. If someone really wants to
    // use them they can write them directly.

    return true;
  }

  void getValueExprCompletions(Type ExprType, ValueDecl *VD = nullptr) {
    Kind = LookupKind::ValueExpr;
    NeedLeadingDot = !HaveDot;
    this->ExprType = ExprType;
    bool Done = false;
    if (tryFunctionCallCompletions(ExprType, VD))
      Done = true;
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
    if (tryStdlibOptionalCompletions(ExprType))
      Done = true;
    if (!Done) {
      lookupVisibleMemberDecls(*this, ExprType, CurrDeclContext,
                               TypeResolver.get());
    }
  }

  void getValueCompletionsInDeclContext(SourceLoc Loc) {
    Kind = LookupKind::ValueInDeclContext;
    NeedLeadingDot = false;
    lookupVisibleDecls(*this, CurrDeclContext, TypeResolver.get(),
                       /*IncludeTopLevel=*/false, Loc);

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
    Type MetaBase = MetatypeType::get(BaseType);
    lookupVisibleMemberDecls(*this, MetaBase,
                             CurrDeclContext, TypeResolver.get());

    addKeyword("Type", MetaBase);
    addKeyword("self", BaseType);
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
    AccessFilteringDeclConsumer FilteringConsumer(CurrDeclContext, *this,
                                                  TypeResolver.get());
    M->lookupVisibleDecls({}, FilteringConsumer, NLKind::UnqualifiedLookup);
  }

  void getVisibleDeclsOfModule(const Module *TheModule,
                               ArrayRef<std::string> AccessPath,
                               bool ResultsHaveLeadingDot) {
    Kind = LookupKind::ImportFromModule;
    NeedLeadingDot = ResultsHaveLeadingDot;

    llvm::SmallVector<std::pair<Identifier, SourceLoc>, 1> LookupAccessPath;
    for (auto Piece : AccessPath) {
      LookupAccessPath.push_back(
          std::make_pair(Ctx.getIdentifier(Piece), SourceLoc()));
    }
    AccessFilteringDeclConsumer FilteringConsumer(CurrDeclContext, *this,
                                                  TypeResolver.get());
    TheModule->lookupVisibleDecls(LookupAccessPath, FilteringConsumer,
                                  NLKind::UnqualifiedLookup);
  }
};

class CompletionOverrideLookup : public swift::VisibleDeclConsumer {
  CodeCompletionResultSink &Sink;
  OwnedResolver TypeResolver;
  const DeclContext *CurrDeclContext;

public:
  CompletionOverrideLookup(CodeCompletionResultSink &Sink,
                           ASTContext &Ctx,
                           const DeclContext *CurrDeclContext)
      : Sink(Sink),
        TypeResolver(createLazyResolver(Ctx)),
        CurrDeclContext(CurrDeclContext) {}

  void addMethodOverride(const FuncDecl *FD, DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super);
    Builder.setAssociatedDecl(FD);

    class DeclNameOffsetLocatorPrinter : public StreamPrinter {
    public:
      using StreamPrinter::StreamPrinter;

      Optional<unsigned> NameOffset;

      void printDeclLoc(const Decl *D) override {
        if (!NameOffset.hasValue())
          NameOffset = OS.tell();
      }
    };

    llvm::SmallString<256> DeclStr;
    unsigned NameOffset = 0;
    {
      llvm::raw_svector_ostream OS(DeclStr);
      DeclNameOffsetLocatorPrinter Printer(OS);
      PrintOptions Options;
      Options.PrintDefaultParameterPlaceholder = false;
      Options.PrintImplicitAttrs = false;
      Options.ExclusiveAttrList.push_back(DAK_NoReturn);
      Options.PrintOverrideKeyword = false;
      FD->print(Printer, Options);
      NameOffset = Printer.NameOffset.getValue();
    }

    Accessibility AccessibilityOfContext;
    if (auto *NTD = dyn_cast<NominalTypeDecl>(CurrDeclContext))
      AccessibilityOfContext = NTD->getAccessibility();
    else
      AccessibilityOfContext = cast<ExtensionDecl>(CurrDeclContext)
                                   ->getExtendedType()
                                   ->getAnyNominal()
                                   ->getAccessibility();
    Builder.addAccessControlKeyword(std::min(
        FD->getAccessibility(), AccessibilityOfContext));

    if (Reason == DeclVisibilityKind::MemberOfSuper)
      Builder.addOverrideKeyword();
    Builder.addDeclIntroducer(DeclStr.str().substr(0, NameOffset));
    Builder.addTextChunk(DeclStr.str().substr(NameOffset));
    Builder.addBraceStmtWithCursor();
  }

  void addConstructor(const ConstructorDecl *CD) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super);
    Builder.setAssociatedDecl(CD);

    llvm::SmallString<256> DeclStr;
    {
      llvm::raw_svector_ostream OS(DeclStr);
      PrintOptions Options;
      Options.PrintImplicitAttrs = false;
      Options.ExclusiveAttrList.push_back(DAK_NoReturn);
      Options.PrintDefaultParameterPlaceholder = false;
      CD->print(OS, Options);
    }
    Builder.addTextChunk(DeclStr);
    Builder.addBraceStmtWithCursor();
  }

  // Implement swift::VisibleDeclConsumer.
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
    if (Reason == DeclVisibilityKind::MemberOfCurrentNominal)
      return;

    if (D->getAttrs().hasAttribute<FinalAttr>())
      return;

    if (!D->hasType())
      TypeResolver->resolveDeclSignature(D);

    if (auto *FD = dyn_cast<FuncDecl>(D)) {
      // We can override operators as members.
      if (FD->isBinaryOperator() || FD->isUnaryOperator())
        return;

      // We can not override individual accessors.
      if (FD->isAccessor())
        return;

      addMethodOverride(FD, Reason);
      return;
    }

    if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
      if (!isa<ProtocolDecl>(CD->getDeclContext()))
        return;
      if (CD->isRequired() || CD->isDesignatedInit())
        addConstructor(CD);
      return;
    }
  }

  void addDesignatedInitializers(Type CurrTy) {
    if (!CurrTy)
      return;
    const auto *NTD = CurrTy->getAnyNominal();
    if (!NTD)
      return;
    const auto *CD = dyn_cast<ClassDecl>(NTD);
    if (!CD)
      return;
    if (!CD->getSuperclass())
      return;
    CD = CD->getSuperclass()->getClassOrBoundGenericClass();
    for (const auto *Member : CD->getMembers()) {
      const auto *Constructor = dyn_cast<ConstructorDecl>(Member);
      if (!Constructor)
        continue;
      if (Constructor->hasStubImplementation())
        continue;
      if (Constructor->isDesignatedInit())
        addConstructor(Constructor);
    }
  }

  void getOverrideCompletions(SourceLoc Loc) {
    auto TypeContext = CurrDeclContext->getInnermostTypeContext();
    if (!TypeContext)
      return;
    Type CurrTy = TypeContext->getDeclaredTypeInContext();
    lookupVisibleMemberDecls(*this, CurrTy, CurrDeclContext,
                             TypeResolver.get());
    addDesignatedInitializers(CurrTy);
  }
};

} // end unnamed namespace

void CodeCompletionCallbacksImpl::completeDotExpr(Expr *E, SourceLoc DotLoc) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::DotExpr;
  ParsedExpr = E;
  this->DotLoc = DotLoc;
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

void CodeCompletionCallbacksImpl::completePostfixExprParen(Expr *E) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::PostfixExprParen;
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

void CodeCompletionCallbacksImpl::completeNominalMemberBeginning() {
  assert(!InEnumElementRawValue);

  Kind = CompletionKind::NominalMemberBeginning;
  CurDeclContext = P.CurDeclContext;
}

static bool isDynamicLookup(Type T) {
  if (auto *PT = T->getRValueType()->getAs<ProtocolType>())
    return PT->getDecl()->isSpecificProtocol(KnownProtocolKind::AnyObject);
  return false;
}

static bool isClangSubModule(Module *TheModule) {
  if (auto ClangMod = TheModule->findUnderlyingClangModule())
    return ClangMod->isSubModule();
  return false;
}

static void addDeclKeywords(CodeCompletionResultSink &Sink) {
  auto AddKeyword = [&](StringRef Name) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None);
    Builder.addTextChunk(Name);
  };

#define DECL_KEYWORD(kw) AddKeyword(#kw);
#include "swift/Parse/Tokens.def"

  // Context-sensitive keywords.
  AddKeyword("weak");
  AddKeyword("unowned");
  AddKeyword("optional");
  AddKeyword("required");
  AddKeyword("lazy");
  AddKeyword("final");
  AddKeyword("dynamic");
  AddKeyword("prefix");
  AddKeyword("postfix");
  AddKeyword("infix");
  AddKeyword("override");
  AddKeyword("mutating");
  AddKeyword("nonmutating");
  AddKeyword("convenience");
}

static void addStmtKeywords(CodeCompletionResultSink &Sink) {
  auto AddKeyword = [&](StringRef Name, StringRef TypeAnnotation) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
  };

#define STMT_KEYWORD(kw) AddKeyword(#kw, StringRef());
#include "swift/Parse/Tokens.def"

  // FIXME: The pedantically correct way to find the type is to resolve the
  // Swift.StringLiteralType type.
  AddKeyword("__FUNCTION__", "String");
  AddKeyword("__FILE__", "String");
  // Same: Swift.IntegerLiteralType.
  AddKeyword("__LINE__", "Int");
  AddKeyword("__COLUMN__", "Int");
  // Same: Swift.BooleanLiteralType.
  AddKeyword("false", "Bool");
  AddKeyword("true", "Bool");

  AddKeyword("__DSO_HANDLE__", "UnsafeMutablePointer<Void>");

  AddKeyword("nil", StringRef());
}

void CodeCompletionCallbacksImpl::addKeywords(CodeCompletionResultSink &Sink) {
  switch (Kind) {
  case CompletionKind::None:
  case CompletionKind::DotExpr:
    break;

  case CompletionKind::PostfixExprBeginning:
    addDeclKeywords(Sink);
    addStmtKeywords(Sink);
    break;

  case CompletionKind::PostfixExpr:
  case CompletionKind::PostfixExprParen:
  case CompletionKind::SuperExpr:
  case CompletionKind::SuperExprDot:
  case CompletionKind::TypeSimpleBeginning:
  case CompletionKind::TypeIdentifierWithDot:
  case CompletionKind::TypeIdentifierWithoutDot:
  case CompletionKind::CaseStmtBeginning:
  case CompletionKind::CaseStmtDotPrefix:
    break;

  case CompletionKind::NominalMemberBeginning:
    addDeclKeywords(Sink);
    break;
  }
}

void CodeCompletionCallbacksImpl::doneParsing() {
  if (Kind == CompletionKind::None) {
    return;
  }

  // Add keywords even if type checking fails completely.
  addKeywords(CompletionContext.getResultSink());

  if (!typecheckContext())
    return;

  if (DelayedParsedDecl && !typecheckDelayedParsedDecl())
    return;

  if (auto *AFD = dyn_cast_or_null<AbstractFunctionDecl>(DelayedParsedDecl))
    CurDeclContext = AFD;

  if (ParsedExpr && !typecheckParsedExpr()) {
    if (Kind != CompletionKind::PostfixExprParen)
      return;
  }

  if (!ParsedTypeLoc.isNull() && !typecheckParsedType())
    return;

  CompletionLookup Lookup(CompletionContext.getResultSink(), P.Context,
                          CurDeclContext);

  auto DoPostfixExprBeginning = [&] {
    if (CStyleForLoopIterationVariable)
      Lookup.addExpressionSpecificDecl(CStyleForLoopIterationVariable);
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    Lookup.getValueCompletionsInDeclContext(Loc);
  };

  switch (Kind) {
  case CompletionKind::None:
    llvm_unreachable("should be already handled");
    return;

  case CompletionKind::DotExpr: {
    Lookup.setHaveDot(DotLoc);
    Type ExprType = ParsedExpr->getType();
    if (isDynamicLookup(ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(ExprType);
    break;
  }

  case CompletionKind::PostfixExprBeginning: {
    DoPostfixExprBeginning();
    break;
  }

  case CompletionKind::PostfixExpr: {
    Type ExprType = ParsedExpr->getType();
    if (isDynamicLookup(ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(ExprType);
    break;
  }

  case CompletionKind::PostfixExprParen: {
    Lookup.setHaveLParen(true);
    ValueDecl *VD = nullptr;
    if (auto *AE = dyn_cast<ApplyExpr>(ParsedExpr)) {
      if (auto *DRE = dyn_cast<DeclRefExpr>(AE->getFn()))
        VD = DRE->getDecl();
    }
    if (ParsedExpr->getType())
      Lookup.getValueExprCompletions(ParsedExpr->getType(), VD);

    if (!Lookup.FoundFunctionCalls ||
        (Lookup.FoundFunctionCalls &&
         Lookup.FoundFunctionsWithoutFirstKeyword)) {
      Lookup.setHaveLParen(false);
      DoPostfixExprBeginning();
    }
    break;
  }

  case CompletionKind::SuperExpr: {
    Lookup.setIsSuperRefExpr();
    Lookup.getValueExprCompletions(ParsedExpr->getType());
    break;
  }

  case CompletionKind::SuperExprDot: {
    Lookup.setIsSuperRefExpr();
    Lookup.setHaveDot(SourceLoc());
    Lookup.getValueExprCompletions(ParsedExpr->getType());
    break;
  }

  case CompletionKind::TypeSimpleBeginning: {
    Lookup.getTypeCompletionsInDeclContext(
        P.Context.SourceMgr.getCodeCompletionLoc());
    break;
  }

  case CompletionKind::TypeIdentifierWithDot: {
    Lookup.setHaveDot(SourceLoc());
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
    Lookup.setHaveDot(SourceLoc());
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    Lookup.getTypeContextEnumElementCompletions(Loc);
    break;
  }

  case CompletionKind::NominalMemberBeginning: {
    Lookup.discardTypeResolver();
    CompletionOverrideLookup OverrideLookup(CompletionContext.getResultSink(),
                                            P.Context, CurDeclContext);
    OverrideLookup.getOverrideCompletions(SourceLoc());
    break;
  }
  }

  if (Lookup.RequestedCachedResults) {
    // Create helpers for result caching.
    auto &SwiftContext = P.Context;
    auto FillCacheCallback =
        [&SwiftContext](CodeCompletionCacheImpl &Cache,
                        const CodeCompletionCacheImpl::Key &K,
                        const Module *TheModule) {
      auto V = Cache.getResultSinkFor(K);
      CompletionLookup Lookup(V->Sink, SwiftContext, nullptr);
      Lookup.getVisibleDeclsOfModule(TheModule, K.AccessPath,
                                     K.ResultsHaveLeadingDot);
      Cache.storeResults(K, V);
      return V;
    };

    auto &Request = Lookup.RequestedCachedResults.getValue();

    llvm::DenseSet<CodeCompletionCacheImpl::Key> ImportsSeen;
    auto handleImport = [&](Module::ImportedModule Import) {
      Module *TheModule = Import.second;
      Module::AccessPathTy Path = Import.first;
      if (TheModule->getFiles().empty())
        return;

      // Clang submodules are ignored and there's no lookup cost involved,
      // so just ignore them and don't put the empty results in the cache
      // because putting a lot of objects in the cache will push out
      // other lookups.
      if (isClangSubModule(TheModule))
        return;

      std::vector<std::string> AccessPath;
      for (auto Piece : Path) {
        AccessPath.push_back(Piece.first.str());
      }

      StringRef ModuleFilename = TheModule->getModuleFilename();
      // ModuleFilename can be empty if something strange happened during
      // module loading, for example, the module file is corrupted.
      if (!ModuleFilename.empty()) {
        CodeCompletionCacheImpl::Key K{ModuleFilename,
                                       TheModule->Name.str(),
                                       AccessPath, Request.NeedLeadingDot};
        std::pair<decltype(ImportsSeen)::iterator, bool>
        Result = ImportsSeen.insert(K);
        if (!Result.second)
          return; // already handled.

        CompletionContext.Cache.Impl->getResults(
            K, CompletionContext.getResultSink(), Request.OnlyTypes,
            TheModule, FillCacheCallback);
      }
    };

    if (Request.TheModule) {
      Lookup.discardTypeResolver();

      // FIXME: actually check imports.
      const_cast<Module*>(Request.TheModule)
          ->forAllVisibleModules({}, handleImport);
    } else {
      // Add results from current module.
      Lookup.getToplevelCompletions(Request.OnlyTypes);
      Lookup.discardTypeResolver();

      // Add results for all imported modules.
      auto *SF = CurDeclContext->getParentSourceFile();
      for (std::pair<Module::ImportedModule, bool> Imported :
               SF->getImports()) {
        Module *TheModule = Imported.first.second;
        Module::AccessPathTy AccessPath = Imported.first.first;
        TheModule->forAllVisibleModules(AccessPath, handleImport);
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
  unsigned NumResults = 0;
  for (auto Result : Results) {
    if (!IncludeKeywords && Result->getKind() == CodeCompletionResult::Keyword)
      continue;
    NumResults++;
  }
  if (NumResults == 0)
    return;

  OS << "Begin completions, " << NumResults << " items\n";
  for (auto Result : Results) {
    if (!IncludeKeywords && Result->getKind() == CodeCompletionResult::Keyword)
      continue;
    Result->print(OS);

    llvm::SmallString<64> Name;
    llvm::raw_svector_ostream NameOs(Name);
    Result->getCompletionString()->getName(NameOs);
    NameOs.flush();
    OS << "; name=" << Name;

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

