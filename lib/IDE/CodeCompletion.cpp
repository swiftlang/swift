//===--- CodeCompletion.cpp - Code completion implementation --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletion.h"
#include "CodeCompletionResultBuilder.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Comment.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IDE/CodeCompletionCache.h"
#include "swift/IDE/Utils.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Comment.h"
#include "clang/AST/CommentVisitor.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <string>

using namespace swift;
using namespace ide;

typedef std::vector<std::pair<StringRef, StringRef>> CommandWordsPairs;

enum CodeCompletionCommandKind {
  none,
  keyword,
  recommended,
  recommendedover,
  mutatingvariant,
  nonmutatingvariant,
};

CodeCompletionCommandKind getCommandKind(StringRef Command) {
#define CHECK_CASE(KIND)                                                      \
  if (Command == #KIND)                                                       \
    return CodeCompletionCommandKind::KIND;
  CHECK_CASE(keyword);
  CHECK_CASE(recommended);
  CHECK_CASE(recommendedover);
  CHECK_CASE(mutatingvariant);
  CHECK_CASE(nonmutatingvariant);
#undef CHECK_CASE
  return CodeCompletionCommandKind::none;
}

StringRef getCommandName(CodeCompletionCommandKind Kind) {
#define CHECK_CASE(KIND)                                                    \
  if (CodeCompletionCommandKind::KIND == Kind) {                            \
    static std::string Name(#KIND);                                         \
    return Name;                                                            \
  }
  CHECK_CASE(keyword)
  CHECK_CASE(recommended)
  CHECK_CASE(recommendedover)
  CHECK_CASE(mutatingvariant);
  CHECK_CASE(nonmutatingvariant);
#undef CHECK_CASE
  llvm_unreachable("Cannot handle this Kind.");
}

bool containsInterestedWords(StringRef Content, StringRef Splitter,
                             bool AllowWhitespace) {
  do {
    Content = Content.split(Splitter).second;
    Content = AllowWhitespace ? Content.trim() : Content;
#define CHECK_CASE(KIND)                                                       \
if (Content.startswith(#KIND))                                                 \
return true;
    CHECK_CASE(keyword)
    CHECK_CASE(recommended)
    CHECK_CASE(recommendedover)
    CHECK_CASE(mutatingvariant);
    CHECK_CASE(nonmutatingvariant);
#undef CHECK_CASE
  } while (!Content.empty());
  return false;
}

void splitTextByComma(StringRef Text, std::vector<StringRef>& Subs) {
  do {
    auto Pair = Text.split(',');
    auto Key = Pair.first.trim();
    if (!Key.empty())
      Subs.push_back(Key);
    Text = Pair.second;
  } while (!Text.empty());
}

namespace clang {
namespace comments {
class WordPairsArrangedViewer {
  ArrayRef<std::pair<StringRef, StringRef>> Content;
  std::vector<StringRef> ViewedText;
  std::vector<StringRef> Words;
  StringRef Key;

  bool isKeyViewed(StringRef K) {
    return std::find(ViewedText.begin(), ViewedText.end(), K) != ViewedText.end();
  }

public:
  WordPairsArrangedViewer(ArrayRef<std::pair<StringRef, StringRef>> Content):
    Content(Content) {}

  bool hasNext() {
    Words.clear();
    bool Found = false;
    for (auto P : Content) {
      if (!Found && !isKeyViewed(P.first)) {
        Key = P.first;
        Found = true;
      }
      if (Found && P.first == Key)
        Words.push_back(P.second);
    }
    return Found;
  }

  std::pair<StringRef, ArrayRef<StringRef>> next() {
    bool HasNext = hasNext();
    (void) HasNext;
    assert(HasNext && "Have no more data.");
    ViewedText.push_back(Key);
    return std::make_pair(Key, llvm::makeArrayRef(Words));
  }
};

class ClangCommentExtractor : public ConstCommentVisitor<ClangCommentExtractor> {
  CommandWordsPairs &Words;
  const CommandTraits &Traits;
  std::vector<const Comment *> Parents;

  void visitChildren(const Comment* C) {
    Parents.push_back(C);
    for (auto It = C->child_begin(); It != C->child_end(); ++ It)
      visit(*It);
    Parents.pop_back();
  }

public:
  ClangCommentExtractor(CommandWordsPairs &Words,
                        const CommandTraits &Traits) : Words(Words),
                                                       Traits(Traits) {}
#define CHILD_VISIT(NAME) \
  void visit##NAME(const NAME *C) {\
    visitChildren(C);\
  }
  CHILD_VISIT(FullComment)
  CHILD_VISIT(ParagraphComment)
#undef CHILD_VISIT

  void visitInlineCommandComment(const InlineCommandComment *C) {
    auto Command = C->getCommandName(Traits);
    auto CommandKind = getCommandKind(Command);
    if (CommandKind == CodeCompletionCommandKind::none)
      return;
    auto &Parent = Parents.back();
    for (auto CIT = std::find(Parent->child_begin(), Parent->child_end(), C) + 1;
         CIT != Parent->child_end(); CIT++) {
      if (auto TC = dyn_cast<TextComment>(*CIT)) {
        auto Text = TC->getText();
        std::vector<StringRef> Subs;
        splitTextByComma(Text, Subs);
        auto Kind = getCommandName(CommandKind);
        for (auto S : Subs)
          Words.push_back(std::make_pair(Kind, S));
      } else
        break;
    }
  }
};

void getClangDocKeyword(ClangImporter &Importer, const Decl *D,
                        CommandWordsPairs &Words) {
  ClangCommentExtractor Extractor(Words, Importer.getClangASTContext().
    getCommentCommandTraits());
  if (auto RC = Importer.getClangASTContext().getRawCommentForAnyRedecl(D)) {
    auto RT = RC->getRawText(Importer.getClangASTContext().getSourceManager());
    if (containsInterestedWords(RT, "@", /*AllowWhitespace*/false)) {
      FullComment* Comment = Importer.getClangASTContext().
        getLocalCommentForDeclUncached(D);
      Extractor.visit(Comment);
    }
  }
}
} // end namespace comments
} // end namespace clang

namespace swift {
namespace markup {
class SwiftDocWordExtractor : public MarkupASTWalker {
  CommandWordsPairs &Pairs;
  CodeCompletionCommandKind Kind;
public:
  SwiftDocWordExtractor(CommandWordsPairs &Pairs) :
    Pairs(Pairs), Kind(CodeCompletionCommandKind::none) {}
  void visitKeywordField(const KeywordField *Field) override {
    Kind = CodeCompletionCommandKind::keyword;
  }
  void visitRecommendedField(const RecommendedField *Field) override {
    Kind = CodeCompletionCommandKind::recommended;
  }
  void visitRecommendedoverField(const RecommendedoverField *Field) override {
    Kind = CodeCompletionCommandKind::recommendedover;
  }
  void visitMutatingvariantField(const MutatingvariantField *Field) override {
    Kind = CodeCompletionCommandKind::mutatingvariant;
  }
  void visitNonmutatingvariantField(const NonmutatingvariantField *Field) override {
    Kind = CodeCompletionCommandKind::nonmutatingvariant;
  }
  void visitText(const Text *Text) override {
    if (Kind == CodeCompletionCommandKind::none)
      return;
    StringRef CommandName = getCommandName(Kind);
    std::vector<StringRef> Subs;
    splitTextByComma(Text->str(), Subs);
    for (auto S : Subs)
      Pairs.push_back(std::make_pair(CommandName, S));
  }
};

void getSwiftDocKeyword(const Decl* D, CommandWordsPairs &Words) {
  auto Interested = false;
  for (auto C : D->getRawComment().Comments) {
    if (containsInterestedWords(C.RawText, "-", /*AllowWhitespace*/true)) {
      Interested = true;
      break;
    }
  }
  if (!Interested)
    return;
  static swift::markup::MarkupContext MC;
  auto DC = getSingleDocComment(MC, D);
  if (!DC.hasValue())
    return;
  SwiftDocWordExtractor Extractor(Words);
  for (auto Part : DC.getValue()->getBodyNodes()) {
    switch (Part->getKind()) {
      case ASTNodeKind::KeywordField:
      case ASTNodeKind::RecommendedField:
      case ASTNodeKind::RecommendedoverField:
      case ASTNodeKind::MutatingvariantField:
      case ASTNodeKind::NonmutatingvariantField:
        Extractor.walk(Part);
        break;
      default:
        break;
    }
  }
}
} // end namespace markup
} // end namespace swift

static bool shouldHideDeclFromCompletionResults(const ValueDecl *D) {
  // Hide private stdlib declarations.
  if (D->isPrivateStdlibDecl(/*whitelistProtocols*/false) ||
      // ShowInInterfaceAttr is for decls to show in interface as exception but
      // they are not intended to be used directly.
      D->getAttrs().hasAttribute<ShowInInterfaceAttr>())
    return true;

  if (AvailableAttr::isUnavailable(D))
    return true;

  if (auto *ClangD = D->getClangDecl()) {
    if (ClangD->hasAttr<clang::SwiftPrivateAttr>())
      return true;
  }

  // Hide editor placeholders.
  if (D->getName().isEditorPlaceholder())
    return true;

  return false;
}

typedef llvm::function_ref<bool(ValueDecl*, DeclVisibilityKind)> DeclFilter;
DeclFilter DefaultFilter = [] (ValueDecl* VD, DeclVisibilityKind Kind) {return true;};
DeclFilter KeyPathFilter = [](ValueDecl* decl, DeclVisibilityKind) -> bool {
  return isa<TypeDecl>(decl) ||
         (isa<VarDecl>(decl) && decl->getDeclContext()->isTypeContext());
};

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
      } while (Ptr < End && *Ptr != '#');
      if (Ptr == End)
        break;
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
    return { SM.rangeContainsTokenLoc(S->getSourceRange(), Loc), S };
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
} // end anonymous namespace

static Stmt *findNearestStmt(const AbstractFunctionDecl *AFD, SourceLoc Loc,
                             StmtKind Kind) {
  auto &SM = AFD->getASTContext().SourceMgr;
  assert(SM.rangeContainsTokenLoc(AFD->getSourceRange(), Loc));
  StmtFinder Finder(SM, Loc, Kind);
  // FIXME(thread-safety): the walker is mutating the AST.
  const_cast<AbstractFunctionDecl *>(AFD)->walk(Finder);
  return Finder.getFoundStmt();
}

/// Erase any ErrorType types on the given expression, allowing later
/// type-checking to make progress.
///
/// FIXME: this is fundamentally a workaround for the fact that we may end up
/// typechecking parts of an expression more than once - first for checking
/// the context, and later for checking more-specific things like unresolved
/// members.  We should restructure code-completion type-checking so that we
/// never typecheck more than once (or find a more principled way to do it).
static void eraseErrorTypes(Expr *E) {
  assert(E);
  struct Eraser : public ASTWalker {
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (expr && expr->getType() && expr->getType()->is<ErrorType>())
        expr->setType(Type());
      return { true, expr };
    }
    bool walkToTypeLocPre(TypeLoc &TL) override {
      if (TL.getType() && TL.getType()->is<ErrorType>())
        TL.setType(Type(), /*was validated*/false);
      return true;
    }

    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      if (P && P->hasType() && P->getType()->is<ErrorType>()) {
        P->setType(Type());
      }
      return { true, P };
    }
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
  };

  E->walk(Eraser());
}

CodeCompletionString::CodeCompletionString(ArrayRef<Chunk> Chunks) {
  std::uninitialized_copy(Chunks.begin(), Chunks.end(),
                          getTrailingObjects<Chunk>());
  NumChunks = Chunks.size();
}

CodeCompletionString *CodeCompletionString::create(llvm::BumpPtrAllocator &Allocator,
                                                   ArrayRef<Chunk> Chunks) {
  void *CCSMem = Allocator.Allocate(totalSizeToAlloc<Chunk>(Chunks.size()),
                                    alignof(CodeCompletionString));
  return new (CCSMem) CodeCompletionString(Chunks);
}

void CodeCompletionString::print(raw_ostream &OS) const {
  unsigned PrevNestingLevel = 0;
  for (auto C : getChunks()) {
    bool AnnotatedTextChunk = false;
    if (C.getNestingLevel() < PrevNestingLevel) {
      OS << "#}";
    }
    switch (C.getKind()) {
    using ChunkKind = Chunk::ChunkKind;
    case ChunkKind::AccessControlKeyword:
    case ChunkKind::DeclAttrKeyword:
    case ChunkKind::DeclAttrParamKeyword:
    case ChunkKind::OverrideKeyword:
    case ChunkKind::ThrowsKeyword:
    case ChunkKind::RethrowsKeyword:
    case ChunkKind::DeclIntroducer:
    case ChunkKind::Text:
    case ChunkKind::LeftParen:
    case ChunkKind::RightParen:
    case ChunkKind::LeftBracket:
    case ChunkKind::RightBracket:
    case ChunkKind::LeftAngle:
    case ChunkKind::RightAngle:
    case ChunkKind::Dot:
    case ChunkKind::Ellipsis:
    case ChunkKind::Comma:
    case ChunkKind::ExclamationMark:
    case ChunkKind::QuestionMark:
    case ChunkKind::Ampersand:
    case ChunkKind::Equal:
    case ChunkKind::Whitespace:
      AnnotatedTextChunk = C.isAnnotation();
      SWIFT_FALLTHROUGH;
    case ChunkKind::CallParameterName:
    case ChunkKind::CallParameterInternalName:
    case ChunkKind::CallParameterColon:
    case ChunkKind::DeclAttrParamEqual:
    case ChunkKind::CallParameterType:
    case ChunkKind::CallParameterClosureType:
    case ChunkKind::GenericParameterName:
      if (AnnotatedTextChunk)
        OS << "['";
      else if (C.getKind() == ChunkKind::CallParameterInternalName)
        OS << "(";
      else if (C.getKind() == ChunkKind::CallParameterClosureType)
        OS << "##";
      for (char Ch : C.getText()) {
        if (Ch == '\n')
          OS << "\\n";
        else
          OS << Ch;
      }
      if (AnnotatedTextChunk)
        OS << "']";
      else if (C.getKind() == ChunkKind::CallParameterInternalName)
        OS << ")";
      break;
    case ChunkKind::OptionalBegin:
    case ChunkKind::CallParameterBegin:
    case ChunkKind::GenericParameterBegin:
      OS << "{#";
      break;
    case ChunkKind::DynamicLookupMethodCallTail:
    case ChunkKind::OptionalMethodCallTail:
      OS << C.getText();
      break;
    case ChunkKind::TypeAnnotation:
      OS << "[#";
      OS << C.getText();
      OS << "#]";
      break;
    case ChunkKind::BraceStmtWithCursor:
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
  case DeclKind::IfConfig:
    llvm_unreachable("not expecting such a declaration result");
  case DeclKind::Module:
    return CodeCompletionDeclKind::Module;
  case DeclKind::TypeAlias:
    return CodeCompletionDeclKind::TypeAlias;
  case DeclKind::AssociatedType:
    return CodeCompletionDeclKind::AssociatedType;
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
    if (FD->isOperator()) {
      if (auto op = FD->getOperatorDecl()) {
        switch (op->getKind()) {
        case DeclKind::PrefixOperator:
          return CodeCompletionDeclKind::PrefixOperatorFunction;
        case DeclKind::PostfixOperator:
          return CodeCompletionDeclKind::PostfixOperatorFunction;
        case DeclKind::InfixOperator:
          return CodeCompletionDeclKind::InfixOperatorFunction;
        default:
          llvm_unreachable("unexpected operator kind");
        }
      } else {
        return CodeCompletionDeclKind::InfixOperatorFunction;
      }
    }
    return CodeCompletionDeclKind::FreeFunction;
  }
  case DeclKind::InfixOperator:
    return CodeCompletionDeclKind::InfixOperatorFunction;
  case DeclKind::PrefixOperator:
    return CodeCompletionDeclKind::PrefixOperatorFunction;
  case DeclKind::PostfixOperator:
    return CodeCompletionDeclKind::PostfixOperatorFunction;
  case DeclKind::PrecedenceGroup:
    return CodeCompletionDeclKind::PrecedenceGroup;
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
    case CodeCompletionDeclKind::AssociatedType:
      Prefix.append("[AssociatedType]");
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
    case CodeCompletionDeclKind::PrefixOperatorFunction:
      Prefix.append("[PrefixOperatorFunction]");
      break;
    case CodeCompletionDeclKind::PostfixOperatorFunction:
      Prefix.append("[PostfixOperatorFunction]");
      break;
    case CodeCompletionDeclKind::InfixOperatorFunction:
      Prefix.append("[InfixOperatorFunction]");
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
    case CodeCompletionDeclKind::Module:
      Prefix.append("[Module]");
      break;
    case CodeCompletionDeclKind::PrecedenceGroup:
      Prefix.append("[PrecedenceGroup]");
      break;
    }
    break;
  case ResultKind::Keyword:
    Prefix.append("Keyword");
    switch (getKeywordKind()) {
    case CodeCompletionKeywordKind::None:
      break;
#define KEYWORD(X) case CodeCompletionKeywordKind::kw_##X: \
      Prefix.append("[" #X "]"); \
      break;
#define POUND_KEYWORD(X) case CodeCompletionKeywordKind::pound_##X: \
      Prefix.append("[#" #X "]"); \
      break;
#include "swift/Parse/Tokens.def"
    }
    break;
  case ResultKind::Pattern:
    Prefix.append("Pattern");
    break;
  case ResultKind::Literal:
    Prefix.append("Literal");
    switch (getLiteralKind()) {
    case CodeCompletionLiteralKind::ArrayLiteral:
      Prefix.append("[Array]");
      break;
    case CodeCompletionLiteralKind::BooleanLiteral:
      Prefix.append("[Boolean]");
      break;
    case CodeCompletionLiteralKind::ColorLiteral:
      Prefix.append("[_Color]");
      break;
    case CodeCompletionLiteralKind::ImageLiteral:
      Prefix.append("[_Image]");
      break;
    case CodeCompletionLiteralKind::DictionaryLiteral:
      Prefix.append("[Dictionary]");
      break;
    case CodeCompletionLiteralKind::IntegerLiteral:
      Prefix.append("[Integer]");
      break;
    case CodeCompletionLiteralKind::NilLiteral:
      Prefix.append("[Nil]");
      break;
    case CodeCompletionLiteralKind::StringLiteral:
      Prefix.append("[String]");
      break;
    case CodeCompletionLiteralKind::Tuple:
      Prefix.append("[Tuple]");
      break;
    }
    break;
  case ResultKind::BuiltinOperator:
    Prefix.append("BuiltinOperator");
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
    if (!ModuleName.empty())
      Prefix.append((Twine("[") + ModuleName + "]").str());
    break;
  }
  if (NotRecommended)
    Prefix.append("/NotRecommended");
  if (NumBytesToErase != 0) {
    Prefix.append("/Erase[");
    Prefix.append(Twine(NumBytesToErase).str());
    Prefix.append("]");
  }
  switch (TypeDistance) {
    case ExpectedTypeRelation::Invalid:
      Prefix.append("/TypeRelation[Invalid]");
      break;
    case ExpectedTypeRelation::Identical:
      Prefix.append("/TypeRelation[Identical]");
      break;
    case ExpectedTypeRelation::Convertible:
      Prefix.append("/TypeRelation[Convertible]");
      break;
    case ExpectedTypeRelation::Unrelated:
      break;
  }

  for (clang::comments::WordPairsArrangedViewer Viewer(DocWords);
       Viewer.hasNext();) {
    auto Pair = Viewer.next();
    Prefix.append("/");
    Prefix.append(Pair.first);
    Prefix.append("[");
    StringRef Sep = ", ";
    for (auto KW : Pair.second) {
      Prefix.append(KW);
      Prefix.append(Sep);
    }
    for (unsigned I = 0, N = Sep.size(); I < N; ++I)
      Prefix.pop_back();
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

static ArrayRef<std::pair<StringRef, StringRef>> copyStringPairArray(
    llvm::BumpPtrAllocator &Allocator,
    ArrayRef<std::pair<StringRef, StringRef>> Arr) {
  std::pair<StringRef, StringRef> *Buff = Allocator.Allocate<std::pair<StringRef,
    StringRef>>(Arr.size());
  std::copy(Arr.begin(), Arr.end(), Buff);
  return llvm::makeArrayRef(Buff, Arr.size());
}

void CodeCompletionResultBuilder::addChunkWithText(
    CodeCompletionString::Chunk::ChunkKind Kind, StringRef Text) {
  addChunkWithTextNoCopy(Kind, copyString(*Sink.Allocator, Text));
}

void CodeCompletionResultBuilder::setAssociatedDecl(const Decl *D) {
  assert(Kind == CodeCompletionResult::ResultKind::Declaration);
  AssociatedDecl = D;

  if (auto *ClangD = D->getClangDecl())
    CurrentModule = ClangD->getImportedOwningModule();
  // FIXME: macros
  // FIXME: imported header module

  if (!CurrentModule)
    CurrentModule = D->getModuleContext();

  if (D->getAttrs().getDeprecated(D->getASTContext()))
    setNotRecommended(CodeCompletionResult::Deprecated);
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
  if (isa<ModuleDecl>(VD))
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

static CodeCompletionResult::ExpectedTypeRelation calculateTypeRelation(
                                                                Type Ty,
                                                                Type ExpectedTy,
                                                                DeclContext *DC) {
  if (Ty.isNull() || ExpectedTy.isNull() ||
      Ty->getKind() == TypeKind::Error ||
      ExpectedTy->getKind() == TypeKind::Error)
    return CodeCompletionResult::ExpectedTypeRelation::Unrelated;
  if (Ty.getCanonicalTypeOrNull() == ExpectedTy.getCanonicalTypeOrNull())
    return CodeCompletionResult::ExpectedTypeRelation::Identical;
  if (isConvertibleTo(Ty, ExpectedTy, *DC))
    return CodeCompletionResult::ExpectedTypeRelation::Convertible;
  if (auto FT = Ty->getAs<AnyFunctionType>()) {
    if (FT->getResult()->isVoid())
      return CodeCompletionResult::ExpectedTypeRelation::Invalid;
    return std::max(calculateTypeRelation(FT->getResult(), ExpectedTy, DC),
                    CodeCompletionResult::ExpectedTypeRelation::Unrelated);
  }
  return CodeCompletionResult::ExpectedTypeRelation::Unrelated;
}

static CodeCompletionResult::ExpectedTypeRelation calculateTypeRelationForDecl (
                                                            const Decl *D,
                                                            Type ExpectedType) {
  auto VD = dyn_cast<ValueDecl>(D);
  auto DC = D->getDeclContext();
  if (!VD)
    return CodeCompletionResult::ExpectedTypeRelation::Unrelated;
  if (auto FD = dyn_cast<FuncDecl>(VD)) {
    return std::max(calculateTypeRelation(FD->getType(), ExpectedType, DC),
                    calculateTypeRelation(FD->getResultType(), ExpectedType, DC));
  }
  if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
    return std::max(calculateTypeRelation(NTD->getType(), ExpectedType, DC),
                    calculateTypeRelation(NTD->getDeclaredType(), ExpectedType, DC));
  }
  return calculateTypeRelation(VD->getType(), ExpectedType, DC);
}

static CodeCompletionResult::ExpectedTypeRelation calculateMaxTypeRelationForDecl (
                                                const Decl *D,
                                                ArrayRef<Type> ExpectedTypes) {
  auto Result = CodeCompletionResult::ExpectedTypeRelation::Unrelated;
  for (auto Type : ExpectedTypes) {
    Result = std::max(Result, calculateTypeRelationForDecl(D, Type));
  }
  return Result;
}

CodeCompletionOperatorKind
CodeCompletionResult::getCodeCompletionOperatorKind(StringRef name) {
  using CCOK = CodeCompletionOperatorKind;
  using OpPair = std::pair<StringRef, CCOK>;

  // This list must be kept in alphabetical order.
  static OpPair ops[] = {
      std::make_pair("!", CCOK::Bang),
      std::make_pair("!=", CCOK::NotEq),
      std::make_pair("!==", CCOK::NotEqEq),
      std::make_pair("%", CCOK::Modulo),
      std::make_pair("%=", CCOK::ModuloEq),
      std::make_pair("&", CCOK::Amp),
      std::make_pair("&&", CCOK::AmpAmp),
      std::make_pair("&*", CCOK::AmpStar),
      std::make_pair("&+", CCOK::AmpPlus),
      std::make_pair("&-", CCOK::AmpMinus),
      std::make_pair("&=", CCOK::AmpEq),
      std::make_pair("(", CCOK::LParen),
      std::make_pair("*", CCOK::Star),
      std::make_pair("*=", CCOK::StarEq),
      std::make_pair("+", CCOK::Plus),
      std::make_pair("+=", CCOK::PlusEq),
      std::make_pair("-", CCOK::Minus),
      std::make_pair("-=", CCOK::MinusEq),
      std::make_pair(".", CCOK::Dot),
      std::make_pair("...", CCOK::DotDotDot),
      std::make_pair("..<", CCOK::DotDotLess),
      std::make_pair("/", CCOK::Slash),
      std::make_pair("/=", CCOK::SlashEq),
      std::make_pair("<", CCOK::Less),
      std::make_pair("<<", CCOK::LessLess),
      std::make_pair("<<=", CCOK::LessLessEq),
      std::make_pair("<=", CCOK::LessEq),
      std::make_pair("=", CCOK::Eq),
      std::make_pair("==", CCOK::EqEq),
      std::make_pair("===", CCOK::EqEqEq),
      std::make_pair(">", CCOK::Greater),
      std::make_pair(">=", CCOK::GreaterEq),
      std::make_pair(">>", CCOK::GreaterGreater),
      std::make_pair(">>=", CCOK::GreaterGreaterEq),
      std::make_pair("?.", CCOK::QuestionDot),
      std::make_pair("^", CCOK::Caret),
      std::make_pair("^=", CCOK::CaretEq),
      std::make_pair("|", CCOK::Pipe),
      std::make_pair("|=", CCOK::PipeEq),
      std::make_pair("||", CCOK::PipePipe),
      std::make_pair("~=", CCOK::TildeEq),
  };
  static auto opsSize = sizeof(ops) / sizeof(ops[0]);

  auto I = std::lower_bound(
      ops, &ops[opsSize], std::make_pair(name, CCOK::None),
      [](const OpPair &a, const OpPair &b) { return a.first < b.first; });

  if (I == &ops[opsSize] || I->first != name)
    return CCOK::Unknown;
  return I->second;
}

static StringRef getOperatorName(CodeCompletionString *str) {
  return str->getFirstTextChunk(/*includeLeadingPunctuation=*/true);
}

CodeCompletionOperatorKind
CodeCompletionResult::getCodeCompletionOperatorKind(CodeCompletionString *str) {
  return getCodeCompletionOperatorKind(getOperatorName(str));
}

CodeCompletionResult *CodeCompletionResultBuilder::takeResult() {
  auto *CCS = CodeCompletionString::create(*Sink.Allocator, Chunks);

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

    StringRef ModuleName;
    if (CurrentModule) {
      if (Sink.LastModule.first == CurrentModule.getOpaqueValue()) {
        ModuleName = Sink.LastModule.second;
      } else {
        if (auto *C = CurrentModule.dyn_cast<const clang::Module *>()) {
          ModuleName = copyString(*Sink.Allocator, C->getFullModuleName());
        } else {
          ModuleName = copyString(
              *Sink.Allocator,
              CurrentModule.get<const swift::Module *>()->getName().str());
        }
        Sink.LastModule.first = CurrentModule.getOpaqueValue();
        Sink.LastModule.second = ModuleName;
      }
    }

    auto typeRelation = ExpectedTypeRelation;
    if (typeRelation == CodeCompletionResult::Unrelated)
      typeRelation =
          calculateMaxTypeRelationForDecl(AssociatedDecl, ExpectedDeclTypes);

    if (typeRelation == CodeCompletionResult::Invalid) {
      IsNotRecommended = true;
      NotRecReason = CodeCompletionResult::NotRecommendedReason::TypeMismatch;
    }

    return new (*Sink.Allocator) CodeCompletionResult(
        SemanticContext, NumBytesToErase, CCS, AssociatedDecl, ModuleName,
        /*NotRecommended=*/IsNotRecommended, NotRecReason,
        copyString(*Sink.Allocator, BriefComment),
        copyAssociatedUSRs(*Sink.Allocator, AssociatedDecl),
        copyStringPairArray(*Sink.Allocator, CommentWords), typeRelation);
  }

  case CodeCompletionResult::ResultKind::Keyword:
    return new (*Sink.Allocator)
        CodeCompletionResult(KeywordKind, SemanticContext, NumBytesToErase,
                             CCS, ExpectedTypeRelation);

  case CodeCompletionResult::ResultKind::BuiltinOperator:
  case CodeCompletionResult::ResultKind::Pattern:
    return new (*Sink.Allocator) CodeCompletionResult(
        Kind, SemanticContext, NumBytesToErase, CCS, ExpectedTypeRelation);

  case CodeCompletionResult::ResultKind::Literal:
    assert(LiteralKind.hasValue());
    return new (*Sink.Allocator)
        CodeCompletionResult(*LiteralKind, SemanticContext, NumBytesToErase,
                             CCS, ExpectedTypeRelation);
  }
}

void CodeCompletionResultBuilder::finishResult() {
  if (!Cancelled)
    Sink.Results.push_back(takeResult());
}


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

Optional<unsigned> CodeCompletionString::getFirstTextChunkIndex(
    bool includeLeadingPunctuation) const {
  for (auto i : indices(getChunks())) {
    auto &C = getChunks()[i];
    switch (C.getKind()) {
    using ChunkKind = Chunk::ChunkKind;
    case ChunkKind::Text:
    case ChunkKind::CallParameterName:
    case ChunkKind::CallParameterInternalName:
    case ChunkKind::GenericParameterName:
    case ChunkKind::LeftParen:
    case ChunkKind::LeftBracket:
    case ChunkKind::Equal:
    case ChunkKind::DeclAttrParamKeyword:
    case ChunkKind::DeclAttrKeyword:
      return i;
    case ChunkKind::Dot:
    case ChunkKind::ExclamationMark:
    case ChunkKind::QuestionMark:
      if (includeLeadingPunctuation)
        return i;
      continue;
    case ChunkKind::RightParen:
    case ChunkKind::RightBracket:
    case ChunkKind::LeftAngle:
    case ChunkKind::RightAngle:
    case ChunkKind::Ellipsis:
    case ChunkKind::Comma:
    case ChunkKind::Ampersand:
    case ChunkKind::Whitespace:
    case ChunkKind::AccessControlKeyword:
    case ChunkKind::OverrideKeyword:
    case ChunkKind::ThrowsKeyword:
    case ChunkKind::RethrowsKeyword:
    case ChunkKind::DeclIntroducer:
    case ChunkKind::CallParameterColon:
    case ChunkKind::DeclAttrParamEqual:
    case ChunkKind::CallParameterType:
    case ChunkKind::CallParameterClosureType:
    case ChunkKind::OptionalBegin:
    case ChunkKind::CallParameterBegin:
    case ChunkKind::GenericParameterBegin:
    case ChunkKind::DynamicLookupMethodCallTail:
    case ChunkKind::OptionalMethodCallTail:
    case ChunkKind::TypeAnnotation:
      continue;

    case ChunkKind::BraceStmtWithCursor:
      llvm_unreachable("should have already extracted the text");
    }
  }
  return None;
}

StringRef
CodeCompletionString::getFirstTextChunk(bool includeLeadingPunctuation) const {
  Optional<unsigned> Idx = getFirstTextChunkIndex(includeLeadingPunctuation);
  if (Idx.hasValue())
    return getChunks()[*Idx].getText();
  return StringRef();
}

void CodeCompletionString::getName(raw_ostream &OS) const {
  auto FirstTextChunk = getFirstTextChunkIndex();
  int TextSize = 0;
  if (FirstTextChunk.hasValue()) {
    for (auto C : getChunks().slice(*FirstTextChunk)) {
      using ChunkKind = Chunk::ChunkKind;

      bool shouldPrint = !C.isAnnotation();
      switch (C.getKind()) {
      case ChunkKind::TypeAnnotation:
      case ChunkKind::CallParameterClosureType:
      case ChunkKind::DeclAttrParamEqual:
        continue;
      case ChunkKind::ThrowsKeyword:
      case ChunkKind::RethrowsKeyword:
        shouldPrint = true; // Even when they're annotations.
        break;
      default:
        break;
      }

      if (C.hasText() && shouldPrint) {
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
  struct ResultAndName {
    CodeCompletionResult *result;
    std::string name;
  };

  // Caching the name of each field is important to avoid unnecessary calls to
  // CodeCompletionString::getName().
  std::vector<ResultAndName> nameCache(Results.size());
  for (unsigned i = 0, n = Results.size(); i < n; ++i) {
    auto *result = Results[i];
    nameCache[i].result = result;
    llvm::raw_string_ostream OS(nameCache[i].name);
    result->getCompletionString()->getName(OS);
    OS.flush();
  }

  // Sort nameCache, and then transform Results to return the pointers in order.
  std::sort(nameCache.begin(), nameCache.end(),
            [](const ResultAndName &LHS, const ResultAndName &RHS) {
    int Result = StringRef(LHS.name).compare_lower(RHS.name);
    // If the case insensitive comparison is equal, then secondary sort order
    // should be case sensitive.
    if (Result == 0)
      Result = LHS.name.compare(RHS.name);
    return Result < 0;
  });

  std::transform(nameCache.begin(), nameCache.end(), Results.begin(),
                 [](const ResultAndName &entry) { return entry.result; });
}

namespace {
class CodeCompletionCallbacksImpl : public CodeCompletionCallbacks {
  CodeCompletionContext &CompletionContext;
  std::vector<RequestedCachedModule> RequestedModules;
  CodeCompletionConsumer &Consumer;
  CodeCompletionExpr *CodeCompleteTokenExpr = nullptr;
  AssignExpr *AssignmentExpr;
  CallExpr *FuncCallExpr;
  UnresolvedMemberExpr *UnresolvedExpr;
  bool UnresolvedExprInReturn;
  std::vector<std::string> TokensBeforeUnresolvedExpr;
  CompletionKind Kind = CompletionKind::None;
  Expr *ParsedExpr = nullptr;
  SourceLoc DotLoc;
  TypeLoc ParsedTypeLoc;
  DeclContext *CurDeclContext = nullptr;
  Decl *CStyleForLoopIterationVariable = nullptr;
  DeclAttrKind AttrKind;
  int AttrParamIndex;
  bool IsInSil;
  bool HasSpace = false;
  bool HasRParen = false;
  bool ShouldCompleteCallPatternAfterParen = true;
  bool PreferFunctionReferencesToCalls = false;
  Optional<DeclKind> AttTargetDK;

  SmallVector<StringRef, 3> ParsedKeywords;

  std::vector<std::pair<std::string, bool>> SubModuleNameVisibilityPairs;
  StmtKind ParentStmtKind;

  void addSuperKeyword(CodeCompletionResultSink &Sink) {
    auto *DC = CurDeclContext->getInnermostTypeContext();
    if (!DC)
      return;
    Type DT = DC->getDeclaredTypeOfContext();
    if (DT.isNull() || DT->is<ErrorType>())
      return;
    OwnedResolver TypeResolver(createLazyResolver(CurDeclContext->getASTContext()));
    Type ST = DT->getSuperclass(TypeResolver.get());
    if (ST.isNull() || ST->is<ErrorType>())
      return;
    if (ST->getNominalOrBoundGenericNominal()) {
      CodeCompletionResultBuilder Builder(Sink,
                                          CodeCompletionResult::ResultKind::Keyword,
                                          SemanticContextKind::CurrentNominal,
                                          {});
      Builder.setKeywordKind(CodeCompletionKeywordKind::kw_super);
      Builder.addTextChunk("super");
      ST = ST->getReferenceStorageReferent();
      assert(!ST->isVoid() && "Cannot get type name.");
      Builder.addTypeAnnotation(ST.getString());
    }
  }

  /// \brief Set to true when we have delivered code completion results
  /// to the \c Consumer.
  bool DeliveredResults = false;

  bool typecheckContextImpl(DeclContext *DC) {
    // Type check the function that contains the expression.
    if (DC->getContextKind() == DeclContextKind::AbstractClosureExpr ||
        DC->getContextKind() == DeclContextKind::AbstractFunctionDecl) {
      SourceLoc EndTypeCheckLoc = P.Context.SourceMgr.getCodeCompletionLoc();
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

  Optional<std::pair<Type, ConcreteDeclRef>> typeCheckParsedExpr() {
    assert(ParsedExpr && "should have an expression");

    // Figure out the kind of type-check we'll be performing.
    auto CheckKind = CompletionTypeCheckKind::Normal;
    if (Kind == CompletionKind::KeyPathExpr ||
        Kind == CompletionKind::KeyPathExprDot)
      CheckKind = CompletionTypeCheckKind::ObjCKeyPath;

    // If we've already successfully type-checked the expression for some
    // reason, just return the type.
    // FIXME: if it's ErrorType but we've already typechecked we shouldn't
    // typecheck again. rdar://21466394
    if (CheckKind == CompletionTypeCheckKind::Normal &&
        ParsedExpr->getType() && !ParsedExpr->getType()->is<ErrorType>())
      return std::make_pair(ParsedExpr->getType(),
                            ParsedExpr->getReferencedDecl());

    eraseErrorTypes(ParsedExpr);

    ConcreteDeclRef ReferencedDecl = nullptr;
    Expr *ModifiedExpr = ParsedExpr;
    if (auto T = getTypeOfCompletionContextExpr(P.Context, CurDeclContext,
                                                CheckKind, ModifiedExpr,
                                                ReferencedDecl)) {
      // FIXME: even though we don't apply the solution, the type checker may
      // modify the original expression. We should understand what effect that
      // may have on code completion.
      ParsedExpr = ModifiedExpr;

      return std::make_pair(*T, ReferencedDecl);
    }
    return None;
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
  void completeStmtOrExpr() override;
  void completePostfixExprBeginning(CodeCompletionExpr *E) override;
  void completePostfixExpr(Expr *E, bool hasSpace) override;
  void completePostfixExprParen(Expr *E, Expr *CodeCompletionE) override;
  void completeExprSuper(SuperRefExpr *SRE) override;
  void completeExprSuperDot(SuperRefExpr *SRE) override;
  void completeExprKeyPath(ObjCKeyPathExpr *KPE, bool HasDot) override;

  void completeTypeSimpleBeginning() override;
  void completeTypeIdentifierWithDot(IdentTypeRepr *ITR) override;
  void completeTypeIdentifierWithoutDot(IdentTypeRepr *ITR) override;

  void completeCaseStmtBeginning() override;
  void completeCaseStmtDotPrefix() override;
  void completeDeclAttrKeyword(Decl *D, bool Sil, bool Param) override;
  void completeDeclAttrParam(DeclAttrKind DK, int Index) override;
  void completeNominalMemberBeginning(
      SmallVectorImpl<StringRef> &Keywords) override;

  void completePoundAvailablePlatform() override;
  void completeImportDecl(std::vector<std::pair<Identifier, SourceLoc>> &Path) override;
  void completeUnresolvedMember(UnresolvedMemberExpr *E,
                                ArrayRef<StringRef> Identifiers,
                                bool HasReturn) override;
  void completeAssignmentRHS(AssignExpr *E) override;
  void completeCallArg(CallExpr *E) override;
  void completeReturnStmt(CodeCompletionExpr *E) override;
  void completeAfterPound(CodeCompletionExpr *E, StmtKind ParentKind) override;
  void completeGenericParams(TypeLoc TL) override;
  void addKeywords(CodeCompletionResultSink &Sink, bool MaybeFuncBody);

  void doneParsing() override;

  void deliverCompletionResults();
};
} // end anonymous namespace

void CodeCompletionCallbacksImpl::completeExpr() {
  if (DeliveredResults)
    return;

  Parser::ParserPositionRAII RestorePosition(P);
  P.restoreParserPosition(ExprBeginPosition);

  // FIXME: implement fallback code completion.

  deliverCompletionResults();
}

ArchetypeTransformer::ArchetypeTransformer(DeclContext *DC, Type Ty) :
    DC(DC), BaseTy(Ty->getRValueType()){
  auto D = BaseTy->getNominalOrBoundGenericNominal();
  if (!D)
    return;
  SmallVector<Type, 3> Scrach;
  auto Params = D->getInnermostGenericParamTypes();
  auto Args = BaseTy->getAllGenericArgs(Scrach);
  assert(Params.size() == Args.size());
  for (unsigned I = 0, N = Params.size(); I < N; I++) {
    Map[Params[I]->getCanonicalType()->castTo<GenericTypeParamType>()] = Args[I];
  }
}

llvm::function_ref<Type(Type)> ArchetypeTransformer::getTransformerFunc() {
  if (TheFunc)
    return TheFunc;
  TheFunc = [&](Type Ty) {
    if (Ty->getKind() != TypeKind::Archetype)
      return Ty;
    if (Cache.count(Ty.getPointer()) > 0) {
      return Cache[Ty.getPointer()];
    }
    Type Result = Ty;
    auto *RootArc = cast<ArchetypeType>(Result.getPointer());
    llvm::SmallVector<Identifier, 1> Names;
    bool SelfDerived = false;
    for (auto *AT = RootArc; AT; AT = AT->getParent()) {
      if (!AT->getSelfProtocol())
        Names.insert(Names.begin(), AT->getName());
      else
        SelfDerived = true;
    }
    if (SelfDerived) {
      if (auto MT = checkMemberType(*DC, BaseTy, Names)) {
        if (auto NAT = dyn_cast<NameAliasType>(MT.getPointer())) {
          Result = NAT->getSinglyDesugaredType();
        } else {
          Result =  MT;
        }
      }
    } else {
      Result = Ty.subst(DC->getParentModule(), Map, SubstFlags::IgnoreMissing);
    }

    auto ATT = dyn_cast<ArchetypeType>(Result.getPointer());
    if (ATT && !ATT->getParent()) {
      auto Conformances = ATT->getConformsTo();
      if (Conformances.size() == 1) {
        Result = Conformances[0]->getDeclaredType();
      } else if (!Conformances.empty()) {
        llvm::SmallVector<Type, 3> ConformedTypes;
        for (auto PD : Conformances) {
          ConformedTypes.push_back(PD->getDeclaredType());
        }
        Result = ProtocolCompositionType::get(DC->getASTContext(),
                                              ConformedTypes);
      }
    }
    if (Result->getKind() != TypeKind::Archetype)
      Result = Result.transform(getTransformerFunc());
    Cache[Ty.getPointer()] = Result;
    return Result;
  };
  return TheFunc;
}

namespace {
static bool isTopLevelContext(const DeclContext *DC) {
  for (; DC && DC->isLocalContext(); DC = DC->getParent()) {
    switch (DC->getContextKind()) {
    case DeclContextKind::TopLevelCodeDecl:
      return true;
    case DeclContextKind::AbstractFunctionDecl:
    case DeclContextKind::SubscriptDecl:
      return false;
    default:
      continue;
    }
  }
  return false;
}

static Type getReturnTypeFromContext(const DeclContext *DC) {
  if (auto FD = dyn_cast<AbstractFunctionDecl>(DC)) {
    if (auto FT = FD->getType()->getAs<FunctionType>()) {
      return FT->getResult();
    }
  } else if (auto CE = dyn_cast<AbstractClosureExpr>(DC)) {
    return CE->getResultType();
  }
  return Type();
}

static KnownProtocolKind
protocolForLiteralKind(CodeCompletionLiteralKind kind) {
  switch (kind) {
  case CodeCompletionLiteralKind::ArrayLiteral:
    return KnownProtocolKind::ExpressibleByArrayLiteral;
  case CodeCompletionLiteralKind::BooleanLiteral:
    return KnownProtocolKind::ExpressibleByBooleanLiteral;
  case CodeCompletionLiteralKind::ColorLiteral:
    return KnownProtocolKind::ExpressibleByColorLiteral;
  case CodeCompletionLiteralKind::ImageLiteral:
    return KnownProtocolKind::ExpressibleByImageLiteral;
  case CodeCompletionLiteralKind::DictionaryLiteral:
    return KnownProtocolKind::ExpressibleByDictionaryLiteral;
  case CodeCompletionLiteralKind::IntegerLiteral:
    return KnownProtocolKind::ExpressibleByIntegerLiteral;
  case CodeCompletionLiteralKind::NilLiteral:
    return KnownProtocolKind::ExpressibleByNilLiteral;
  case CodeCompletionLiteralKind::StringLiteral:
    return KnownProtocolKind::ExpressibleByStringLiteral;
  case CodeCompletionLiteralKind::Tuple:
    llvm_unreachable("no such protocol kind");
  }
}

/// Whether funcType has a single argument (not including defaulted arguments)
/// that is of type () -> ().
static bool hasTrivialTrailingClosure(const FuncDecl *FD,
                                      AnyFunctionType *funcType) {
  unsigned level = FD->isInstanceMember() ? 1 : 0;
  auto Args = decomposeParamType(funcType->getInput(), FD, level);
  bool OneArg = Args.size() == 1;
  if (Args.size() > 1) {
    unsigned NonDefault =
        std::count_if(Args.begin(), Args.end() - 1, [](const CallArgParam &P) {
          return !P.HasDefaultArgument;
        });

    OneArg = NonDefault == 0;
  }

  if (OneArg)
    if (auto Fn = Args.back().Ty->getAs<AnyFunctionType>())
      return Fn->getInput()->isVoid() && Fn->getResult()->isVoid();

  return false;
}

/// Build completions by doing visible decl lookup from a context.
class CompletionLookup final : public swift::VisibleDeclConsumer {
  CodeCompletionResultSink &Sink;
  ASTContext &Ctx;
  OwnedResolver TypeResolver;
  const DeclContext *CurrDeclContext;
  ClangImporter *Importer;

  enum class LookupKind {
    ValueExpr,
    ValueInDeclContext,
    EnumElement,
    Type,
    TypeInDeclContext,
    ImportFromModule
  };

  LookupKind Kind;

  /// Type of the user-provided expression for LookupKind::ValueExpr
  /// completions.
  Type ExprType;

  /// Whether the expr is of statically inferred metatype.
  bool IsStaticMetatype;

  /// User-provided base type for LookupKind::Type completions.
  Type BaseType;

  /// Expected types of the code completion expression.
  std::vector<Type> ExpectedTypes;

  bool HaveDot = false;
  SourceLoc DotLoc;
  bool NeedLeadingDot = false;

  bool NeedOptionalUnwrap = false;
  unsigned NumBytesToEraseForOptionalUnwrap = 0;

  bool HaveLParen = false;
  bool HaveRParen = false;
  bool IsSuperRefExpr = false;
  bool IsKeyPathExpr = false;
  bool IsDynamicLookup = false;
  bool PreferFunctionReferencesToCalls = false;
  bool HaveLeadingSpace = false;

  bool IncludeInstanceMembers = false;

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

  std::unique_ptr<ArchetypeTransformer> TransformerPt = nullptr;

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
    auto Elements = InTuple->getElements();
    if (Elements.empty())
      return;
    if (!Elements[0].hasName())
      FoundFunctionsWithoutFirstKeyword = true;
  }

  void setClangDeclKeywords(const ValueDecl *VD, CommandWordsPairs &Pairs,
                            CodeCompletionResultBuilder &Builder) {
    if (auto *CD = VD->getClangDecl()) {
      clang::comments::getClangDocKeyword(*Importer, CD, Pairs);
    } else {
      swift::markup::getSwiftDocKeyword(VD, Pairs);
    }
    Builder.addDeclDocCommentWords(llvm::makeArrayRef(Pairs));
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
        TypeResolver(createLazyResolver(Ctx)), CurrDeclContext(CurrDeclContext),
        Importer(static_cast<ClangImporter *>(CurrDeclContext->getASTContext().
          getClangModuleLoader())) {

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

  void initializeArchetypeTransformer(DeclContext *DC, Type BaseTy) {
    TransformerPt = llvm::make_unique<ArchetypeTransformer>(DC, BaseTy);
  }

  void setIsStaticMetatype(bool value) {
    IsStaticMetatype = value;
  }

  void setExpectedTypes(ArrayRef<Type> Types) {
    ExpectedTypes.reserve(Types.size());
    for (auto T : Types)
      if (T)
        ExpectedTypes.push_back(T);
  }

  bool hasExpectedTypes() const { return !ExpectedTypes.empty(); }

  bool needDot() const {
    return NeedLeadingDot;
  }

  void setHaveLParen(bool Value) {
    HaveLParen = Value;
  }

  void setHaveRParen(bool Value) {
    HaveRParen = Value;
  }

  void setIsSuperRefExpr() {
    IsSuperRefExpr = true;
  }

  void setIsKeyPathExpr() {
    IsKeyPathExpr = true;
  }

  void setIsDynamicLookup() {
    IsDynamicLookup = true;
  }

  void setPreferFunctionReferencesToCalls() {
    PreferFunctionReferencesToCalls = true;
  }

  void setHaveLeadingSpace(bool value) { HaveLeadingSpace = value; }

  void includeInstanceMembers() {
    IncludeInstanceMembers = true;
  }

  void addExpressionSpecificDecl(const Decl *D) {
    ExpressionSpecificDecls.insert(D);
  }

  void addSubModuleNames(std::vector<std::pair<std::string, bool>>
      &SubModuleNameVisibilityPairs) {
    for (auto &Pair : SubModuleNameVisibilityPairs) {
      CodeCompletionResultBuilder Builder(Sink,
                                          CodeCompletionResult::ResultKind::
                                          Declaration,
                                          SemanticContextKind::OtherModule,
                                          ExpectedTypes);
      auto MD = ModuleDecl::create(Ctx.getIdentifier(Pair.first), Ctx);
      Builder.setAssociatedDecl(MD);
      Builder.addTextChunk(MD->getNameStr());
      Builder.addTypeAnnotation("Module");
      if (Pair.second)
        Builder.setNotRecommended(CodeCompletionResult::NotRecommendedReason::
                                    Redundant);
    }
  }

  void collectImportedModules(llvm::StringSet<> &ImportedModules) {
    SmallVector<Module::ImportedModule, 16> Imported;
    SmallVector<Module::ImportedModule, 16> FurtherImported;
    CurrDeclContext->getParentSourceFile()->getImportedModules(Imported,
      Module::ImportFilter::All);
    while (!Imported.empty()) {
      ModuleDecl *MD = Imported.back().second;
      Imported.pop_back();
      if (!ImportedModules.insert(MD->getNameStr()).second)
        continue;
      FurtherImported.clear();
      MD->getImportedModules(FurtherImported, Module::ImportFilter::Public);
      Imported.append(FurtherImported.begin(), FurtherImported.end());
      for (auto SubMod : FurtherImported) {
        Imported.push_back(SubMod);
      }
    }
  }

  void addImportModuleNames() {
    // FIXME: Add user-defined swift modules
    SmallVector<clang::Module*, 20> Modules;
    Ctx.getVisibleTopLevelClangModules(Modules);
    std::sort(Modules.begin(), Modules.end(),
              [](clang::Module* LHS , clang::Module* RHS) {
                return LHS->getTopLevelModuleName().compare_lower(
                  RHS->getTopLevelModuleName()) < 0;
              });
    llvm::StringSet<> ImportedModules;
    collectImportedModules(ImportedModules);
    for (auto *M : Modules) {
      if (M->isAvailable() &&
          !M->getTopLevelModuleName().startswith("_") &&
          M->getTopLevelModuleName() != CurrDeclContext->getASTContext().
            SwiftShimsModuleName.str()) {
        auto MD = ModuleDecl::create(Ctx.getIdentifier(M->getTopLevelModuleName()),
                                     Ctx);
        CodeCompletionResultBuilder Builder(Sink,
                                            CodeCompletionResult::ResultKind::
                                              Declaration,
                                            SemanticContextKind::OtherModule,
                                            ExpectedTypes);
        Builder.setAssociatedDecl(MD);
        Builder.addTextChunk(MD->getNameStr());
        Builder.addTypeAnnotation("Module");

        // Imported modules are not recommended.
        if (ImportedModules.count(MD->getNameStr()) != 0)
          Builder.setNotRecommended(CodeCompletionResult::NotRecommendedReason::
                                      Redundant);
      }
    }
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
          D->getModuleContext() == CurrDeclContext->getParentModule()) {
        // Treat global variables from the same source file as local when
        // completing at top-level.
        if (isa<VarDecl>(D) && isTopLevelContext(CurrDeclContext) &&
            D->getDeclContext()->getParentSourceFile() ==
                CurrDeclContext->getParentSourceFile()) {
          return SemanticContextKind::Local;
        } else {
          return SemanticContextKind::CurrentModule;
        }
      } else {
        return SemanticContextKind::OtherModule;
      }

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
        if (ContextTy->lookThroughAllAnyOptionalTypes()->getAnyNominal() ==
            MaybeNominalType->lookThroughAllAnyOptionalTypes()->getAnyNominal() &&
            !isBoringBoundGenericType(MaybeNominalType)) {
          if (Type T = MaybeNominalType->lookThroughAllAnyOptionalTypes()->getTypeOfMember(
              CurrDeclContext->getParentModule(), VD, TypeResolver.get()))
            return TransformerPt ? T.transform(TransformerPt->getTransformerFunc()) :
                                   T;
        }
      }
    }
    return TransformerPt ? VD->getType().transform(TransformerPt->getTransformerFunc()) :
                           VD->getType();
  }

  const DeducedAssociatedTypes &
  getAssociatedTypeMap(const NominalTypeDecl *NTD) {
    {
      auto It = DeducedAssociatedTypeCache.find(NTD);
      if (It != DeducedAssociatedTypeCache.end())
        return It->second;
    }

    DeducedAssociatedTypes Types;
    for (auto Conformance : NTD->getAllConformances()) {
      if (!Conformance->isComplete())
        continue;
      Conformance->forEachTypeWitness(TypeResolver.get(),
                                      [&](const AssociatedTypeDecl *ATD,
                                          const Substitution &Subst,
                                          TypeDecl *TD) -> bool {
        Types[ATD] = Subst.getReplacement();
        return false;
      });
    }

    auto ItAndInserted = DeducedAssociatedTypeCache.insert({ NTD, Types });
    assert(ItAndInserted.second && "should not be in the map");
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
    if (!VD->hasName() ||
        !VD->isUserAccessible() ||
        (VD->hasAccessibility() && !VD->isAccessibleFrom(CurrDeclContext)) ||
        shouldHideDeclFromCompletionResults(VD))
      return;

    StringRef Name = VD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(VD, Reason), ExpectedTypes);
    Builder.setAssociatedDecl(VD);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    setClangDeclKeywords(VD, Pairs, Builder);
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

  void addParameters(CodeCompletionResultBuilder &Builder,
                     const ParameterList *params) {
    bool NeedComma = false;
    for (auto &param : *params) {
      if (NeedComma)
        Builder.addComma();
      NeedComma = true;

      Type type = param->getType();
      if (param->isVariadic())
        type = ParamDecl::getVarargBaseTy(type);

      Builder.addCallParameter(param->getArgumentName(), type,
                               param->isVariadic(), true);
    }
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
      for (auto TupleElt : TT->getElements()) {
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
                               /*IsVarArg*/false, IsTopLevel);
      if (IsTopLevel)
        Builder.addRightParen();
      return;
    }

    if (IsTopLevel && !HaveLParen)
      Builder.addLeftParen();
    else if (IsTopLevel)
      Builder.addAnnotatedLeftParen();

    Builder.addCallParameter(Label, T, IsVarArg, IsTopLevel);
    if (IsTopLevel)
      Builder.addRightParen();
  }

  void addPatternFromType(CodeCompletionResultBuilder &Builder, Type T) {
    addPatternFromTypeImpl(Builder, T, Identifier(), true, /*isVarArg*/false);
  }

  static bool hasInterestingDefaultValues(const AbstractFunctionDecl *func) {
    if (!func) return false;

    bool isMemberOfType = func->getDeclContext()->isTypeContext();
    for (auto param : *func->getParameterList(isMemberOfType ? 1 : 0)) {
      switch (param->getDefaultArgumentKind()) {
      case DefaultArgumentKind::Normal:
      case DefaultArgumentKind::Inherited: // FIXME: include this?
        return true;
      default:
        break;
      }
    }
    return false;
  }

  // Returns true if any content was added to Builder.
  bool addParamPatternFromFunction(CodeCompletionResultBuilder &Builder,
                                   const AnyFunctionType *AFT,
                                   const AbstractFunctionDecl *AFD,
                                   bool includeDefaultArgs = true) {

    const ParameterList *BodyParams = nullptr;
    if (AFD) {
      BodyParams = AFD->getParameterList(AFD->getImplicitSelfDecl() ? 1 : 0);

      // FIXME: Hack because we don't know which parameter list we're
      // actually working with.
      unsigned expectedNumParams;
      if (auto *TT = dyn_cast<TupleType>(AFT->getInput().getPointer()))
        expectedNumParams = TT->getNumElements();
      else
        expectedNumParams = 1;

      if (expectedNumParams != BodyParams->size()) {
        // Adjust to the "self" list if that is present, otherwise give up.
        if (expectedNumParams == 1 && AFD->getImplicitSelfDecl())
          BodyParams = AFD->getParameterList(0);
        else
          BodyParams = nullptr;
      }
    }

    bool modifiedBuilder = false;

    // Determine whether we should skip this argument because it is defaulted.
    auto shouldSkipArg = [&](unsigned i) -> bool {
      if (!BodyParams || i >= BodyParams->size())
        return false;

      switch (BodyParams->get(i)->getDefaultArgumentKind()) {
        case DefaultArgumentKind::None:
          return false;

        case DefaultArgumentKind::Normal:
        case DefaultArgumentKind::Inherited:
        case DefaultArgumentKind::Nil:
        case DefaultArgumentKind::EmptyArray:
        case DefaultArgumentKind::EmptyDictionary:
          return !includeDefaultArgs;

        case DefaultArgumentKind::File:
        case DefaultArgumentKind::Line:
        case DefaultArgumentKind::Column:
        case DefaultArgumentKind::Function:
        case DefaultArgumentKind::DSOHandle:
          // Skip parameters that are defaulted to source location or other
          // caller context information.  Users typically don't want to specify
          // these parameters.
          return true;
      }
    };

    // Do not desugar AFT->getInput(), as we want to treat (_: (a,b)) distinctly
    // from (a,b) for code-completion.
    if (auto *TT = dyn_cast<TupleType>(AFT->getInput().getPointer())) {
      bool NeedComma = false;
      // Iterate over the tuple type fields, corresponding to each parameter.
      for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
        // If we should skip this argument, do so.
        if (shouldSkipArg(i)) continue;

        const auto &TupleElt = TT->getElement(i);
        auto ParamType = TupleElt.isVararg() ? TupleElt.getVarargBaseTy()
                                             : TupleElt.getType();
        auto Name = TupleElt.getName();

        if (NeedComma)
          Builder.addComma();
        if (BodyParams) {
          // If we have a local name for the parameter, pass in that as well.
          auto name = BodyParams->get(i)->getName();
          Builder.addCallParameter(Name, name, ParamType, TupleElt.isVararg(),
                                   true);
        } else {
          Builder.addCallParameter(Name, ParamType, TupleElt.isVararg(), true);
        }
        modifiedBuilder = true;
        NeedComma = true;
      }
    } else if (!shouldSkipArg(0)) {
      // If it's not a tuple, it could be a unary function.
      Type T = AFT->getInput();
      if (auto *PT = dyn_cast<ParenType>(T.getPointer())) {
        // Only unwrap the paren sugar, if it exists.
        T = PT->getUnderlyingType();
      }

      modifiedBuilder = true;
      if (BodyParams) {
        auto name = BodyParams->get(0)->getName();
        Builder.addCallParameter(Identifier(), name, T,
                                 /*IsVarArg*/false, true);
      } else
        Builder.addCallParameter(Identifier(), T, /*IsVarArg*/false, true);
    }

    return modifiedBuilder;
  }

  static void addThrows(CodeCompletionResultBuilder &Builder,
                        const AnyFunctionType *AFT,
                        const AbstractFunctionDecl *AFD) {
    if (AFD && AFD->getAttrs().hasAttribute<RethrowsAttr>())
      Builder.addAnnotatedRethrows();
    else if (AFT->throws())
      Builder.addAnnotatedThrows();
  }

  void addPoundAvailable(StmtKind ParentKind) {
    if (ParentKind != StmtKind::If && ParentKind != StmtKind::Guard)
      return;
    CodeCompletionResultBuilder Builder(Sink, CodeCompletionResult::ResultKind::Keyword,
      SemanticContextKind::ExpressionSpecific, ExpectedTypes);
    Builder.addTextChunk("available");
    Builder.addLeftParen();
    Builder.addSimpleTypedParameter("Platform", /*isVarArg=*/true);
    Builder.addComma();
    Builder.addTextChunk("*");
    Builder.addRightParen();
  }

  void addPoundSelector(bool needPound) {
    // #selector is only available when the Objective-C runtime is.
    if (!Ctx.LangOpts.EnableObjCInterop) return;

    CodeCompletionResultBuilder Builder(
                                  Sink,
                                  CodeCompletionResult::ResultKind::Keyword,
                                  SemanticContextKind::ExpressionSpecific,
                                  ExpectedTypes);
    if (needPound)
      Builder.addTextChunk("#selector");
    else
      Builder.addTextChunk("selector");
    Builder.addLeftParen();
    Builder.addSimpleTypedParameter("@objc method", /*isVarArg=*/false);
    Builder.addRightParen();
  }

  void addPoundKeyPath(bool needPound) {
    // #keyPath is only available when the Objective-C runtime is.
    if (!Ctx.LangOpts.EnableObjCInterop) return;

    // After #, this is a very likely result. When just in a String context,
    // it's not.
    auto semanticContext = needPound ? SemanticContextKind::None
                                     : SemanticContextKind::ExpressionSpecific;

    CodeCompletionResultBuilder Builder(
                                  Sink,
                                  CodeCompletionResult::ResultKind::Keyword,
                                  semanticContext, ExpectedTypes);
    if (needPound)
      Builder.addTextChunk("#keyPath");
    else
      Builder.addTextChunk("keyPath");
    Builder.addLeftParen();
    Builder.addSimpleTypedParameter("@objc property sequence",
                                    /*isVarArg=*/false);
    Builder.addRightParen();
  }

  void addFunctionCallPattern(const AnyFunctionType *AFT,
                              const AbstractFunctionDecl *AFD = nullptr) {
    foundFunction(AFT);

    // Add the pattern, possibly including any default arguments.
    auto addPattern = [&](bool includeDefaultArgs = true) {
      CodeCompletionResultBuilder Builder(
          Sink, CodeCompletionResult::ResultKind::Pattern,
          SemanticContextKind::ExpressionSpecific, ExpectedTypes);
      if (!HaveLParen)
        Builder.addLeftParen();
      else
        Builder.addAnnotatedLeftParen();

      bool anyParam = addParamPatternFromFunction(Builder, AFT, AFD, includeDefaultArgs);

      if (HaveLParen && HaveRParen && !anyParam) {
        // Empty result, don't add it.
        Builder.cancel();
        return;
      }

      if (!HaveRParen)
        Builder.addRightParen();
      else
        Builder.addAnnotatedRightParen();

      addThrows(Builder, AFT, AFD);

      addTypeAnnotation(Builder, AFT->getResult());
    };

    if (hasInterestingDefaultValues(AFD))
      addPattern(/*includeDefaultArgs*/ false);
    addPattern();
  }

  void addMethodCall(const FuncDecl *FD, DeclVisibilityKind Reason) {
    if (FD->getName().empty())
      return;
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
      llvm_unreachable("cannot have a method call while doing a "
                       "type completion");
    case LookupKind::ImportFromModule:
      IsImplicitlyCurriedInstanceMethod = false;
      break;
    }

    StringRef Name = FD->getName().get();
    assert(!Name.empty() && "name should not be empty");

    unsigned FirstIndex = 0;
    if (!IsImplicitlyCurriedInstanceMethod && FD->getImplicitSelfDecl())
      FirstIndex = 1;
    Type FunctionType = getTypeOfMember(FD);
    assert(FunctionType);
    if (FirstIndex != 0 && !FunctionType->is<ErrorType>())
      FunctionType = FunctionType->castTo<AnyFunctionType>()->getResult();

    bool trivialTrailingClosure = false;
    if (!IsImplicitlyCurriedInstanceMethod && !FunctionType->is<ErrorType>()) {
      trivialTrailingClosure = hasTrivialTrailingClosure(
          FD, FunctionType->castTo<AnyFunctionType>());
    }

    // Add the method, possibly including any default arguments.
    auto addMethodImpl = [&](bool includeDefaultArgs = true,
                             bool trivialTrailingClosure = false) {
      CommandWordsPairs Pairs;
      CodeCompletionResultBuilder Builder(
          Sink, CodeCompletionResult::ResultKind::Declaration,
          getSemanticContext(FD, Reason), ExpectedTypes);
      setClangDeclKeywords(FD, Pairs, Builder);
      Builder.setAssociatedDecl(FD);
      addLeadingDot(Builder);
      Builder.addTextChunk(Name);
      if (IsDynamicLookup)
        Builder.addDynamicLookupMethodCallTail();
      else if (FD->getAttrs().hasAttribute<OptionalAttr>())
        Builder.addOptionalMethodCallTail();

      llvm::SmallString<32> TypeStr;

      if (FunctionType->is<ErrorType>()) {
        llvm::raw_svector_ostream OS(TypeStr);
        FunctionType.print(OS);
        Builder.addTypeAnnotation(OS.str());
        return;
      }

      Type FirstInputType = FunctionType->castTo<AnyFunctionType>()->getInput();

      if (IsImplicitlyCurriedInstanceMethod) {
        if (auto PT = dyn_cast<ParenType>(FirstInputType.getPointer()))
          FirstInputType = PT->getUnderlyingType();

        Builder.addLeftParen();
        Builder.addCallParameter(Ctx.Id_self, FirstInputType,
                                 /*IsVarArg*/ false, true);
        Builder.addRightParen();
      } else if (trivialTrailingClosure) {
        Builder.addBraceStmtWithCursor(" { code }");
      } else {
        Builder.addLeftParen();
        auto AFT = FunctionType->castTo<AnyFunctionType>();
        addParamPatternFromFunction(Builder, AFT, FD, includeDefaultArgs);
        Builder.addRightParen();
        addThrows(Builder, AFT, FD);
      }

      Type ResultType = FunctionType->castTo<AnyFunctionType>()->getResult();

      // Build type annotation.
      {
        llvm::raw_svector_ostream OS(TypeStr);
        for (unsigned i = FirstIndex + 1, e = FD->getParameterLists().size();
             i != e; ++i) {
          ResultType->castTo<AnyFunctionType>()->getInput()->print(OS);
          ResultType = ResultType->castTo<AnyFunctionType>()->getResult();
          OS << " -> ";
        }
        // What's left is the result type.
        if (ResultType->isVoid())
          OS << "Void";
        else
          ResultType.print(OS);
      }
      Builder.addTypeAnnotation(TypeStr);
    };

    if (!FunctionType->is<ErrorType>() &&
        hasInterestingDefaultValues(FD)) {
      addMethodImpl(/*includeDefaultArgs*/ false);
    }
    if (trivialTrailingClosure) {
      addMethodImpl(/*includeDefaultArgs=*/false,
                    /*trivialTrailingClosure=*/true);
    }
    addMethodImpl();
  }

  void addConstructorCall(const ConstructorDecl *CD, DeclVisibilityKind Reason,
                          Optional<Type> Result,
                          Identifier addName = Identifier()) {
    foundFunction(CD);
    Type MemberType = getTypeOfMember(CD);
    AnyFunctionType *ConstructorType = nullptr;
    if (!MemberType->is<ErrorType>())
      ConstructorType = MemberType->castTo<AnyFunctionType>()
                            ->getResult()
                            ->castTo<AnyFunctionType>();

    bool needInit = false;
    if (IsSuperRefExpr) {
      assert(addName.empty());
      assert(isa<ConstructorDecl>(CurrDeclContext) &&
             "can call super.init only inside a constructor");
      needInit = true;
    } else if (addName.empty() && HaveDot &&
               Reason == DeclVisibilityKind::MemberOfCurrentNominal) {
      // This case is querying the init function as member
      needInit = true;
    }

    // If we won't be able to provide a result, bail out.
    if (MemberType->is<ErrorType>() && addName.empty() && !needInit)
      return;

    // Add the constructor, possibly including any default arguments.
    auto addConstructorImpl = [&](bool includeDefaultArgs = true) {
      CommandWordsPairs Pairs;
      CodeCompletionResultBuilder Builder(
          Sink, CodeCompletionResult::ResultKind::Declaration,
          getSemanticContext(CD, Reason), ExpectedTypes);
      setClangDeclKeywords(CD, Pairs, Builder);
      Builder.setAssociatedDecl(CD);
      if (needInit) {
        assert(addName.empty());
        addLeadingDot(Builder);
        Builder.addTextChunk("init");
      } else if (!addName.empty()) {
        Builder.addTextChunk(addName.str());
      } else {
        assert(!MemberType->is<ErrorType>() && "will insert empty result");
      }

      if (MemberType->is<ErrorType>()) {
        addTypeAnnotation(Builder, MemberType);
        return;
      }
      assert(ConstructorType);

      if (!HaveLParen)
        Builder.addLeftParen();
      else
        Builder.addAnnotatedLeftParen();

      bool anyParam = addParamPatternFromFunction(Builder, ConstructorType, CD,
                                  includeDefaultArgs);

      if (HaveLParen && HaveRParen && !anyParam) {
        // Empty result, don't add it.
        Builder.cancel();
        return;
      }

      if (!HaveRParen)
        Builder.addRightParen();
      else
        Builder.addAnnotatedRightParen();

      addThrows(Builder, ConstructorType, CD);

      addTypeAnnotation(Builder,
                        Result.hasValue() ? Result.getValue() :
                                            ConstructorType->getResult());
    };

    if (ConstructorType && hasInterestingDefaultValues(CD))
      addConstructorImpl(/*includeDefaultArgs*/ false);
    addConstructorImpl();
  }

  void addConstructorCallsForType(Type type, Identifier name,
                                  DeclVisibilityKind Reason) {
    if (!Ctx.LangOpts.CodeCompleteInitsInPostfixExpr)
      return;

    assert(CurrDeclContext);
    SmallVector<ValueDecl *, 16> initializers;
    if (CurrDeclContext->lookupQualified(type, Ctx.Id_init, NL_QualifiedDefault,
                                         TypeResolver.get(), initializers)) {
      for (auto *init : initializers) {
        if (shouldHideDeclFromCompletionResults(init))
          continue;
        addConstructorCall(cast<ConstructorDecl>(init), Reason, None, name);
      }
    }
  }

  void addSubscriptCall(const SubscriptDecl *SD, DeclVisibilityKind Reason) {
    assert(!HaveDot && "cannot add a subscript after a dot");
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(SD, Reason), ExpectedTypes);
    Builder.setAssociatedDecl(SD);
    setClangDeclKeywords(SD, Pairs, Builder);
    Builder.addLeftBracket();
    addParameters(Builder, SD->getIndices());
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
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(NTD, Reason), ExpectedTypes);
    Builder.setAssociatedDecl(NTD);
    setClangDeclKeywords(NTD, Pairs, Builder);
    addLeadingDot(Builder);
    Builder.addTextChunk(NTD->getName().str());
    addTypeAnnotation(Builder, NTD->getDeclaredType());
  }

  void addTypeAliasRef(const TypeAliasDecl *TAD, DeclVisibilityKind Reason) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(TAD, Reason), ExpectedTypes);
    Builder.setAssociatedDecl(TAD);
    setClangDeclKeywords(TAD, Pairs, Builder);
    addLeadingDot(Builder);
    Builder.addTextChunk(TAD->getName().str());
    if (TAD->hasUnderlyingType() && !TAD->getUnderlyingType()->is<ErrorType>())
      addTypeAnnotation(Builder, TAD->getUnderlyingType());
    else {
      addTypeAnnotation(Builder, TAD->getDeclaredType());
    }
  }

  void addGenericTypeParamRef(const GenericTypeParamDecl *GP,
                              DeclVisibilityKind Reason) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(GP, Reason), ExpectedTypes);
    setClangDeclKeywords(GP, Pairs, Builder);
    Builder.setAssociatedDecl(GP);
    addLeadingDot(Builder);
    Builder.addTextChunk(GP->getName().str());
    addTypeAnnotation(Builder, GP->getDeclaredType());
  }

  void addAssociatedTypeRef(const AssociatedTypeDecl *AT,
                            DeclVisibilityKind Reason) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(AT, Reason), ExpectedTypes);
    setClangDeclKeywords(AT, Pairs, Builder);
    Builder.setAssociatedDecl(AT);
    addLeadingDot(Builder);
    Builder.addTextChunk(AT->getName().str());
    if (Type T = getAssociatedTypeType(AT))
      addTypeAnnotation(Builder, T);
  }

  void addEnumElementRef(const EnumElementDecl *EED,
                         DeclVisibilityKind Reason,
                         bool HasTypeContext) {
    if (!EED->hasName() ||
        (EED->hasAccessibility() && !EED->isAccessibleFrom(CurrDeclContext)) ||
        shouldHideDeclFromCompletionResults(EED))
      return;

    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        HasTypeContext ? SemanticContextKind::ExpressionSpecific
                       : getSemanticContext(EED, Reason), ExpectedTypes);
    Builder.setAssociatedDecl(EED);
    setClangDeclKeywords(EED, Pairs, Builder);
    addLeadingDot(Builder);
    Builder.addTextChunk(EED->getName().str());
    if (EED->hasArgumentType())
      addPatternFromType(Builder, EED->getArgumentType());

    // Enum element is of function type such as EnumName.type -> Int ->
    // EnumName; however we should show Int -> EnumName as the type
    Type EnumType;
    if (EED->hasType()) {
      EnumType = EED->getType();
      if (auto FuncType = EnumType->getAs<AnyFunctionType>()) {
        EnumType = FuncType->getResult();
      }
    }
    if (EnumType)
      addTypeAnnotation(Builder, EnumType);
  }

  void addKeyword(StringRef Name, Type TypeAnnotation,
                  SemanticContextKind SK = SemanticContextKind::None) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword, SK, ExpectedTypes);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.isNull())
      addTypeAnnotation(Builder, TypeAnnotation);
  }

  void addKeyword(StringRef Name, StringRef TypeAnnotation) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, ExpectedTypes);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
  }

  void addDeclAttrParamKeyword(StringRef Name, StringRef Annotation,
                             bool NeedSpecify) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, ExpectedTypes);
    Builder.addDeclAttrParamKeyword(Name, Annotation, NeedSpecify);
  }

  void addDeclAttrKeyword(StringRef Name, StringRef Annotation) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, ExpectedTypes);
    Builder.addDeclAttrKeyword(Name, Annotation);
  }

  /// Add the compound function name for the given function.
  void addCompoundFunctionName(AbstractFunctionDecl *AFD,
                               DeclVisibilityKind Reason) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(AFD, Reason), ExpectedTypes);
    setClangDeclKeywords(AFD, Pairs, Builder);
    Builder.setAssociatedDecl(AFD);

    // Base name
    addLeadingDot(Builder);
    Builder.addTextChunk(AFD->getFullName().getBaseName().str());

    // Add the argument labels.
    auto ArgLabels = AFD->getFullName().getArgumentNames();
    if (ArgLabels.size() > 0) {
      if (!HaveLParen)
        Builder.addLeftParen();
      else
        Builder.addAnnotatedLeftParen();

      for (auto ArgLabel : ArgLabels) {
        if (ArgLabel.empty())
          Builder.addTextChunk("_");
        else
          Builder.addTextChunk(ArgLabel.str());
        Builder.addTextChunk(":");
      }

      if (!HaveRParen)
        Builder.addRightParen();
      else
        Builder.addAnnotatedRightParen();
    }
  }

  // Implement swift::VisibleDeclConsumer.
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
    if (shouldHideDeclFromCompletionResults(D))
      return;

    if (IsKeyPathExpr && !KeyPathFilter(D, Reason))
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
        // Do we want compound function names here?
        if (PreferFunctionReferencesToCalls) {
          addCompoundFunctionName(CD, Reason);
          return;
        }

        if (auto MT = ExprType->getRValueType()->getAs<AnyMetatypeType>()) {
          if (HaveDot) {
            Type Ty;
            for (Ty = MT; Ty && Ty->is<AnyMetatypeType>();
                 Ty = Ty->getAs<AnyMetatypeType>()->getInstanceType());
            assert(Ty && "Cannot find instance type.");

            // Add init() as member of the metatype.
            if (Reason == DeclVisibilityKind::MemberOfCurrentNominal) {
              if (IsStaticMetatype || CD->isRequired() ||
                  !Ty->is<ClassType>())
                addConstructorCall(CD, Reason, None);
            }
            return;
          }
        }

        if (auto MT = ExprType->getAs<AnyMetatypeType>()) {
          if (HaveDot)
            return;

          // If instance type is type alias, showing users that the constructed
          // type is the typealias instead of the underlying type of the alias.
          Optional<Type> Result = None;
          if (auto AT = MT->getInstanceType()) {
            if (AT->getKind() == TypeKind::NameAlias &&
                AT->getDesugaredType() == CD->getResultType().getPointer())
              Result = AT;
          }
          addConstructorCall(CD, Reason, Result);
        }
        if (IsSuperRefExpr) {
          if (!isa<ConstructorDecl>(CurrDeclContext))
            return;
          addConstructorCall(CD, Reason, None);
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
        // We cannot call operators with a postfix parenthesis syntax.
        if (FD->isBinaryOperator() || FD->isUnaryOperator())
          return;

        // We cannot call accessors.  We use VarDecls and SubscriptDecls to
        // produce completions that refer to getters and setters.
        if (FD->isAccessor())
          return;

        // Do we want compound function names here?
        if (PreferFunctionReferencesToCalls) {
          addCompoundFunctionName(FD, Reason);
          return;
        }

        addMethodCall(FD, Reason);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason);
        addConstructorCallsForType(NTD->getType(), NTD->getName(), Reason);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason);
        addConstructorCallsForType(TAD->getUnderlyingType(), TAD->getName(),
                                   Reason);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason);
        for (auto *protocol : GP->getConformingProtocols(nullptr))
          addConstructorCallsForType(protocol->getType(), GP->getName(),
                                     Reason);
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
        // We cannot call operators with a postfix parenthesis syntax.
        if (FD->isBinaryOperator() || FD->isUnaryOperator())
          return;

        // We cannot call accessors.  We use VarDecls and SubscriptDecls to
        // produce completions that refer to getters and setters.
        if (FD->isAccessor())
          return;

        // Do we want compound function names here?
        if (PreferFunctionReferencesToCalls) {
          addCompoundFunctionName(FD, Reason);
          return;
        }

        addMethodCall(FD, Reason);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason);
        addConstructorCallsForType(NTD->getType(), NTD->getName(), Reason);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason);
        addConstructorCallsForType(TAD->getUnderlyingType(), TAD->getName(),
                                   Reason);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason);
        for (auto *protocol : GP->getConformingProtocols(nullptr))
          addConstructorCallsForType(protocol->getType(), GP->getName(),
                                     Reason);
        return;
      }

      if (auto *AT = dyn_cast<AssociatedTypeDecl>(D)) {
        addAssociatedTypeRef(AT, Reason);
        return;
      }

      return;

    case LookupKind::EnumElement:
      handleEnumElement(D, Reason);
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

  bool handleEnumElement(ValueDecl *D, DeclVisibilityKind Reason) {
    if (!D->hasType())
      TypeResolver->resolveDeclSignature(D);

    if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
      addEnumElementRef(EED, Reason, /*HasTypeContext=*/true);
      return true;
    } else if (auto *ED = dyn_cast<EnumDecl>(D)) {
      llvm::DenseSet<EnumElementDecl *> Elements;
      ED->getAllElements(Elements);
      for (auto *Ele : Elements) {
        if (!Ele->hasType())
          TypeResolver->resolveDeclSignature(Ele);
        addEnumElementRef(Ele, Reason, /*HasTypeContext=*/true);
      }
      return true;
    }
    return false;
  }

  void handleOptionSet(Decl *D, DeclVisibilityKind Reason) {
    if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
      if (isOptionSetDecl(NTD)) {
        for (auto M : NTD->getMembers()) {
          if (auto *VD = dyn_cast<VarDecl>(M)) {
            if (isOptionSet(VD->getType()) && VD->isStatic()) {
              addVarDeclRef(VD, Reason);
            }
          }
        }
      }
    }
  }

  bool isOptionSetDecl(NominalTypeDecl *D) {
    auto optionSetType = dyn_cast<ProtocolDecl>(Ctx.getOptionSetDecl());
    if (!optionSetType)
      return false;

    SmallVector<ProtocolConformance *, 1> conformances;
    return D->lookupConformance(CurrDeclContext->getParentModule(),
                                optionSetType, conformances);
  }

  bool isOptionSet(Type Ty) {
    return Ty &&
           Ty->getNominalOrBoundGenericNominal() &&
           isOptionSetDecl(Ty->getNominalOrBoundGenericNominal());
  }

  void getTupleExprCompletions(TupleType *ExprType) {
    unsigned Index = 0;
    for (auto TupleElt : ExprType->getElements()) {
      CodeCompletionResultBuilder Builder(
          Sink,
          CodeCompletionResult::ResultKind::Pattern,
          SemanticContextKind::CurrentNominal, ExpectedTypes);
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
          CodeCompletionResult::MaxNumBytesToErase) {
        if (auto *TT = Unwrapped->getAs<TupleType>()) {
          getTupleExprCompletions(TT);
        } else {
          lookupVisibleMemberDecls(*this, Unwrapped, CurrDeclContext,
                                   TypeResolver.get(),
                                   IncludeInstanceMembers);
        }
      }
    } else if (Type Unwrapped = ExprType->getImplicitlyUnwrappedOptionalObjectType()) {
      lookupVisibleMemberDecls(*this, Unwrapped, CurrDeclContext,
                               TypeResolver.get(),
                               IncludeInstanceMembers);
    } else {
      return false;
    }

    // Ignore the members of Optional, like getLogicValue(), map(), and
    // flatMap().
    //
    // These are not commonly used and cause noise and confusion when showing
    // among the members of the underlying type. If someone really wants to
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
    tryStdlibOptionalCompletions(ExprType);
    if (!Done) {
      lookupVisibleMemberDecls(*this, ExprType, CurrDeclContext,
                               TypeResolver.get(),
                               IncludeInstanceMembers);
    }
  }

  template <typename T>
  void collectOperatorsFromMap(SourceFile::OperatorMap<T> &map,
                               bool includePrivate,
                               std::vector<OperatorDecl *> &results) {
    for (auto &pair : map) {
      if (pair.second.getInt() || includePrivate) {
        results.push_back(pair.second.getPointer());
      }
    }
  }

  void collectOperatorsFrom(SourceFile *SF,
                            std::vector<OperatorDecl *> &results) {
    bool includePrivate = CurrDeclContext->getParentSourceFile() == SF;
    collectOperatorsFromMap(SF->PrefixOperators, includePrivate, results);
    collectOperatorsFromMap(SF->PostfixOperators, includePrivate, results);
    collectOperatorsFromMap(SF->InfixOperators, includePrivate, results);
  }

  void collectOperatorsFrom(LoadedFile *F,
                            std::vector<OperatorDecl *> &results) {
    SmallVector<Decl *, 64> topLevelDecls;
    F->getTopLevelDecls(topLevelDecls);
    for (auto D : topLevelDecls) {
      if (auto op = dyn_cast<OperatorDecl>(D))
        results.push_back(op);
    }
  }

  std::vector<OperatorDecl *> collectOperators() {
    std::vector<OperatorDecl *> results;
    assert(CurrDeclContext);
    CurrDeclContext->getParentSourceFile()->forAllVisibleModules(
    [&](Module::ImportedModule import) {
      for (auto fileUnit : import.second->getFiles()) {
        switch (fileUnit->getKind()) {
        case FileUnitKind::Builtin:
        case FileUnitKind::Derived:
        case FileUnitKind::ClangModule:
          continue;
        case FileUnitKind::Source:
          collectOperatorsFrom(cast<SourceFile>(fileUnit), results);
          break;
        case FileUnitKind::SerializedAST:
          collectOperatorsFrom(cast<LoadedFile>(fileUnit), results);
          break;
        }
      }
    });
    return results;
  }

  void addPostfixBang(Type resultType) {
    CodeCompletionResultBuilder builder(
        Sink, CodeCompletionResult::ResultKind::BuiltinOperator,
        SemanticContextKind::None, {});
    // FIXME: we can't use the exclamation mark chunk kind, or it isn't
    // included in the completion name.
    builder.addTextChunk("!");
    assert(resultType);
    addTypeAnnotation(builder, resultType);
  }

  void addPostfixOperatorCompletion(OperatorDecl *op, Type resultType) {
    // FIXME: we should get the semantic context of the function, not the
    // operator decl.
    auto semanticContext =
        getSemanticContext(op, DeclVisibilityKind::VisibleAtTopLevel);
    CodeCompletionResultBuilder builder(
        Sink, CodeCompletionResult::ResultKind::Declaration, semanticContext,
        {});

    // FIXME: handle variable amounts of space.
    if (HaveLeadingSpace)
      builder.setNumBytesToErase(1);
    builder.setAssociatedDecl(op);
    builder.addTextChunk(op->getName().str());
    assert(resultType);
    addTypeAnnotation(builder, resultType);
  }

  void tryPostfixOperator(Expr *expr, PostfixOperatorDecl *op) {
    if (!expr->getType())
      return;
    // We allocate these expressions on the stack because we know they can't
    // escape and there isn't a better way to allocate scratch Expr nodes.
    UnresolvedDeclRefExpr UDRE(op->getName(), DeclRefKind::PostfixOperator,
                               DeclNameLoc(expr->getSourceRange().End));
    PostfixUnaryExpr opExpr(&UDRE, expr);
    Expr *tempExpr = &opExpr;
    ConcreteDeclRef referencedDecl;
    if (auto T = getTypeOfCompletionContextExpr(
            CurrDeclContext->getASTContext(),
            const_cast<DeclContext *>(CurrDeclContext),
            CompletionTypeCheckKind::Normal,
            tempExpr,
            referencedDecl))
      addPostfixOperatorCompletion(op, *T);
  }

  void addAssignmentOperator(Type RHSType, Type resultType) {
    CodeCompletionResultBuilder builder(
        Sink, CodeCompletionResult::ResultKind::BuiltinOperator,
        SemanticContextKind::None, {});

    if (HaveLeadingSpace)
      builder.addAnnotatedWhitespace(" ");
    else
      builder.addWhitespace(" ");
    builder.addEqual();
    builder.addWhitespace(" ");
    assert(RHSType && resultType);
    builder.addCallParameter(Identifier(), Identifier(), RHSType, false, true);
    addTypeAnnotation(builder, resultType);
  }

  void addInfixOperatorCompletion(OperatorDecl *op, Type resultType,
                                  Type RHSType) {
    // FIXME: we should get the semantic context of the function, not the
    // operator decl.
    auto semanticContext =
        getSemanticContext(op, DeclVisibilityKind::VisibleAtTopLevel);
    CodeCompletionResultBuilder builder(
        Sink, CodeCompletionResult::ResultKind::Declaration, semanticContext,
        {});
    builder.setAssociatedDecl(op);

    if (HaveLeadingSpace)
      builder.addAnnotatedWhitespace(" ");
    else
      builder.addWhitespace(" ");
    builder.addTextChunk(op->getName().str());
    builder.addWhitespace(" ");
    if (RHSType)
      builder.addCallParameter(Identifier(), Identifier(), RHSType, false,
                               true);
    if (resultType)
      addTypeAnnotation(builder, resultType);
  }

  void tryInfixOperatorCompletion(InfixOperatorDecl *op, SequenceExpr *SE) {
    if (op->getName().str() == "~>")
      return;

    MutableArrayRef<Expr *> sequence = SE->getElements();
    assert(sequence.size() >= 3 && !sequence.back() &&
           !sequence.drop_back(1).back() && "sequence not cleaned up");
    assert((sequence.size() & 1) && "sequence expr ending with operator");

    // FIXME: these checks should apply to the LHS of the operator, not the
    // immediately left expression.  Move under the type-checking.
    Expr *LHS = sequence.drop_back(2).back();
    if (LHS->getType() && (LHS->getType()->is<MetatypeType>() ||
                           LHS->getType()->is<AnyFunctionType>()))
      return;

    // We allocate these expressions on the stack because we know they can't
    // escape and there isn't a better way to allocate scratch Expr nodes.
    UnresolvedDeclRefExpr UDRE(op->getName(), DeclRefKind::BinaryOperator,
                               DeclNameLoc(LHS->getEndLoc()));
    sequence.drop_back(1).back() = &UDRE;
    CodeCompletionExpr CCE(LHS->getSourceRange());
    sequence.back() = &CCE;

    SWIFT_DEFER {
      // Reset sequence.
      SE->setElement(SE->getNumElements() - 1, nullptr);
      SE->setElement(SE->getNumElements() - 2, nullptr);
      eraseErrorTypes(SE);

      // Reset any references to operators in types, so they are properly
      // handled as operators by sequence folding.
      //
      // FIXME: Would be better to have some kind of 'OperatorRefExpr'?
      for (auto &element : sequence.drop_back(2)) {
        if (auto operatorRef = element->getMemberOperatorRef()) {
          operatorRef->setType(nullptr);
          element = operatorRef;
        }
      }
    };

    Expr *expr = SE;
    if (!typeCheckCompletionSequence(const_cast<DeclContext *>(CurrDeclContext),
                                     expr)) {

      if (!LHS->getType()->getRValueType()->getAnyOptionalObjectType()) {
        // Don't complete optional operators on non-optional types.
        // FIXME: can we get the type-checker to disallow these for us?
        if (op->getName().str() == "??")
          return;
        if (auto NT = CCE.getType()->getNominalOrBoundGenericNominal()) {
          if (NT->getName() ==
              CurrDeclContext->getASTContext().Id_OptionalNilComparisonType)
            return;
        }
      }

      addInfixOperatorCompletion(op, expr->getType(), CCE.getType());
    }
  }

  void flattenBinaryExpr(BinaryExpr *expr, SmallVectorImpl<Expr *> &sequence) {
    auto LHS = expr->getArg()->getElement(0);
    if (auto binexpr = dyn_cast<BinaryExpr>(LHS))
      flattenBinaryExpr(binexpr, sequence);
    else
      sequence.push_back(LHS);

    sequence.push_back(expr->getFn());

    auto RHS = expr->getArg()->getElement(1);
    if (auto binexpr = dyn_cast<BinaryExpr>(RHS))
      flattenBinaryExpr(binexpr, sequence);
    else
      sequence.push_back(RHS);
  }

  void typeCheckLeadingSequence(SmallVectorImpl<Expr *> &sequence) {
    Expr *expr =
        SequenceExpr::create(CurrDeclContext->getASTContext(), sequence);
    eraseErrorTypes(expr);
    // Take advantage of the fact the type-checker leaves the types on the AST.
    if (!typeCheckExpression(const_cast<DeclContext *>(CurrDeclContext),
                             expr)) {
      if (auto binexpr = dyn_cast<BinaryExpr>(expr)) {
        // Rebuild the sequence from the type-checked version.
        sequence.clear();
        flattenBinaryExpr(binexpr, sequence);
        return;
      }
    }

    // Fall back to just using the immediate LHS.
    auto LHS = sequence.back();
    sequence.clear();
    sequence.push_back(LHS);
  }

  void getOperatorCompletions(Expr *LHS, ArrayRef<Expr *> leadingSequence) {
    std::vector<OperatorDecl *> operators = collectOperators();

    // FIXME: this always chooses the first operator with the given name.
    llvm::DenseSet<Identifier> seenPostfixOperators;
    llvm::DenseSet<Identifier> seenInfixOperators;

    SmallVector<Expr *, 3> sequence(leadingSequence.begin(),
                                    leadingSequence.end());
    sequence.push_back(LHS);
    assert((sequence.size() & 1) && "sequence expr ending with operator");

    if (sequence.size() > 1)
      typeCheckLeadingSequence(sequence);

    // Create a single sequence expression, which we will modify for each
    // operator, filling in the operator and dummy right-hand side.
    sequence.push_back(nullptr); // operator
    sequence.push_back(nullptr); // RHS
    auto *SE = SequenceExpr::create(CurrDeclContext->getASTContext(), sequence);
    eraseErrorTypes(SE);

    for (auto op : operators) {
      switch (op->getKind()) {
      case DeclKind::PrefixOperator:
        // Don't insert prefix operators in postfix position.
        // FIXME: where should these get completed?
        break;
      case DeclKind::PostfixOperator:
        if (seenPostfixOperators.insert(op->getName()).second)
          tryPostfixOperator(LHS, cast<PostfixOperatorDecl>(op));
        break;
      case DeclKind::InfixOperator:
        if (seenInfixOperators.insert(op->getName()).second)
          tryInfixOperatorCompletion(cast<InfixOperatorDecl>(op), SE);
        break;
      default:
        llvm_unreachable("unexpected operator kind");
      }
    }

    if (leadingSequence.empty() && LHS->getType() &&
        LHS->getType()->isLValueType()) {
      addAssignmentOperator(LHS->getType()->getRValueType(),
                            CurrDeclContext->getASTContext().TheEmptyTupleType);
    }

    // FIXME: unify this with the ?.member completions.
    if (auto T = LHS->getType())
      if (auto ValueT = T->getRValueType()->getOptionalObjectType())
        addPostfixBang(ValueT);
  }

  void addValueLiteralCompletions() {
    auto &context = CurrDeclContext->getASTContext();
    auto *module = CurrDeclContext->getParentModule();

    auto addFromProto = [&](
        CodeCompletionLiteralKind kind, StringRef defaultTypeName,
        llvm::function_ref<void(CodeCompletionResultBuilder &)> consumer,
        bool isKeyword = false) {

      CodeCompletionResultBuilder builder(Sink, CodeCompletionResult::Literal,
                                          SemanticContextKind::None, {});
      builder.setLiteralKind(kind);

      consumer(builder);

      // Check for matching ExpectedTypes.
      auto *P = context.getProtocol(protocolForLiteralKind(kind));
      bool foundConformance = false;
      for (auto T : ExpectedTypes) {
        if (!T)
          continue;

        auto typeRelation = CodeCompletionResult::Identical;
        // Convert through optional types unless we're looking for a protocol
        // that Optional itself conforms to.
        if (kind != CodeCompletionLiteralKind::NilLiteral) {
          if (auto optionalObjT = T->getAnyOptionalObjectType()) {
            T = optionalObjT;
            typeRelation = CodeCompletionResult::Convertible;
          }
        }

        // Check for conformance to the literal protocol.
        if (auto *NTD = T->getAnyNominal()) {
          SmallVector<ProtocolConformance *, 2> conformances;
          if (NTD->lookupConformance(module, P, conformances)) {
            foundConformance = true;
            addTypeAnnotation(builder, T);
            builder.setExpectedTypeRelation(typeRelation);
          }
        }
      }

      // Fallback to showing the default type.
      if (!foundConformance && !defaultTypeName.empty())
        builder.addTypeAnnotation(defaultTypeName);
    };

    // FIXME: the pedantically correct way is to resolve Swift.*LiteralType.

    using LK = CodeCompletionLiteralKind;
    using Builder = CodeCompletionResultBuilder;

    // Add literal completions that conform to specific protocols.
    addFromProto(LK::IntegerLiteral, "Int", [](Builder &builder) {
      builder.addTextChunk("0");
    });
    addFromProto(LK::BooleanLiteral, "Bool", [](Builder &builder) {
      builder.addTextChunk("true");
    }, /*isKeyword=*/true);
    addFromProto(LK::BooleanLiteral, "Bool", [](Builder &builder) {
      builder.addTextChunk("false");
    }, /*isKeyword=*/true);
    addFromProto(LK::NilLiteral, "", [](Builder &builder) {
      builder.addTextChunk("nil");
    }, /*isKeyword=*/true);
    addFromProto(LK::StringLiteral, "String", [&](Builder &builder) {
      builder.addTextChunk("\"");
      builder.addSimpleNamedParameter("abc");
      builder.addTextChunk("\"");
    });
    addFromProto(LK::ArrayLiteral, "Array", [&](Builder &builder) {
      builder.addLeftBracket();
      builder.addSimpleNamedParameter("values");
      builder.addRightBracket();
    });
    addFromProto(LK::DictionaryLiteral, "Dictionary", [&](Builder &builder) {
      builder.addLeftBracket();
      builder.addSimpleNamedParameter("key");
      builder.addTextChunk(": ");
      builder.addSimpleNamedParameter("value");
      builder.addRightBracket();
    });

    auto floatType = context.getFloatDecl()->getDeclaredType();
    addFromProto(LK::ColorLiteral, "", [&](Builder &builder) {
      builder.addTextChunk("#colorLiteral");
      builder.addLeftParen();
      builder.addCallParameter(context.getIdentifier("red"),
                               floatType, false, true);
      builder.addComma();
      builder.addCallParameter(context.getIdentifier("green"), floatType,
                               false, true);
      builder.addComma();
      builder.addCallParameter(context.getIdentifier("blue"), floatType,
                               false, true);
      builder.addComma();
      builder.addCallParameter(context.getIdentifier("alpha"), floatType,
                               false, true);
      builder.addRightParen();
    });

    auto stringType = context.getStringDecl()->getDeclaredType();
    addFromProto(LK::ImageLiteral, "", [&](Builder &builder) {
      builder.addTextChunk("#imageLiteral");
      builder.addLeftParen();
      builder.addCallParameter(context.getIdentifier("resourceName"),
                               stringType, false, true);
      builder.addRightParen();
    });

    // Add tuple completion (item, item).
    {
      CodeCompletionResultBuilder builder(Sink, CodeCompletionResult::Literal,
                                          SemanticContextKind::None, {});
      builder.setLiteralKind(LK::Tuple);

      builder.addLeftParen();
      builder.addSimpleNamedParameter("values");
      builder.addRightParen();
      for (auto T : ExpectedTypes) {
        if (!T)
          continue;
        if (T->getAs<TupleType>()) {
          addTypeAnnotation(builder, T);
          builder.setExpectedTypeRelation(CodeCompletionResult::Identical);
          break;
        }
      }
    }
  }

  struct FilteredDeclConsumer : public swift::VisibleDeclConsumer {
    swift::VisibleDeclConsumer &Consumer;
    DeclFilter Filter;
    FilteredDeclConsumer(swift::VisibleDeclConsumer &Consumer,
                         DeclFilter Filter) : Consumer(Consumer), Filter(Filter) {}
    void foundDecl(ValueDecl *VD, DeclVisibilityKind Kind) override {
      if (Filter(VD, Kind))
        Consumer.foundDecl(VD, Kind);
    }
  };

  void getValueCompletionsInDeclContext(SourceLoc Loc,
                                        DeclFilter Filter = DefaultFilter,
                                        bool IncludeTopLevel = false,
                                        bool RequestCache = true,
                                        bool LiteralCompletions = true) {
    Kind = LookupKind::ValueInDeclContext;
    NeedLeadingDot = false;
    FilteredDeclConsumer Consumer(*this, Filter);
    lookupVisibleDecls(Consumer, CurrDeclContext, TypeResolver.get(),
                       /*IncludeTopLevel=*/IncludeTopLevel, Loc);
    if (RequestCache)
      RequestedCachedResults = RequestedResultsTy::toplevelResults();

    // Manually add any expected nominal types from imported modules so that
    // they get their expected type relation. Don't include protocols, since
    // they can't be initialized from the type name.
    // FIXME: this does not include types that conform to an expected protocol.
    // FIXME: this creates duplicate results.
    for (auto T : ExpectedTypes) {
      if (auto NT = T->getAs<NominalType>()) {
        if (auto NTD = NT->getDecl()) {
          if (!isa<ProtocolDecl>(NTD) &&
              NTD->getModuleContext() != CurrDeclContext->getParentModule()) {
            addNominalTypeRef(NT->getDecl(),
                              DeclVisibilityKind::VisibleAtTopLevel);
          }
        }
      }
    }

    if (LiteralCompletions)
      addValueLiteralCompletions();

    // If the expected type is ObjectiveC.Selector, add #selector. If
    // it's String, add #keyPath.
    if (Ctx.LangOpts.EnableObjCInterop) {
      bool addedSelector = false;
      bool addedKeyPath = false;
      for (auto T : ExpectedTypes) {
        T = T->lookThroughAllAnyOptionalTypes();
        if (auto structDecl = T->getStructOrBoundGenericStruct()) {
          if (!addedSelector &&
              structDecl->getName() == Ctx.Id_Selector &&
              structDecl->getParentModule()->getName() == Ctx.Id_ObjectiveC) {
            addPoundSelector(/*needPound=*/true);
            if (addedKeyPath) break;
            addedSelector = true;
            continue;
          }
        }

        if (!addedKeyPath && T->getAnyNominal() == Ctx.getStringDecl()) {
          addPoundKeyPath(/*needPound=*/true);
          if (addedSelector) break;
          addedKeyPath = true;
          continue;
        }
      }
    }
  }

  struct LookupByName : public swift::VisibleDeclConsumer {
    CompletionLookup &Lookup;
    std::vector<std::string> &SortedNames;
    llvm::SmallPtrSet<Decl*, 3> HandledDecls;

    bool isNameHit(StringRef Name) {
      return std::binary_search(SortedNames.begin(), SortedNames.end(), Name);
    }

    void collectEnumElementTypes(EnumElementDecl *EED) {
      if (isNameHit(EED->getNameStr()) && EED->getType()) {
        unboxType(EED->getType());
      }
    }

    void unboxType(Type T) {
      if (T->getKind() == TypeKind::Paren) {
        unboxType(T->getDesugaredType());
      } else if (T->getKind() == TypeKind::Tuple) {
        for (auto Ele : T->getAs<TupleType>()->getElements()) {
          unboxType(Ele.getType());
        }
      } else if (auto FT = T->getAs<FunctionType>()) {
        unboxType(FT->getInput());
        unboxType(FT->getResult());
      } else if (auto NTD = T->getNominalOrBoundGenericNominal()){
        if (HandledDecls.count(NTD) == 0) {
          auto Reason = DeclVisibilityKind::MemberOfCurrentNominal;
          if (!Lookup.handleEnumElement(NTD, Reason)) {
            Lookup.handleOptionSet(NTD, Reason);
          }
          HandledDecls.insert(NTD);
        }
      }
    }

    LookupByName(CompletionLookup &Lookup, std::vector<std::string> &SortedNames) :
                   Lookup(Lookup), SortedNames(SortedNames) {
      std::sort(SortedNames.begin(), SortedNames.end());
    }

    void handleDeclRange(const DeclRange &Members,
                         DeclVisibilityKind Reason) {
      for (auto M : Members) {
        if (auto VD = dyn_cast<ValueDecl>(M)) {
          foundDecl(VD, Reason);
        }
      }
    }

    void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
      if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
        if (isNameHit(NTD->getNameStr())) {
          unboxType(NTD->getDeclaredType());
        }
        handleDeclRange(NTD->getMembers(), Reason);
        for (auto Ex : NTD->getExtensions()) {
          handleDeclRange(Ex->getMembers(), Reason);
        }
      } else if (isNameHit(VD->getNameStr())) {
        if (VD->hasType())
          unboxType(VD->getType());
      }
    }
  };

  void getUnresolvedMemberCompletions(SourceLoc Loc, SmallVectorImpl<Type> &Types) {
    NeedLeadingDot = !HaveDot;
    for (auto T : Types) {
      if (T && T->getNominalOrBoundGenericNominal()) {
        auto Reason = DeclVisibilityKind::MemberOfCurrentNominal;
        if (!handleEnumElement(T->getNominalOrBoundGenericNominal(), Reason)) {
          handleOptionSet(T->getNominalOrBoundGenericNominal(), Reason);
        }
      }
    }
  }

  void getUnresolvedMemberCompletions(SourceLoc Loc,
                                      std::vector<std::string> &FuncNames,
                                      bool HasReturn) {
    NeedLeadingDot = !HaveDot;
    LookupByName Lookup(*this, FuncNames);
    lookupVisibleDecls(Lookup, CurrDeclContext, TypeResolver.get(), true);
    if (HasReturn)
      if (auto ReturnType = getReturnTypeFromContext(CurrDeclContext))
        Lookup.unboxType(ReturnType);
  }

  static bool getPositionInTupleExpr(DeclContext &DC, Expr *Target,
                                     TupleExpr *Tuple, unsigned &Pos,
                                     bool &HasName,
                                     llvm::SmallVectorImpl<Type> &TupleEleTypes) {
    auto &SM = DC.getASTContext().SourceMgr;
    Pos = 0;
    for (auto E : Tuple->getElements()) {
      if (SM.isBeforeInBuffer(E->getEndLoc(), Target->getStartLoc())) {
        TupleEleTypes.push_back(E->getType());
        Pos ++;
        continue;
      }
      HasName = !Tuple->getElementName(Pos).empty();
      return true;
    }
    return false;
  }

  void addArgNameCompletionResults(ArrayRef<StringRef> Names) {
    for (auto Name : Names) {
      CodeCompletionResultBuilder Builder(Sink,
                                          CodeCompletionResult::ResultKind::Keyword,
                                          SemanticContextKind::ExpressionSpecific, {});
      Builder.addTextChunk(Name);
      Builder.addCallParameterColon();
      Builder.addTypeAnnotation("Argument name");
    }
  }

  static void collectArgumentExpectation(unsigned Position, bool HasName,
                                         ArrayRef<Type> Types, SourceLoc Loc,
                                         std::vector<Type> &ExpectedTypes,
                                         std::vector<StringRef> &ExpectedNames) {
    SmallPtrSet<TypeBase *, 4> seenTypes;
    SmallPtrSet<const char *, 4> seenNames;

    for (auto Type : Types) {
      if (auto TT = Type->getAs<TupleType>()) {
        if (Position >= TT->getElements().size()) {
          continue;
        }
        auto Ele = TT->getElement(Position);
        if (Ele.hasName() && !HasName) {
          if (seenNames.insert(Ele.getName().get()).second)
            ExpectedNames.push_back(Ele.getName().str());
        } else {
          if (seenTypes.insert(Ele.getType().getPointer()).second)
            ExpectedTypes.push_back(Ele.getType());
        }
      } else if (Position == 0) {
        // The only param.
        TypeBase *T = Type->getDesugaredType();
        if (seenTypes.insert(T).second)
          ExpectedTypes.push_back(T);
      }
    }
  }

  bool lookupArgCompletionsAtPosition(unsigned Position, bool HasName,
                                      ArrayRef<Type> Types, SourceLoc Loc) {
    std::vector<Type> ExpectedTypes;
    std::vector<StringRef> ExpectedNames;
    collectArgumentExpectation(Position, HasName, Types, Loc, ExpectedTypes,
                               ExpectedNames);
    addArgNameCompletionResults(ExpectedNames);
    if (!ExpectedTypes.empty()) {
      setExpectedTypes(ExpectedTypes);
      getValueCompletionsInDeclContext(Loc, DefaultFilter);
    }
    return true;
  }

  static bool isPotentialSignatureMatch(ArrayRef<Type> TupleEles,
                                        ArrayRef<Type> ExprTypes,
                                        DeclContext *DC) {
    // Not likely to be a match if users provide more arguments than expected.
    if (ExprTypes.size() >= TupleEles.size())
      return false;
    for (unsigned I = 0; I < ExprTypes.size(); ++ I) {
      auto Ty = ExprTypes[I];
      if (Ty && !Ty->is<ErrorType>()) {
        if (!isConvertibleTo(Ty, TupleEles[I], *DC)) {
          return false;
        }
      }
    }
    return true;
  }

  static void removeUnlikelyOverloads(SmallVectorImpl<Type> &PossibleArgTypes,
                                      ArrayRef<Type> TupleEleTypes,
                                      DeclContext *DC) {
    for (auto It = PossibleArgTypes.begin(); It != PossibleArgTypes.end(); ) {
      llvm::SmallVector<Type, 3> ExpectedTypes;
      if ((*It)->getKind() == TypeKind::Tuple) {
        auto Elements = (*It)->getAs<TupleType>()->getElements();
        for (auto Ele : Elements)
          ExpectedTypes.push_back(Ele.getType());
      } else {
        ExpectedTypes.push_back(*It);
      }
      if (isPotentialSignatureMatch(ExpectedTypes, TupleEleTypes, DC)) {
        ++ It;
      } else {
        PossibleArgTypes.erase(It);
      }
    }
  }

  static bool collectPossibleArgTypes(DeclContext &DC, CallExpr *CallE, Expr *CCExpr,
                                      SmallVectorImpl<Type> &PossibleTypes,
                                      unsigned &Position, bool &HasName,
                                      bool RemoveUnlikelyOverloads) {
    if (auto Ty = CallE->getFn()->getType()) {
      if (auto FT = Ty->getAs<FunctionType>()) {
        PossibleTypes.push_back(FT->getInput());
      }
    }
    if (auto TAG = dyn_cast<TupleExpr>(CallE->getArg())) {
      llvm::SmallVector<Type, 3> TupleEleTypesBeforeTarget;
      if (!getPositionInTupleExpr(DC, CCExpr, TAG, Position, HasName,
                                  TupleEleTypesBeforeTarget))
        return false;
      if (PossibleTypes.empty() &&
          !typeCheckUnresolvedExpr(DC, CallE->getArg(), CallE, PossibleTypes))
        return false;
      if (RemoveUnlikelyOverloads) {
        removeUnlikelyOverloads(PossibleTypes, TupleEleTypesBeforeTarget, &DC);
        return !PossibleTypes.empty();
      }
    } else if (CallE->getArg()->getKind() == ExprKind::Paren) {
      Position = 0;
      HasName = false;
      if (PossibleTypes.empty() &&
          !typeCheckUnresolvedExpr(DC, CallE->getArg(), CallE, PossibleTypes))
        return false;
    } else
      return false;
    return true;
  }

  static bool
  collectArgumentExpectation(DeclContext &DC, CallExpr *CallE, Expr *CCExpr,
                             std::vector<Type> &ExpectedTypes,
                             std::vector<StringRef> &ExpectedNames) {
    SmallVector<Type, 2> PossibleTypes;
    unsigned Position;
    bool HasName;
    if (collectPossibleArgTypes(DC, CallE, CCExpr, PossibleTypes, Position,
                                HasName, true)) {
      collectArgumentExpectation(Position, HasName, PossibleTypes,
                                 CCExpr->getStartLoc(), ExpectedTypes, ExpectedNames);
      return !ExpectedTypes.empty() || !ExpectedNames.empty();
    }
    return false;
  }

  bool getCallArgCompletions(DeclContext &DC, CallExpr *CallE, Expr *CCExpr) {
    SmallVector<Type, 2> PossibleTypes;
    unsigned Position;
    bool HasName;
    return collectPossibleArgTypes(DC, CallE, CCExpr, PossibleTypes, Position,
                                   HasName, true) &&
           lookupArgCompletionsAtPosition(Position, HasName, PossibleTypes,
                                          CCExpr->getStartLoc());
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
                             CurrDeclContext, TypeResolver.get(),
                             IncludeInstanceMembers);
    addKeyword("Type", MetaBase);
    addKeyword("self", BaseType, SemanticContextKind::CurrentNominal);
  }

  void getAttributeDeclCompletions(bool IsInSil, Optional<DeclKind> DK) {
    // FIXME: also include user-defined attribute keywords
    StringRef TargetName = "Declaration";
    if (DK.hasValue()) {
      switch (DK.getValue()) {
#define DECL(Id, ...)                                                         \
      case DeclKind::Id:                                                      \
        TargetName = #Id;                                                     \
        break;
#include "swift/AST/DeclNodes.def"
      }
    }
    std::string Description = TargetName.str() + " Attribute";
#define DECL_ATTR(KEYWORD, NAME, ...)                                         \
    if (!DeclAttribute::isUserInaccessible(DAK_##NAME) &&                     \
        !DeclAttribute::isDeclModifier(DAK_##NAME) &&                         \
        !DeclAttribute::shouldBeRejectedByParser(DAK_##NAME) &&               \
        (!DeclAttribute::isSilOnly(DAK_##NAME) || IsInSil)) {                 \
          if (!DK.hasValue() || DeclAttribute::canAttributeAppearOnDeclKind   \
            (DAK_##NAME, DK.getValue()))                                      \
              addDeclAttrKeyword(#KEYWORD, Description);                      \
    }
#include "swift/AST/Attr.def"
  }

  void getAttributeDeclParamCompletions(DeclAttrKind AttrKind, int ParamIndex) {
    if (AttrKind == DAK_Available) {
      if (ParamIndex == 0) {
        addDeclAttrParamKeyword("*", "Platform", false);
#define AVAILABILITY_PLATFORM(X, PrettyName)                                  \
        addDeclAttrParamKeyword(#X, "Platform", false);
#include "swift/AST/PlatformKinds.def"
      } else {
        addDeclAttrParamKeyword("unavailable", "", false);
        addDeclAttrParamKeyword("message", "Specify message", true);
        addDeclAttrParamKeyword("renamed", "Specify replacing name", true);
        addDeclAttrParamKeyword("introduced", "Specify version number", true);
        addDeclAttrParamKeyword("deprecated", "Specify version number", true);
      }
    }
  }

  void getPoundAvailablePlatformCompletions() {

    // The platform names should be identical to those in @available.
    getAttributeDeclParamCompletions(DAK_Available, 0);
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
  SmallVectorImpl<StringRef> &ParsedKeywords;

  bool hasFuncIntroducer = false;
  bool hasVarIntroducer = false;

public:
  CompletionOverrideLookup(CodeCompletionResultSink &Sink, ASTContext &Ctx,
                           const DeclContext *CurrDeclContext,
                           SmallVectorImpl<StringRef> &ParsedKeywords)
      : Sink(Sink), TypeResolver(createLazyResolver(Ctx)),
        CurrDeclContext(CurrDeclContext), ParsedKeywords(ParsedKeywords) {
    hasFuncIntroducer = isKeywordSpecified("func");
    hasVarIntroducer = isKeywordSpecified("var") || isKeywordSpecified("let");
  }

  bool isKeywordSpecified(StringRef Word) {
    return std::find(ParsedKeywords.begin(), ParsedKeywords.end(), Word)
      != ParsedKeywords.end();
  }

  void addValueOverride(const ValueDecl *VD, DeclVisibilityKind Reason,
                        CodeCompletionResultBuilder &Builder,
                        StringRef DeclIntroducer = "") {

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
      if (auto transformType = CurrDeclContext->getDeclaredTypeInContext())
        Options.setArchetypeSelfTransform(transformType, VD->getDeclContext());
      Options.PrintDefaultParameterPlaceholder = false;
      Options.PrintImplicitAttrs = false;
      Options.SkipAttributes = true;
      Options.PrintOverrideKeyword = false;
      Options.PrintPropertyAccessors = false;
      VD->print(Printer, Options);
      NameOffset = Printer.NameOffset.getValue();
    }

    assert(CurrDeclContext->getAsGenericTypeOrGenericTypeExtensionContext());
    Accessibility AccessibilityOfContext =
        CurrDeclContext->getAsGenericTypeOrGenericTypeExtensionContext()
            ->getFormalAccess();

    bool missingDeclIntroducer = !hasVarIntroducer && !hasFuncIntroducer;
    bool missingAccess = !isKeywordSpecified("private") &&
                         !isKeywordSpecified("fileprivate") &&
                         !isKeywordSpecified("internal") &&
                         !isKeywordSpecified("public");
    bool missingOverride = Reason == DeclVisibilityKind::MemberOfSuper &&
                           !isKeywordSpecified("override");

    if (missingDeclIntroducer && missingAccess)
      Builder.addAccessControlKeyword(
          std::min(VD->getFormalAccess(), AccessibilityOfContext));

    // FIXME: if we're missing 'override', but have the decl introducer we
    // should delete it and re-add both in the correct order.
    if (missingDeclIntroducer && missingOverride)
      Builder.addOverrideKeyword();

    if (missingDeclIntroducer) {
      if (DeclIntroducer.empty())
        Builder.addDeclIntroducer(DeclStr.str().substr(0, NameOffset));
      else
        Builder.addDeclIntroducer(DeclIntroducer);
    }

    Builder.addTextChunk(DeclStr.str().substr(NameOffset));
  }

  void addMethodOverride(const FuncDecl *FD, DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(FD);
    addValueOverride(FD, Reason, Builder);
    Builder.addBraceStmtWithCursor();
  }

  void addVarOverride(const VarDecl *VD, DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(VD);
    addValueOverride(VD, Reason, Builder);
  }

  void addTypeAlias(const AssociatedTypeDecl *ATD, DeclVisibilityKind Reason) {
    CodeCompletionResultBuilder Builder(Sink,
      CodeCompletionResult::ResultKind::Declaration,
      SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(ATD);
    addValueOverride(ATD, Reason, Builder, "typealias ");
    Builder.addTextChunk(" = ");
    Builder.addSimpleNamedParameter("Type");
  }

  void addConstructor(const ConstructorDecl *CD) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(CD);

    llvm::SmallString<256> DeclStr;
    {
      llvm::raw_svector_ostream OS(DeclStr);
      PrintOptions Options;
      Options.PrintImplicitAttrs = false;
      Options.SkipAttributes = true;
      Options.PrintDefaultParameterPlaceholder = false;
      CD->print(OS, Options);
    }
    Builder.addTextChunk(DeclStr);
    Builder.addBraceStmtWithCursor();
  }

  llvm::StringSet<> SatisfiedAssociatedTypes;

  // Implement swift::VisibleDeclConsumer.
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
    if (Reason == DeclVisibilityKind::MemberOfCurrentNominal) {
      if (D->getKind() == DeclKind::TypeAlias) {
        ValueDecl *VD = dyn_cast<ValueDecl>(D);
        SatisfiedAssociatedTypes.insert(VD->getName().str());
      }
      return;
    }

    if (shouldHideDeclFromCompletionResults(D))
      return;

    if (D->getAttrs().hasAttribute<FinalAttr>())
      return;

    if (!D->hasType())
      TypeResolver->resolveDeclSignature(D);

    bool hasIntroducer = hasFuncIntroducer || hasVarIntroducer;

    if (auto *FD = dyn_cast<FuncDecl>(D)) {
      // We cannot override operators as members.
      if (FD->isBinaryOperator() || FD->isUnaryOperator())
        return;

      // We cannot override individual accessors.
      if (FD->isAccessor())
        return;

      if (!hasIntroducer || hasFuncIntroducer)
        addMethodOverride(FD, Reason);
      return;
    }

    if (auto *VD = dyn_cast<VarDecl>(D)) {
      if (!hasIntroducer || hasVarIntroducer) {
        addVarOverride(VD, Reason);
      }
    }

    if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
      if (!isa<ProtocolDecl>(CD->getDeclContext()))
        return;
      if (hasIntroducer || isKeywordSpecified("override"))
        return;
      if (CD->isRequired() || CD->isDesignatedInit())
        addConstructor(CD);
      return;
    }
  }

  void addDesignatedInitializers(Type CurrTy) {
    if (hasFuncIntroducer || hasVarIntroducer || isKeywordSpecified("override"))
      return;

    assert(CurrTy);
    const auto *CD = dyn_cast_or_null<ClassDecl>(CurrTy->getAnyNominal());
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

  void addAssociatedTypes(Type CurrTy) {
    if (hasFuncIntroducer || hasVarIntroducer || isKeywordSpecified("override"))
      return;

    NominalTypeDecl *NTD = CurrTy->getAnyNominal();

    for (auto Conformance : NTD->getAllConformances()) {
      auto Proto = Conformance->getProtocol();
      if (!Proto->isAccessibleFrom(CurrDeclContext))
        continue;
      auto NormalConformance = Conformance->getRootNormalConformance();
      for (auto Member : Proto->getMembers()) {
        auto *ATD = dyn_cast<AssociatedTypeDecl>(Member);
        if (!ATD)
          continue;
        // FIXME: Also exclude the type alias that has already been specified.
        if (!NormalConformance->hasTypeWitness(ATD) ||
            !ATD->getDefaultDefinitionLoc().isNull())
          continue;
        addTypeAlias(ATD,
          DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal);
      }
    }
  }

  void getOverrideCompletions(SourceLoc Loc) {
    if (!CurrDeclContext->getAsGenericTypeOrGenericTypeExtensionContext())
      return;

    Type CurrTy = CurrDeclContext->getDeclaredTypeInContext();
    if (CurrTy && !CurrTy->is<ErrorType>()) {
      lookupVisibleMemberDecls(*this, CurrTy, CurrDeclContext,
                               TypeResolver.get(),
                               /*includeInstanceMembers=*/false);
      addDesignatedInitializers(CurrTy);
      addAssociatedTypes(CurrTy);
    }
  }
};
} // end anonymous namespace

static void addSelectorModifierKeywords(CodeCompletionResultSink &sink) {
  auto addKeyword = [&](StringRef Name, CodeCompletionKeywordKind Kind) {
    CodeCompletionResultBuilder Builder(
                                  sink,
                                  CodeCompletionResult::ResultKind::Keyword,
                                  SemanticContextKind::None, {});
    Builder.setKeywordKind(Kind);
    Builder.addTextChunk(Name);
    Builder.addCallParameterColon();
    Builder.addSimpleTypedParameter("@objc property", /*isVarArg=*/false);
  };

  addKeyword("getter", CodeCompletionKeywordKind::None);
  addKeyword("setter", CodeCompletionKeywordKind::None);
}

void CodeCompletionCallbacksImpl::completeDotExpr(Expr *E, SourceLoc DotLoc) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::DotExpr;
  if (ParseExprSelectorContext != ObjCSelectorContext::None) {
    PreferFunctionReferencesToCalls = true;
    CompleteExprSelectorContext = ParseExprSelectorContext;
  }

  ParsedExpr = E;
  this->DotLoc = DotLoc;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeStmtOrExpr() {
  assert(P.Tok.is(tok::code_complete));
  Kind = CompletionKind::StmtOrExpr;
  CurDeclContext = P.CurDeclContext;
  CStyleForLoopIterationVariable =
      CodeCompletionCallbacks::CStyleForLoopIterationVariable;
}

void CodeCompletionCallbacksImpl::completePostfixExprBeginning(CodeCompletionExpr *E) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::PostfixExprBeginning;
  if (ParseExprSelectorContext != ObjCSelectorContext::None) {
    PreferFunctionReferencesToCalls = true;
    CompleteExprSelectorContext = ParseExprSelectorContext;
    if (CompleteExprSelectorContext == ObjCSelectorContext::MethodSelector) {
      addSelectorModifierKeywords(CompletionContext.getResultSink());
    }
  }


  CurDeclContext = P.CurDeclContext;
  CStyleForLoopIterationVariable =
      CodeCompletionCallbacks::CStyleForLoopIterationVariable;
  CodeCompleteTokenExpr = E;
}

void CodeCompletionCallbacksImpl::completePostfixExpr(Expr *E, bool hasSpace) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  HasSpace = hasSpace;
  Kind = CompletionKind::PostfixExpr;
  if (ParseExprSelectorContext != ObjCSelectorContext::None) {
    PreferFunctionReferencesToCalls = true;
    CompleteExprSelectorContext = ParseExprSelectorContext;
  }

  ParsedExpr = E;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completePostfixExprParen(Expr *E,
                                                           Expr *CodeCompletionE) {
  assert(P.Tok.is(tok::code_complete));

  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::PostfixExprParen;
  ParsedExpr = E;
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = static_cast<CodeCompletionExpr*>(CodeCompletionE);

  // Lookahead one token to decide what kind of call completions to provide.
  // When it appears that there is already code for the call present, just
  // complete values and/or argument labels.  Otherwise give the entire call
  // pattern.
  Token next = P.peekToken();
  if (next.isAtStartOfLine() || next.is(tok::eof)) {
    ShouldCompleteCallPatternAfterParen = true;
  } else if (next.is(tok::r_paren)) {
    HasRParen = true;
    ShouldCompleteCallPatternAfterParen = true;
  } else {
    ShouldCompleteCallPatternAfterParen = false;
  }
}

void CodeCompletionCallbacksImpl::completeExprSuper(SuperRefExpr *SRE) {
  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::SuperExpr;
  if (ParseExprSelectorContext != ObjCSelectorContext::None) {
    PreferFunctionReferencesToCalls = true;
    CompleteExprSelectorContext = ParseExprSelectorContext;
  }

  ParsedExpr = SRE;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeExprSuperDot(SuperRefExpr *SRE) {
  // Don't produce any results in an enum element.
  if (InEnumElementRawValue)
    return;

  Kind = CompletionKind::SuperExprDot;
  if (ParseExprSelectorContext != ObjCSelectorContext::None) {
    PreferFunctionReferencesToCalls = true;
    CompleteExprSelectorContext = ParseExprSelectorContext;
  }

  ParsedExpr = SRE;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeExprKeyPath(ObjCKeyPathExpr *KPE,
                                                      bool HasDot) {
  Kind = HasDot ? CompletionKind::KeyPathExprDot : CompletionKind::KeyPathExpr;
  ParsedExpr = KPE;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completePoundAvailablePlatform() {
  Kind = CompletionKind::PoundAvailablePlatform;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeSimpleBeginning() {
  Kind = CompletionKind::TypeSimpleBeginning;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeDeclAttrParam(DeclAttrKind DK,
                                                        int Index) {
  Kind = CompletionKind::AttributeDeclParen;
  AttrKind = DK;
  AttrParamIndex = Index;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeDeclAttrKeyword(Decl *D,
                                                          bool Sil,
                                                          bool Param) {
  Kind = CompletionKind::AttributeBegin;
  IsInSil = Sil;
  if (Param) {
    AttTargetDK = DeclKind::Param;
  } else if (D) {
    AttTargetDK = D->getKind();
  }
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

void CodeCompletionCallbacksImpl::completeImportDecl(
    std::vector<std::pair<Identifier, SourceLoc>> &Path) {
  Kind = CompletionKind::Import;
  CurDeclContext = P.CurDeclContext;
  DotLoc = Path.empty() ? SourceLoc() : Path.back().second;
  if (DotLoc.isInvalid())
    return;
  auto Importer = static_cast<ClangImporter *>(CurDeclContext->getASTContext().
                                               getClangModuleLoader());
  std::vector<std::string> SubNames;
  Importer->collectSubModuleNames(Path, SubNames);
  ASTContext &Ctx = CurDeclContext->getASTContext();
  for (StringRef Sub : SubNames) {
    Path.push_back(std::make_pair(Ctx.getIdentifier(Sub), SourceLoc()));
    SubModuleNameVisibilityPairs.push_back(
      std::make_pair(Sub.str(), Ctx.getLoadedModule(Path)));
    Path.pop_back();
  }
}

void CodeCompletionCallbacksImpl::completeUnresolvedMember(UnresolvedMemberExpr *E,
    ArrayRef<StringRef> Identifiers, bool HasReturn) {
  Kind = CompletionKind::UnresolvedMember;
  CurDeclContext = P.CurDeclContext;
  UnresolvedExpr = E;
  UnresolvedExprInReturn = HasReturn;
  for (auto Id : Identifiers) {
    TokensBeforeUnresolvedExpr.push_back(Id);
  }
}

void CodeCompletionCallbacksImpl::completeAssignmentRHS(AssignExpr *E) {
  AssignmentExpr = E;
  ParsedExpr = E->getDest();
  CurDeclContext = P.CurDeclContext;
  Kind = CompletionKind::AssignmentRHS;
}

void CodeCompletionCallbacksImpl::completeCallArg(CallExpr *E) {
  if (Kind == CompletionKind::PostfixExprBeginning ||
      Kind == CompletionKind::None) {
    CurDeclContext = P.CurDeclContext;
    Kind = CompletionKind::CallArg;
    FuncCallExpr = E;
    ParsedExpr = E;
  }
}

void CodeCompletionCallbacksImpl::completeReturnStmt(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  Kind = CompletionKind::ReturnStmtExpr;
}

void CodeCompletionCallbacksImpl::completeAfterPound(CodeCompletionExpr *E,
                                                     StmtKind ParentKind) {
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  Kind = CompletionKind::AfterPound;
  ParentStmtKind = ParentKind;
}

void CodeCompletionCallbacksImpl::completeGenericParams(TypeLoc TL) {
  CurDeclContext = P.CurDeclContext;
  Kind = CompletionKind::GenericParams;
  ParsedTypeLoc = TL;
}

void CodeCompletionCallbacksImpl::completeNominalMemberBeginning(
    SmallVectorImpl<StringRef> &Keywords) {
  assert(!InEnumElementRawValue);
  ParsedKeywords.clear();
  ParsedKeywords.append(Keywords.begin(), Keywords.end());
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
  auto AddKeyword = [&](StringRef Name, CodeCompletionKeywordKind Kind) {
    if (Name == "let" || Name == "var") {
      // Treat keywords that could be the start of a pattern specially.
      return;
    }
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    Builder.setKeywordKind(Kind);
    Builder.addTextChunk(Name);
  };

#define DECL_KEYWORD(kw) AddKeyword(#kw, CodeCompletionKeywordKind::kw_##kw);
#include "swift/Parse/Tokens.def"

  // Context-sensitive keywords.
  auto AddCSKeyword = [&](StringRef Name) {
    AddKeyword(Name, CodeCompletionKeywordKind::None);
  };
  AddCSKeyword("weak");
  AddCSKeyword("unowned");
  AddCSKeyword("optional");
  AddCSKeyword("required");
  AddCSKeyword("lazy");
  AddCSKeyword("final");
  AddCSKeyword("dynamic");
  AddCSKeyword("prefix");
  AddCSKeyword("postfix");
  AddCSKeyword("infix");
  AddCSKeyword("override");
  AddCSKeyword("mutating");
  AddCSKeyword("nonmutating");
  AddCSKeyword("convenience");
}

static void addStmtKeywords(CodeCompletionResultSink &Sink, bool MaybeFuncBody) {
  auto AddKeyword = [&](StringRef Name, CodeCompletionKeywordKind Kind) {
    if (!MaybeFuncBody && Kind == CodeCompletionKeywordKind::kw_return)
      return;

    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    Builder.setKeywordKind(Kind);
    Builder.addTextChunk(Name);
  };
#define STMT_KEYWORD(kw) AddKeyword(#kw, CodeCompletionKeywordKind::kw_##kw);
#include "swift/Parse/Tokens.def"

  // Throw is not marked as a STMT_KEYWORD.
  AddKeyword("throw", CodeCompletionKeywordKind::kw_throw);
}

static void addLetVarKeywords(CodeCompletionResultSink &Sink) {
  auto AddKeyword = [&](StringRef Name, CodeCompletionKeywordKind Kind) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    Builder.setKeywordKind(Kind);
    Builder.addTextChunk(Name);
  };

  AddKeyword("let", CodeCompletionKeywordKind::kw_let);
  AddKeyword("var", CodeCompletionKeywordKind::kw_var);
}

static void addExprKeywords(CodeCompletionResultSink &Sink) {
  auto AddKeyword = [&](StringRef Name, StringRef TypeAnnotation, CodeCompletionKeywordKind Kind) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    Builder.setKeywordKind(Kind);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
  };

  // Expr keywords.
  AddKeyword("try", StringRef(), CodeCompletionKeywordKind::kw_try);
  AddKeyword("try!", StringRef(), CodeCompletionKeywordKind::kw_try);
  AddKeyword("try?", StringRef(), CodeCompletionKeywordKind::kw_try);
  // FIXME: The pedantically correct way to find the type is to resolve the
  // Swift.StringLiteralType type.
  AddKeyword("#function", "String", CodeCompletionKeywordKind::pound_function);
  AddKeyword("#file", "String", CodeCompletionKeywordKind::pound_file);
  // Same: Swift.IntegerLiteralType.
  AddKeyword("#line", "Int", CodeCompletionKeywordKind::pound_line);
  AddKeyword("#column", "Int", CodeCompletionKeywordKind::pound_column);
  AddKeyword("#dsohandle", "UnsafeMutablePointer<Void>", CodeCompletionKeywordKind::pound_dsohandle);
}

static void addAnyTypeKeyword(CodeCompletionResultSink &Sink) {
  CodeCompletionResultBuilder Builder(
      Sink, CodeCompletionResult::ResultKind::Keyword,
      SemanticContextKind::None, {});
  Builder.setKeywordKind(CodeCompletionKeywordKind::None);
  Builder.addTextChunk("Any");
  Builder.addTypeAnnotation("Any");
}


void CodeCompletionCallbacksImpl::addKeywords(CodeCompletionResultSink &Sink,
                                              bool MaybeFuncBody) {
  switch (Kind) {
  case CompletionKind::None:
  case CompletionKind::DotExpr:
  case CompletionKind::AttributeDeclParen:
  case CompletionKind::AttributeBegin:
  case CompletionKind::PoundAvailablePlatform:
  case CompletionKind::Import:
  case CompletionKind::UnresolvedMember:
  case CompletionKind::CallArg:
  case CompletionKind::AfterPound:
  case CompletionKind::GenericParams:
  case CompletionKind::KeyPathExpr:
  case CompletionKind::KeyPathExprDot:
    break;

  case CompletionKind::StmtOrExpr:
    addDeclKeywords(Sink);
    addStmtKeywords(Sink, MaybeFuncBody);
    SWIFT_FALLTHROUGH;
  case CompletionKind::AssignmentRHS:
  case CompletionKind::ReturnStmtExpr:
  case CompletionKind::PostfixExprBeginning:
    addSuperKeyword(Sink);
    addLetVarKeywords(Sink);
    addExprKeywords(Sink);
    addAnyTypeKeyword(Sink);
    break;

  case CompletionKind::PostfixExpr:
  case CompletionKind::PostfixExprParen:
  case CompletionKind::SuperExpr:
  case CompletionKind::SuperExprDot:
  case CompletionKind::CaseStmtBeginning:
  case CompletionKind::CaseStmtDotPrefix:
  case CompletionKind::TypeIdentifierWithDot:
  case CompletionKind::TypeIdentifierWithoutDot:
    break;

  case CompletionKind::TypeSimpleBeginning:
    addAnyTypeKeyword(Sink);
    break;
      
  case CompletionKind::NominalMemberBeginning:
    addDeclKeywords(Sink);
    addLetVarKeywords(Sink);
    break;
  }
}

namespace  {
  class ExprParentFinder : public ASTWalker {
    friend class CodeCompletionTypeContextAnalyzer;
    Expr *ChildExpr;
    llvm::function_ref<bool(ASTNode)> Predicate;

    bool arePositionsSame(Expr *E1, Expr *E2) {
      return E1->getSourceRange().Start == E2->getSourceRange().Start &&
        E1->getSourceRange().End == E2->getSourceRange().End;
    }

  public:
    llvm::SmallVector<ASTNode, 5> Ancestors;
    ASTNode ParentClosest;
    ASTNode ParentFarthest;
    ExprParentFinder(Expr* ChildExpr,
                     llvm::function_ref<bool(ASTNode)> Predicate) :
                     ChildExpr(ChildExpr), Predicate(Predicate) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (E == ChildExpr || arePositionsSame(E, ChildExpr)) {
        if (!Ancestors.empty()) {
          ParentClosest = Ancestors.back();
          ParentFarthest = Ancestors.front();
        }
        return {false, nullptr};
      }
      if (Predicate(E))
        Ancestors.push_back(E);
      return { true, E };
    }

    Expr *walkToExprPost(Expr *E) override {
      if (Predicate(E))
        Ancestors.pop_back();
      return E;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      if (Predicate(S))
        Ancestors.push_back(S);
      return { true, S };
    }

    Stmt *walkToStmtPost(Stmt *S) override {
      if (Predicate(S))
        Ancestors.pop_back();
      return S;
    }

    bool walkToDeclPre(Decl *D) override {
      if (Predicate(D))
        Ancestors.push_back(D);
      return true;
    }

    bool walkToDeclPost(Decl *D) override {
      if (Predicate(D))
        Ancestors.pop_back();
      return true;
    }
  };
} // end anonymous namespace

/// Given an expression and its context, the analyzer tries to figure out the
/// expected type of the expression by analyzing its context.
class CodeCompletionTypeContextAnalyzer {
  DeclContext *DC;
  Expr *ParsedExpr;
  SourceManager &SM;
  ASTContext &Context;
  ExprParentFinder Finder;

public:
  CodeCompletionTypeContextAnalyzer(DeclContext *DC, Expr *ParsedExpr) : DC(DC),
    ParsedExpr(ParsedExpr), SM(DC->getASTContext().SourceMgr),
    Context(DC->getASTContext()), Finder(ParsedExpr, [](ASTNode Node) {
      if (auto E = Node.dyn_cast<Expr *>()) {
        switch(E->getKind()) {
        case ExprKind::Call:
        case ExprKind::Assign:
          return true;
        default:
          return false;
      }
      } else if (auto S = Node.dyn_cast<Stmt *>()) {
        switch (S->getKind()) {
        case StmtKind::Return:
        case StmtKind::ForEach:
        case StmtKind::RepeatWhile:
          return true;
        default:
          return false;
        }
      } else if (auto D = Node.dyn_cast<Decl *>()) {
        switch (D->getKind()) {
        case DeclKind::PatternBinding:
          return true;
        default:
          return false;
        }
      } else
        return false;
  }) {}

  void analyzeExpr(Expr *Parent, llvm::function_ref<void(Type)> Callback,
                   SmallVectorImpl<StringRef> &PossibleNames) {
    switch (Parent->getKind()) {
      case ExprKind::Call: {
        std::vector<Type> PotentialTypes;
        std::vector<StringRef> ExpectedNames;
        CompletionLookup::collectArgumentExpectation(
            *DC, cast<CallExpr>(Parent), ParsedExpr, PotentialTypes,
            ExpectedNames);
        for (Type Ty : PotentialTypes)
          Callback(Ty);
        for (auto name : ExpectedNames)
          PossibleNames.push_back(name);
        break;
      }
      case ExprKind::Assign: {
        auto &SM = DC->getASTContext().SourceMgr;
        auto *AE = cast<AssignExpr>(Parent);

        // Make sure code completion is on the right hand side.
        if (SM.isBeforeInBuffer(AE->getEqualLoc(), ParsedExpr->getStartLoc())) {

          // The destination is of the expected type.
          Callback(AE->getDest()->getType());
        }
        break;
      }
      default:
        llvm_unreachable("Unhandled expression kinds.");
    }
  }

   void analyzeStmt(Stmt *Parent, llvm::function_ref<void(Type)> Callback) {
     switch (Parent->getKind()) {
       case StmtKind::Return: {
         Callback(getReturnTypeFromContext(DC));
         break;
       }
       case StmtKind::ForEach: {
         auto FES = cast<ForEachStmt>(Parent);
         if (auto SEQ = FES->getSequence()) {
           if (SM.rangeContains(SEQ->getSourceRange(),
                                ParsedExpr->getSourceRange())) {
             Callback(Context.getSequenceDecl()->getDeclaredInterfaceType());
           }
         }
         break;
       }
       case StmtKind::RepeatWhile: {
         auto Cond = cast<RepeatWhileStmt>(Parent)->getCond();
         if (Cond &&
             SM.rangeContains(Cond->getSourceRange(),
                              ParsedExpr->getSourceRange())) {
           Callback(Context.getBoolDecl()->getDeclaredType());
         }
         break;
       }
       default:
         llvm_unreachable("Unhandled statement kinds.");
     }
   }

  void analyzeDecl(Decl *D, llvm::function_ref<void(Type)> Callback) {
    switch (D->getKind()) {
      case DeclKind::PatternBinding: {
        auto PBD = cast<PatternBindingDecl>(D);
        for (unsigned I = 0; I < PBD->getNumPatternEntries(); ++ I) {
          if (auto Init = PBD->getInit(I)) {
            if (SM.rangeContains(Init->getSourceRange(), ParsedExpr->getLoc())) {
              if (PBD->getPattern(I)->hasType()) {
                Callback(PBD->getPattern(I)->getType());
                break;
              }
            }
          }
        }
        break;
      }
      default:
        llvm_unreachable("Unhandled decl kinds.");
    }
  }

  bool Analyze(llvm::SmallVectorImpl<Type> &PossibleTypes) {
    SmallVector<StringRef, 1> PossibleNames;
    return Analyze(PossibleTypes, PossibleNames) && !PossibleTypes.empty();
  }
  bool Analyze(SmallVectorImpl<Type> &PossibleTypes,
               SmallVectorImpl<StringRef> &PossibleNames) {
    // We cannot analyze without target.
    if (!ParsedExpr)
      return false;
    DC->walkContext(Finder);
    auto Callback = [&] (Type Result) {
      if (Result &&
          Result->getKind() != TypeKind::Error)
        PossibleTypes.push_back(Result->getRValueType());
    };

    for (auto It = Finder.Ancestors.rbegin(); It != Finder.Ancestors.rend();
         ++ It) {
      if (auto Parent = It->dyn_cast<Expr *>()) {
        analyzeExpr(Parent, Callback, PossibleNames);
      } else if (auto Parent = It->dyn_cast<Stmt *>()) {
        analyzeStmt(Parent, Callback);
      } else if (auto Parent = It->dyn_cast<Decl *>()) {
        analyzeDecl(Parent, Callback);
      }
      if (!PossibleTypes.empty() || !PossibleNames.empty())
        return true;
    }
    return false;
  }
};

void CodeCompletionCallbacksImpl::doneParsing() {
  CompletionContext.CodeCompletionKind = Kind;

  if (Kind == CompletionKind::None) {
    return;
  }

  bool MaybeFuncBody = true;
  if (CurDeclContext) {
    auto *CD = CurDeclContext->getLocalContext();
    if (!CD || CD->getContextKind() == DeclContextKind::Initializer ||
        CD->getContextKind() == DeclContextKind::TopLevelCodeDecl)
      MaybeFuncBody = false;
  }
  // Add keywords even if type checking fails completely.
  addKeywords(CompletionContext.getResultSink(), MaybeFuncBody);

  if (!typecheckContext())
    return;

  if (DelayedParsedDecl && !typecheckDelayedParsedDecl())
    return;

  if (auto *AFD = dyn_cast_or_null<AbstractFunctionDecl>(DelayedParsedDecl))
    CurDeclContext = AFD;

  Optional<Type> ExprType;
  ConcreteDeclRef ReferencedDecl = nullptr;
  if (ParsedExpr) {
    if (auto typechecked = typeCheckParsedExpr()) {
      ExprType = typechecked->first;
      ReferencedDecl = typechecked->second;
      ParsedExpr->setType(*ExprType);
    }

    if (!ExprType && Kind != CompletionKind::PostfixExprParen &&
        Kind != CompletionKind::CallArg &&
        Kind != CompletionKind::KeyPathExpr &&
        Kind != CompletionKind::KeyPathExprDot)
      return;
  }

  if (!ParsedTypeLoc.isNull() && !typecheckParsedType())
    return;

  CompletionLookup Lookup(CompletionContext.getResultSink(), P.Context,
                          CurDeclContext);
  if (ExprType) {
    Lookup.setIsStaticMetatype(ParsedExpr->isStaticallyDerivedMetatype());
  }
  if (isInsideObjCSelector())
    Lookup.includeInstanceMembers();
  if (PreferFunctionReferencesToCalls)
    Lookup.setPreferFunctionReferencesToCalls();

  auto DoPostfixExprBeginning = [&] (){
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

    if (isDynamicLookup(*ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.initializeArchetypeTransformer(CurDeclContext, *ExprType);

    CodeCompletionTypeContextAnalyzer TypeAnalyzer(CurDeclContext, ParsedExpr);
    llvm::SmallVector<Type, 2> PossibleTypes;
    if (TypeAnalyzer.Analyze(PossibleTypes)) {
      Lookup.setExpectedTypes(PossibleTypes);
    }
    Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    break;
  }

  case CompletionKind::StmtOrExpr:
    DoPostfixExprBeginning();
    break;

  case CompletionKind::PostfixExprBeginning: {
    CodeCompletionTypeContextAnalyzer Analyzer(CurDeclContext,
                                               CodeCompleteTokenExpr);
    llvm::SmallVector<Type, 1> Types;
    if (Analyzer.Analyze(Types)) {
      Lookup.setExpectedTypes(Types);
    }
    DoPostfixExprBeginning();
    break;
  }

  case CompletionKind::PostfixExpr: {
    Lookup.setHaveLeadingSpace(HasSpace);
    if (isDynamicLookup(*ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    Lookup.getOperatorCompletions(ParsedExpr, leadingSequenceExprs);
    break;
  }

  case CompletionKind::PostfixExprParen: {
    Lookup.setHaveLParen(true);

    CodeCompletionTypeContextAnalyzer TypeAnalyzer(CurDeclContext,
                                                   CodeCompleteTokenExpr);
    SmallVector<Type, 2> PossibleTypes;
    SmallVector<StringRef, 2> PossibleNames;
    if (TypeAnalyzer.Analyze(PossibleTypes, PossibleNames)) {
      Lookup.setExpectedTypes(PossibleTypes);
    }

    if (ExprType) {
      if (ShouldCompleteCallPatternAfterParen) {
        Lookup.setHaveRParen(HasRParen);
        Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
      } else {
        // Add argument labels, then fallthrough to get values.
        Lookup.addArgNameCompletionResults(PossibleNames);
      }
    }

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
    Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    break;
  }

  case CompletionKind::SuperExprDot: {
    Lookup.setIsSuperRefExpr();
    Lookup.setHaveDot(SourceLoc());
    Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    break;
  }

  case CompletionKind::KeyPathExprDot:
    Lookup.setHaveDot(SourceLoc());
    SWIFT_FALLTHROUGH;

  case CompletionKind::KeyPathExpr: {
    Lookup.setIsKeyPathExpr();
    Lookup.includeInstanceMembers();

    if (ExprType) {
      if (isDynamicLookup(*ExprType))
        Lookup.setIsDynamicLookup();

      Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    } else {
      SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
      Lookup.getValueCompletionsInDeclContext(Loc, KeyPathFilter,
                                              false, true, false);
    }
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
                                            P.Context, CurDeclContext,
                                            ParsedKeywords);
    OverrideLookup.getOverrideCompletions(SourceLoc());
    break;
  }
  case CompletionKind::AttributeBegin: {
    Lookup.getAttributeDeclCompletions(IsInSil, AttTargetDK);
    break;
  }
  case CompletionKind::AttributeDeclParen: {
    Lookup.getAttributeDeclParamCompletions(AttrKind, AttrParamIndex);
    break;
  }
  case CompletionKind::PoundAvailablePlatform: {
    Lookup.getPoundAvailablePlatformCompletions();
    break;
  }
  case CompletionKind::Import: {
    if (DotLoc.isValid())
      Lookup.addSubModuleNames(SubModuleNameVisibilityPairs);
    else
      Lookup.addImportModuleNames();
    break;
  }
  case CompletionKind::UnresolvedMember : {
    Lookup.setHaveDot(SourceLoc());
    SmallVector<Type, 1> PossibleTypes;
    ExprParentFinder Walker(UnresolvedExpr, [&](ASTNode Node) {
      return Node.is<Expr *>();
    });
    CurDeclContext->walkContext(Walker);
    bool Success = false;
    if (auto PE = Walker.ParentFarthest.get<Expr *>()) {
      eraseErrorTypes(PE);
      Success = typeCheckUnresolvedExpr(*CurDeclContext, UnresolvedExpr, PE,
                                        PossibleTypes);
      Lookup.getUnresolvedMemberCompletions(
        P.Context.SourceMgr.getCodeCompletionLoc(), PossibleTypes);
    }
    if (!Success) {
      Lookup.getUnresolvedMemberCompletions(
        P.Context.SourceMgr.getCodeCompletionLoc(),
        TokensBeforeUnresolvedExpr,
        UnresolvedExprInReturn);
    }
    break;
  }
  case CompletionKind::AssignmentRHS : {
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    if (auto destType = AssignmentExpr->getDest()->getType())
      Lookup.setExpectedTypes(destType->getRValueType());
    Lookup.getValueCompletionsInDeclContext(Loc, DefaultFilter);
    break;
  }
  case CompletionKind::CallArg : {
    if (!CodeCompleteTokenExpr || !Lookup.getCallArgCompletions(*CurDeclContext,
                                                                FuncCallExpr,
                                                                CodeCompleteTokenExpr))
      DoPostfixExprBeginning();
    break;
  }

  case CompletionKind::ReturnStmtExpr : {
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    if (auto FD = dyn_cast<AbstractFunctionDecl>(CurDeclContext)) {
      if (auto FT = FD->getType()->getAs<FunctionType>()) {
        Lookup.setExpectedTypes(FT->getResult());
      }
    }
    Lookup.getValueCompletionsInDeclContext(Loc);
    break;
  }

  case CompletionKind::AfterPound: {
    Lookup.addPoundAvailable(ParentStmtKind);
    Lookup.addPoundSelector(/*needPound=*/false);
    Lookup.addPoundKeyPath(/*needPound=*/false);
    break;
  }

  case CompletionKind::GenericParams:
    if (auto GT = ParsedTypeLoc.getType()->getAnyGeneric()) {
      if (auto Params = GT->getGenericParams()) {
        for (auto GP : Params->getParams()) {
          Lookup.addGenericTypeParamRef(GP,
                                        DeclVisibilityKind::GenericParameter);
        }
      }
    }
    break;
  }

  if (Lookup.RequestedCachedResults) {
    // Use the current SourceFile as the DeclContext so that we can use it to
    // perform qualified lookup, and to get the correct visibility for
    // @testable imports.
    const SourceFile &SF = P.SF;

    auto &Request = Lookup.RequestedCachedResults.getValue();

    llvm::DenseSet<CodeCompletionCache::Key> ImportsSeen;
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
        CodeCompletionCache::Key K{ModuleFilename, TheModule->getName().str(),
                                   AccessPath, Request.NeedLeadingDot,
                                   SF.hasTestableImport(TheModule)};
        std::pair<decltype(ImportsSeen)::iterator, bool>
        Result = ImportsSeen.insert(K);
        if (!Result.second)
          return; // already handled.

        RequestedModules.push_back(
            {std::move(K), TheModule, Request.OnlyTypes});
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
      SmallVector<Module::ImportedModule, 4> Imports;
      auto *SF = CurDeclContext->getParentSourceFile();
      SF->getImportedModules(Imports, Module::ImportFilter::All);

      for (auto Imported : Imports) {
        Module *TheModule = Imported.second;
        Module::AccessPathTy AccessPath = Imported.first;
        TheModule->forAllVisibleModules(AccessPath, handleImport);
      }
    }
    Lookup.RequestedCachedResults.reset();
  }

  CompletionContext.HasExpectedTypeRelation = Lookup.hasExpectedTypes();

  deliverCompletionResults();
}

void CodeCompletionCallbacksImpl::deliverCompletionResults() {
  // Use the current SourceFile as the DeclContext so that we can use it to
  // perform qualified lookup, and to get the correct visibility for
  // @testable imports.
  DeclContext *DCForModules = &P.SF;

  Consumer.handleResultsAndModules(CompletionContext, RequestedModules,
                                   DCForModules);
  RequestedModules.clear();
  DeliveredResults = true;
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
} // end anonymous namespace

CodeCompletionCallbacksFactory *
swift::ide::makeCodeCompletionCallbacksFactory(
    CodeCompletionContext &CompletionContext,
    CodeCompletionConsumer &Consumer) {
  return new CodeCompletionCallbacksFactoryImpl(CompletionContext, Consumer);
}

void swift::ide::lookupCodeCompletionResultsFromModule(
    CodeCompletionResultSink &targetSink, const Module *module,
    ArrayRef<std::string> accessPath, bool needLeadingDot,
    const DeclContext *currDeclContext) {
  CompletionLookup Lookup(targetSink, module->getASTContext(), currDeclContext);
  Lookup.getVisibleDeclsOfModule(module, accessPath, needLeadingDot);
}

void swift::ide::copyCodeCompletionResults(CodeCompletionResultSink &targetSink,
                                           CodeCompletionResultSink &sourceSink,
                                           bool onlyTypes) {

  // We will be adding foreign results (from another sink) into TargetSink.
  // TargetSink should have an owning pointer to the allocator that keeps the
  // results alive.
  targetSink.ForeignAllocators.push_back(sourceSink.Allocator);

  if (onlyTypes) {
    std::copy_if(sourceSink.Results.begin(), sourceSink.Results.end(),
                 std::back_inserter(targetSink.Results),
                 [](CodeCompletionResult *R) -> bool {
      if (R->getKind() != CodeCompletionResult::Declaration)
        return false;
      switch(R->getAssociatedDeclKind()) {
      case CodeCompletionDeclKind::PrecedenceGroup:
      case CodeCompletionDeclKind::Module:
      case CodeCompletionDeclKind::Class:
      case CodeCompletionDeclKind::Struct:
      case CodeCompletionDeclKind::Enum:
      case CodeCompletionDeclKind::Protocol:
      case CodeCompletionDeclKind::TypeAlias:
      case CodeCompletionDeclKind::AssociatedType:
      case CodeCompletionDeclKind::GenericTypeParam:
        return true;
      case CodeCompletionDeclKind::EnumElement:
      case CodeCompletionDeclKind::Constructor:
      case CodeCompletionDeclKind::Destructor:
      case CodeCompletionDeclKind::Subscript:
      case CodeCompletionDeclKind::StaticMethod:
      case CodeCompletionDeclKind::InstanceMethod:
      case CodeCompletionDeclKind::PrefixOperatorFunction:
      case CodeCompletionDeclKind::PostfixOperatorFunction:
      case CodeCompletionDeclKind::InfixOperatorFunction:
      case CodeCompletionDeclKind::FreeFunction:
      case CodeCompletionDeclKind::StaticVar:
      case CodeCompletionDeclKind::InstanceVar:
      case CodeCompletionDeclKind::LocalVar:
      case CodeCompletionDeclKind::GlobalVar:
        return false;
      }
    });
  } else {
    targetSink.Results.insert(targetSink.Results.end(),
                              sourceSink.Results.begin(),
                              sourceSink.Results.end());
  }
}

void SimpleCachingCodeCompletionConsumer::handleResultsAndModules(
    CodeCompletionContext &context,
    ArrayRef<RequestedCachedModule> requestedModules,
    DeclContext *DCForModules) {
  for (auto &R : requestedModules) {
    // FIXME(thread-safety): lock the whole AST context.  We might load a
    // module.
    llvm::Optional<CodeCompletionCache::ValueRefCntPtr> V =
        context.Cache.get(R.Key);
    if (!V.hasValue()) {
      // No cached results found. Fill the cache.
      V = context.Cache.createValue();
      lookupCodeCompletionResultsFromModule(
          (*V)->Sink, R.TheModule, R.Key.AccessPath,
          R.Key.ResultsHaveLeadingDot, DCForModules);
      context.Cache.set(R.Key, *V);
    }
    assert(V.hasValue());
    copyCodeCompletionResults(context.getResultSink(), (*V)->Sink, R.OnlyTypes);
  }

  handleResults(context.takeResults());
}
