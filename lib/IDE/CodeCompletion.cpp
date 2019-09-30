//===--- CodeCompletion.cpp - Code completion implementation --------------===//
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

#include "swift/IDE/CodeCompletion.h"
#include "CodeCompletionResultBuilder.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Comment.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IDE/CodeCompletionCache.h"
#include "swift/IDE/Utils.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Syntax/SyntaxKind.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Comment.h"
#include "clang/AST/CommentVisitor.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>
#include <string>

using namespace swift;
using namespace ide;

using CommandWordsPairs = std::vector<std::pair<StringRef, StringRef>>;

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
  if (!DC)
    return;
  SwiftDocWordExtractor Extractor(Words);
  for (auto Part : DC->getBodyNodes()) {
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

using DeclFilter = std::function<bool(ValueDecl *, DeclVisibilityKind)>;
static bool DefaultFilter(ValueDecl* VD, DeclVisibilityKind Kind) {
  return true;
}
static bool KeyPathFilter(ValueDecl* decl, DeclVisibilityKind) {
  return isa<TypeDecl>(decl) ||
         (isa<VarDecl>(decl) && decl->getDeclContext()->isTypeContext());
}

static bool SwiftKeyPathFilter(ValueDecl* decl, DeclVisibilityKind) {
  switch(decl->getKind()){
  case DeclKind::Var:
  case DeclKind::Subscript:
    return true;
  default:
    return false;
  }
}

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
      LLVM_FALLTHROUGH;
    case ChunkKind::CallParameterName:
    case ChunkKind::CallParameterInternalName:
    case ChunkKind::CallParameterColon:
    case ChunkKind::DeclAttrParamColon:
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
  case DeclKind::PoundDiagnostic:
  case DeclKind::MissingMember:
  case DeclKind::OpaqueType:
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
  case DeclKind::Accessor:
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
#include "swift/Syntax/TokenKinds.def"
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
      Ty->is<ErrorType>() ||
      ExpectedTy->is<ErrorType>())
    return CodeCompletionResult::ExpectedTypeRelation::Unrelated;

  // Equality/Conversion of GenericTypeParameterType won't account for
  // requirements – ignore them
  if (!Ty->hasTypeParameter() && !ExpectedTy->hasTypeParameter()) {
    if (Ty->isEqual(ExpectedTy))
      return CodeCompletionResult::ExpectedTypeRelation::Identical;
    bool isAny = false;
    isAny |= ExpectedTy->isAny();
    isAny |= ExpectedTy->is<ArchetypeType>() &&
             !ExpectedTy->castTo<ArchetypeType>()->hasRequirements();

    if (!isAny && isConvertibleTo(Ty, ExpectedTy, /*openArchetypes=*/true, *DC))
      return CodeCompletionResult::ExpectedTypeRelation::Convertible;
  }
  if (auto FT = Ty->getAs<AnyFunctionType>()) {
    if (FT->getResult()->isVoid())
      return CodeCompletionResult::ExpectedTypeRelation::Invalid;
  }
  return CodeCompletionResult::ExpectedTypeRelation::Unrelated;
}

static CodeCompletionResult::ExpectedTypeRelation
calculateTypeRelationForDecl(const Decl *D, Type ExpectedType,
                             bool IsImplicitlyCurriedInstanceMethod,
                             bool UseFuncResultType = true) {
  auto VD = dyn_cast<ValueDecl>(D);
  auto DC = D->getDeclContext();
  if (!VD || !VD->getInterfaceType())
    return CodeCompletionResult::ExpectedTypeRelation::Unrelated;

  if (auto FD = dyn_cast<AbstractFunctionDecl>(VD)) {
    auto funcType = FD->getInterfaceType()->getAs<AnyFunctionType>();
    if (DC->isTypeContext() && funcType && funcType->is<AnyFunctionType>() &&
        !IsImplicitlyCurriedInstanceMethod)
      funcType = funcType->getResult()->getAs<AnyFunctionType>();
    if (funcType) {
      funcType = funcType->removeArgumentLabels(1)->castTo<AnyFunctionType>();
      auto relation = calculateTypeRelation(funcType, ExpectedType, DC);
      if (UseFuncResultType)
        relation =
            std::max(relation, calculateTypeRelation(funcType->getResult(),
                                                     ExpectedType, DC));
      return relation;
    }
  }
  if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
    return std::max(
        calculateTypeRelation(NTD->getInterfaceType(), ExpectedType, DC),
        calculateTypeRelation(NTD->getDeclaredInterfaceType(), ExpectedType, DC));
  }
  return calculateTypeRelation(VD->getInterfaceType(), ExpectedType, DC);
}

static CodeCompletionResult::ExpectedTypeRelation
calculateMaxTypeRelationForDecl(
    const Decl *D, const ExpectedTypeContext &typeContext,
    bool IsImplicitlyCurriedInstanceMethod = false) {
  auto Result = CodeCompletionResult::ExpectedTypeRelation::Unrelated;
  for (auto Type : typeContext.possibleTypes) {
    // Do not use Void type context for a single-expression body, since the
    // implicit return does not constrain the expression.
    //
    //     { ... -> ()  in x } // x can be anything
    //
    // This behaves differently from explicit return, and from non-Void:
    //
    //     { ... -> Int in x }        // x must be Int
    //     { ... -> ()  in return x } // x must be Void
    if (typeContext.isSingleExpressionBody && Type->isVoid())
      continue;

    Result = std::max(Result, calculateTypeRelationForDecl(
                                  D, Type, IsImplicitlyCurriedInstanceMethod));

    // Map invalid -> unrelated when in a single-expression body, since the
    // input may be incomplete.
    if (typeContext.isSingleExpressionBody &&
        Result == CodeCompletionResult::ExpectedTypeRelation::Invalid)
      Result = CodeCompletionResult::ExpectedTypeRelation::Unrelated;
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
              CurrentModule.get<const swift::ModuleDecl *>()->getName().str());
        }
        Sink.LastModule.first = CurrentModule.getOpaqueValue();
        Sink.LastModule.second = ModuleName;
      }
    }

    auto typeRelation = ExpectedTypeRelation;
    if (typeRelation == CodeCompletionResult::Unrelated)
      typeRelation =
          calculateMaxTypeRelationForDecl(AssociatedDecl, declTypeContext);

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

  llvm_unreachable("Unhandled CodeCompletionResult in switch.");
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
    case ChunkKind::DeclAttrParamColon:
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
      case ChunkKind::DeclAttrParamColon:
      case ChunkKind::OptionalMethodCallTail:
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
  CompletionKind Kind = CompletionKind::None;
  Expr *ParsedExpr = nullptr;
  SourceLoc DotLoc;
  DeclContext *CurDeclContext = nullptr;
  DeclAttrKind AttrKind;

  /// In situations when \c SyntaxKind hints or determines
  /// completions, i.e. a precedence group attribute, this
  /// can be set and used to control the code completion scenario.
  SyntaxKind SyntxKind;

  int AttrParamIndex;
  bool IsInSil = false;
  bool HasSpace = false;
  bool ShouldCompleteCallPatternAfterParen = true;
  bool PreferFunctionReferencesToCalls = false;
  bool AttTargetIsIndependent = false;
  Optional<DeclKind> AttTargetDK;
  Optional<StmtKind> ParentStmtKind;

  SmallVector<StringRef, 3> ParsedKeywords;
  SourceLoc introducerLoc;

  std::vector<std::pair<std::string, bool>> SubModuleNameVisibilityPairs;

  void addSuperKeyword(CodeCompletionResultSink &Sink) {
    auto *DC = CurDeclContext->getInnermostTypeContext();
    if (!DC)
      return;
    auto *CD = DC->getSelfClassDecl();
    if (!CD)
      return;
    Type ST = CD->getSuperclass();
    if (ST.isNull() || ST->is<ErrorType>())
      return;

    CodeCompletionResultBuilder Builder(Sink,
                                        CodeCompletionResult::ResultKind::Keyword,
                                        SemanticContextKind::CurrentNominal,
                                        {});
    Builder.setKeywordKind(CodeCompletionKeywordKind::kw_super);
    Builder.addTextChunk("super");
    Builder.addTypeAnnotation(ST.getString());
  }

  /// Set to true when we have delivered code completion results
  /// to the \c Consumer.
  bool DeliveredResults = false;


  Optional<std::pair<Type, ConcreteDeclRef>> typeCheckParsedExpr() {
    assert(ParsedExpr && "should have an expression");

    // Figure out the kind of type-check we'll be performing.
    auto CheckKind = CompletionTypeCheckKind::Normal;
    if (Kind == CompletionKind::KeyPathExprObjC)
      CheckKind = CompletionTypeCheckKind::KeyPath;

    // If we've already successfully type-checked the expression for some
    // reason, just return the type.
    // FIXME: if it's ErrorType but we've already typechecked we shouldn't
    // typecheck again. rdar://21466394
    if (CheckKind == CompletionTypeCheckKind::Normal &&
        ParsedExpr->getType() && !ParsedExpr->getType()->is<ErrorType>()) {
      auto refDecl = ParsedExpr->getReferencedDecl();
      auto exprTy = ParsedExpr->getType();
      if (!refDecl) {
        // FIXME: do this in the not-already-type-checked branch too?
        if (auto *apply = dyn_cast<SelfApplyExpr>(ParsedExpr)) {
          refDecl = apply->getFn()->getReferencedDecl();
        } else if (auto *apply = dyn_cast<ApplyExpr>(ParsedExpr)) {
          // If this is an IUO, use the underlying non-optional type instead
          auto fnDecl = apply->getFn()->getReferencedDecl();
          if (auto FD = fnDecl.getDecl()) {
            if (FD->isImplicitlyUnwrappedOptional()) {
              if (auto OT = exprTy->getOptionalObjectType())
                exprTy = OT;
            }
          }
        }
      }
      return std::make_pair(exprTy, refDecl);
    }

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
    return !performTypeLocChecking(P.Context, ParsedTypeLoc,
                                   CurDeclContext, false);
  }

public:
  CodeCompletionCallbacksImpl(Parser &P,
                              CodeCompletionContext &CompletionContext,
                              CodeCompletionConsumer &Consumer)
      : CodeCompletionCallbacks(P), CompletionContext(CompletionContext),
        Consumer(Consumer) {
  }

  void setAttrTargetDeclKind(Optional<DeclKind> DK) override {
    if (DK == DeclKind::PatternBinding)
      DK = DeclKind::Var;
    else if (DK == DeclKind::Param)
      // For params, consider the attribute is always for the decl.
      AttTargetIsIndependent = false;

    if (!AttTargetIsIndependent)
      AttTargetDK = DK;
  }

  void completeExpr() override;
  void completeDotExpr(Expr *E, SourceLoc DotLoc) override;
  void completeStmtOrExpr(CodeCompletionExpr *E) override;
  void completePostfixExprBeginning(CodeCompletionExpr *E) override;
  void completeForEachSequenceBeginning(CodeCompletionExpr *E) override;
  void completePostfixExpr(Expr *E, bool hasSpace) override;
  void completePostfixExprParen(Expr *E, Expr *CodeCompletionE) override;
  void completeExprKeyPath(KeyPathExpr *KPE, SourceLoc DotLoc) override;

  void completeTypeDeclResultBeginning() override;
  void completeTypeSimpleBeginning() override;
  void completeTypeIdentifierWithDot() override;
  void completeTypeIdentifierWithoutDot() override;

  void completeCaseStmtKeyword() override;
  void completeCaseStmtBeginning(CodeCompletionExpr *E) override;
  void completeDeclAttrBeginning(bool Sil, bool isIndependent) override;
  void completeDeclAttrParam(DeclAttrKind DK, int Index) override;
  void completeInPrecedenceGroup(SyntaxKind SK) override;
  void completeNominalMemberBeginning(
      SmallVectorImpl<StringRef> &Keywords, SourceLoc introducerLoc) override;
  void completeAccessorBeginning(CodeCompletionExpr *E) override;

  void completePoundAvailablePlatform() override;
  void completeImportDecl(std::vector<std::pair<Identifier, SourceLoc>> &Path) override;
  void completeUnresolvedMember(CodeCompletionExpr *E,
                                SourceLoc DotLoc) override;
  void completeCallArg(CodeCompletionExpr *E, bool isFirst) override;
  void completeReturnStmt(CodeCompletionExpr *E) override;
  void completeYieldStmt(CodeCompletionExpr *E,
                         Optional<unsigned> yieldIndex) override;
  void completeAfterPoundExpr(CodeCompletionExpr *E,
                              Optional<StmtKind> ParentKind) override;
  void completeAfterPoundDirective() override;
  void completePlatformCondition() override;
  void completeGenericParams(TypeLoc TL) override;
  void completeAfterIfStmt(bool hasElse) override;

  void doneParsing() override;

private:
  void addKeywords(CodeCompletionResultSink &Sink, bool MaybeFuncBody);
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

namespace {
static bool isTopLevelContext(const DeclContext *DC) {
  for (; DC && DC->isLocalContext(); DC = DC->getParent()) {
    switch (DC->getContextKind()) {
    case DeclContextKind::TopLevelCodeDecl:
      return true;
    case DeclContextKind::AbstractFunctionDecl:
    case DeclContextKind::SubscriptDecl:
    case DeclContextKind::EnumElementDecl:
      return false;
    default:
      continue;
    }
  }
  return false;
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
    return KnownProtocolKind::ExpressibleByUnicodeScalarLiteral;
  case CodeCompletionLiteralKind::Tuple:
    llvm_unreachable("no such protocol kind");
  }

  llvm_unreachable("Unhandled CodeCompletionLiteralKind in switch.");
}

/// Whether funcType has a single argument (not including defaulted arguments)
/// that is of type () -> ().
static bool hasTrivialTrailingClosure(const FuncDecl *FD,
                                      AnyFunctionType *funcType) {
  ParameterListInfo paramInfo(funcType->getParams(), FD,
                              /*skipCurriedSelf*/ FD->hasCurriedSelf());

  if (paramInfo.size() - paramInfo.numNonDefaultedParameters() == 1) {
    auto param = funcType->getParams().back();
    if (!param.isAutoClosure()) {
      if (auto Fn = param.getOldType()->getAs<AnyFunctionType>()) {
        return Fn->getParams().empty() && Fn->getResult()->isVoid();
      }
    }
  }

  return false;
}

/// Build completions by doing visible decl lookup from a context.
class CompletionLookup final : public swift::VisibleDeclConsumer {
  CodeCompletionResultSink &Sink;
  ASTContext &Ctx;
  const DeclContext *CurrDeclContext;
  ModuleDecl *CurrModule;
  ClangImporter *Importer;
  CodeCompletionContext *CompletionContext;

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
  bool IsStaticMetatype = false;

  /// User-provided base type for LookupKind::Type completions.
  Type BaseType;

  /// Expected types of the code completion expression.
  ExpectedTypeContext expectedTypeContext;

  bool HaveDot = false;
  bool IsUnwrappedOptional = false;
  SourceLoc DotLoc;
  bool NeedLeadingDot = false;

  bool NeedOptionalUnwrap = false;
  unsigned NumBytesToEraseForOptionalUnwrap = 0;

  bool HaveLParen = false;
  bool IsSuperRefExpr = false;
  bool IsSelfRefExpr = false;
  bool IsKeyPathExpr = false;
  bool IsSwiftKeyPathExpr = false;
  bool IsAfterSwiftKeyPathRoot = false;
  bool IsDynamicLookup = false;
  bool PreferFunctionReferencesToCalls = false;
  bool HaveLeadingSpace = false;

  bool IncludeInstanceMembers = false;

  /// True if we are code completing inside a static method.
  bool InsideStaticMethod = false;

  /// Innermost method that the code completion point is in.
  const AbstractFunctionDecl *CurrentMethod = nullptr;

  Optional<SemanticContextKind> ForcedSemanticContext = None;
  bool IsUnresolvedMember = false;

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
    auto Params = AFT->getParams();
    if (Params.empty())
      return;
    if (Params.size() == 1 && !Params[0].hasLabel()) {
      FoundFunctionsWithoutFirstKeyword = true;
      return;
    }
    if (!Params[0].hasLabel())
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

  bool shouldUseFunctionReference(AbstractFunctionDecl *D) {
    if (PreferFunctionReferencesToCalls)
      return true;
    bool isImplicitlyCurriedIM = isImplicitlyCurriedInstanceMethod(D);
    for (auto expectedType : expectedTypeContext.possibleTypes) {
      if (expectedType &&
          expectedType->lookThroughAllOptionalTypes()
              ->is<AnyFunctionType>() &&
          calculateTypeRelationForDecl(D, expectedType, isImplicitlyCurriedIM,
                                       /*UseFuncResultType=*/false) >=
              CodeCompletionResult::ExpectedTypeRelation::Convertible) {
        return true;
      }
    }
    return false;
  }

public:
  struct RequestedResultsTy {
    const ModuleDecl *TheModule;
    bool OnlyTypes;
    bool OnlyPrecedenceGroups;
    bool NeedLeadingDot;

    static RequestedResultsTy fromModule(const ModuleDecl *TheModule) {
      return { TheModule, false, false, false };
    }

    RequestedResultsTy onlyTypes() const {
      return { TheModule, true, false, NeedLeadingDot };
    }

    RequestedResultsTy onlyPrecedenceGroups() const {
      assert(!OnlyTypes && "onlyTypes() already includes precedence groups");
      return { TheModule, false, true, false };
    }

    RequestedResultsTy needLeadingDot(bool NeedDot) const {
      return { TheModule, OnlyTypes, OnlyPrecedenceGroups, NeedDot };
    }

    static RequestedResultsTy toplevelResults() {
      return { nullptr, false, false, false };
    }
  };

  std::vector<RequestedResultsTy> RequestedCachedResults;

public:
  CompletionLookup(CodeCompletionResultSink &Sink,
                   ASTContext &Ctx,
                   const DeclContext *CurrDeclContext,
                   CodeCompletionContext *CompletionContext = nullptr)
      : Sink(Sink), Ctx(Ctx), CurrDeclContext(CurrDeclContext),
        CurrModule(CurrDeclContext ? CurrDeclContext->getParentModule()
                                   : nullptr),
        Importer(static_cast<ClangImporter *>(CurrDeclContext->getASTContext().
          getClangModuleLoader())),
        CompletionContext(CompletionContext) {
    (void)createTypeChecker(Ctx);

    // Determine if we are doing code completion inside a static method.
    if (CurrDeclContext) {
      CurrentMethod = CurrDeclContext->getInnermostMethodContext();
      if (auto *FD = dyn_cast_or_null<FuncDecl>(CurrentMethod))
        InsideStaticMethod = FD->isStatic();
    }
  }

  void setHaveDot(SourceLoc DotLoc) {
    HaveDot = true;
    this->DotLoc = DotLoc;
  }

  void setIsUnwrappedOptional(bool value) {
    IsUnwrappedOptional = value;
  }

  void setIsStaticMetatype(bool value) {
    IsStaticMetatype = value;
  }

  void setExpectedTypes(ArrayRef<Type> Types, bool isSingleExpressionBody) {
    expectedTypeContext.isSingleExpressionBody = isSingleExpressionBody;
    expectedTypeContext.possibleTypes.clear();
    expectedTypeContext.possibleTypes.reserve(Types.size());
    for (auto T : Types)
      if (T)
        expectedTypeContext.possibleTypes.push_back(T);
  }

  CodeCompletionContext::TypeContextKind typeContextKind() const {
    if (expectedTypeContext.empty()) {
      return CodeCompletionContext::TypeContextKind::None;
    } else if (expectedTypeContext.isSingleExpressionBody) {
      return CodeCompletionContext::TypeContextKind::SingleExpressionBody;
    } else {
      return CodeCompletionContext::TypeContextKind::Required;
    }
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

  void setIsSelfRefExpr(bool value) { IsSelfRefExpr = value; }

  void setIsKeyPathExpr() {
    IsKeyPathExpr = true;
  }

  void setIsSwiftKeyPathExpr(bool onRoot) {
    IsSwiftKeyPathExpr = true;
    IsAfterSwiftKeyPathRoot = onRoot;
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

  void addSubModuleNames(std::vector<std::pair<std::string, bool>>
      &SubModuleNameVisibilityPairs) {
    for (auto &Pair : SubModuleNameVisibilityPairs) {
      CodeCompletionResultBuilder Builder(Sink,
                                          CodeCompletionResult::ResultKind::
                                          Declaration,
                                          SemanticContextKind::OtherModule,
                                          expectedTypeContext);
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
    ModuleDecl::ImportFilter ImportFilter;
    ImportFilter |= ModuleDecl::ImportFilterKind::Public;
    ImportFilter |= ModuleDecl::ImportFilterKind::Private;
    ImportFilter |= ModuleDecl::ImportFilterKind::ImplementationOnly;

    SmallVector<ModuleDecl::ImportedModule, 16> Imported;
    SmallVector<ModuleDecl::ImportedModule, 16> FurtherImported;
    CurrDeclContext->getParentSourceFile()->getImportedModules(Imported,
                                                               ImportFilter);
    while (!Imported.empty()) {
      ModuleDecl *MD = Imported.back().second;
      Imported.pop_back();
      if (!ImportedModules.insert(MD->getNameStr()).second)
        continue;
      FurtherImported.clear();
      MD->getImportedModules(FurtherImported,
                             ModuleDecl::ImportFilterKind::Public);
      Imported.append(FurtherImported.begin(), FurtherImported.end());
      for (auto SubMod : FurtherImported) {
        Imported.push_back(SubMod);
      }
    }
  }

  void addImportModuleNames() {
    SmallVector<Identifier, 0> ModuleNames;
    Ctx.getVisibleTopLevelModuleNames(ModuleNames);

    llvm::StringSet<> ImportedModules;
    collectImportedModules(ImportedModules);

    auto mainModuleName = CurrModule->getName();
    for (auto ModuleName : ModuleNames) {
      if (ModuleName.str().startswith("_") ||
          ModuleName == mainModuleName ||
          ModuleName == Ctx.SwiftShimsModuleName ||
          ModuleName.str() == SWIFT_ONONE_SUPPORT)
        continue;

      auto MD = ModuleDecl::create(ModuleName, Ctx);
      CodeCompletionResultBuilder Builder(
          Sink,
          CodeCompletionResult::ResultKind::Declaration,
          SemanticContextKind::OtherModule,
          expectedTypeContext);
      Builder.setAssociatedDecl(MD);
      Builder.addTextChunk(MD->getNameStr());
      Builder.addTypeAnnotation("Module");

      // Imported modules are not recommended.
      if (ImportedModules.count(MD->getNameStr()) != 0)
        Builder.setNotRecommended(
            CodeCompletionResult::NotRecommendedReason::Redundant);
    }
  }

  SemanticContextKind getSemanticContext(const Decl *D,
                                         DeclVisibilityKind Reason,
                                         DynamicLookupInfo dynamicLookupInfo) {
    if (ForcedSemanticContext)
      return *ForcedSemanticContext;

    if (IsUnresolvedMember) {
      if (isa<EnumElementDecl>(D)) {
        return SemanticContextKind::ExpressionSpecific;
      }
    }

    switch (Reason) {
    case DeclVisibilityKind::LocalVariable:
    case DeclVisibilityKind::FunctionParameter:
    case DeclVisibilityKind::GenericParameter:
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
      if (CurrDeclContext && D->getModuleContext() == CurrModule) {
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
      switch (dynamicLookupInfo.getKind()) {
      case DynamicLookupInfo::None:
        llvm_unreachable("invalid DynamicLookupInfo::Kind for dynamic lookup");

      case DynamicLookupInfo::AnyObject:
        // AnyObject results can come from different modules, including the
        // current module, but we always assign them the OtherModule semantic
        // context.  These declarations are uniqued by signature, so it is
        // totally random (determined by the hash function) which of the
        // equivalent declarations (across multiple modules) we will get.
        return SemanticContextKind::OtherModule;

      case DynamicLookupInfo::KeyPathDynamicMember:
        // Use the visibility of the underlying declaration.
        // FIXME: KeyPath<AnyObject, U> !?!?
        assert(dynamicLookupInfo.getKeyPathDynamicMember().originalVisibility !=
               DeclVisibilityKind::DynamicLookup);
        return getSemanticContext(
            D, dynamicLookupInfo.getKeyPathDynamicMember().originalVisibility,
            {});
      }
    }
    llvm_unreachable("unhandled kind");
  }

  void addValueBaseName(CodeCompletionResultBuilder &Builder,
                        DeclBaseName Name) {
    auto NameStr = Name.userFacingName();
    bool shouldEscapeKeywords;
    if (Name.isSpecial()) {
      // Special names (i.e. 'init') are always displayed as its user facing
      // name.
      shouldEscapeKeywords = false;
    } else if (ExprType) {
      // After dot. User can write any keyword after '.' except for `init` and
      // `self`. E.g. 'func `init`()' must be called by 'expr.`init`()'.
      shouldEscapeKeywords = NameStr == "self" || NameStr == "init";
    } else {
      // As primary expresson. We have to escape almost every keywords except
      // for 'self' and 'Self'.
      shouldEscapeKeywords = NameStr != "self" && NameStr != "Self";
    }

    if (!shouldEscapeKeywords) {
      Builder.addTextChunk(NameStr);
    } else {
      SmallString<16> buffer;
      Builder.addTextChunk(Builder.escapeKeyword(NameStr, true, buffer));
    }
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
    if (T->isVoid()) {
      Builder.addTypeAnnotation("Void");
    } else {
      PrintOptions PO;
      PO.OpaqueReturnTypePrinting =
          PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
      Builder.addTypeAnnotation(T.getString(PO));
    }
  }

  void addTypeAnnotationForImplicitlyUnwrappedOptional(
      CodeCompletionResultBuilder &Builder, Type T,
      bool dynamicOrOptional = false) {

    std::string suffix;
    // FIXME: This retains previous behavior, but in reality the type of dynamic
    // lookups is IUO, not Optional as it is for the @optional attribute.
    if (dynamicOrOptional) {
      T = T->getOptionalObjectType();
      suffix = "?";
    }

    T = T->getReferenceStorageReferent();
    PrintOptions PO;
    PO.PrintOptionalAsImplicitlyUnwrapped = true;
    PO.OpaqueReturnTypePrinting =
        PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
    Builder.addTypeAnnotation(T.getString(PO) + suffix);
  }

  /// For printing in code completion results, replace archetypes with
  /// protocol compositions.
  ///
  /// FIXME: Perhaps this should be an option in PrintOptions instead.
  Type eraseArchetypes(Type type, GenericSignature genericSig) {
    if (!genericSig)
      return type;

    auto buildProtocolComposition = [&](ArrayRef<ProtocolDecl *> protos) -> Type {
      SmallVector<Type, 2> types;
      for (auto proto : protos)
        types.push_back(proto->getDeclaredInterfaceType());
      return ProtocolCompositionType::get(Ctx, types,
                                          /*HasExplicitAnyObject=*/false);
    };

    if (auto *genericFuncType = type->getAs<GenericFunctionType>()) {
      SmallVector<AnyFunctionType::Param, 8> erasedParams;
      for (const auto &param : genericFuncType->getParams()) {
        auto erasedTy = eraseArchetypes(param.getPlainType(), genericSig);
        erasedParams.emplace_back(erasedTy, param.getLabel(),
                                  param.getParameterFlags());
      }
      return GenericFunctionType::get(genericSig,
          erasedParams,
          eraseArchetypes(genericFuncType->getResult(), genericSig),
          genericFuncType->getExtInfo());
    }

    return type.transform([&](Type t) -> Type {
      // FIXME: Code completion should only deal with one or the other,
      // and not both.
      if (auto *archetypeType = t->getAs<ArchetypeType>()) {
        // Don't erase opaque archetype.
        if (isa<OpaqueTypeArchetypeType>(archetypeType))
          return t;

        auto protos = archetypeType->getConformsTo();
        if (!protos.empty())
          return buildProtocolComposition(protos);
      }

      if (t->isTypeParameter()) {
        auto protos = genericSig->getConformsTo(t);
        if (!protos.empty())
          return buildProtocolComposition(protos);
      }

      return t;
    });
  }

  Type getTypeOfMember(const ValueDecl *VD,
                       DynamicLookupInfo dynamicLookupInfo) {
    switch (dynamicLookupInfo.getKind()) {
    case DynamicLookupInfo::None:
      return getTypeOfMember(VD, this->ExprType);
    case DynamicLookupInfo::AnyObject:
      return getTypeOfMember(VD, Type());
    case DynamicLookupInfo::KeyPathDynamicMember: {
      auto &keyPathInfo = dynamicLookupInfo.getKeyPathDynamicMember();

      // Map the result of VD to keypath member lookup results.
      // Given:
      //   struct Wrapper<T> {
      //     subscript<U>(dynamicMember: KeyPath<T, U>) -> Wrapped<U> { get }
      //   }
      //   struct Circle {
      //     var center: Point { get }
      //     var radius: Length { get }
      //   }
      //
      // Consider 'Wrapper<Circle>.center'.
      //   'VD' is 'Circle.center' decl.
      //   'keyPathInfo.subscript' is 'Wrapper<T>.subscript' decl.
      //   'keyPathInfo.baseType' is 'Wrapper<Circle>' type.

      // FIXME: Handle nested keypath member lookup.
      // i.e. cases where 'ExprType' != 'keyPathInfo.baseType'.

      auto *SD = keyPathInfo.subscript;
      auto elementTy = SD->getElementTypeLoc().getType();
      if (!elementTy->hasTypeParameter())
        return elementTy;

      // Map is:
      //   { τ_0_0(T) => Circle
      //     τ_1_0(U) => U }
      auto subs = keyPathInfo.baseType->getMemberSubstitutions(SD);

      // If the keyPath result type has type parameters, that might affect the
      // subscript result type.
      auto keyPathResultTy = getResultTypeOfKeypathDynamicMember(SD)->
        mapTypeOutOfContext();
      if (keyPathResultTy->hasTypeParameter()) {
        auto keyPathRootTy = getRootTypeOfKeypathDynamicMember(SD).
          subst(QueryTypeSubstitutionMap{subs},
                LookUpConformanceInModule(CurrModule));

        // The result type of the VD.
        // i.e. 'Circle.center' => 'Point'.
        auto innerResultTy = getTypeOfMember(VD, keyPathRootTy);

        if (auto paramTy = keyPathResultTy->getAs<GenericTypeParamType>()) {
          // Replace keyPath result type in the map with the inner result type.
          // i.e. Make the map as:
          //   { τ_0_0(T) => Circle
          //     τ_1_0(U) => Point }
          auto key =
              paramTy->getCanonicalType()->castTo<GenericTypeParamType>();
          subs[key] = innerResultTy;
        } else {
          // FIXME: Handle the case where the KeyPath result is generic.
          // e.g. 'subscript<U>(dynamicMember: KeyPath<T, Box<U>>) -> Bag<U>'
          // For now, just return the inner type.
          return innerResultTy;
        }
      }

      // Substitute the element type of the subscript using modified map.
      // i.e. 'Wrapped<U>' => 'Wrapped<Point>'.
      return elementTy.subst(QueryTypeSubstitutionMap{subs},
                             LookUpConformanceInModule(CurrModule));
    }
    }
    llvm_unreachable("Unhandled DynamicLookupInfo Kind in switch");
  }

  Type getTypeOfMember(const ValueDecl *VD, Type ExprType) {
    auto GenericSig = VD->getInnermostDeclContext()
        ->getGenericSignatureOfContext();

    Type T = VD->getInterfaceType();
    assert(!T.isNull());

    if (ExprType) {
      Type ContextTy = VD->getDeclContext()->getDeclaredInterfaceType();
      if (ContextTy) {
        // Look through lvalue types and metatypes
        Type MaybeNominalType = ExprType->getRValueType();

        if (auto Metatype = MaybeNominalType->getAs<MetatypeType>())
          MaybeNominalType = Metatype->getInstanceType();

        if (auto SelfType = MaybeNominalType->getAs<DynamicSelfType>())
          MaybeNominalType = SelfType->getSelfType();

        // For optional protocol requirements and dynamic dispatch,
        // strip off optionality from the base type, but only if
        // we're not actually completing a member of Optional.
        if (!ContextTy->getOptionalObjectType() &&
            MaybeNominalType->getOptionalObjectType())
          MaybeNominalType = MaybeNominalType->getOptionalObjectType();

        // For dynamic lookup don't substitute in the base type.
        if (MaybeNominalType->isAnyObject())
          return T;

        // FIXME: Sometimes ExprType is the type of the member here,
        // and not the type of the base. That is inconsistent and
        // should be cleaned up.
        if (!MaybeNominalType->mayHaveMembers())
          return T;

        // We can't do anything if the base type has unbound generic parameters.
        if (MaybeNominalType->hasUnboundGenericType())
          return T;

        // For everything else, substitute in the base type.
        auto Subs = MaybeNominalType->getMemberSubstitutionMap(CurrModule, VD);

        // Pass in DesugarMemberTypes so that we see the actual
        // concrete type witnesses instead of type alias types.
        T = T.subst(Subs, SubstFlags::DesugarMemberTypes);
      }
    }

    return eraseArchetypes(T, GenericSig);
  }

  Type getAssociatedTypeType(const AssociatedTypeDecl *ATD) {
    Type BaseTy = BaseType;
    if (!BaseTy)
      BaseTy = ExprType;
    if (!BaseTy && CurrDeclContext)
      BaseTy = CurrDeclContext->getInnermostTypeContext()
                   ->getDeclaredTypeInContext();
    if (BaseTy) {
      BaseTy = BaseTy->getInOutObjectType()->getMetatypeInstanceType();
      if (auto NTD = BaseTy->getAnyNominal()) {
        auto *Module = NTD->getParentModule();
        auto Conformance = Module->lookupConformance(
            BaseTy, ATD->getProtocol());
        if (Conformance && Conformance->isConcrete()) {
          return Conformance->getConcrete()
              ->getTypeWitness(const_cast<AssociatedTypeDecl *>(ATD));
        }
      }
    }
    return Type();
  }

  void addVarDeclRef(const VarDecl *VD, DeclVisibilityKind Reason,
                     DynamicLookupInfo dynamicLookupInfo) {
    if (!VD->hasName() ||
        !VD->isAccessibleFrom(CurrDeclContext) ||
        VD->shouldHideFromEditor())
      return;

    Identifier Name = VD->getName();
    assert(!Name.empty() && "name should not be empty");

    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(VD, Reason, dynamicLookupInfo), expectedTypeContext);
    Builder.setAssociatedDecl(VD);
    addLeadingDot(Builder);
    addValueBaseName(Builder, Name);
    setClangDeclKeywords(VD, Pairs, Builder);

    if (!VD->hasInterfaceType())
      return;

    // Add a type annotation.
    Type VarType = getTypeOfMember(VD, dynamicLookupInfo);
    if (auto *PD = dyn_cast<ParamDecl>(VD)) {
      if (Name != Ctx.Id_self && PD->isInOut()) {
        // It is useful to show inout for function parameters.
        // But for 'self' it is just noise.
        VarType = InOutType::get(VarType);
      }
    }
    auto DynamicOrOptional =
        IsDynamicLookup || VD->getAttrs().hasAttribute<OptionalAttr>();
    if (DynamicOrOptional) {
      // Values of properties that were found on a AnyObject have
      // Optional<T> type.  Same applies to optional members.
      VarType = OptionalType::get(VarType);
    }
    if (VD->isImplicitlyUnwrappedOptional())
      addTypeAnnotationForImplicitlyUnwrappedOptional(Builder, VarType,
                                                      DynamicOrOptional);
    else
      addTypeAnnotation(Builder, VarType);
  }

  static bool hasInterestingDefaultValues(const AbstractFunctionDecl *func) {
    if (!func) return false;

    for (auto param : *func->getParameters()) {
      switch (param->getDefaultArgumentKind()) {
      case DefaultArgumentKind::Normal:
      case DefaultArgumentKind::StoredProperty:
      case DefaultArgumentKind::Inherited: // FIXME: include this?
        return true;
      default:
        break;
      }
    }
    return false;
  }

  /// Build argument patterns for calling. Returns \c true if any content was
  /// added to \p Builder. If \p declParams is non-empty, the size must match
  /// with \p typeParams.
  bool addCallArgumentPatterns(CodeCompletionResultBuilder &Builder,
                               ArrayRef<AnyFunctionType::Param> typeParams,
                               ArrayRef<const ParamDecl *> declParams,
                               bool includeDefaultArgs = true) {
    assert(declParams.empty() || typeParams.size() == declParams.size());

    bool modifiedBuilder = false;

    // Determine whether we should skip this argument because it is defaulted.
    auto shouldSkipArg = [&](const ParamDecl *PD) -> bool {
      switch (PD->getDefaultArgumentKind()) {
      case DefaultArgumentKind::None:
        return false;

      case DefaultArgumentKind::Normal:
      case DefaultArgumentKind::StoredProperty:
      case DefaultArgumentKind::Inherited:
      case DefaultArgumentKind::NilLiteral:
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

      llvm_unreachable("Unhandled DefaultArgumentKind in switch.");
    };

    bool NeedComma = false;
    // Iterate over each parameter.
    for (unsigned i = 0; i != typeParams.size(); ++i) {
      auto &typeParam = typeParams[i];

      Identifier argName;
      Identifier bodyName;
      bool isIUO = false;

      if (!declParams.empty()) {
        auto *PD = declParams[i];
        if (shouldSkipArg(PD))
          continue;
        argName = PD->getArgumentName();
        bodyName = PD->getParameterName();
        isIUO = PD->isImplicitlyUnwrappedOptional();
      } else {
        isIUO = false;
        argName = typeParam.getLabel();
      }

      bool isVariadic = typeParam.isVariadic();
      bool isInOut = typeParam.isInOut();
      bool isAutoclosure = typeParam.isAutoClosure();
      Type paramTy = typeParam.getPlainType();
      if (isVariadic)
        paramTy = ParamDecl::getVarargBaseTy(paramTy);

      if (NeedComma)
        Builder.addComma();

      Builder.addCallParameter(argName, bodyName, paramTy, isVariadic, isInOut,
                               isIUO, isAutoclosure);

      modifiedBuilder = true;
      NeedComma = true;
    }
    return modifiedBuilder;
  }

  /// Build argument patterns for calling. Returns \c true if any content was
  /// added to \p Builder. If \p Params is non-nullptr, \F
  bool addCallArgumentPatterns(CodeCompletionResultBuilder &Builder,
                               const AnyFunctionType *AFT,
                               const ParameterList *Params,
                               bool includeDefaultArgs = true) {
    ArrayRef<const ParamDecl *> declParams;
    if (Params)
      declParams = Params->getArray();
    return addCallArgumentPatterns(Builder, AFT->getParams(), declParams,
                                   includeDefaultArgs);
  }

  static void addThrows(CodeCompletionResultBuilder &Builder,
                        const AnyFunctionType *AFT,
                        const AbstractFunctionDecl *AFD) {
    if (AFD && AFD->getAttrs().hasAttribute<RethrowsAttr>())
      Builder.addAnnotatedRethrows();
    else if (AFT->throws())
      Builder.addAnnotatedThrows();
  }

  void addPoundAvailable(Optional<StmtKind> ParentKind) {
    if (ParentKind != StmtKind::If && ParentKind != StmtKind::Guard)
      return;
    CodeCompletionResultBuilder Builder(Sink, CodeCompletionResult::ResultKind::Keyword,
      SemanticContextKind::ExpressionSpecific, expectedTypeContext);
    Builder.addTextChunk("available");
    Builder.addLeftParen();
    Builder.addSimpleTypedParameter("Platform", /*IsVarArg=*/true);
    Builder.addComma();
    Builder.addTextChunk("*");
    Builder.addRightParen();
  }

  void addPoundSelector(bool needPound) {
    // #selector is only available when the Objective-C runtime is.
    if (!Ctx.LangOpts.EnableObjCInterop) return;

    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    if (needPound)
      Builder.addTextChunk("#selector");
    else
      Builder.addTextChunk("selector");
    Builder.addLeftParen();
    Builder.addSimpleTypedParameter("@objc method", /*IsVarArg=*/false);
    Builder.addRightParen();
    Builder.addTypeAnnotation("Selector");
    // This function is called only if the context type is 'Selector'.
    Builder.setExpectedTypeRelation(
        CodeCompletionResult::ExpectedTypeRelation::Identical);
  }

  void addPoundKeyPath(bool needPound) {
    // #keyPath is only available when the Objective-C runtime is.
    if (!Ctx.LangOpts.EnableObjCInterop) return;

    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    if (needPound)
      Builder.addTextChunk("#keyPath");
    else
      Builder.addTextChunk("keyPath");
    Builder.addLeftParen();
    Builder.addSimpleTypedParameter("@objc property sequence",
                                    /*IsVarArg=*/false);
    Builder.addRightParen();
    Builder.addTypeAnnotation("String");
    // This function is called only if the context type is 'String'.
    Builder.setExpectedTypeRelation(
        CodeCompletionResult::ExpectedTypeRelation::Identical);
  }

  SemanticContextKind getSemanticContextKind(const ValueDecl *VD) {
    // FIXME: to get the corect semantic context we need to know how lookup
    // would have found the VD. For now, just infer a reasonable semantics.

    if (!VD)
      return SemanticContextKind::CurrentModule;

    DeclContext *calleeDC = VD->getDeclContext();
    
    if (calleeDC->isTypeContext())
      // FIXME: We should distinguish CurrentNominal and Super. We need to
      // propagate the base type to do that.
      return SemanticContextKind::CurrentNominal;

    if (calleeDC->isLocalContext())
      return SemanticContextKind::Local;
    if (calleeDC->getParentModule() == CurrModule)
      return SemanticContextKind::CurrentModule;

    return SemanticContextKind::OtherModule;
  }

  void addSubscriptCallPattern(const AnyFunctionType *AFT,
                               const SubscriptDecl *SD) {
    foundFunction(AFT);
    auto genericSig =
        SD->getInnermostDeclContext()->getGenericSignatureOfContext();
    AFT = eraseArchetypes(const_cast<AnyFunctionType *>(AFT), genericSig)
              ->castTo<AnyFunctionType>();

    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContextKind(SD), expectedTypeContext);
    Builder.setAssociatedDecl(SD);
    setClangDeclKeywords(SD, Pairs, Builder);
    if (!HaveLParen)
      Builder.addLeftBracket();
    else
      Builder.addAnnotatedLeftBracket();
    addCallArgumentPatterns(Builder, AFT, SD->getIndices());
    if (!HaveLParen)
      Builder.addRightBracket();
    else
      Builder.addAnnotatedRightBracket();
    if (SD && SD->isImplicitlyUnwrappedOptional())
      addTypeAnnotationForImplicitlyUnwrappedOptional(Builder,
                                                      AFT->getResult());
    else
      addTypeAnnotation(Builder, AFT->getResult());
  }

  void addFunctionCallPattern(const AnyFunctionType *AFT,
                              const AbstractFunctionDecl *AFD = nullptr) {
    if (AFD) {
      auto genericSig =
          AFD->getInnermostDeclContext()->getGenericSignatureOfContext();
      AFT = eraseArchetypes(const_cast<AnyFunctionType *>(AFT), genericSig)
                ->castTo<AnyFunctionType>();
    }

    // Add the pattern, possibly including any default arguments.
    auto addPattern = [&](ArrayRef<const ParamDecl *> declParams = {},
                          bool includeDefaultArgs = true) {
      CommandWordsPairs Pairs;
      CodeCompletionResultBuilder Builder(
          Sink,
          AFD ? CodeCompletionResult::ResultKind::Declaration
              : CodeCompletionResult::ResultKind::Pattern,
          getSemanticContextKind(AFD), expectedTypeContext);
      if (AFD) {
        Builder.setAssociatedDecl(AFD);
        setClangDeclKeywords(AFD, Pairs, Builder);
      }

      if (!HaveLParen)
        Builder.addLeftParen();
      else
        Builder.addAnnotatedLeftParen();

      addCallArgumentPatterns(Builder, AFT->getParams(), declParams,
                              includeDefaultArgs);

      // The rparen matches the lparen here so that we insert both or neither.
      if (!HaveLParen)
        Builder.addRightParen();
      else
        Builder.addAnnotatedRightParen();

      addThrows(Builder, AFT, AFD);

      if (AFD &&
          AFD->isImplicitlyUnwrappedOptional())
        addTypeAnnotationForImplicitlyUnwrappedOptional(Builder,
                                                        AFT->getResult());
      else
        addTypeAnnotation(Builder, AFT->getResult());
    };

    if (!AFD || !AFD->getInterfaceType() ||
        !AFD->getInterfaceType()->is<AnyFunctionType>()) {
      // Probably, calling closure type expression.
      foundFunction(AFT);
      addPattern();
      return;
    } else {
      // Calling function or method.
      foundFunction(AFD);

      // FIXME: Hack because we don't know we are calling instance
      // method or not. There's invariant that funcTy is derived from AFD.
      // Only if we are calling instance method on meta type, AFT is still
      // curried. So we should be able to detect that by comparing curried level
      // of AFT and the interface type of AFD.
      auto getCurriedLevel = [](const AnyFunctionType *funcTy) -> unsigned {
        unsigned level = 0;
        while ((funcTy = funcTy->getResult()->getAs<AnyFunctionType>()))
          ++level;
        return level;
      };
      bool isImplicitlyCurriedInstanceMethod =
          (AFD->hasImplicitSelfDecl() &&
           getCurriedLevel(AFT) ==
               getCurriedLevel(
                   AFD->getInterfaceType()->castTo<AnyFunctionType>()) &&
           // NOTE: shouldn't be necessary, but just in case curried level check
           // is insufficient.
           AFT->getParams().size() == 1 &&
           AFT->getParams()[0].getLabel().empty());

      if (isImplicitlyCurriedInstanceMethod) {
        addPattern({AFD->getImplicitSelfDecl()}, /*includeDefaultArgs=*/true);
      } else {
        if (hasInterestingDefaultValues(AFD))
          addPattern(AFD->getParameters()->getArray(),
                     /*includeDefaultArgs=*/false);
        addPattern(AFD->getParameters()->getArray(),
                   /*includeDefaultArgs=*/true);
      }
    }
  }

  bool isImplicitlyCurriedInstanceMethod(const AbstractFunctionDecl *FD) {
    switch (Kind) {
    case LookupKind::ValueExpr:
      return ExprType->is<AnyMetatypeType>() && !FD->isStatic();
    case LookupKind::ValueInDeclContext:
      if (InsideStaticMethod &&
          FD->getDeclContext() == CurrentMethod->getDeclContext() &&
          !FD->isStatic())
        return true;
      if (auto Init = dyn_cast<Initializer>(CurrDeclContext))
        return FD->getDeclContext() == Init->getParent() && !FD->isStatic();
      return false;
    case LookupKind::EnumElement:
    case LookupKind::Type:
    case LookupKind::TypeInDeclContext:
      llvm_unreachable("cannot have a method call while doing a "
                       "type completion");
    case LookupKind::ImportFromModule:
      return false;
    }

    llvm_unreachable("Unhandled LookupKind in switch.");
  }

  void addMethodCall(const FuncDecl *FD, DeclVisibilityKind Reason,
                     DynamicLookupInfo dynamicLookupInfo) {
    if (FD->getName().empty())
      return;
    foundFunction(FD);

    Identifier Name = FD->getName();
    assert(!Name.empty() && "name should not be empty");

    Type FunctionType = getTypeOfMember(FD, dynamicLookupInfo);
    assert(FunctionType);

    auto AFT = FunctionType->getAs<AnyFunctionType>();

    bool IsImplicitlyCurriedInstanceMethod = false;
    if (FD->hasImplicitSelfDecl()) {
      IsImplicitlyCurriedInstanceMethod = isImplicitlyCurriedInstanceMethod(FD);

      // Strip off '(_ self: Self)' if needed.
      if (AFT && !IsImplicitlyCurriedInstanceMethod)
        AFT = AFT->getResult()->getAs<AnyFunctionType>();
    }

    bool trivialTrailingClosure = false;
    if (AFT && !IsImplicitlyCurriedInstanceMethod)
      trivialTrailingClosure = hasTrivialTrailingClosure(FD, AFT);

    // Add the method, possibly including any default arguments.
    auto addMethodImpl = [&](bool includeDefaultArgs = true,
                             bool trivialTrailingClosure = false) {
      CommandWordsPairs Pairs;
      CodeCompletionResultBuilder Builder(
          Sink, CodeCompletionResult::ResultKind::Declaration,
          getSemanticContext(FD, Reason, dynamicLookupInfo),
          expectedTypeContext);
      setClangDeclKeywords(FD, Pairs, Builder);
      Builder.setAssociatedDecl(FD);
      addLeadingDot(Builder);
      addValueBaseName(Builder, Name);
      if (IsDynamicLookup)
        Builder.addDynamicLookupMethodCallTail();
      else if (FD->getAttrs().hasAttribute<OptionalAttr>())
        Builder.addOptionalMethodCallTail();

      llvm::SmallString<32> TypeStr;

      if (!AFT) {
        llvm::raw_svector_ostream OS(TypeStr);
        FunctionType.print(OS);
        Builder.addTypeAnnotation(OS.str());
        return;
      }

      if (IsImplicitlyCurriedInstanceMethod) {
        Builder.addLeftParen();
        addCallArgumentPatterns(Builder, AFT->getParams(),
                                {FD->getImplicitSelfDecl()},
                                includeDefaultArgs);
        Builder.addRightParen();
      } else if (trivialTrailingClosure) {
        Builder.addBraceStmtWithCursor(" { code }");
        addThrows(Builder, AFT, FD);
      } else {
        Builder.addLeftParen();
        addCallArgumentPatterns(Builder, AFT, FD->getParameters(),
                                includeDefaultArgs);
        Builder.addRightParen();
        addThrows(Builder, AFT, FD);
      }

      Type ResultType = AFT->getResult();

      // Build type annotation.
      {
        llvm::raw_svector_ostream OS(TypeStr);
        if (IsImplicitlyCurriedInstanceMethod) {
          auto *FnType = ResultType->castTo<AnyFunctionType>();
          AnyFunctionType::printParams(FnType->getParams(), OS);
          ResultType = FnType->getResult();
          OS << " -> ";
        }

        // What's left is the result type.
        if (ResultType->isVoid()) {
          OS << "Void";
        } else {
          // As we did with parameters in addParamPatternFromFunction,
          // for regular methods we'll print '!' after implicitly
          // unwrapped optional results.
          bool IsIUO =
              !IsImplicitlyCurriedInstanceMethod &&
              FD->isImplicitlyUnwrappedOptional();

          PrintOptions PO;
          PO.OpaqueReturnTypePrinting =
              PrintOptions::OpaqueReturnTypePrintingMode::WithoutOpaqueKeyword;
          PO.PrintOptionalAsImplicitlyUnwrapped = IsIUO;
          ResultType.print(OS, PO);
        }
      }
      Builder.addTypeAnnotation(TypeStr);
    };

    if (!AFT || IsImplicitlyCurriedInstanceMethod) {
      addMethodImpl();
    } else {
      if (trivialTrailingClosure)
        addMethodImpl(/*includeDefaultArgs=*/false,
                      /*trivialTrailingClosure=*/true);
      if (hasInterestingDefaultValues(FD))
        addMethodImpl(/*includeDefaultArgs=*/false);
      addMethodImpl(/*includeDefaultArgs=*/true);
    }
  }

  void addConstructorCall(const ConstructorDecl *CD, DeclVisibilityKind Reason,
                          DynamicLookupInfo dynamicLookupInfo,
                          Optional<Type> BaseType, Optional<Type> Result,
                          bool IsOnType = true,
                          Identifier addName = Identifier()) {
    foundFunction(CD);
    Type MemberType = getTypeOfMember(CD, BaseType.getValueOr(ExprType));
    AnyFunctionType *ConstructorType = nullptr;
    if (auto MemberFuncType = MemberType->getAs<AnyFunctionType>())
      ConstructorType = MemberFuncType->getResult()
                                      ->castTo<AnyFunctionType>();

    bool needInit = false;
    if (!IsOnType) {
      assert(addName.empty());
      needInit = true;
    } else if (addName.empty() && HaveDot) {
      needInit = true;
    }

    // If we won't be able to provide a result, bail out.
    if (!ConstructorType && addName.empty() && !needInit)
      return;

    // Add the constructor, possibly including any default arguments.
    auto addConstructorImpl = [&](bool includeDefaultArgs = true) {
      CommandWordsPairs Pairs;
      CodeCompletionResultBuilder Builder(
          Sink, CodeCompletionResult::ResultKind::Declaration,
          getSemanticContext(CD, Reason, dynamicLookupInfo),
          expectedTypeContext);
      setClangDeclKeywords(CD, Pairs, Builder);
      Builder.setAssociatedDecl(CD);
      if (needInit) {
        assert(addName.empty());
        addLeadingDot(Builder);
        Builder.addTextChunk("init");
      } else if (!addName.empty()) {
        Builder.addTextChunk(addName.str());
      }

      if (!ConstructorType) {
        addTypeAnnotation(Builder, MemberType);
        return;
      }

      if (!HaveLParen)
        Builder.addLeftParen();
      else
        Builder.addAnnotatedLeftParen();

      addCallArgumentPatterns(Builder, ConstructorType, CD->getParameters(),
                              includeDefaultArgs);

      // The rparen matches the lparen here so that we insert both or neither.
      if (!HaveLParen)
        Builder.addRightParen();
      else
        Builder.addAnnotatedRightParen();

      addThrows(Builder, ConstructorType, CD);

      if (CD->isImplicitlyUnwrappedOptional()) {
        addTypeAnnotationForImplicitlyUnwrappedOptional(
            Builder, Result.hasValue() ? Result.getValue()
                                       : ConstructorType->getResult());
      } else {
        addTypeAnnotation(Builder, Result.hasValue()
                                       ? Result.getValue()
                                       : ConstructorType->getResult());
      }
    };

    if (ConstructorType && hasInterestingDefaultValues(CD))
      addConstructorImpl(/*includeDefaultArgs*/ false);
    addConstructorImpl();
  }

  void addConstructorCallsForType(Type type, Identifier name,
                                  DeclVisibilityKind Reason,
                                  DynamicLookupInfo dynamicLookupInfo) {
    if (!Ctx.LangOpts.CodeCompleteInitsInPostfixExpr && !IsUnresolvedMember)
      return;

    assert(CurrDeclContext);
    SmallVector<ValueDecl *, 16> initializers;
    if (CurrDeclContext->lookupQualified(type, DeclBaseName::createConstructor(),
                                         NL_QualifiedDefault,
                                         initializers)) {
      for (auto *init : initializers) {
        if (init->shouldHideFromEditor())
          continue;
        if (IsUnresolvedMember &&
            cast<ConstructorDecl>(init)->isFailable() &&
            !cast<ConstructorDecl>(init)->isImplicitlyUnwrappedOptional()) {
          continue;
        }
        addConstructorCall(cast<ConstructorDecl>(init), Reason,
                           dynamicLookupInfo, type, None,
                           /*IsOnType=*/true, name);
      }
    }
  }

  void addSubscriptCall(const SubscriptDecl *SD, DeclVisibilityKind Reason,
                        DynamicLookupInfo dynamicLookupInfo) {
    // Don't add subscript call to unqualified completion.
    if (!ExprType)
      return;

    // Subscript after '.' is valid only after type part of Swift keypath
    // expression. (e.g. '\TyName.SubTy.[0])
    if (HaveDot && !IsAfterSwiftKeyPathRoot)
      return;

    auto subscriptType =
        getTypeOfMember(SD, dynamicLookupInfo)->getAs<AnyFunctionType>();
    if (!subscriptType)
      return;

    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(SD, Reason, dynamicLookupInfo), expectedTypeContext);
    Builder.setAssociatedDecl(SD);
    setClangDeclKeywords(SD, Pairs, Builder);

    // '\TyName#^TOKEN^#' requires leading dot.
    if (!HaveDot && IsAfterSwiftKeyPathRoot)
      Builder.addLeadingDot();

    if (NeedOptionalUnwrap) {
      Builder.setNumBytesToErase(NumBytesToEraseForOptionalUnwrap);
      Builder.addQuestionMark();
    }

    Builder.addLeftBracket();
    addCallArgumentPatterns(Builder, subscriptType, SD->getIndices(), true);
    Builder.addRightBracket();

    // Add a type annotation.
    Type resultTy = subscriptType->getResult();
    if (IsDynamicLookup) {
      // Values of properties that were found on a AnyObject have
      // Optional<T> type.
      resultTy = OptionalType::get(resultTy);
    }
    addTypeAnnotation(Builder, resultTy);
  }

  void addNominalTypeRef(const NominalTypeDecl *NTD, DeclVisibilityKind Reason,
                         DynamicLookupInfo dynamicLookupInfo) {
    if (IsUnresolvedMember)
      return;
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(NTD, Reason, dynamicLookupInfo),
        expectedTypeContext);
    Builder.setAssociatedDecl(NTD);
    setClangDeclKeywords(NTD, Pairs, Builder);
    addLeadingDot(Builder);
    Builder.addTextChunk(NTD->getName().str());
    addTypeAnnotation(Builder, NTD->getDeclaredType());
  }

  void addTypeAliasRef(const TypeAliasDecl *TAD, DeclVisibilityKind Reason,
                       DynamicLookupInfo dynamicLookupInfo) {
    if (IsUnresolvedMember)
      return;
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(TAD, Reason, dynamicLookupInfo),
        expectedTypeContext);
    Builder.setAssociatedDecl(TAD);
    setClangDeclKeywords(TAD, Pairs, Builder);
    addLeadingDot(Builder);
    Builder.addTextChunk(TAD->getName().str());
    if (auto underlyingType = TAD->getUnderlyingType()) {
      if (underlyingType->hasError()) {
        Type parentType;
        if (auto nominal = TAD->getDeclContext()->getSelfNominalTypeDecl()) {
          parentType = nominal->getDeclaredInterfaceType();
        }
        addTypeAnnotation(
                      Builder,
                      TypeAliasType::get(const_cast<TypeAliasDecl *>(TAD),
                                         parentType, SubstitutionMap(),
                                         underlyingType));

      } else {
        addTypeAnnotation(Builder, underlyingType);
      }
    }
  }

  void addGenericTypeParamRef(const GenericTypeParamDecl *GP,
                              DeclVisibilityKind Reason,
                              DynamicLookupInfo dynamicLookupInfo) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(GP, Reason, dynamicLookupInfo), expectedTypeContext);
    setClangDeclKeywords(GP, Pairs, Builder);
    Builder.setAssociatedDecl(GP);
    addLeadingDot(Builder);
    Builder.addTextChunk(GP->getName().str());
    addTypeAnnotation(Builder, GP->getDeclaredInterfaceType());
  }

  void addAssociatedTypeRef(const AssociatedTypeDecl *AT,
                            DeclVisibilityKind Reason,
                            DynamicLookupInfo dynamicLookupInfo) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(AT, Reason, dynamicLookupInfo), expectedTypeContext);
    setClangDeclKeywords(AT, Pairs, Builder);
    Builder.setAssociatedDecl(AT);
    addLeadingDot(Builder);
    Builder.addTextChunk(AT->getName().str());
    if (Type T = getAssociatedTypeType(AT))
      addTypeAnnotation(Builder, T);
  }

  void addPrecedenceGroupRef(PrecedenceGroupDecl *PGD) {
    auto semanticContext =
        getSemanticContext(PGD, DeclVisibilityKind::VisibleAtTopLevel, {});
    CodeCompletionResultBuilder builder(
      Sink, CodeCompletionResult::ResultKind::Declaration,
      semanticContext, {});

    builder.addTextChunk(PGD->getName().str());
    builder.setAssociatedDecl(PGD);
  };

  void addEnumElementRef(const EnumElementDecl *EED, DeclVisibilityKind Reason,
                         DynamicLookupInfo dynamicLookupInfo,
                         bool HasTypeContext) {
    if (!EED->hasName() ||
        !EED->isAccessibleFrom(CurrDeclContext) ||
        EED->shouldHideFromEditor())
      return;

    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        HasTypeContext ? SemanticContextKind::ExpressionSpecific
                       : getSemanticContext(EED, Reason, dynamicLookupInfo),
        expectedTypeContext);
    Builder.setAssociatedDecl(EED);
    setClangDeclKeywords(EED, Pairs, Builder);
    addLeadingDot(Builder);
    addValueBaseName(Builder, EED->getName());

    // Enum element is of function type; (Self.type) -> Self or
    // (Self.Type) -> (Args...) -> Self.
    Type EnumType = getTypeOfMember(EED, dynamicLookupInfo);
    if (EnumType->is<AnyFunctionType>())
      EnumType = EnumType->castTo<AnyFunctionType>()->getResult();

    if (EnumType->is<FunctionType>()) {
      Builder.addLeftParen();
      addCallArgumentPatterns(Builder, EnumType->castTo<FunctionType>(),
                              EED->getParameterList());
      Builder.addRightParen();

      // Extract result as the enum type.
      EnumType = EnumType->castTo<FunctionType>()->getResult();
    }

    addTypeAnnotation(Builder, EnumType);
  }

  void addKeyword(StringRef Name, Type TypeAnnotation = Type(),
                  SemanticContextKind SK = SemanticContextKind::None,
                  CodeCompletionKeywordKind KeyKind
                    = CodeCompletionKeywordKind::None,
                  unsigned NumBytesToErase = 0) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword, SK,
        expectedTypeContext);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    Builder.setKeywordKind(KeyKind);
    if (TypeAnnotation)
      addTypeAnnotation(Builder, TypeAnnotation);
    if (NumBytesToErase > 0)
      Builder.setNumBytesToErase(NumBytesToErase);
  }

  void addKeyword(StringRef Name, StringRef TypeAnnotation,
                  CodeCompletionKeywordKind KeyKind
                    = CodeCompletionKeywordKind::None) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, expectedTypeContext);
    addLeadingDot(Builder);
    Builder.addTextChunk(Name);
    Builder.setKeywordKind(KeyKind);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
  }

  void addDeclAttrParamKeyword(StringRef Name, StringRef Annotation,
                             bool NeedSpecify) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, expectedTypeContext);
    Builder.addDeclAttrParamKeyword(Name, Annotation, NeedSpecify);
  }

  void addDeclAttrKeyword(StringRef Name, StringRef Annotation) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, expectedTypeContext);
    Builder.addDeclAttrKeyword(Name, Annotation);
  }

  /// Add the compound function name for the given function.
  void addCompoundFunctionName(AbstractFunctionDecl *AFD,
                               DeclVisibilityKind Reason,
                               DynamicLookupInfo dynamicLookupInfo) {
    CommandWordsPairs Pairs;
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        getSemanticContext(AFD, Reason, dynamicLookupInfo),
        expectedTypeContext);
    setClangDeclKeywords(AFD, Pairs, Builder);
    Builder.setAssociatedDecl(AFD);

    // Base name
    addLeadingDot(Builder);
    addValueBaseName(Builder, AFD->getBaseName());

    // Add the argument labels.
    auto ArgLabels = AFD->getFullName().getArgumentNames();
    if (!ArgLabels.empty()) {
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

      Builder.addRightParen();
    }
  }

  // Implement swift::VisibleDeclConsumer.
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    if (D->shouldHideFromEditor())
      return;

    if (IsKeyPathExpr && !KeyPathFilter(D, Reason))
      return;

    if (IsSwiftKeyPathExpr && !SwiftKeyPathFilter(D, Reason))
      return;
    
    // FIXME(InterfaceTypeRequest): Remove this.
    (void)D->getInterfaceType();
    switch (Kind) {
    case LookupKind::ValueExpr:
      if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
        // Do we want compound function names here?
        if (shouldUseFunctionReference(CD)) {
          addCompoundFunctionName(CD, Reason, dynamicLookupInfo);
          return;
        }

        if (auto MT = ExprType->getAs<AnyMetatypeType>()) {
          Type Ty = MT->getInstanceType();
          assert(Ty && "Cannot find instance type.");

          // If instance type is type alias, show users that the constructed
          // type is the typealias instead of the underlying type of the alias.
          Optional<Type> Result = None;
          if (!CD->getInterfaceType()->is<ErrorType>() &&
              isa<TypeAliasType>(Ty.getPointer()) &&
              Ty->getDesugaredType() ==
                CD->getResultInterfaceType().getPointer()) {
            Result = Ty;
          }
          // If the expression type is not a static metatype or an archetype, the base
          // is not a type. Direct call syntax is illegal on values, so we only add
          // initializer completions if we do not have a left parenthesis and either
          // the initializer is required, the base type's instance type is not a class,
          // or this is a 'self' or 'super' reference.
          if (IsStaticMetatype || IsUnresolvedMember || Ty->is<ArchetypeType>())
            addConstructorCall(CD, Reason, dynamicLookupInfo, None, Result,
                               /*isOnType*/ true);
          else if ((IsSelfRefExpr || IsSuperRefExpr || !Ty->is<ClassType>() ||
                    CD->isRequired()) && !HaveLParen)
            addConstructorCall(CD, Reason, dynamicLookupInfo, None, Result,
                               /*isOnType*/ false);
          return;
        }
        if (!HaveLParen) {
          auto CDC = dyn_cast<ConstructorDecl>(CurrDeclContext);
          if (!CDC)
            return;
          // We do not want 'init' completions for 'self' in non-convenience
          // initializers and for 'super' in convenience initializers.
          if ((IsSelfRefExpr && CDC->isConvenienceInit()) ||
              ((IsSuperRefExpr && !CDC->isConvenienceInit())))
            addConstructorCall(CD, Reason, dynamicLookupInfo, None, None,
                               /*IsOnType=*/false);
        }
        return;
      }

      if (HaveLParen)
        return;

      LLVM_FALLTHROUGH;

    case LookupKind::ValueInDeclContext:
    case LookupKind::ImportFromModule:
      if (auto *VD = dyn_cast<VarDecl>(D)) {
        addVarDeclRef(VD, Reason, dynamicLookupInfo);
        return;
      }

      if (auto *FD = dyn_cast<FuncDecl>(D)) {
        // We cannot call operators with a postfix parenthesis syntax.
        if (FD->isBinaryOperator() || FD->isUnaryOperator())
          return;

        // We cannot call accessors.  We use VarDecls and SubscriptDecls to
        // produce completions that refer to getters and setters.
        if (isa<AccessorDecl>(FD))
          return;

        // Do we want compound function names here?
        if (shouldUseFunctionReference(FD)) {
          addCompoundFunctionName(FD, Reason, dynamicLookupInfo);
          return;
        }

        addMethodCall(FD, Reason, dynamicLookupInfo);
        return;
      }

      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason, dynamicLookupInfo);
        addConstructorCallsForType(NTD->getDeclaredInterfaceType(),
                                   NTD->getName(), Reason, dynamicLookupInfo);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason, dynamicLookupInfo);
        auto type = TAD->mapTypeIntoContext(TAD->getDeclaredInterfaceType());
        if (type->mayHaveMembers())
          addConstructorCallsForType(type, TAD->getName(), Reason,
                                     dynamicLookupInfo);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason, dynamicLookupInfo);
        for (auto *protocol : GP->getConformingProtocols())
          addConstructorCallsForType(protocol->getDeclaredInterfaceType(),
                                     GP->getName(), Reason, dynamicLookupInfo);
        return;
      }

      if (auto *AT = dyn_cast<AssociatedTypeDecl>(D)) {
        addAssociatedTypeRef(AT, Reason, dynamicLookupInfo);
        return;
      }

      if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
        addEnumElementRef(EED, Reason, dynamicLookupInfo,
                          /*HasTypeContext=*/false);
        return;
      }

      // Swift key path allows .[0]
      if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
        addSubscriptCall(SD, Reason, dynamicLookupInfo);
        return;
      }
      return;

    case LookupKind::EnumElement:
      handleEnumElement(D, Reason, dynamicLookupInfo);
      return;

    case LookupKind::Type:
    case LookupKind::TypeInDeclContext:
      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        addNominalTypeRef(NTD, Reason, dynamicLookupInfo);
        return;
      }

      if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {
        addTypeAliasRef(TAD, Reason, dynamicLookupInfo);
        return;
      }

      if (auto *GP = dyn_cast<GenericTypeParamDecl>(D)) {
        addGenericTypeParamRef(GP, Reason, dynamicLookupInfo);
        return;
      }

      if (auto *AT = dyn_cast<AssociatedTypeDecl>(D)) {
        addAssociatedTypeRef(AT, Reason, dynamicLookupInfo);
        return;
      }

      return;
    }
  }

  bool handleEnumElement(ValueDecl *D, DeclVisibilityKind Reason,
                         DynamicLookupInfo dynamicLookupInfo) {
    if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
      addEnumElementRef(EED, Reason, dynamicLookupInfo,
                        /*HasTypeContext=*/true);
      return true;
    } else if (auto *ED = dyn_cast<EnumDecl>(D)) {
      llvm::DenseSet<EnumElementDecl *> Elements;
      ED->getAllElements(Elements);
      for (auto *Ele : Elements) {
        addEnumElementRef(Ele, Reason, dynamicLookupInfo,
                          /*HasTypeContext=*/true);
      }
      return true;
    }
    return false;
  }

  bool tryTupleExprCompletions(Type ExprType) {
    auto *TT = ExprType->getAs<TupleType>();
    if (!TT)
      return false;

    unsigned Index = 0;
    for (auto TupleElt : TT->getElements()) {
      CodeCompletionResultBuilder Builder(
          Sink,
          CodeCompletionResult::ResultKind::Pattern,
          SemanticContextKind::CurrentNominal, expectedTypeContext);
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
    return true;
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

  bool tryModuleCompletions(Type ExprType) {
    if (auto MT = ExprType->getAs<ModuleType>()) {
      ModuleDecl *M = MT->getModule();
      if (CurrModule != M) {
        // Only use the cache if it is not the current module.
        RequestedCachedResults.push_back(
          RequestedResultsTy::fromModule(M).needLeadingDot(needDot()));
        return true;
      }
    }
    return false;
  }

  /// If the given ExprType is optional, this adds completions for the unwrapped
  /// type.
  ///
  /// \return true if the given type was Optional .
  bool tryUnwrappedCompletions(Type ExprType, bool isIUO) {
    // FIXME: consider types convertible to T?.

    ExprType = ExprType->getRValueType();
    // FIXME: We don't always pass down whether a type is from an
    // unforced IUO.
    if (isIUO) {
      if (Type Unwrapped = ExprType->getOptionalObjectType()) {
        lookupVisibleMemberDecls(*this, Unwrapped, CurrDeclContext,
                                 IncludeInstanceMembers);
        return true;
      }
      assert(IsUnwrappedOptional && "IUOs should be optional if not bound/forced");
      return false;
    }

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
        if (!tryTupleExprCompletions(Unwrapped)) {
          lookupVisibleMemberDecls(*this, Unwrapped, CurrDeclContext,
                                   IncludeInstanceMembers);
        }
      }
      return true;
    }
    return false;
  }

  void getPostfixKeywordCompletions(Type ExprType, Expr *ParsedExpr) {
    if (IsSuperRefExpr)
      return;

    if (!ExprType->getAs<ModuleType>()) {
      addKeyword(getTokenText(tok::kw_self), ExprType->getRValueType(),
                 SemanticContextKind::CurrentNominal,
                 CodeCompletionKeywordKind::kw_self);
    }

    if (isa<TypeExpr>(ParsedExpr)) {
      if (auto *T = ExprType->getAs<AnyMetatypeType>()) {
        auto instanceTy = T->getInstanceType();
        if (instanceTy->isAnyExistentialType()) {
          addKeyword("Protocol", MetatypeType::get(instanceTy),
                     SemanticContextKind::CurrentNominal);
          addKeyword("Type", ExistentialMetatypeType::get(instanceTy),
                     SemanticContextKind::CurrentNominal);
        } else {
          addKeyword("Type", MetatypeType::get(instanceTy),
                     SemanticContextKind::CurrentNominal);
        }
      }
    }
  }

  void getValueExprCompletions(Type ExprType, ValueDecl *VD = nullptr) {
    Kind = LookupKind::ValueExpr;
    NeedLeadingDot = !HaveDot;

    ExprType = ExprType->getRValueType();
    assert(!ExprType->hasTypeParameter());

    this->ExprType = ExprType;

    // Open existential types, so that lookupVisibleMemberDecls() can properly
    // substitute them.
    bool WasOptional = false;
    if (auto OptionalType = ExprType->getOptionalObjectType()) {
      ExprType = OptionalType;
      WasOptional = true;
    }

    if (!ExprType->getMetatypeInstanceType()->isAnyObject())
      if (ExprType->isAnyExistentialType())
        ExprType = OpenedArchetypeType::getAny(ExprType);

    if (WasOptional)
      ExprType = OptionalType::get(ExprType);

    // Handle special cases
    bool isIUO = VD && VD->isImplicitlyUnwrappedOptional();
    if (tryFunctionCallCompletions(ExprType, VD))
      return;
    if (tryModuleCompletions(ExprType))
      return;
    if (tryTupleExprCompletions(ExprType))
      return;
    // Don't check/return so we still add the members of Optional itself below
    tryUnwrappedCompletions(ExprType, isIUO);

    lookupVisibleMemberDecls(*this, ExprType, CurrDeclContext,
                             IncludeInstanceMembers);
  }

  template <typename T>
  void collectOperatorsFromMap(SourceFile::OperatorMap<T> &map,
                               bool includePrivate,
                               std::vector<OperatorDecl *> &results) {
    for (auto &pair : map) {
      if (pair.second.getPointer() &&
          (pair.second.getInt() || includePrivate)) {
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
    for (auto import : namelookup::getAllImports(CurrDeclContext)) {
      for (auto fileUnit : import.second->getFiles()) {
        switch (fileUnit->getKind()) {
        case FileUnitKind::Builtin:
        case FileUnitKind::ClangModule:
        case FileUnitKind::DWARFModule:
          continue;
        case FileUnitKind::Source:
          collectOperatorsFrom(cast<SourceFile>(fileUnit), results);
          break;
        case FileUnitKind::SerializedAST:
          collectOperatorsFrom(cast<LoadedFile>(fileUnit), results);
          break;
        }
      }
    }
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
        getSemanticContext(op, DeclVisibilityKind::VisibleAtTopLevel, {});
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
    ConcreteDeclRef referencedDecl;
    FunctionType *funcTy = getTypeOfCompletionOperator(
        const_cast<DeclContext *>(CurrDeclContext), expr, op->getName(),
        DeclRefKind::PostfixOperator, referencedDecl);
    if (!funcTy)
      return;

    // TODO: Use referencedDecl (FuncDecl) instead of 'op' (OperatorDecl).
    addPostfixOperatorCompletion(op, funcTy->getResult());
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
    builder.addCallParameter(Identifier(), Identifier(), RHSType,
                             /*IsVarArg*/ false, /*IsInOut*/ false,
                             /*isIUO*/ false, /*isAutoClosure*/ false);
    addTypeAnnotation(builder, resultType);
  }

  void addInfixOperatorCompletion(OperatorDecl *op, Type resultType,
                                  Type RHSType) {
    // FIXME: we should get the semantic context of the function, not the
    // operator decl.
    auto semanticContext =
        getSemanticContext(op, DeclVisibilityKind::VisibleAtTopLevel, {});
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
      builder.addCallParameter(Identifier(), Identifier(), RHSType,
                               /*IsVarArg*/ false, /*IsInOut*/ false,
                               /*isIUO*/ false, /*isAutoClosure*/ false);
    if (resultType)
      addTypeAnnotation(builder, resultType);
  }

  void tryInfixOperatorCompletion(Expr *foldedExpr, InfixOperatorDecl *op) {
    ConcreteDeclRef referencedDecl;
    FunctionType *funcTy = getTypeOfCompletionOperator(
        const_cast<DeclContext *>(CurrDeclContext), foldedExpr, op->getName(),
        DeclRefKind::BinaryOperator, referencedDecl);
    if (!funcTy)
      return;

    Type lhsTy = funcTy->getParams()[0].getPlainType();
    Type rhsTy = funcTy->getParams()[1].getPlainType();
    Type resultTy = funcTy->getResult();

    // Don't complete optional operators on non-optional types.
    if (!lhsTy->getRValueType()->getOptionalObjectType()) {
      // 'T ?? T'
      if (op->getName().str() == "??")
        return;
      // 'T == nil'
      if (auto NT = rhsTy->getNominalOrBoundGenericNominal())
        if (NT->getName() ==
            CurrDeclContext->getASTContext().Id_OptionalNilComparisonType)
          return;
    }

    // If the right-hand side and result type are both type parameters, we're
    // not providing a useful completion.
    if (resultTy->isTypeParameter() && rhsTy->isTypeParameter())
      return;

    // TODO: Use referencedDecl (FuncDecl) instead of 'op' (OperatorDecl).
    addInfixOperatorCompletion(op, funcTy->getResult(),
                               funcTy->getParams()[1].getPlainType());
  }

  Expr *typeCheckLeadingSequence(Expr *LHS, ArrayRef<Expr *> leadingSequence) {
    if (leadingSequence.empty())
      return LHS;

    SourceRange sequenceRange(leadingSequence.front()->getStartLoc(),
                              LHS->getEndLoc());
    auto *expr = findParsedExpr(CurrDeclContext, sequenceRange);
    if (!expr)
      return LHS;

    if (expr->getType() && !expr->getType()->hasError())
      return expr;

    if (!typeCheckExpression(const_cast<DeclContext *>(CurrDeclContext), expr))
      return expr;
    return LHS;
  }

  void getOperatorCompletions(Expr *LHS, ArrayRef<Expr *> leadingSequence) {
    if (IsSuperRefExpr)
      return;

    Expr *foldedExpr = typeCheckLeadingSequence(LHS, leadingSequence);

    std::vector<OperatorDecl *> operators = collectOperators();
    // FIXME: this always chooses the first operator with the given name.
    llvm::DenseSet<Identifier> seenPostfixOperators;
    llvm::DenseSet<Identifier> seenInfixOperators;

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
          tryInfixOperatorCompletion(foldedExpr, cast<InfixOperatorDecl>(op));
        break;
      default:
        llvm_unreachable("unexpected operator kind");
      }
    }

    if (leadingSequence.empty() && LHS->getType() &&
        LHS->getType()->hasLValueType()) {
      addAssignmentOperator(LHS->getType()->getRValueType(),
                            CurrDeclContext->getASTContext().TheEmptyTupleType);
    }

    // FIXME: unify this with the ?.member completions.
    if (auto T = LHS->getType())
      if (auto ValueT = T->getRValueType()->getOptionalObjectType())
        addPostfixBang(ValueT);
  }

  void addTypeRelationFromProtocol(CodeCompletionResultBuilder &builder,
                                   CodeCompletionLiteralKind kind,
                                   StringRef defaultTypeName) {
    // Check for matching ExpectedTypes.
    auto *P = Ctx.getProtocol(protocolForLiteralKind(kind));
    for (auto T : expectedTypeContext.possibleTypes) {
      if (!T)
        continue;

      auto typeRelation = CodeCompletionResult::Identical;
      // Convert through optional types unless we're looking for a protocol
      // that Optional itself conforms to.
      if (kind != CodeCompletionLiteralKind::NilLiteral) {
        if (auto optionalObjT = T->getOptionalObjectType()) {
          T = optionalObjT;
          typeRelation = CodeCompletionResult::Convertible;
        }
      }

      // Check for conformance to the literal protocol.
      if (auto *NTD = T->getAnyNominal()) {
        SmallVector<ProtocolConformance *, 2> conformances;
        if (NTD->lookupConformance(CurrModule, P, conformances)) {
          addTypeAnnotation(builder, T);
          builder.setExpectedTypeRelation(typeRelation);
          return;
        }
      }
    }

    // Fallback to showing the default type.
    if (!defaultTypeName.empty())
      builder.addTypeAnnotation(defaultTypeName);
  }

  /// Add '#file', '#line', et at.
  void addPoundLiteralCompletions(bool needPound) {
    auto addFromProto = [&](StringRef name, CodeCompletionKeywordKind kwKind,
                            CodeCompletionLiteralKind literalKind,
                            StringRef defaultTypeName) {
      if (!needPound)
        name = name.substr(1);

      CodeCompletionResultBuilder builder(
          Sink, CodeCompletionResult::ResultKind::Keyword,
          SemanticContextKind::None, {});
      builder.setLiteralKind(literalKind);
      builder.setKeywordKind(kwKind);
      builder.addTextChunk(name);
      addTypeRelationFromProtocol(builder, literalKind, defaultTypeName);
    };

    addFromProto("#function", CodeCompletionKeywordKind::pound_function,
                 CodeCompletionLiteralKind::StringLiteral, "String");
    addFromProto("#file", CodeCompletionKeywordKind::pound_file,
                 CodeCompletionLiteralKind::StringLiteral, "String");
    addFromProto("#line", CodeCompletionKeywordKind::pound_line,
                 CodeCompletionLiteralKind::IntegerLiteral, "Int");
    addFromProto("#column", CodeCompletionKeywordKind::pound_column,
                 CodeCompletionLiteralKind::IntegerLiteral, "Int");

    addKeyword(needPound ? "#dsohandle" : "dsohandle", "UnsafeRawPointer",
               CodeCompletionKeywordKind::pound_dsohandle);
  }

  void addValueLiteralCompletions() {
    auto &context = CurrDeclContext->getASTContext();

    auto addFromProto = [&](
        CodeCompletionLiteralKind kind, StringRef defaultTypeName,
        llvm::function_ref<void(CodeCompletionResultBuilder &)> consumer,
        bool isKeyword = false) {

      CodeCompletionResultBuilder builder(Sink, CodeCompletionResult::Literal,
                                          SemanticContextKind::None, {});
      builder.setLiteralKind(kind);

      consumer(builder);
      addTypeRelationFromProtocol(builder, kind, defaultTypeName);
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
      builder.addCallParameter(context.getIdentifier("red"), floatType,
                               /*IsVarArg*/ false, /*IsInOut*/ false,
                               /*isIUO*/ false, /*isAutoClosure*/ false);
      builder.addComma();
      builder.addCallParameter(context.getIdentifier("green"), floatType,
                               /*IsVarArg*/ false, /*IsInOut*/ false,
                               /*isIUO*/ false, /*isAutoClosure*/ false);
      builder.addComma();
      builder.addCallParameter(context.getIdentifier("blue"), floatType,
                               /*IsVarArg*/ false, /*IsInOut*/ false,
                               /*isIUO*/ false, /*isAutoClosure*/ false);
      builder.addComma();
      builder.addCallParameter(context.getIdentifier("alpha"), floatType,
                               /*IsVarArg*/ false, /*IsInOut*/ false,
                               /*isIUO*/ false, /*isAutoClosure*/ false);
      builder.addRightParen();
    });

    auto stringType = context.getStringDecl()->getDeclaredType();
    addFromProto(LK::ImageLiteral, "", [&](Builder &builder) {
      builder.addTextChunk("#imageLiteral");
      builder.addLeftParen();
      builder.addCallParameter(context.getIdentifier("resourceName"),
                               stringType, /*IsVarArg*/ false,
                               /*IsInOut*/ false, /*isIUO*/ false,
                               /*isAutoClosure*/ false);
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
      for (auto T : expectedTypeContext.possibleTypes) {
        if (T && T->is<TupleType>() && !T->isVoid()) {
          addTypeAnnotation(builder, T);
          builder.setExpectedTypeRelation(CodeCompletionResult::Identical);
          break;
        }
      }
    }
  }

  void addObjCPoundKeywordCompletions(bool needPound) {
    if (!Ctx.LangOpts.EnableObjCInterop)
      return;

    // If the expected type is ObjectiveC.Selector, add #selector. If
    // it's String, add #keyPath.
    bool addedSelector = false;
    bool addedKeyPath = false;

    for (auto T : expectedTypeContext.possibleTypes) {
      T = T->lookThroughAllOptionalTypes();
      if (auto structDecl = T->getStructOrBoundGenericStruct()) {
        if (!addedSelector && structDecl->getName() == Ctx.Id_Selector &&
            structDecl->getParentModule()->getName() == Ctx.Id_ObjectiveC) {
          addPoundSelector(needPound);
          if (addedKeyPath)
            break;
          addedSelector = true;
          continue;
        }
      }

      if (!addedKeyPath && T->getAnyNominal() == Ctx.getStringDecl()) {
        addPoundKeyPath(needPound);
        if (addedSelector)
          break;
        addedKeyPath = true;
        continue;
      }
    }
  }

  struct FilteredDeclConsumer : public swift::VisibleDeclConsumer {
    swift::VisibleDeclConsumer &Consumer;
    DeclFilter Filter;
    FilteredDeclConsumer(swift::VisibleDeclConsumer &Consumer,
                         DeclFilter Filter) : Consumer(Consumer), Filter(Filter) {}
    void foundDecl(ValueDecl *VD, DeclVisibilityKind Kind,
                   DynamicLookupInfo dynamicLookupInfo) override {
      if (Filter(VD, Kind))
        Consumer.foundDecl(VD, Kind, dynamicLookupInfo);
    }
  };

  void getValueCompletionsInDeclContext(SourceLoc Loc,
                                        DeclFilter Filter = DefaultFilter,
                                        bool IncludeTopLevel = false,
                                        bool RequestCache = true,
                                        bool LiteralCompletions = true) {
    ExprType = Type();
    Kind = LookupKind::ValueInDeclContext;
    NeedLeadingDot = false;
    FilteredDeclConsumer Consumer(*this, Filter);
    lookupVisibleDecls(Consumer, CurrDeclContext,
                       /*IncludeTopLevel=*/IncludeTopLevel, Loc);
    if (RequestCache)
      RequestedCachedResults.push_back(RequestedResultsTy::toplevelResults());

    // Manually add any expected nominal types from imported modules so that
    // they get their expected type relation. Don't include protocols, since
    // they can't be initialized from the type name.
    // FIXME: this does not include types that conform to an expected protocol.
    // FIXME: this creates duplicate results.
    for (auto T : expectedTypeContext.possibleTypes) {
      if (auto NT = T->getAs<NominalType>()) {
        if (auto NTD = NT->getDecl()) {
          if (!isa<ProtocolDecl>(NTD) &&
              NTD->getModuleContext() != CurrModule) {
            addNominalTypeRef(NT->getDecl(),
                              DeclVisibilityKind::VisibleAtTopLevel, {});
          }
        }
      }
    }

    if (CompletionContext) {
      // FIXME: this is an awful simplification that says all and only enums can
      // use implicit member syntax (leading dot). Computing the accurate answer
      // using lookup (e.g. getUnresolvedMemberCompletions) is too expensive,
      // and for some clients this approximation is good enough.
      CompletionContext->MayUseImplicitMemberExpr =
          std::any_of(expectedTypeContext.possibleTypes.begin(),
                      expectedTypeContext.possibleTypes.end(),
          [](Type T) {
            if (auto *NTD = T->getAnyNominal())
              return isa<EnumDecl>(NTD);
            return false;
          });
    }

    if (LiteralCompletions) {
      addValueLiteralCompletions();
      addPoundLiteralCompletions(/*needPound=*/true);
    }

    addObjCPoundKeywordCompletions(/*needPound=*/true);
  }

  void getUnresolvedMemberCompletions(Type T) {
    if (!T->mayHaveMembers())
      return;

    DeclContext *DC = const_cast<DeclContext *>(CurrDeclContext);

    // We can only say .foo where foo is a static member of the contextual
    // type and has the same type (or if the member is a function, then the
    // same result type) as the contextual type.
    FilteredDeclConsumer consumer(*this, [=](ValueDecl *VD,
                                             DeclVisibilityKind Reason) {
      return isReferenceableByImplicitMemberExpr(CurrModule, DC, T, VD);
    });

    auto baseType = MetatypeType::get(T);
    llvm::SaveAndRestore<LookupKind> SaveLook(Kind, LookupKind::ValueExpr);
    llvm::SaveAndRestore<Type> SaveType(ExprType, baseType);
    llvm::SaveAndRestore<bool> SaveUnresolved(IsUnresolvedMember, true);
    lookupVisibleMemberDecls(consumer, baseType, CurrDeclContext,
                             /*includeInstanceMembers=*/false);
  }

  void getUnresolvedMemberCompletions(ArrayRef<Type> Types) {
    NeedLeadingDot = !HaveDot;
    for (auto T : Types) {
      if (!T)
        continue;
      if (auto objT = T->getOptionalObjectType()) {
        // If this is optional type, perform completion for the object type.
        // i.e. 'let _: Enum??? = .enumMember' is legal.
        getUnresolvedMemberCompletions(objT->lookThroughAllOptionalTypes());

        // Add 'nil' keyword with erasing '.' instruction.
        unsigned bytesToErase = 0;
        auto &SM = CurrDeclContext->getASTContext().SourceMgr;
        if (DotLoc.isValid())
          bytesToErase = SM.getByteDistance(DotLoc, SM.getCodeCompletionLoc());
        addKeyword("nil", T, SemanticContextKind::ExpressionSpecific,
                   CodeCompletionKeywordKind::kw_nil, bytesToErase);
      }
      getUnresolvedMemberCompletions(T);
    }
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

  void getTypeCompletions(Type BaseType) {
    Kind = LookupKind::Type;
    this->BaseType = BaseType;
    NeedLeadingDot = !HaveDot;
    lookupVisibleMemberDecls(*this, MetatypeType::get(BaseType),
                             CurrDeclContext,
                             IncludeInstanceMembers);
    if (BaseType->isAnyExistentialType()) {
      addKeyword("Protocol", MetatypeType::get(BaseType));
      addKeyword("Type", ExistentialMetatypeType::get(BaseType));
    } else {
      addKeyword("Type", MetatypeType::get(BaseType));
    }
  }

  static bool canUseAttributeOnDecl(DeclAttrKind DAK, bool IsInSil,
                                    Optional<DeclKind> DK) {
    if (DeclAttribute::isUserInaccessible(DAK))
      return false;
    if (DeclAttribute::isDeclModifier(DAK))
      return false;
    if (DeclAttribute::shouldBeRejectedByParser(DAK))
      return false;
    if (!IsInSil && DeclAttribute::isSilOnly(DAK))
      return false;
    if (!DK.hasValue())
      return true;
    return DeclAttribute::canAttributeAppearOnDeclKind(DAK, DK.getValue());
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
    if (canUseAttributeOnDecl(DAK_##NAME, IsInSil, DK))                       \
      addDeclAttrKeyword(#KEYWORD, Description);
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

  void collectPrecedenceGroups() {
    assert(CurrDeclContext);

    if (CurrModule) {
      for (auto FU: CurrModule->getFiles()) {
        // We are looking through the current module,
        // inspect only source files.
        if (FU->getKind() != FileUnitKind::Source)
          continue;

        llvm::SmallVector<PrecedenceGroupDecl*, 4> results;
        cast<SourceFile>(FU)->getPrecedenceGroups(results);

        for (auto PG: results)
            addPrecedenceGroupRef(PG);
      }
    }
    for (auto Import : namelookup::getAllImports(CurrDeclContext)) {
      auto Module = Import.second;
      if (Module == CurrModule)
        continue;

      RequestedCachedResults.push_back(
        RequestedResultsTy::fromModule(Module).onlyPrecedenceGroups());
    }
  }

  void getPrecedenceGroupCompletions(SyntaxKind SK) {
    switch (SK) {
    case SyntaxKind::PrecedenceGroupAssociativity:
      addKeyword(getAssociativitySpelling(Associativity::None));
      addKeyword(getAssociativitySpelling(Associativity::Left));
      addKeyword(getAssociativitySpelling(Associativity::Right));
      break;
    case SyntaxKind::PrecedenceGroupAssignment:
      addKeyword(getTokenText(tok::kw_false), Type(), SemanticContextKind::None,
                 CodeCompletionKeywordKind::kw_false);
      addKeyword(getTokenText(tok::kw_true), Type(), SemanticContextKind::None,
                 CodeCompletionKeywordKind::kw_true);
      break;
    case SyntaxKind::PrecedenceGroupAttributeList:
      addKeyword("associativity");
      addKeyword("higherThan");
      addKeyword("lowerThan");
      addKeyword("assignment");
      break;
    case SyntaxKind::PrecedenceGroupRelation:
      collectPrecedenceGroups();
      break;
    default:
        llvm_unreachable("not a precedencegroup SyntaxKind");
    }
  }

  void getPoundAvailablePlatformCompletions() {

    // The platform names should be identical to those in @available.
    getAttributeDeclParamCompletions(DAK_Available, 0);
  }

  void getTypeCompletionsInDeclContext(SourceLoc Loc) {
    Kind = LookupKind::TypeInDeclContext;
    lookupVisibleDecls(*this, CurrDeclContext,
                       /*IncludeTopLevel=*/false, Loc);

    RequestedCachedResults.push_back(
      RequestedResultsTy::toplevelResults().onlyTypes());
  }

  void getToplevelCompletions(bool OnlyTypes) {
    Kind = OnlyTypes ? LookupKind::TypeInDeclContext
                     : LookupKind::ValueInDeclContext;
    NeedLeadingDot = false;
    AccessFilteringDeclConsumer FilteringConsumer(CurrDeclContext, *this);
    CurrModule->lookupVisibleDecls({}, FilteringConsumer,
                                   NLKind::UnqualifiedLookup);
  }

  void getVisibleDeclsOfModule(const ModuleDecl *TheModule,
                               ArrayRef<std::string> AccessPath,
                               bool ResultsHaveLeadingDot) {
    Kind = LookupKind::ImportFromModule;
    NeedLeadingDot = ResultsHaveLeadingDot;

    llvm::SmallVector<std::pair<Identifier, SourceLoc>, 1> LookupAccessPath;
    for (auto Piece : AccessPath) {
      LookupAccessPath.push_back(
          std::make_pair(Ctx.getIdentifier(Piece), SourceLoc()));
    }
    AccessFilteringDeclConsumer FilteringConsumer(CurrDeclContext, *this);
    TheModule->lookupVisibleDecls(LookupAccessPath, FilteringConsumer,
                                  NLKind::UnqualifiedLookup);

    llvm::SmallVector<PrecedenceGroupDecl*, 16> precedenceGroups;
    TheModule->getPrecedenceGroups(precedenceGroups);

    for (auto PGD: precedenceGroups)
      addPrecedenceGroupRef(PGD);
  }
};

class CompletionOverrideLookup : public swift::VisibleDeclConsumer {
  CodeCompletionResultSink &Sink;
  ASTContext &Ctx;
  const DeclContext *CurrDeclContext;
  SmallVectorImpl<StringRef> &ParsedKeywords;
  SourceLoc introducerLoc;

  bool hasFuncIntroducer = false;
  bool hasVarIntroducer = false;
  bool hasTypealiasIntroducer = false;
  bool hasInitializerModifier = false;
  bool hasAccessModifier = false;
  bool hasOverride = false;
  bool hasOverridabilityModifier = false;
  bool hasStaticOrClass = false;

public:
  CompletionOverrideLookup(CodeCompletionResultSink &Sink, ASTContext &Ctx,
                           const DeclContext *CurrDeclContext,
                           SmallVectorImpl<StringRef> &ParsedKeywords,
                           SourceLoc introducerLoc)
      : Sink(Sink), Ctx(Ctx), CurrDeclContext(CurrDeclContext),
        ParsedKeywords(ParsedKeywords), introducerLoc(introducerLoc) {
    (void)createTypeChecker(Ctx);

    hasFuncIntroducer = isKeywordSpecified("func");
    hasVarIntroducer = isKeywordSpecified("var") ||
                       isKeywordSpecified("let");
    hasTypealiasIntroducer = isKeywordSpecified("typealias");
    hasInitializerModifier = isKeywordSpecified("required") ||
                             isKeywordSpecified("convenience");
    hasAccessModifier = isKeywordSpecified("private") ||
                        isKeywordSpecified("fileprivate") ||
                        isKeywordSpecified("internal") ||
                        isKeywordSpecified("public") ||
                        isKeywordSpecified("open");
    hasOverride = isKeywordSpecified("override");
    hasOverridabilityModifier = isKeywordSpecified("final") ||
                                isKeywordSpecified("open");
    hasStaticOrClass = isKeywordSpecified(getTokenText(tok::kw_class)) ||
                       isKeywordSpecified(getTokenText(tok::kw_static));
  }

  bool isKeywordSpecified(StringRef Word) {
    return std::find(ParsedKeywords.begin(), ParsedKeywords.end(), Word)
      != ParsedKeywords.end();
  }

  bool missingOverride(DeclVisibilityKind Reason) {
    return !hasOverride && Reason == DeclVisibilityKind::MemberOfSuper &&
           !CurrDeclContext->getSelfProtocolDecl();
  }

  void addAccessControl(const ValueDecl *VD,
                        CodeCompletionResultBuilder &Builder) {
    assert(CurrDeclContext->getSelfNominalTypeDecl());
    auto AccessOfContext =
        CurrDeclContext->getSelfNominalTypeDecl()->getFormalAccess();
    auto Access = std::min(VD->getFormalAccess(), AccessOfContext);
    // Only emit 'public', not needed otherwise.
    if (Access >= AccessLevel::Public)
      Builder.addAccessControlKeyword(Access);
  }

  /// Return type if the result type if \p VD should be represented as opaque
  /// result type.
  TypeLoc getOpaqueResultTypeLoc(const ValueDecl *VD, DeclVisibilityKind Reason,
                                 DynamicLookupInfo dynamicLookupInfo) {
    if (Reason !=
        DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal)
      return nullptr;

    auto currTy = CurrDeclContext->getDeclaredTypeInContext();
    if (!currTy)
      return nullptr;

    Type ResultT;
    if (auto *FD = dyn_cast<FuncDecl>(VD))
      ResultT = FD->getResultInterfaceType();
    else if (auto *SD = dyn_cast<SubscriptDecl>(VD))
      ResultT = SD->getElementInterfaceType();
    else if (auto *VarD = dyn_cast<VarDecl>(VD))
      ResultT = VarD->getInterfaceType();
    else
      return nullptr;

    if (!ResultT->is<DependentMemberType>())
      // The result is not associatedtype.
      return nullptr;

    // If associatedtype doesn't have conformance/superclass constraint, we
    // can't use opaque type.
    auto assocTyD = ResultT->castTo<DependentMemberType>()->getAssocType();
    if (!assocTyD->getInherited().size())
      return nullptr;

    // Try substitution to see if the associated type is resolved to concrete
    // type.
    auto substMap = currTy->getMemberSubstitutionMap(
        CurrDeclContext->getParentModule(), VD);
    ResultT = ResultT.subst(substMap);
    if (!ResultT || !ResultT->is<DependentMemberType>())
      // If resolved print it.
      return nullptr;

    return assocTyD->getInherited()[0];
  }

  void addValueOverride(const ValueDecl *VD, DeclVisibilityKind Reason,
                        DynamicLookupInfo dynamicLookupInfo,
                        CodeCompletionResultBuilder &Builder,
                        bool hasDeclIntroducer) {
    class DeclPrinter : public StreamPrinter {
      TypeLoc OpaqueBaseTy;

    public:
      using StreamPrinter::StreamPrinter;

      Optional<unsigned> NameOffset;

      DeclPrinter(raw_ostream &OS, TypeLoc OpaqueBaseTy)
          : StreamPrinter(OS), OpaqueBaseTy(OpaqueBaseTy) {}

      void printDeclLoc(const Decl *D) override {
        if (!NameOffset.hasValue())
          NameOffset = OS.tell();
      }

      // As for FuncDecl, SubscriptDecl, and VarDecl,
      void printDeclResultTypePre(ValueDecl *VD, TypeLoc &TL) override {
        if (!OpaqueBaseTy.isNull()) {
          OS << "some ";
          TL = OpaqueBaseTy;
        }
      }
    };

    llvm::SmallString<256> DeclStr;
    unsigned NameOffset = 0;
    {
      llvm::raw_svector_ostream OS(DeclStr);
      DeclPrinter Printer(
          OS, getOpaqueResultTypeLoc(VD, Reason, dynamicLookupInfo));
      PrintOptions Options;
      if (auto transformType = CurrDeclContext->getDeclaredTypeInContext())
        Options.setBaseType(transformType);
      Options.PrintImplicitAttrs = false;
      Options.ExclusiveAttrList.push_back(TAK_escaping);
      Options.ExclusiveAttrList.push_back(TAK_autoclosure);
      Options.PrintOverrideKeyword = false;
      Options.PrintPropertyAccessors = false;
      Options.PrintSubscriptAccessors = false;
      Options.PrintStaticKeyword = !hasStaticOrClass;
      VD->print(Printer, Options);
      NameOffset = Printer.NameOffset.getValue();
    }

    if (!hasDeclIntroducer && !hasAccessModifier)
      addAccessControl(VD, Builder);

    if (missingOverride(Reason)) {
      if (!hasDeclIntroducer)
        Builder.addOverrideKeyword();
      else {
        auto dist = Ctx.SourceMgr.getByteDistance(
            introducerLoc, Ctx.SourceMgr.getCodeCompletionLoc());
        Builder.setNumBytesToErase(dist);
        Builder.addOverrideKeyword();
        Builder.addDeclIntroducer(DeclStr.str().substr(0, NameOffset));
      }
    }

    if (!hasDeclIntroducer)
      Builder.addDeclIntroducer(DeclStr.str().substr(0, NameOffset));

    Builder.addTextChunk(DeclStr.str().substr(NameOffset));
  }

  void addMethodOverride(const FuncDecl *FD, DeclVisibilityKind Reason,
                         DynamicLookupInfo dynamicLookupInfo) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(FD);
    addValueOverride(FD, Reason, dynamicLookupInfo, Builder, hasFuncIntroducer);
    Builder.addBraceStmtWithCursor();
  }

  void addVarOverride(const VarDecl *VD, DeclVisibilityKind Reason,
                      DynamicLookupInfo dynamicLookupInfo) {
    // Overrides cannot use 'let', but if the 'override' keyword is specified
    // then the intention is clear, so provide the results anyway.  The compiler
    // can then provide an error telling you to use 'var' instead.
    // If we don't need override then it's a protocol requirement, so show it.
    if (missingOverride(Reason) && hasVarIntroducer &&
        isKeywordSpecified("let"))
      return;

    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(VD);
    addValueOverride(VD, Reason, dynamicLookupInfo, Builder, hasVarIntroducer);
  }

  void addSubscriptOverride(const SubscriptDecl *SD, DeclVisibilityKind Reason,
                            DynamicLookupInfo dynamicLookupInfo) {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(SD);
    addValueOverride(SD, Reason, dynamicLookupInfo, Builder, false);
    Builder.addBraceStmtWithCursor();
  }

  void addTypeAlias(const AssociatedTypeDecl *ATD, DeclVisibilityKind Reason,
                    DynamicLookupInfo dynamicLookupInfo) {
    CodeCompletionResultBuilder Builder(Sink,
      CodeCompletionResult::ResultKind::Declaration,
      SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(ATD);
    if (!hasTypealiasIntroducer && !hasAccessModifier)
      addAccessControl(ATD, Builder);
    if (!hasTypealiasIntroducer)
      Builder.addDeclIntroducer("typealias ");
    Builder.addTextChunk(ATD->getName().str());
    Builder.addTextChunk(" = ");
    Builder.addSimpleNamedParameter("Type");
  }

  void addConstructor(const ConstructorDecl *CD, DeclVisibilityKind Reason,
                      DynamicLookupInfo dynamicLookupInfo) {
    CodeCompletionResultBuilder Builder(
        Sink,
        CodeCompletionResult::ResultKind::Declaration,
        SemanticContextKind::Super, {});
    Builder.setAssociatedDecl(CD);

    if (!hasAccessModifier)
      addAccessControl(CD, Builder);

    if (missingOverride(Reason) && CD->isDesignatedInit() && !CD->isRequired())
      Builder.addOverrideKeyword();

    // Emit 'required' if we're in class context, 'required' is not specified,
    // and 1) this is a protocol conformance and the class is not final, or 2)
    // this is subclass and the initializer is marked as required.
    bool needRequired = false;
    auto C = CurrDeclContext->getSelfClassDecl();
    if (C && !isKeywordSpecified("required")) {
      if (Reason ==
            DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal &&
          !C->isFinal())
        needRequired = true;
      else if (Reason == DeclVisibilityKind::MemberOfSuper && CD->isRequired())
        needRequired = true;
    }

    llvm::SmallString<256> DeclStr;
    if (needRequired)
      DeclStr += "required ";
    {
      llvm::raw_svector_ostream OS(DeclStr);
      PrintOptions Options;
      Options.PrintImplicitAttrs = false;
      Options.SkipAttributes = true;
      CD->print(OS, Options);
    }
    Builder.addTextChunk(DeclStr);
    Builder.addBraceStmtWithCursor();
  }

  // Implement swift::VisibleDeclConsumer.
  void foundDecl(ValueDecl *D, DeclVisibilityKind Reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    if (Reason == DeclVisibilityKind::MemberOfCurrentNominal)
      return;

    if (D->shouldHideFromEditor())
      return;

    if (D->isFinal() ||
        // A 'class' member with an initial value cannot be overriden either.
        (D->isStatic() && D->getAttrs().hasAttribute<HasInitialValueAttr>()))
      return;

    bool hasIntroducer = hasFuncIntroducer ||
                         hasVarIntroducer ||
                         hasTypealiasIntroducer;

    if (hasStaticOrClass && !D->isStatic())
      return;

    // As per the current convention, only instance members are
    // suggested if an introducer is not accompanied by a 'static' or
    // 'class' modifier.
    if (hasIntroducer && !hasStaticOrClass && D->isStatic())
      return;

    if (auto *FD = dyn_cast<FuncDecl>(D)) {
      // We cannot override operators as members.
      if (FD->isBinaryOperator() || FD->isUnaryOperator())
        return;

      // We cannot override individual accessors.
      if (isa<AccessorDecl>(FD))
        return;

      if (hasFuncIntroducer || (!hasIntroducer && !hasInitializerModifier))
        addMethodOverride(FD, Reason, dynamicLookupInfo);
      return;
    }

    if (auto *VD = dyn_cast<VarDecl>(D)) {
      if (hasVarIntroducer || (!hasIntroducer && !hasInitializerModifier))
        addVarOverride(VD, Reason, dynamicLookupInfo);
      return;
    }

    if (auto *SD = dyn_cast<SubscriptDecl>(D)) {
      if (!hasIntroducer && !hasInitializerModifier)
        addSubscriptOverride(SD, Reason, dynamicLookupInfo);
    }

    if (auto *CD = dyn_cast<ConstructorDecl>(D)) {
      if (!isa<ProtocolDecl>(CD->getDeclContext()))
        return;
      if (hasIntroducer || hasOverride || hasOverridabilityModifier ||
          hasStaticOrClass)
        return;
      if (CD->isRequired() || CD->isDesignatedInit())
        addConstructor(CD, Reason, dynamicLookupInfo);
      return;
    }
  }

  void addDesignatedInitializers(NominalTypeDecl *NTD) {
    if (hasFuncIntroducer || hasVarIntroducer || hasTypealiasIntroducer ||
        hasOverridabilityModifier || hasStaticOrClass)
      return;

    const auto *CD = dyn_cast<ClassDecl>(NTD);
    if (!CD)
      return;
    if (!CD->hasSuperclass())
      return;
    CD = CD->getSuperclassDecl();
    for (const auto *Member : CD->getMembers()) {
      const auto *Constructor = dyn_cast<ConstructorDecl>(Member);
      if (!Constructor)
        continue;
      if (Constructor->hasStubImplementation())
        continue;
      if (Constructor->isDesignatedInit())
        addConstructor(Constructor, DeclVisibilityKind::MemberOfSuper, {});
    }
  }

  void addAssociatedTypes(NominalTypeDecl *NTD) {
    if (!hasTypealiasIntroducer &&
        (hasFuncIntroducer || hasVarIntroducer || hasInitializerModifier ||
         hasOverride || hasOverridabilityModifier || hasStaticOrClass))
      return;

    for (auto Conformance : NTD->getAllConformances()) {
      auto Proto = Conformance->getProtocol();
      if (!Proto->isAccessibleFrom(CurrDeclContext))
        continue;
      for (auto *ATD : Proto->getAssociatedTypeMembers()) {
        // FIXME: Also exclude the type alias that has already been specified.
        if (!Conformance->hasTypeWitness(ATD) ||
            ATD->hasDefaultDefinitionType())
          continue;
        addTypeAlias(
            ATD,
            DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal,
            {});
      }
    }
  }

  void getOverrideCompletions(SourceLoc Loc) {
    if (!CurrDeclContext->isTypeContext())
      return;
    if (isa<ProtocolDecl>(CurrDeclContext))
      return;

    Type CurrTy = CurrDeclContext->getSelfTypeInContext();
    auto *NTD = CurrDeclContext->getSelfNominalTypeDecl();
    if (CurrTy && !CurrTy->is<ErrorType>()) {
      // Look for overridable static members too.
      Type Meta = MetatypeType::get(CurrTy);
      lookupVisibleMemberDecls(*this, Meta, CurrDeclContext,
                               /*includeInstanceMembers=*/true);
      addDesignatedInitializers(NTD);
      addAssociatedTypes(NTD);
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
    Builder.addSimpleTypedParameter("@objc property", /*IsVarArg=*/false);
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

void CodeCompletionCallbacksImpl::completeStmtOrExpr(CodeCompletionExpr *E) {
  assert(P.Tok.is(tok::code_complete));
  Kind = CompletionKind::StmtOrExpr;
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
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
  CodeCompleteTokenExpr = E;
}

void CodeCompletionCallbacksImpl::completeForEachSequenceBeginning(
    CodeCompletionExpr *E) {
  assert(P.Tok.is(tok::code_complete));
  Kind = CompletionKind::ForEachSequence;
  CurDeclContext = P.CurDeclContext;
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

  ShouldCompleteCallPatternAfterParen = true;
  if (Context.LangOpts.CodeCompleteCallPatternHeuristics) {
    // Lookahead one token to decide what kind of call completions to provide.
    // When it appears that there is already code for the call present, just
    // complete values and/or argument labels.  Otherwise give the entire call
    // pattern.
    Token next = P.peekToken();
    if (!next.isAtStartOfLine() && !next.is(tok::eof) && !next.is(tok::r_paren)) {
      ShouldCompleteCallPatternAfterParen = false;
    }
  }
}

void CodeCompletionCallbacksImpl::completeExprKeyPath(KeyPathExpr *KPE,
                                                      SourceLoc DotLoc) {
  Kind = (!KPE || KPE->isObjC()) ? CompletionKind::KeyPathExprObjC
                                 : CompletionKind::KeyPathExprSwift;
  ParsedExpr = KPE;
  this->DotLoc = DotLoc;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completePoundAvailablePlatform() {
  Kind = CompletionKind::PoundAvailablePlatform;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeDeclResultBeginning() {
  Kind = CompletionKind::TypeDeclResultBeginning;
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

void CodeCompletionCallbacksImpl::completeDeclAttrBeginning(
    bool Sil, bool isIndependent) {
  Kind = CompletionKind::AttributeBegin;
  IsInSil = Sil;
  CurDeclContext = P.CurDeclContext;
  AttTargetIsIndependent = isIndependent;
}

void CodeCompletionCallbacksImpl::completeInPrecedenceGroup(SyntaxKind SK) {
  assert(P.Tok.is(tok::code_complete));

  SyntxKind = SK;
  Kind = CompletionKind::PrecedenceGroup;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeIdentifierWithDot() {
  Kind = CompletionKind::TypeIdentifierWithDot;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeTypeIdentifierWithoutDot() {
  Kind = CompletionKind::TypeIdentifierWithoutDot;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeCaseStmtKeyword() {
  Kind = CompletionKind::CaseStmtKeyword;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeCaseStmtBeginning(CodeCompletionExpr *E) {
  assert(!InEnumElementRawValue);

  Kind = CompletionKind::CaseStmtBeginning;
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
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

void CodeCompletionCallbacksImpl::completeUnresolvedMember(CodeCompletionExpr *E,
    SourceLoc DotLoc) {
  Kind = CompletionKind::UnresolvedMember;
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  this->DotLoc = DotLoc;
}

void CodeCompletionCallbacksImpl::completeCallArg(CodeCompletionExpr *E,
                                                  bool isFirst) {
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  Kind = CompletionKind::CallArg;

  ShouldCompleteCallPatternAfterParen = false;
  if (isFirst) {
    ShouldCompleteCallPatternAfterParen = true;
    if (Context.LangOpts.CodeCompleteCallPatternHeuristics) {
      // Lookahead one token to decide what kind of call completions to provide.
      // When it appears that there is already code for the call present, just
      // complete values and/or argument labels.  Otherwise give the entire call
      // pattern.
      Token next = P.peekToken();
      if (!next.isAtStartOfLine() && !next.is(tok::eof) && !next.is(tok::r_paren)) {
        ShouldCompleteCallPatternAfterParen = false;
      }
    }
  }
}

void CodeCompletionCallbacksImpl::completeReturnStmt(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  Kind = CompletionKind::ReturnStmtExpr;
}

void CodeCompletionCallbacksImpl::completeYieldStmt(CodeCompletionExpr *E,
                                                    Optional<unsigned> index) {
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  // TODO: use a different completion kind when completing without an index
  // in a multiple-value context.
  Kind = CompletionKind::YieldStmtExpr;
}

void CodeCompletionCallbacksImpl::completeAfterPoundExpr(
    CodeCompletionExpr *E, Optional<StmtKind> ParentKind) {
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
  Kind = CompletionKind::AfterPoundExpr;
  ParentStmtKind = ParentKind;
}

void CodeCompletionCallbacksImpl::completeAfterPoundDirective() {
  CurDeclContext = P.CurDeclContext;
  Kind = CompletionKind::AfterPoundDirective;
}

void CodeCompletionCallbacksImpl::completePlatformCondition() {
  CurDeclContext = P.CurDeclContext;
  Kind = CompletionKind::PlatformConditon;
}

void CodeCompletionCallbacksImpl::completeAfterIfStmt(bool hasElse) {
  CurDeclContext = P.CurDeclContext;
  if (hasElse) {
    Kind = CompletionKind::AfterIfStmtElse;
  } else {
    Kind = CompletionKind::StmtOrExpr;
  }
}

void CodeCompletionCallbacksImpl::completeGenericParams(TypeLoc TL) {
  CurDeclContext = P.CurDeclContext;
  Kind = CompletionKind::GenericParams;
  ParsedTypeLoc = TL;
}

void CodeCompletionCallbacksImpl::completeNominalMemberBeginning(
    SmallVectorImpl<StringRef> &Keywords, SourceLoc introducerLoc) {
  assert(!InEnumElementRawValue);
  this->introducerLoc = introducerLoc;
  ParsedKeywords.clear();
  ParsedKeywords.append(Keywords.begin(), Keywords.end());
  Kind = CompletionKind::NominalMemberBeginning;
  CurDeclContext = P.CurDeclContext;
}

void CodeCompletionCallbacksImpl::completeAccessorBeginning(
    CodeCompletionExpr *E) {
  Kind = CompletionKind::AccessorBeginning;
  CurDeclContext = P.CurDeclContext;
  CodeCompleteTokenExpr = E;
}

static bool isDynamicLookup(Type T) {
  return T->getRValueType()->isAnyObject();
}

static bool isClangSubModule(ModuleDecl *TheModule) {
  if (auto ClangMod = TheModule->findUnderlyingClangModule())
    return ClangMod->isSubModule();
  return false;
}

static void addKeyword(CodeCompletionResultSink &Sink, StringRef Name,
                       CodeCompletionKeywordKind Kind,
                       StringRef TypeAnnotation = "") {
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::None, {});
    Builder.setKeywordKind(Kind);
    Builder.addTextChunk(Name);
    if (!TypeAnnotation.empty())
      Builder.addTypeAnnotation(TypeAnnotation);
}

static void addDeclKeywords(CodeCompletionResultSink &Sink) {
  auto AddDeclKeyword = [&](StringRef Name, CodeCompletionKeywordKind Kind,
                        Optional<DeclAttrKind> DAK) {
    if (Name == "let" || Name == "var") {
      // Treat keywords that could be the start of a pattern specially.
      return;
    }

    // Remove user inaccessible keywords.
    if (DAK.hasValue() && DeclAttribute::isUserInaccessible(*DAK)) return;

    addKeyword(Sink, Name, Kind);
  };

#define DECL_KEYWORD(kw) AddDeclKeyword(#kw, CodeCompletionKeywordKind::kw_##kw, None);
#include "swift/Syntax/TokenKinds.def"

  // Context-sensitive keywords.
  auto AddCSKeyword = [&](StringRef Name, DeclAttrKind Kind) {
    AddDeclKeyword(Name, CodeCompletionKeywordKind::None, Kind);
  };

#define CONTEXTUAL_CASE(KW, CLASS) AddCSKeyword(#KW, DAK_##CLASS);
#define CONTEXTUAL_DECL_ATTR(KW, CLASS, ...) CONTEXTUAL_CASE(KW, CLASS)
#define CONTEXTUAL_DECL_ATTR_ALIAS(KW, CLASS) CONTEXTUAL_CASE(KW, CLASS)
#define CONTEXTUAL_SIMPLE_DECL_ATTR(KW, CLASS, ...) CONTEXTUAL_CASE(KW, CLASS)
#include <swift/AST/Attr.def>
#undef CONTEXTUAL_CASE

}

static void addStmtKeywords(CodeCompletionResultSink &Sink, bool MaybeFuncBody) {
  auto AddStmtKeyword = [&](StringRef Name, CodeCompletionKeywordKind Kind) {
    if (!MaybeFuncBody && Kind == CodeCompletionKeywordKind::kw_return)
      return;
    addKeyword(Sink, Name, Kind);
  };
#define STMT_KEYWORD(kw) AddStmtKeyword(#kw, CodeCompletionKeywordKind::kw_##kw);
#include "swift/Syntax/TokenKinds.def"
}

static void addCaseStmtKeywords(CodeCompletionResultSink &Sink) {
  addKeyword(Sink, "case", CodeCompletionKeywordKind::kw_case);
  addKeyword(Sink, "default", CodeCompletionKeywordKind::kw_default);
}

static void addLetVarKeywords(CodeCompletionResultSink &Sink) {
  addKeyword(Sink, "let", CodeCompletionKeywordKind::kw_let);
  addKeyword(Sink, "var", CodeCompletionKeywordKind::kw_var);
}

static void addAccessorKeywords(CodeCompletionResultSink &Sink) {
  addKeyword(Sink, "get", CodeCompletionKeywordKind::None);
  addKeyword(Sink, "set", CodeCompletionKeywordKind::None);
}

static void addObserverKeywords(CodeCompletionResultSink &Sink) {
  addKeyword(Sink, "willSet", CodeCompletionKeywordKind::None);
  addKeyword(Sink, "didSet", CodeCompletionKeywordKind::None);
}

static void addExprKeywords(CodeCompletionResultSink &Sink) {
  // Expr keywords.
  addKeyword(Sink, "try", CodeCompletionKeywordKind::kw_try);
  addKeyword(Sink, "try!", CodeCompletionKeywordKind::kw_try);
  addKeyword(Sink, "try?", CodeCompletionKeywordKind::kw_try);
}

static void addOpaqueTypeKeyword(CodeCompletionResultSink &Sink) {
  addKeyword(Sink, "some", CodeCompletionKeywordKind::None, "some");
}

static void addAnyTypeKeyword(CodeCompletionResultSink &Sink) {
  addKeyword(Sink, "Any", CodeCompletionKeywordKind::None, "Any");
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
  case CompletionKind::AfterPoundExpr:
  case CompletionKind::AfterPoundDirective:
  case CompletionKind::PlatformConditon:
  case CompletionKind::GenericParams:
  case CompletionKind::KeyPathExprObjC:
  case CompletionKind::KeyPathExprSwift:
  case CompletionKind::PrecedenceGroup:
    break;

  case CompletionKind::AccessorBeginning: {
    // TODO: Omit already declared or mutally exclusive accessors.
    //       E.g. If 'get' is already declared, emit 'set' only.
    addAccessorKeywords(Sink);

    // Only 'var' for non-protocol context can have 'willSet' and 'didSet'.
    assert(ParsedDecl);
    VarDecl *var = dyn_cast<VarDecl>(ParsedDecl);
    if (auto accessor = dyn_cast<AccessorDecl>(ParsedDecl))
      var = dyn_cast<VarDecl>(accessor->getStorage());
    if (var && !var->getDeclContext()->getSelfProtocolDecl())
      addObserverKeywords(Sink);

    if (!isa<AccessorDecl>(ParsedDecl))
      break;

    MaybeFuncBody = true;
    LLVM_FALLTHROUGH;
  }
  case CompletionKind::StmtOrExpr:
    addDeclKeywords(Sink);
    addStmtKeywords(Sink, MaybeFuncBody);
    LLVM_FALLTHROUGH;
  case CompletionKind::ReturnStmtExpr:
  case CompletionKind::YieldStmtExpr:
  case CompletionKind::PostfixExprBeginning:
  case CompletionKind::ForEachSequence:
    addSuperKeyword(Sink);
    addLetVarKeywords(Sink);
    addExprKeywords(Sink);
    addAnyTypeKeyword(Sink);
    break;

  case CompletionKind::CaseStmtKeyword:
    addCaseStmtKeywords(Sink);
    break;

  case CompletionKind::PostfixExpr:
  case CompletionKind::PostfixExprParen:
  case CompletionKind::CaseStmtBeginning:
  case CompletionKind::TypeIdentifierWithDot:
  case CompletionKind::TypeIdentifierWithoutDot:
    break;

  case CompletionKind::TypeDeclResultBeginning:
    if (!isa<ProtocolDecl>(CurDeclContext))
      if (CurDeclContext->isTypeContext() ||
          (ParsedDecl && isa<FuncDecl>(ParsedDecl)))
        addOpaqueTypeKeyword(Sink);

    LLVM_FALLTHROUGH;
  case CompletionKind::TypeSimpleBeginning:
    addAnyTypeKeyword(Sink);
    break;

  case CompletionKind::NominalMemberBeginning: {
    bool HasDeclIntroducer = llvm::find_if(ParsedKeywords,
                                           [this](const StringRef kw) {
      return llvm::StringSwitch<bool>(kw)
        .Case("associatedtype", true)
        .Case("class", !CurDeclContext || !isa<ClassDecl>(CurDeclContext))
        .Case("deinit", true)
        .Case("enum", true)
        .Case("extension", true)
        .Case("func", true)
        .Case("import", true)
        .Case("init", true)
        .Case("let", true)
        .Case("operator", true)
        .Case("precedencegroup", true)
        .Case("protocol", true)
        .Case("struct", true)
        .Case("subscript", true)
        .Case("typealias", true)
        .Case("var", true)
        .Default(false);
    }) != ParsedKeywords.end();
    if (!HasDeclIntroducer) {
      addDeclKeywords(Sink);
      addLetVarKeywords(Sink);
    }
    break;
  }

  case CompletionKind::AfterIfStmtElse:
    addKeyword(Sink, "if", CodeCompletionKeywordKind::kw_if);
    break;
  }
}

static void addPoundDirectives(CodeCompletionResultSink &Sink) {
  auto addWithName =
      [&](StringRef name, CodeCompletionKeywordKind K,
          llvm::function_ref<void(CodeCompletionResultBuilder &)> consumer =
              nullptr) {
        CodeCompletionResultBuilder Builder(Sink, CodeCompletionResult::Keyword,
                                            SemanticContextKind::None, {});
        Builder.addTextChunk(name);
        Builder.setKeywordKind(K);
        if (consumer)
          consumer(Builder);
      };

  addWithName("sourceLocation", CodeCompletionKeywordKind::pound_sourceLocation,
              [&] (CodeCompletionResultBuilder &Builder) {
    Builder.addLeftParen();
    Builder.addTextChunk("file");
    Builder.addCallParameterColon();
    Builder.addSimpleTypedParameter("String");
    Builder.addComma();
    Builder.addTextChunk("line");
    Builder.addCallParameterColon();
    Builder.addSimpleTypedParameter("Int");
    Builder.addRightParen();
  });
  addWithName("warning", CodeCompletionKeywordKind::pound_warning,
              [&] (CodeCompletionResultBuilder &Builder) {
    Builder.addLeftParen();
    Builder.addTextChunk("\"");
    Builder.addSimpleNamedParameter("message");
    Builder.addTextChunk("\"");
    Builder.addRightParen();
  });
  addWithName("error", CodeCompletionKeywordKind::pound_error,
              [&] (CodeCompletionResultBuilder &Builder) {
    Builder.addLeftParen();
    Builder.addTextChunk("\"");
    Builder.addSimpleNamedParameter("message");
    Builder.addTextChunk("\"");
    Builder.addRightParen();
  });

  addWithName("if ", CodeCompletionKeywordKind::pound_if,
              [&] (CodeCompletionResultBuilder &Builder) {
    Builder.addSimpleNamedParameter("condition");
  });

  // FIXME: These directives are only valid in conditional completion block.
  addWithName("elseif ", CodeCompletionKeywordKind::pound_elseif,
              [&] (CodeCompletionResultBuilder &Builder) {
    Builder.addSimpleNamedParameter("condition");
  });
  addWithName("else", CodeCompletionKeywordKind::pound_else);
  addWithName("endif", CodeCompletionKeywordKind::pound_endif);
}

/// Add platform conditions used in '#if' and '#elseif' directives.
static void addPlatformConditions(CodeCompletionResultSink &Sink) {
  auto addWithName =
      [&](StringRef Name,
          llvm::function_ref<void(CodeCompletionResultBuilder & Builder)>
              consumer) {
        CodeCompletionResultBuilder Builder(
            Sink, CodeCompletionResult::ResultKind::Pattern,
            SemanticContextKind::ExpressionSpecific, {});
        Builder.addTextChunk(Name);
        Builder.addLeftParen();
        consumer(Builder);
        Builder.addRightParen();
      };
  addWithName("os", [](CodeCompletionResultBuilder &Builder) {
    Builder.addSimpleNamedParameter("name");
  });
  addWithName("arch", [](CodeCompletionResultBuilder &Builder) {
    Builder.addSimpleNamedParameter("name");
  });
  addWithName("canImport", [](CodeCompletionResultBuilder &Builder) {
    Builder.addSimpleNamedParameter("module");
  });
  addWithName("targetEnvironment", [](CodeCompletionResultBuilder &Builder) {
    Builder.addTextChunk("simulator");
  });
  addWithName("swift", [](CodeCompletionResultBuilder &Builder) {
    Builder.addTextChunk(">=");
    Builder.addSimpleNamedParameter("version");
  });
  addWithName("swift", [](CodeCompletionResultBuilder &Builder) {
    Builder.addTextChunk("<");
    Builder.addSimpleNamedParameter("version");
  });
  addWithName("compiler", [](CodeCompletionResultBuilder &Builder) {
    Builder.addTextChunk(">=");
    Builder.addSimpleNamedParameter("version");
  });
  addWithName("compiler", [](CodeCompletionResultBuilder &Builder) {
    Builder.addTextChunk("<");
    Builder.addSimpleNamedParameter("version");
  });

  addKeyword(Sink, "true", CodeCompletionKeywordKind::kw_true, "Bool");
  addKeyword(Sink, "false", CodeCompletionKeywordKind::kw_false, "Bool");
}

/// Add flags specified by '-D' to completion results.
static void addConditionalCompilationFlags(ASTContext &Ctx,
                                           CodeCompletionResultSink &Sink) {
  for (auto Flag : Ctx.LangOpts.getCustomConditionalCompilationFlags()) {
    // TODO: Should we filter out some flags?
    CodeCompletionResultBuilder Builder(
        Sink, CodeCompletionResult::ResultKind::Keyword,
        SemanticContextKind::ExpressionSpecific, {});
    Builder.addTextChunk(Flag);
  }
}

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

  if (auto *DC = dyn_cast_or_null<DeclContext>(ParsedDecl)) {
    if (DC->isChildContextOf(CurDeclContext))
      CurDeclContext = DC;
  }

  typeCheckContextUntil(
      CurDeclContext,
      CurDeclContext->getASTContext().SourceMgr.getCodeCompletionLoc());

  Optional<Type> ExprType;
  ConcreteDeclRef ReferencedDecl = nullptr;
  if (ParsedExpr) {
    if (auto *checkedExpr = findParsedExpr(CurDeclContext,
                                           ParsedExpr->getSourceRange())) {
      ParsedExpr = checkedExpr;
    }

    if (auto typechecked = typeCheckParsedExpr()) {
      ExprType = typechecked->first;
      ReferencedDecl = typechecked->second;
      ParsedExpr->setType(*ExprType);
    }

    if (!ExprType && Kind != CompletionKind::PostfixExprParen &&
        Kind != CompletionKind::CallArg &&
        Kind != CompletionKind::KeyPathExprObjC)
      return;
  }

  if (!ParsedTypeLoc.isNull() && !typecheckParsedType())
    return;

  CompletionLookup Lookup(CompletionContext.getResultSink(), P.Context,
                          CurDeclContext, &CompletionContext);
  if (ExprType) {
    Lookup.setIsStaticMetatype(ParsedExpr->isStaticallyDerivedMetatype());
  }
  if (auto *DRE = dyn_cast_or_null<DeclRefExpr>(ParsedExpr)) {
    Lookup.setIsSelfRefExpr(DRE->getDecl()->getFullName() == Context.Id_self);
  } else if (auto *SRE = dyn_cast_or_null<SuperRefExpr>(ParsedExpr)) {
    Lookup.setIsSuperRefExpr();
  }

  if (isInsideObjCSelector())
    Lookup.includeInstanceMembers();
  if (PreferFunctionReferencesToCalls)
    Lookup.setPreferFunctionReferencesToCalls();

  auto DoPostfixExprBeginning = [&] (){
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

    Lookup.getPostfixKeywordCompletions(*ExprType, ParsedExpr);

    if (isa<BindOptionalExpr>(ParsedExpr) || isa<ForceValueExpr>(ParsedExpr))
      Lookup.setIsUnwrappedOptional(true);

    ExprContextInfo ContextInfo(CurDeclContext, ParsedExpr);
    Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                            ContextInfo.isSingleExpressionBody());
    Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    break;
  }

  case CompletionKind::KeyPathExprSwift: {
    auto KPE = dyn_cast<KeyPathExpr>(ParsedExpr);
    auto BGT = (*ExprType)->getAs<BoundGenericType>();
    if (!KPE || !BGT || BGT->getGenericArgs().size() != 2)
      break;
    assert(!KPE->isObjC());

    if (DotLoc.isValid())
      Lookup.setHaveDot(DotLoc);

    bool OnRoot = !KPE->getComponents().front().isValid();
    Lookup.setIsSwiftKeyPathExpr(OnRoot);

    Type baseType = BGT->getGenericArgs()[OnRoot ? 0 : 1];
    if (OnRoot && baseType->is<UnresolvedType>()) {
      // Infer the root type of the keypath from the context type.
      ExprContextInfo ContextInfo(CurDeclContext, ParsedExpr);
      for (auto T : ContextInfo.getPossibleTypes()) {
        if (auto unwrapped = T->getOptionalObjectType())
          T = unwrapped;
        if (!T->getAnyNominal() || !T->getAnyNominal()->getKeyPathTypeKind() ||
            T->hasUnresolvedType() || !T->is<BoundGenericType>())
          continue;
        // Use the first KeyPath context type found.
        baseType = T->castTo<BoundGenericType>()->getGenericArgs()[0];
        break;
      }
    }
    if (!OnRoot && KPE->getComponents().back().getKind() ==
                       KeyPathExpr::Component::Kind::OptionalWrap) {
      // KeyPath expr with '?' (e.g. '\Ty.[0].prop?.another').
      // Althogh expected type is optional, we should unwrap it because it's
      // unwrapped.
      baseType = baseType->getOptionalObjectType();
    }

    Lookup.getValueExprCompletions(baseType);
    break;
  }

  case CompletionKind::StmtOrExpr:
  case CompletionKind::ForEachSequence:
  case CompletionKind::PostfixExprBeginning: {
    ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);
    Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                            ContextInfo.isSingleExpressionBody());
    DoPostfixExprBeginning();
    break;
  }

  case CompletionKind::PostfixExpr: {
    Lookup.setHaveLeadingSpace(HasSpace);
    if (isDynamicLookup(*ExprType))
      Lookup.setIsDynamicLookup();
    Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
    Lookup.getOperatorCompletions(ParsedExpr, leadingSequenceExprs);
    Lookup.getPostfixKeywordCompletions(*ExprType, ParsedExpr);
    break;
  }

  case CompletionKind::PostfixExprParen: {
    Lookup.setHaveLParen(true);

    ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);

    if (ShouldCompleteCallPatternAfterParen) {
      ExprContextInfo ParentContextInfo(CurDeclContext, ParsedExpr);
      Lookup.setExpectedTypes(ParentContextInfo.getPossibleTypes(),
                              ParentContextInfo.isSingleExpressionBody());
      if (ExprType && ((*ExprType)->is<AnyFunctionType>() ||
                       (*ExprType)->is<AnyMetatypeType>())) {
        Lookup.getValueExprCompletions(*ExprType, ReferencedDecl.getDecl());
      } else {
        for (auto &typeAndDecl : ContextInfo.getPossibleCallees())
          Lookup.tryFunctionCallCompletions(typeAndDecl.first, typeAndDecl.second);
      }
    } else {
      // Add argument labels, then fallthrough to get values.
      Lookup.addArgNameCompletionResults(ContextInfo.getPossibleNames());
    }

    if (!Lookup.FoundFunctionCalls ||
        (Lookup.FoundFunctionCalls &&
         Lookup.FoundFunctionsWithoutFirstKeyword)) {
      Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                              ContextInfo.isSingleExpressionBody());
      Lookup.setHaveLParen(false);
      DoPostfixExprBeginning();
    }
    break;
  }

  case CompletionKind::KeyPathExprObjC: {
    if (DotLoc.isValid())
      Lookup.setHaveDot(DotLoc);
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

  case CompletionKind::TypeDeclResultBeginning:
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
    ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);
    Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                            ContextInfo.isSingleExpressionBody());
    Lookup.getUnresolvedMemberCompletions(ContextInfo.getPossibleTypes());
    DoPostfixExprBeginning();
    break;
  }

  case CompletionKind::NominalMemberBeginning: {
    CompletionOverrideLookup OverrideLookup(CompletionContext.getResultSink(),
                                            P.Context, CurDeclContext,
                                            ParsedKeywords, introducerLoc);
    OverrideLookup.getOverrideCompletions(SourceLoc());
    break;
  }

  case CompletionKind::AccessorBeginning: {
    if (isa<AccessorDecl>(ParsedDecl)) {
      ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);
      Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                              ContextInfo.isSingleExpressionBody());
      DoPostfixExprBeginning();
    }
    break;
  }

  case CompletionKind::AttributeBegin: {
    Lookup.getAttributeDeclCompletions(IsInSil, AttTargetDK);

    // TypeName at attribute position after '@'.
    // - VarDecl: Property Wrappers.
    // - ParamDecl/VarDecl/FuncDecl: Function Buildres.
    if (!AttTargetDK || *AttTargetDK == DeclKind::Var ||
        *AttTargetDK == DeclKind::Param || *AttTargetDK == DeclKind::Func)
      Lookup.getTypeCompletionsInDeclContext(
          P.Context.SourceMgr.getCodeCompletionLoc());
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
  case CompletionKind::UnresolvedMember: {
    Lookup.setHaveDot(DotLoc);
    ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);
    Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                            ContextInfo.isSingleExpressionBody());
    Lookup.getUnresolvedMemberCompletions(ContextInfo.getPossibleTypes());
    break;
  }
  case CompletionKind::CallArg : {
    ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);

    bool shouldPerformGlobalCompletion = true;

    if (ShouldCompleteCallPatternAfterParen &&
        !ContextInfo.getPossibleCallees().empty()) {
      Lookup.setHaveLParen(true);
      for (auto &typeAndDecl : ContextInfo.getPossibleCallees()) {
        if (auto SD = dyn_cast_or_null<SubscriptDecl>(typeAndDecl.second)) {
          Lookup.addSubscriptCallPattern(typeAndDecl.first, SD);
        } else {
          Lookup.addFunctionCallPattern(
              typeAndDecl.first,
              dyn_cast_or_null<AbstractFunctionDecl>(typeAndDecl.second));
        }
      }
      Lookup.setHaveLParen(false);

      shouldPerformGlobalCompletion =
          !Lookup.FoundFunctionCalls ||
          (Lookup.FoundFunctionCalls &&
           Lookup.FoundFunctionsWithoutFirstKeyword);
    } else if (!ContextInfo.getPossibleNames().empty()) {
      Lookup.addArgNameCompletionResults(ContextInfo.getPossibleNames());

      shouldPerformGlobalCompletion = !ContextInfo.getPossibleTypes().empty();
    }

    if (shouldPerformGlobalCompletion) {
      Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                              ContextInfo.isSingleExpressionBody());
      DoPostfixExprBeginning();
    }
    break;
  }

  case CompletionKind::ReturnStmtExpr : {
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    Lookup.setExpectedTypes(getReturnTypeFromContext(CurDeclContext),
                            /*isSingleExpressionBody*/ false);
    Lookup.getValueCompletionsInDeclContext(Loc);
    break;
  }

  case CompletionKind::YieldStmtExpr: {
    SourceLoc Loc = P.Context.SourceMgr.getCodeCompletionLoc();
    if (auto FD = dyn_cast<AccessorDecl>(CurDeclContext)) {
      if (FD->isCoroutine()) {
        // TODO: handle multi-value yields.
        Lookup.setExpectedTypes(FD->getStorage()->getValueInterfaceType(),
                                /*isSingleExpressionBody*/ false);
      }
    }
    Lookup.getValueCompletionsInDeclContext(Loc);
    break;
  }

  case CompletionKind::AfterPoundExpr: {
    ExprContextInfo ContextInfo(CurDeclContext, CodeCompleteTokenExpr);
    Lookup.setExpectedTypes(ContextInfo.getPossibleTypes(),
                            ContextInfo.isSingleExpressionBody());

    Lookup.addPoundAvailable(ParentStmtKind);
    Lookup.addPoundLiteralCompletions(/*needPound=*/false);
    Lookup.addObjCPoundKeywordCompletions(/*needPound=*/false);
    break;
  }

  case CompletionKind::AfterPoundDirective: {
    addPoundDirectives(CompletionContext.getResultSink());
    // FIXME: Add pound expressions (e.g. '#selector()') if it's at statements
    // position.
    break;
  }

  case CompletionKind::PlatformConditon: {
    addPlatformConditions(CompletionContext.getResultSink());
    addConditionalCompilationFlags(CurDeclContext->getASTContext(),
                                   CompletionContext.getResultSink());
    break;
  }

  case CompletionKind::GenericParams:
    if (auto GT = ParsedTypeLoc.getType()->getAnyGeneric()) {
      if (auto Params = GT->getGenericParams()) {
        for (auto GP : Params->getParams()) {
          Lookup.addGenericTypeParamRef(
              GP, DeclVisibilityKind::GenericParameter, {});
        }
      }
    }
    break;
  case CompletionKind::PrecedenceGroup:
    Lookup.getPrecedenceGroupCompletions(SyntxKind);
    break;
  case CompletionKind::AfterIfStmtElse:
  case CompletionKind::CaseStmtKeyword:
    // Handled earlier by keyword completions.
    break;
  }

  for (auto &Request: Lookup.RequestedCachedResults) {
    // Use the current SourceFile as the DeclContext so that we can use it to
    // perform qualified lookup, and to get the correct visibility for
    // @testable imports.
    const SourceFile &SF = P.SF;

    llvm::DenseSet<CodeCompletionCache::Key> ImportsSeen;
    auto handleImport = [&](ModuleDecl::ImportedModule Import) {
      ModuleDecl *TheModule = Import.second;
      ModuleDecl::AccessPathTy Path = Import.first;
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
        auto &Ctx = TheModule->getASTContext();
        CodeCompletionCache::Key K{
          ModuleFilename, TheModule->getName().str(), AccessPath,
              Request.NeedLeadingDot,
              SF.hasTestableOrPrivateImport(
                  AccessLevel::Internal, TheModule,
                  SourceFile::ImportQueryKind::TestableOnly),
              SF.hasTestableOrPrivateImport(
                  AccessLevel::Internal, TheModule,
                  SourceFile::ImportQueryKind::PrivateOnly),
          Ctx.LangOpts.CodeCompleteInitsInPostfixExpr};

        using PairType = llvm::DenseSet<swift::ide::CodeCompletionCache::Key,
            llvm::DenseMapInfo<CodeCompletionCache::Key>>::iterator;
        std::pair<PairType, bool> Result = ImportsSeen.insert(K);
        if (!Result.second)
          return; // already handled.
        RequestedModules.push_back({std::move(K), TheModule,
          Request.OnlyTypes, Request.OnlyPrecedenceGroups});
      }
    };

    if (Request.TheModule) {
      // FIXME: actually check imports.
      for (auto Import : namelookup::getAllImports(Request.TheModule)) {
        handleImport(Import);
      }
    } else {
      // Add results from current module.
      Lookup.getToplevelCompletions(Request.OnlyTypes);

      // Add results for all imported modules.
      ModuleDecl::ImportFilter ImportFilter;
      ImportFilter |= ModuleDecl::ImportFilterKind::Public;
      ImportFilter |= ModuleDecl::ImportFilterKind::Private;
      ImportFilter |= ModuleDecl::ImportFilterKind::ImplementationOnly;
      SmallVector<ModuleDecl::ImportedModule, 4> Imports;
      auto *SF = CurDeclContext->getParentSourceFile();
      SF->getImportedModules(Imports, ImportFilter);

      for (auto Imported : Imports) {
        for (auto Import : namelookup::getAllImports(Imported.second))
          handleImport(Import);
      }
    }
  }
  Lookup.RequestedCachedResults.clear();

  CompletionContext.typeContextKind = Lookup.typeContextKind();

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

    StringRef comment = Result->getBriefDocComment();
    if (IncludeComments && !comment.empty()) {
      OS << "; comment=" << comment;
    }

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
    CodeCompletionResultSink &targetSink, const ModuleDecl *module,
    ArrayRef<std::string> accessPath, bool needLeadingDot,
    const DeclContext *currDeclContext) {
  CompletionLookup Lookup(targetSink, module->getASTContext(), currDeclContext);
  Lookup.getVisibleDeclsOfModule(module, accessPath, needLeadingDot);
}

void swift::ide::copyCodeCompletionResults(CodeCompletionResultSink &targetSink,
                                           CodeCompletionResultSink &sourceSink,
                                           bool onlyTypes,
                                           bool onlyPrecedenceGroups) {

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
      case CodeCompletionDeclKind::Module:
      case CodeCompletionDeclKind::Class:
      case CodeCompletionDeclKind::Struct:
      case CodeCompletionDeclKind::Enum:
      case CodeCompletionDeclKind::Protocol:
      case CodeCompletionDeclKind::TypeAlias:
      case CodeCompletionDeclKind::AssociatedType:
      case CodeCompletionDeclKind::GenericTypeParam:
        return true;
      case CodeCompletionDeclKind::PrecedenceGroup:
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

      llvm_unreachable("Unhandled CodeCompletionDeclKind in switch.");
    });
  } else if (onlyPrecedenceGroups) {
    std::copy_if(sourceSink.Results.begin(), sourceSink.Results.end(),
                 std::back_inserter(targetSink.Results),
                 [](CodeCompletionResult *R) -> bool {
      return R->getAssociatedDeclKind() ==
               CodeCompletionDeclKind::PrecedenceGroup;
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
    copyCodeCompletionResults(context.getResultSink(), (*V)->Sink,
                              R.OnlyTypes, R.OnlyPrecedenceGroups);
  }

  handleResults(context.takeResults());
}
