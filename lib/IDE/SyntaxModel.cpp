//===--- SyntaxModel.cpp - Routines for IDE syntax model ------------------===//
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

#include "swift/IDE/SyntaxModel.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Token.h"
#include "swift/Config.h"
#include "swift/Subsystems.h"
#include "clang/Basic/CharInfo.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/MemoryBuffer.h"
#include <vector>
#include <regex>

using namespace swift;
using namespace ide;

void SyntaxModelWalker::anchor() {}

struct SyntaxModelContext::Implementation {
  SourceFile &SrcFile;
  const LangOptions &LangOpts;
  const SourceManager &SrcMgr;
  std::vector<SyntaxNode> TokenNodes;

  Implementation(SourceFile &SrcFile)
    : SrcFile(SrcFile),
      LangOpts(SrcFile.getASTContext().LangOpts),
      SrcMgr(SrcFile.getASTContext().SourceMgr) {}
};

SyntaxModelContext::SyntaxModelContext(SourceFile &SrcFile)
  : Impl(*new Implementation(SrcFile)) {
  const bool IsPlayground = Impl.LangOpts.Playground;
  const SourceManager &SM = Impl.SrcMgr;
  ArrayRef<Token> Tokens = SrcFile.getAllTokens();
  std::vector<SyntaxNode> Nodes;
  SourceLoc AttrLoc;
  SourceLoc UnaryMinusLoc;
  auto LiteralStartLoc = Optional<SourceLoc>();
  for (unsigned I = 0, E = Tokens.size(); I != E; ++I) {
    auto &Tok = Tokens[I];
    // Ignore empty string literals between interpolations, e.g. "\(1)\(2)"
    if (!Tok.getLength())
        continue;
    SyntaxNodeKind Kind;
    SourceLoc Loc;
    Optional<unsigned> Length;
    if (AttrLoc.isValid()) {
      // This token is following @, see if it's a known attribute name.
      if (TypeAttributes::getAttrKindFromString(Tok.getText()) != TAK_Count ||
          DeclAttribute::getAttrKindFromString(Tok.getText()) != DAK_Count) {
        // It's a known attribute, so treat it as a syntactic attribute node for
        // syntax coloring. If swift gets user attributes then all identifiers
        // will be treated as syntactic attribute nodes.
        Loc = AttrLoc;
        Length = SM.getByteDistance(Loc, Tok.getLoc()) + Tok.getLength();
        Kind = SyntaxNodeKind::AttributeId;
      }
      AttrLoc = SourceLoc();
    }

    if (!Loc.isValid()) {
      Loc = Tok.getLoc();
      Length = Tok.getLength();

      if (LiteralStartLoc.hasValue() && Length.hasValue()) {
        if (Tok.getKind() != tok::r_paren)
          continue;
        Kind = SyntaxNodeKind::ObjectLiteral;
        Nodes.emplace_back(Kind, CharSourceRange(SM, LiteralStartLoc.getValue(),
                                                 Tok.getRange().getEnd()));
        LiteralStartLoc = Optional<SourceLoc>();
        continue;
      }

      switch(Tok.getKind()) {
#define KEYWORD(X) case tok::kw_##X:
#include "swift/Syntax/TokenKinds.def"
#undef KEYWORD
        case tok::contextual_keyword:
        Kind = SyntaxNodeKind::Keyword;
        break;

#define POUND_OBJECT_LITERAL(Name, Desc, Proto) case tok::pound_##Name:
#include "swift/Syntax/TokenKinds.def"
        LiteralStartLoc = Loc;
        continue;

#define POUND_COND_DIRECTIVE_KEYWORD(Name) case tok::pound_##Name:
#include "swift/Syntax/TokenKinds.def"
        Kind = SyntaxNodeKind::BuildConfigKeyword;
        break;

#define POUND_DIRECTIVE_KEYWORD(Name) case tok::pound_##Name:
#define POUND_COND_DIRECTIVE_KEYWORD(Name)
#include "swift/Syntax/TokenKinds.def"
        Kind = SyntaxNodeKind::PoundDirectiveKeyword;
        break;

#define POUND_OBJECT_LITERAL(Name, Desc, Proto)
#define POUND_DIRECTIVE_KEYWORD(Name)
#define POUND_KEYWORD(Name) case tok::pound_##Name:
#include "swift/Syntax/TokenKinds.def"
        Kind = SyntaxNodeKind::Keyword;
        break;
      case tok::identifier:
        if (Tok.getText().startswith("<#"))
          Kind = SyntaxNodeKind::EditorPlaceholder;
        else
          Kind = SyntaxNodeKind::Identifier;
        break;
      case tok::dollarident: Kind = SyntaxNodeKind::DollarIdent; break;
      case tok::string_literal: Kind = SyntaxNodeKind::String; break;

      case tok::integer_literal:
        Kind = SyntaxNodeKind::Integer;
        if (UnaryMinusLoc.isValid()) {
          Loc = UnaryMinusLoc;
          Length = *Length + SM.getByteDistance(UnaryMinusLoc, Tok.getLoc());
        }
        break;
      case tok::floating_literal:
        Kind = SyntaxNodeKind::Floating;
        if (UnaryMinusLoc.isValid()) {
          Loc = UnaryMinusLoc;
          Length = *Length + SM.getByteDistance(UnaryMinusLoc, Tok.getLoc());
        }
        break;

      case tok::oper_prefix:
        if (Tok.getText() == "-")
          UnaryMinusLoc = Loc;
        continue;

      case tok::comment:
        if (Tok.getText().startswith("///") ||
            (IsPlayground && Tok.getText().startswith("//:")))
          Kind = SyntaxNodeKind::DocCommentLine;
        else if (Tok.getText().startswith("/**") ||
                 (IsPlayground && Tok.getText().startswith("/*:")))
          Kind = SyntaxNodeKind::DocCommentBlock;
        else if (Tok.getText().startswith("//"))
          Kind = SyntaxNodeKind::CommentLine;
        else
          Kind = SyntaxNodeKind::CommentBlock;
        break;
      case tok::at_sign:
        // Set the location of @ and continue. Next token should be the
        // attribute name.
        AttrLoc = Tok.getLoc();
        continue;

      case tok::string_interpolation_anchor: {
        Kind = SyntaxNodeKind::StringInterpolationAnchor;
        break;
      }

      case tok::unknown: {
        if (Tok.getRawText().startswith("\"")) {
          // This is an invalid string literal
          Kind = SyntaxNodeKind::String;
          break;
        }
        continue;
      }

      default:
        continue;
      }
    }

    UnaryMinusLoc = SourceLoc(); // Reset.

    assert(Loc.isValid());
    assert(Nodes.empty() || SM.isBeforeInBuffer(Nodes.back().Range.getStart(),
                                                Loc));
    Nodes.emplace_back(Kind, CharSourceRange(Loc, Length.getValue()));
  }

  Impl.TokenNodes = std::move(Nodes);
}

SyntaxModelContext::~SyntaxModelContext() {
  delete &Impl;
}

namespace {

using ASTNodeType = ASTWalker::ParentTy;

struct StructureElement {
  SyntaxStructureNode StructureNode;
  ASTNodeType ASTNode;
  StructureElement(const SyntaxStructureNode &StructureNode,
                   const ASTNodeType &ASTNode)
    :StructureNode(StructureNode), ASTNode(ASTNode) { }
};

static const std::vector<std::string> URLProtocols = {

  // Use RegexStrURL:
  "acap", "afp", "afs", "cid", "data", "fax", "feed", "file", "ftp", "go",
  "gopher", "http", "https", "imap", "ldap", "mailserver", "mid", "modem",
  "news", "nntp", "opaquelocktoken", "pop", "prospero", "rdar", "rtsp", "service"
  "sip", "soap.beep", "soap.beeps", "tel", "telnet", "tip", "tn3270", "urn",
  "vemmi", "wais", "xcdoc", "z39.50r","z39.50s",

  // Use RegexStrMailURL:
  "mailto", "im",

  // Use RegexStrRadarURL:
  "radar"
};

static const char *const RegexStrURL =
  "(acap|afp|afs|cid|data|fax|feed|file|ftp|go|"
  "gopher|http|https|imap|ldap|mailserver|mid|modem|news|nntp|opaquelocktoken|"
  "pop|prospero|rdar|rtsp|service|sip|soap\\.beep|soap\\.beeps|tel|telnet|tip|"
  "tn3270|urn|vemmi|wais|xcdoc|z39\\.50r|z39\\.50s)://"
  "([a-zA-Z0-9\\-_.]+/)?[a-zA-Z0-9;/?:@\\&=+$,\\-_.!~*'()%#]+";

static const char *const RegexStrMailURL =
  "(mailto|im):[a-zA-Z0-9\\-_]+@[a-zA-Z0-9\\-_\\.!%]+";

static const char *const RegexStrRadarURL =
  "radar:[a-zA-Z0-9;/?:@\\&=+$,\\-_.!~*'()%#]+";

class ModelASTWalker : public ASTWalker {
  ArrayRef<Token> AllTokensInFile;
  const LangOptions &LangOpts;
  const SourceManager &SM;
  unsigned BufferID;
  std::vector<StructureElement> SubStructureStack;
  SourceLoc LastLoc;
  static const std::regex &getURLRegex(StringRef Protocol);

  Optional<SyntaxNode> parseFieldNode(StringRef Text, StringRef OrigText,
                                      SourceLoc OrigLoc);
  llvm::DenseSet<ASTNode> VisitedNodesInsideIfConfig;

public:
  SyntaxModelWalker &Walker;
  ArrayRef<SyntaxNode> TokenNodes;

  ModelASTWalker(const SourceFile &File, SyntaxModelWalker &Walker)
      : AllTokensInFile(File.getAllTokens()),
        LangOpts(File.getASTContext().LangOpts),
        SM(File.getASTContext().SourceMgr),
        BufferID(File.getBufferID().getValue()),
        Walker(Walker) { }

  void visitSourceFile(SourceFile &SrcFile, ArrayRef<SyntaxNode> Tokens);

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override;
  Expr *walkToExprPost(Expr *E) override;
  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override;
  Stmt *walkToStmtPost(Stmt *S) override;
  bool walkToDeclPre(Decl *D) override;
  bool walkToDeclPost(Decl *D) override;
  bool walkToTypeReprPre(TypeRepr *T) override;
  bool shouldWalkIntoGenericParams() override { return true; }

private:
  static bool findUrlStartingLoc(StringRef Text, unsigned &Start,
                                 std::regex& Regex);
  bool annotateIfConfigConditionIdentifiers(Expr *Cond);
  bool handleAttrs(const DeclAttributes &Attrs);
  bool handleAttrs(const TypeAttributes &Attrs);

  using DeclAttributeAndRange = std::pair<const DeclAttribute *, SourceRange>;

  bool handleSpecialDeclAttribute(const DeclAttribute *Decl,
                                  ArrayRef<Token> Toks);
  bool handleAttrRanges(ArrayRef<DeclAttributeAndRange> DeclRanges);

  bool shouldPassBraceStructureNode(BraceStmt *S);

  enum PassNodesBehavior {
    /// Pass all nodes up to but not including the location.
    ExcludeNodeAtLocation,
    /// Pass all nodes up to and including the location.
    IncludeNodeAtLocation,
    /// Like ExcludeNodeAtLocation, and skip past any node at the location.
    DisplaceNodeAtLocation
  };
  bool passTokenNodesUntil(SourceLoc Loc, PassNodesBehavior Behavior);
  bool passNonTokenNode(const SyntaxNode &Node);
  bool passNode(const SyntaxNode &Node);
  bool pushStructureNode(const SyntaxStructureNode &Node,
                         const ASTNodeType& ASTNode);
  bool popStructureNode();
  bool isCurrentCallArgExpr(const Expr *E);

  bool processComment(CharSourceRange Range);
  bool searchForURL(CharSourceRange Range);
  bool findFieldsInDocCommentLine(SyntaxNode Node);
  bool findFieldsInDocCommentBlock(SyntaxNode Node);
  bool isVisitedBeforeInIfConfig(ASTNode Node) {
    return VisitedNodesInsideIfConfig.count(Node) > 0;
  }
};

const std::regex &ModelASTWalker::getURLRegex(StringRef Pro) {
  static const std::regex Regexes[3] =  {
    std::regex{ RegexStrURL, std::regex::ECMAScript | std::regex::nosubs },
    std::regex{ RegexStrMailURL, std::regex::ECMAScript | std::regex::nosubs },
    std::regex{ RegexStrRadarURL, std::regex::ECMAScript | std::regex::nosubs }
  };

  static const auto MailToPosition = std::find(URLProtocols.begin(),
                                               URLProtocols.end(),
                                               "mailto");
  static const auto RadarPosition = std::find(URLProtocols.begin(),
                                              URLProtocols.end(),
                                              "radar");
  auto Found = std::find(URLProtocols.begin(), URLProtocols.end(), Pro);
  assert(Found != URLProtocols.end() && "bad protocol name");
  if (Found < MailToPosition)
    return Regexes[0];
  else if (Found < RadarPosition)
    return Regexes[1];
  else
    return Regexes[2];
}

SyntaxStructureKind syntaxStructureKindFromNominalTypeDecl(NominalTypeDecl *N) {
  if (isa<ClassDecl>(N))
    return SyntaxStructureKind::Class;
  else if (isa<StructDecl>(N))
    return SyntaxStructureKind::Struct;
  else if (isa<ProtocolDecl>(N))
    return SyntaxStructureKind::Protocol;
  else {
    // All other known NominalTypeDecl derived classes covered, so assert() here.
    assert(isa<EnumDecl>(N));
    return SyntaxStructureKind::Enum;
  }
}

CharSourceRange charSourceRangeFromSourceRange(const SourceManager &SM,
                                               const SourceRange &SR) {
  return Lexer::getCharSourceRangeFromSourceRange(SM, SR);
}

CharSourceRange innerCharSourceRangeFromSourceRange(const SourceManager &SM,
                                                    const SourceRange &SR) {
  if (SR.isInvalid())
    return CharSourceRange();

  SourceLoc SRS = Lexer::getLocForEndOfToken(SM, SR.Start);
  return CharSourceRange(SM, SRS, (SR.End != SR.Start) ? SR.End : SRS);
}

CharSourceRange parameterNameRangeOfCallArg(const TupleExpr *TE,
                                            const Expr *Arg) {
  if (!TE->hasElementNameLocs() || !TE->hasElementNames())
    return CharSourceRange();

  // Loop over the elements to find the index representing Arg.
  // This is somewhat inefficient but the only way to find the corresponding
  // name without the index, and the number of parameters in a call is normally
  // very low. If this becomes a performance problem, we could perhaps have
  // ASTWalker visit the element name as well.
  unsigned i = 0;
  for (auto E : TE->getElements()) {
    if (E == Arg) {
      SourceLoc NL = TE->getElementNameLoc(i);
      Identifier Name = TE->getElementName(i);
      if (NL.isValid() && !Name.empty())
        return CharSourceRange(NL, Name.getLength());

      return CharSourceRange();
    }
    ++i;
  }

  return CharSourceRange();
}

static void setDecl(SyntaxStructureNode &N, Decl *D) {
  N.Dcl = D;
  N.Attrs = D->getAttrs();
  N.DocRange = D->getRawComment().getCharSourceRange();
}

} // anonymous namespace

bool SyntaxModelContext::walk(SyntaxModelWalker &Walker) {
  ModelASTWalker ASTWalk(Impl.SrcFile, Walker);
  ASTWalk.visitSourceFile(Impl.SrcFile, Impl.TokenNodes);
  return true;
}

void ModelASTWalker::visitSourceFile(SourceFile &SrcFile,
                                     ArrayRef<SyntaxNode> Tokens) {
  TokenNodes = Tokens;
  SrcFile.walk(*this);

  // Pass the rest of the token nodes.
  for (auto &TokNode : TokenNodes)
    passNode(TokNode);
}

std::pair<bool, Expr *> ModelASTWalker::walkToExprPre(Expr *E) {
  if (isVisitedBeforeInIfConfig(E))
    return {false, E};

  auto addCallArgExpr = [&](Expr *Elem, TupleExpr *ParentTupleExpr) {
    if (isCurrentCallArgExpr(ParentTupleExpr)) {
      CharSourceRange NR = parameterNameRangeOfCallArg(ParentTupleExpr, Elem);
      SyntaxStructureNode SN;
      SN.Kind = SyntaxStructureKind::Argument;
      SN.NameRange = NR;
      SN.BodyRange = charSourceRangeFromSourceRange(SM, Elem->getSourceRange());
      if (NR.isValid()) {
        SN.Range = charSourceRangeFromSourceRange(SM, SourceRange(NR.getStart(),
                                                            Elem->getEndLoc()));
        passTokenNodesUntil(NR.getStart(),
                            PassNodesBehavior::ExcludeNodeAtLocation);
      }
      else
        SN.Range = SN.BodyRange;

      pushStructureNode(SN, Elem);
    }
  };

  if (auto *ParentTupleExpr = dyn_cast_or_null<TupleExpr>(Parent.getAsExpr())) {
    // the argument value is a tuple expression already, we can just extract it
    addCallArgExpr(E, ParentTupleExpr);
  } else if (auto *ParentOptionalExpr = dyn_cast_or_null<OptionalEvaluationExpr>(Parent.getAsExpr())) {
    // if an argument value is an optional expression, we should extract the
    // argument from the subexpression
    if (auto *ParentTupleExpr = dyn_cast_or_null<TupleExpr>(ParentOptionalExpr->getSubExpr())) {
      addCallArgExpr(E, ParentTupleExpr);
    }
  }

  if (E->isImplicit())
    return { true, E };

  auto addExprElem = [&](const Expr *Elem, SyntaxStructureNode &SN) {
    if (isa<ErrorExpr>(Elem))
      return;
    SourceRange R = Elem->getSourceRange();
    if (R.isInvalid())
      return;
    SN.Elements.emplace_back(SyntaxStructureElementKind::Expr,
                             charSourceRangeFromSourceRange(SM, R));
  };

  if (auto *CE = dyn_cast<CallExpr>(E)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::CallExpression;
    SN.Range = charSourceRangeFromSourceRange(SM, E->getSourceRange());
    if (CE->getFn() && CE->getFn()->getSourceRange().isValid())
      SN.NameRange = charSourceRangeFromSourceRange(SM,
                                                 CE->getFn()->getSourceRange());
    if (CE->getArg() && CE->getArg()->getSourceRange().isValid())
      SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                CE->getArg()->getSourceRange());
    pushStructureNode(SN, CE);

  } else if (auto *ObjectE = dyn_cast<ObjectLiteralExpr>(E)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::ObjectLiteralExpression;
    SN.Range = charSourceRangeFromSourceRange(SM, ObjectE->getSourceRange());
    SourceLoc NRStart = ObjectE->getSourceLoc().getAdvancedLoc(1);    
    SourceLoc NREnd =
      NRStart.getAdvancedLoc(ObjectE->getLiteralKindRawName().size());
    SN.NameRange = CharSourceRange(SM, NRStart, NREnd);
    SN.BodyRange =
      innerCharSourceRangeFromSourceRange(SM, ObjectE->getSourceRange());
    pushStructureNode(SN, E);

  } else if (auto *ArrayE = dyn_cast<ArrayExpr>(E)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::ArrayExpression;
    SN.Range = charSourceRangeFromSourceRange(SM, E->getSourceRange());
    for (auto *Elem : ArrayE->getElements())
      addExprElem(Elem, SN);
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, E->getSourceRange());
    pushStructureNode(SN, E);

  } else if (auto *DictE = dyn_cast<DictionaryExpr>(E)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::DictionaryExpression;
    SN.Range = charSourceRangeFromSourceRange(SM, E->getSourceRange());
    for (auto *Elem : DictE->getElements()) {
      if (auto *TupleE = dyn_cast<TupleExpr>(Elem)) {
        for (auto *TE : TupleE->getElements())
          addExprElem(TE, SN);
      } else {
        addExprElem(Elem, SN);
      }
    }
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, E->getSourceRange());
    pushStructureNode(SN, E);
  } else if (auto *Tup = dyn_cast<TupleExpr>(E)) {
    if (isCurrentCallArgExpr(Tup)) {
      for (unsigned I = 0; I < Tup->getNumElements(); ++ I) {
        SourceLoc NameLoc = Tup->getElementNameLoc(I);
        if (NameLoc.isValid())
          passTokenNodesUntil(NameLoc, PassNodesBehavior::ExcludeNodeAtLocation);
      }
    } else {
      SyntaxStructureNode SN;
      SN.Kind = SyntaxStructureKind::TupleExpression;
      SN.Range = charSourceRangeFromSourceRange(SM, Tup->getSourceRange());
      SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                         Tup->getSourceRange());

      for (auto *Elem : Tup->getElements()) {
        addExprElem(Elem, SN);
      }

      pushStructureNode(SN, Tup);
    }
  } else if (auto *Closure = dyn_cast<ClosureExpr>(E)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::ClosureExpression;
    SN.Range = charSourceRangeFromSourceRange(SM, E->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, E->getSourceRange());
    if (Closure->hasExplicitResultType())
      SN.TypeRange = charSourceRangeFromSourceRange(SM,
                          Closure->getExplicitResultTypeLoc().getSourceRange());

    pushStructureNode(SN, Closure);
  }

  return { true, E };
}

Expr *ModelASTWalker::walkToExprPost(Expr *E) {
  while (!SubStructureStack.empty() &&
      SubStructureStack.back().ASTNode.getAsExpr() == E)
    popStructureNode();

  return E;
}

std::pair<bool, Stmt *> ModelASTWalker::walkToStmtPre(Stmt *S) {
  if (isVisitedBeforeInIfConfig(S)) {
    return {false, S};
  }
  auto addExprElem = [&](SyntaxStructureElementKind K, const Expr *Elem,
                         SyntaxStructureNode &SN) {
    if (isa<ErrorExpr>(Elem))
      return;
    SourceRange R = Elem->getSourceRange();
    if (R.isInvalid())
      return;
    SN.Elements.emplace_back(K, charSourceRangeFromSourceRange(SM, R));
  };

  if (auto *ForEachS = dyn_cast<ForEachStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::ForEachStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    if (ForEachS->getPattern()) {
      auto Pat = ForEachS->getPattern();
      if (!Pat->isImplicit()) {
        SourceRange ElemRange = Pat->getSourceRange();
        SN.Elements.emplace_back(SyntaxStructureElementKind::Id,
                                 charSourceRangeFromSourceRange(SM, ElemRange));
      }
    }
    if (ForEachS->getSequence())
      addExprElem(SyntaxStructureElementKind::Expr, ForEachS->getSequence(),SN);
    pushStructureNode(SN, S);

  } else if (auto *WhileS = dyn_cast<WhileStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::WhileStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    if (!WhileS->getCond().empty()) {
      auto Conds = WhileS->getCond();
      SourceRange ElemRange = SourceRange(Conds.front().getSourceRange().Start,
                                          Conds.back().getSourceRange().End);
      SN.Elements.emplace_back(SyntaxStructureElementKind::ConditionExpr,
                               charSourceRangeFromSourceRange(SM, ElemRange));
    }
    pushStructureNode(SN, S);

  } else if (auto *RepeatWhileS = dyn_cast<RepeatWhileStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::RepeatWhileStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    if (RepeatWhileS->getCond()) {
      addExprElem(SyntaxStructureElementKind::Expr, RepeatWhileS->getCond(), SN);
    }
    pushStructureNode(SN, S);

  } else if (auto *IfS = dyn_cast<IfStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::IfStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    if (!IfS->getCond().empty()) {
      auto Conds = IfS->getCond();
      SourceRange ElemRange = SourceRange(Conds.front().getSourceRange().Start,
                                          Conds.back().getSourceRange().End);
      SN.Elements.emplace_back(SyntaxStructureElementKind::ConditionExpr,
                               charSourceRangeFromSourceRange(SM, ElemRange));
    }
    pushStructureNode(SN, S);

  } else if (auto *GS = dyn_cast<GuardStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::GuardStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    if (!GS->getCond().empty()) {
      auto Conds = GS->getCond();
      SourceRange ElemRange = SourceRange(Conds.front().getSourceRange().Start,
                                          Conds.back().getSourceRange().End);
      SN.Elements.emplace_back(SyntaxStructureElementKind::ConditionExpr,
                               charSourceRangeFromSourceRange(SM, ElemRange));
    }
    pushStructureNode(SN, S);

  } else if (auto *SwitchS = dyn_cast<SwitchStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::SwitchStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    if (SwitchS->getSubjectExpr()) {
      addExprElem(SyntaxStructureElementKind::Expr, SwitchS->getSubjectExpr(),
                  SN);
    }
    pushStructureNode(SN, S);

  } else if (auto *CaseS = dyn_cast<CaseStmt>(S)) {
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::CaseStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    for (const CaseLabelItem &Item : CaseS->getCaseLabelItems()) {
      SN.Elements.emplace_back(SyntaxStructureElementKind::Pattern,
                               charSourceRangeFromSourceRange(SM,
                                                        Item.getSourceRange()));
    }
    pushStructureNode(SN, S);

  } else if (isa<BraceStmt>(S) && shouldPassBraceStructureNode(cast<BraceStmt>(S))) {
    // Pass BraceStatement structure node.
    SyntaxStructureNode SN;
    SN.Kind = SyntaxStructureKind::BraceStatement;
    SN.Range = charSourceRangeFromSourceRange(SM, S->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                       S->getSourceRange());
    pushStructureNode(SN, S);

  } else if (auto *SW = dyn_cast<SwitchStmt>(S)) {
    if (SW->getLBraceLoc().isValid() && SW->getRBraceLoc().isValid()) {
      SourceRange BraceRange(SW->getLBraceLoc(), SW->getRBraceLoc());
      SyntaxStructureNode SN;
      SN.Kind = SyntaxStructureKind::BraceStatement;
      SN.Range = charSourceRangeFromSourceRange(SM, BraceRange);
      SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, BraceRange);
      pushStructureNode(SN, SW);
    }

  } else if (auto *DeferS = dyn_cast<DeferStmt>(S)) {
    // Since 'DeferStmt::getTempDecl()' is marked as implicit, we manually walk
    // into the body.
    if (auto *FD = DeferS->getTempDecl()) {
      auto *RetS = FD->getBody()->walk(*this);
      assert(RetS == FD->getBody());
      (void)RetS;
      walkToStmtPost(DeferS);
    }
    // Already walked children.
    return { false, DeferS };
  }

  return { true, S };
}

Stmt *ModelASTWalker::walkToStmtPost(Stmt *S) {
  while (!SubStructureStack.empty() &&
      SubStructureStack.back().ASTNode.getAsStmt() == S)
    popStructureNode();

  return S;
}

bool ModelASTWalker::walkToDeclPre(Decl *D) {
  if (isVisitedBeforeInIfConfig(D))
    return false;
  if (D->isImplicit())
    return false;

  if (!handleAttrs(D->getAttrs()))
    return false;

  if (isa<AccessorDecl>(D)) {
    // Don't push structure nodes for accessors.
  } else if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
    // Pass Function / Method structure node.
    SyntaxStructureNode SN;
    setDecl(SN, D);
    const DeclContext *DC = AFD->getDeclContext();
    auto *FD = dyn_cast<FuncDecl>(AFD);
    if (DC->isTypeContext()) {
      if (FD && FD->isStatic()) {
        if (FD->getStaticSpelling() == StaticSpellingKind::KeywordClass)
          SN.Kind = SyntaxStructureKind::ClassFunction;
        else
          SN.Kind = SyntaxStructureKind::StaticFunction;
      } else {
        SN.Kind = SyntaxStructureKind::InstanceFunction;
      }
    }
    else
      SN.Kind = SyntaxStructureKind::FreeFunction;
    SN.Range = charSourceRangeFromSourceRange(SM, AFD->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                                   AFD->getBodySourceRange());
    SN.NameRange = charSourceRangeFromSourceRange(SM,
                        AFD->getSignatureSourceRange());
    if (FD) {
      SN.TypeRange = charSourceRangeFromSourceRange(SM,
                                    FD->getReturnTypeLoc().getSourceRange());
    }
    pushStructureNode(SN, AFD);
  } else if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = syntaxStructureKindFromNominalTypeDecl(NTD);
    SN.Range = charSourceRangeFromSourceRange(SM, NTD->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, NTD->getBraces());
    SourceLoc NRStart = NTD->getNameLoc();
    SourceLoc NREnd = NRStart.getAdvancedLoc(NTD->getName().getLength());
    SN.NameRange = CharSourceRange(SM, NRStart, NREnd);

    for (const TypeLoc &TL : NTD->getInherited()) {
      CharSourceRange TR = charSourceRangeFromSourceRange(SM,
                                                          TL.getSourceRange());
      SN.InheritedTypeRanges.push_back(TR);
      SN.Elements.emplace_back(SyntaxStructureElementKind::TypeRef, TR);
    }

    pushStructureNode(SN, NTD);

  } else if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = SyntaxStructureKind::Extension;
    SN.Range = charSourceRangeFromSourceRange(SM, ED->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, ED->getBraces());
    SourceRange NSR = ED->getExtendedTypeLoc().getSourceRange();
    SN.NameRange = charSourceRangeFromSourceRange(SM, NSR);

    for (const TypeLoc &TL : ED->getInherited()) {
      CharSourceRange TR = charSourceRangeFromSourceRange(SM,
                                                          TL.getSourceRange());
      SN.InheritedTypeRanges.push_back(TR);
      SN.Elements.emplace_back(SyntaxStructureElementKind::TypeRef, TR);
    }

    pushStructureNode(SN, ED);

  } else if (auto *PD = dyn_cast<ParamDecl>(D)) {
    SyntaxStructureNode SN;
    SN.Dcl = D;
    SN.Kind = SyntaxStructureKind::Parameter;
    if (!PD->getArgumentName().empty()) {
      SourceLoc ArgStart = PD->getSourceRange().Start;
      SN.NameRange = CharSourceRange(ArgStart, PD->getArgumentName().getLength());
      passTokenNodesUntil(ArgStart, PassNodesBehavior::ExcludeNodeAtLocation);
    }
    SN.Range = charSourceRangeFromSourceRange(SM, PD->getSourceRange());
    SN.Attrs = PD->getAttrs();
    SN.TypeRange = charSourceRangeFromSourceRange(SM,
                                      PD->getTypeSourceRangeForDiagnostics());
    pushStructureNode(SN, PD);
  } else if (auto *VD = dyn_cast<VarDecl>(D)) {
    const DeclContext *DC = VD->getDeclContext();
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SourceRange SR;
    if (auto *PBD = VD->getParentPatternBinding())
      SR = PBD->getSourceRange();
    else
      SR = VD->getSourceRange();
    SN.Range = charSourceRangeFromSourceRange(SM, SR);
    auto bracesRange = VD->getBracesRange();
    if (bracesRange.isValid())
      SN.BodyRange = innerCharSourceRangeFromSourceRange(SM, bracesRange);
    SourceLoc NRStart = VD->getNameLoc();
    SourceLoc NREnd = NRStart.getAdvancedLoc(VD->getName().getLength());
    SN.NameRange = CharSourceRange(SM, NRStart, NREnd);
    SN.TypeRange = charSourceRangeFromSourceRange(SM,
                                        VD->getTypeSourceRangeForDiagnostics());

    if (DC->isLocalContext()) {
      SN.Kind = SyntaxStructureKind::LocalVariable;
    } else if (DC->isTypeContext()) {
      if (VD->isStatic()) {
        StaticSpellingKind Spell = StaticSpellingKind::KeywordStatic;
        if (auto *PBD = VD->getParentPatternBinding())
          Spell = PBD->getStaticSpelling();
        if (Spell == StaticSpellingKind::KeywordClass)
          SN.Kind = SyntaxStructureKind::ClassVariable;
        else
          SN.Kind = SyntaxStructureKind::StaticVariable;
      } else {
        SN.Kind = SyntaxStructureKind::InstanceVariable;
      }
    } else {
      SN.Kind = SyntaxStructureKind::GlobalVariable;
    }
    pushStructureNode(SN, VD);

  } else if (auto *ConfigD = dyn_cast<IfConfigDecl>(D)) {
    for (auto &Clause : ConfigD->getClauses()) {
      if (Clause.Cond && !annotateIfConfigConditionIdentifiers(Clause.Cond))
        return false;

      for (auto &Element : Clause.Elements) {
        if (auto *E = Element.dyn_cast<Expr*>()) {
          E->walk(*this);
        } else if (auto *S = Element.dyn_cast<Stmt*>()) {
          S->walk(*this);
        } else {
          Element.get<Decl*>()->walk(*this);
        }
        VisitedNodesInsideIfConfig.insert(Element);
      }
    }

  } else if (auto *EnumCaseD = dyn_cast<EnumCaseDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = SyntaxStructureKind::EnumCase;
    SN.Range = charSourceRangeFromSourceRange(SM, D->getSourceRange());

    // We need to handle the special case where attributes semantically
    // attach to enum element decls while syntactically locate before enum case decl.
    for (auto *EnumElemD : EnumCaseD->getElements()) {
      for (auto *Att : EnumElemD->getAttrs()) {
        if (Att->isDeclModifier() &&
            SM.isBeforeInBuffer(Att->getLocation(), D->getSourceRange().Start)) {
          passNonTokenNode({SyntaxNodeKind::AttributeBuiltin,
                            charSourceRangeFromSourceRange(SM,
                                                           Att->getLocation())});
        }
      }
    }
    if (pushStructureNode(SN, D)) {
      // FIXME: ASTWalker walks enum elements as members of the enum decl, not
      // as members of the enum case decl. Walk them manually here so that they
      // end up as child nodes of enum case.
      for (auto *EnumElemD : EnumCaseD->getElements()) {
        if (EnumElemD->getName().empty())
          continue;
        SyntaxStructureNode SN;
        setDecl(SN, EnumElemD);
        SN.Kind = SyntaxStructureKind::EnumElement;
        SN.Range = charSourceRangeFromSourceRange(SM,
                                                  EnumElemD->getSourceRange());
        if (auto ParamList = EnumElemD->getParameterList()) {
          SourceRange NameRange = SourceRange(EnumElemD->getNameLoc(),
                                              ParamList->getSourceRange().End);
          SN.NameRange = charSourceRangeFromSourceRange(SM, NameRange);
        } else {
          SN.NameRange = CharSourceRange(EnumElemD->getNameLoc(),
                                         EnumElemD->getName().getLength());
        }

        if (auto *E = EnumElemD->getRawValueExpr()) {
          SourceRange ElemRange = E->getSourceRange();
          SN.Elements.emplace_back(SyntaxStructureElementKind::InitExpr,
                                 charSourceRangeFromSourceRange(SM, ElemRange));
        }
        pushStructureNode(SN, EnumElemD);
        popStructureNode();
      }
    }
  } else if (auto *TypeAliasD = dyn_cast<TypeAliasDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = SyntaxStructureKind::TypeAlias;
    SN.Range = charSourceRangeFromSourceRange(SM,
                                              TypeAliasD->getSourceRange());
    SN.NameRange = CharSourceRange(TypeAliasD->getNameLoc(),
                                   TypeAliasD->getName().getLength());
    pushStructureNode(SN, TypeAliasD);
  } else if (auto *SubscriptD = dyn_cast<SubscriptDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = SyntaxStructureKind::Subscript;
    SN.Range = charSourceRangeFromSourceRange(SM,
                                              SubscriptD->getSourceRange());
    SN.BodyRange = innerCharSourceRangeFromSourceRange(SM,
                                               SubscriptD->getBracesRange());
    SN.NameRange = charSourceRangeFromSourceRange(SM,
                                        SubscriptD->getSignatureSourceRange());
    SN.TypeRange = charSourceRangeFromSourceRange(SM,
                            SubscriptD->getElementTypeLoc().getSourceRange());
    pushStructureNode(SN, SubscriptD);
  } else if (auto *AssociatedTypeD = dyn_cast<AssociatedTypeDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = SyntaxStructureKind::AssociatedType;
    SN.Range = charSourceRangeFromSourceRange(SM,
                                            AssociatedTypeD->getSourceRange());
    SN.NameRange = CharSourceRange(AssociatedTypeD->getNameLoc(),
                                   AssociatedTypeD->getName().getLength());
    pushStructureNode(SN, AssociatedTypeD);
  } else if (auto *GenericParamD = dyn_cast<GenericTypeParamDecl>(D)) {
    SyntaxStructureNode SN;
    setDecl(SN, D);
    SN.Kind = SyntaxStructureKind::GenericTypeParam;
    SN.Range = charSourceRangeFromSourceRange(SM,
                                              GenericParamD->getSourceRange());
    SN.NameRange = CharSourceRange(GenericParamD->getNameLoc(),
                                   GenericParamD->getName().getLength());
    for (const TypeLoc &TL : GenericParamD->getInherited()) {
      CharSourceRange TR = charSourceRangeFromSourceRange(SM,
                                                          TL.getSourceRange());
      SN.InheritedTypeRanges.push_back(TR);
      SN.Elements.emplace_back(SyntaxStructureElementKind::TypeRef, TR);
    }

    pushStructureNode(SN, GenericParamD);
  }

  return true;
}

bool ModelASTWalker::walkToDeclPost(swift::Decl *D) {
  while (!SubStructureStack.empty() &&
      SubStructureStack.back().ASTNode.getAsDecl() == D)
    popStructureNode();

  return true;
}

bool ModelASTWalker::walkToTypeReprPre(TypeRepr *T) {
  if (auto AttrT = dyn_cast<AttributedTypeRepr>(T)) {
    if (!handleAttrs(AttrT->getAttrs()))
      return false;

  } else if (auto IdT = dyn_cast<ComponentIdentTypeRepr>(T)) {
    if (!passNonTokenNode({ SyntaxNodeKind::TypeId,
                            CharSourceRange(IdT->getIdLoc(),
                                            IdT->getIdentifier().getLength())
                          }))
      return false;
  }
  return true;
}

namespace {
template <typename FnTy>
class IdRefWalker : public ASTWalker {
  const FnTy &Fn;

public:
  IdRefWalker(const FnTy &Fn) : Fn(Fn) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (auto DRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
      if (!DRE->hasName())
        return { true, E };
      if (DRE->getRefKind() != DeclRefKind::Ordinary)
        return { true, E };
      if (!Fn(CharSourceRange(
              DRE->getSourceRange().Start,
              DRE->getName().getBaseName().userFacingName().size())))
        return { false, nullptr };
    }
    return { true, E };
  }
};
} // end anonymous namespace

bool ModelASTWalker::annotateIfConfigConditionIdentifiers(Expr *Cond) {
  if (!Cond)
    return true;
  auto passNode = [&](CharSourceRange R) {
    return passNonTokenNode({ SyntaxNodeKind::BuildConfigId, R });
  };

  IdRefWalker<decltype(passNode)> Walker(passNode);
  return Cond->walk(Walker);
}

bool ModelASTWalker::handleSpecialDeclAttribute(const DeclAttribute *D,
                                                ArrayRef<Token> Toks) {
  if (!D)
    return false;
  if (isa<AvailableAttr>(D)) {
    unsigned I = 0;
    for (; I < TokenNodes.size(); ++ I) {
      auto Node = TokenNodes[I];
      if (SM.isBeforeInBuffer(D->getRange().End, Node.Range.getStart()))
        break;
      if (Node.Range.contains(D->AtLoc)) {
        if (!passNode({SyntaxNodeKind::AttributeBuiltin, Node.Range}))
          break;
        continue;
      }

      if (!passNode(Node))
        break;
    }
    TokenNodes = TokenNodes.slice(I);
    return true;
  }
  return false;
}

bool ModelASTWalker::handleAttrs(const DeclAttributes &Attrs) {
  SmallVector<DeclAttributeAndRange, 4> DeclRanges;
  for (auto At : Attrs) {
    if (At->getRangeWithAt().isValid())
      DeclRanges.push_back(std::make_pair(At, At->getRangeWithAt()));
  }
  return handleAttrRanges(DeclRanges);
}

bool ModelASTWalker::handleAttrs(const TypeAttributes &Attrs) {
  SmallVector<SourceRange, 4> Ranges;
  Attrs.getAttrRanges(Ranges);
  SmallVector<DeclAttributeAndRange, 4> DeclRanges;
  for (auto R : Ranges) {
    DeclRanges.push_back(std::make_pair(nullptr, R));
  }
  return handleAttrRanges(DeclRanges);
}

bool ModelASTWalker::handleAttrRanges(ArrayRef<DeclAttributeAndRange> DeclRanges) {
  if (DeclRanges.empty())
    return true;

  SmallVector<DeclAttributeAndRange, 4> SortedRanges(DeclRanges.begin(),
                                                     DeclRanges.end());
  std::sort(SortedRanges.begin(), SortedRanges.end(),
            [&](DeclAttributeAndRange LHS, DeclAttributeAndRange RHS) {
    return SM.isBeforeInBuffer(LHS.second.Start, RHS.second.End);
  });
  DeclRanges = SortedRanges;

  SourceLoc BeginLoc = DeclRanges.front().second.Start;

  auto Toks = slice_token_array(AllTokensInFile, BeginLoc,
                                DeclRanges.back().second.End);

  auto passAttrNode = [&](SourceRange AttrRange) -> bool {
    SourceRange Range = AttrRange;
    if (!passNonTokenNode({SyntaxNodeKind::AttributeBuiltin,
                           charSourceRangeFromSourceRange(SM, Range)}))
      return false;

    while (!TokenNodes.empty() &&
           SM.rangeContainsTokenLoc(AttrRange,
                                    TokenNodes.front().Range.getStart()))
      TokenNodes = TokenNodes.slice(1);
    return true;
  };

  for (auto Tok : Toks) {
    if (DeclRanges.empty())
      break;
    if (Tok.getLoc() == DeclRanges.front().second.Start) {
      auto R = DeclRanges.front().second;
      auto D = DeclRanges.front().first;
      DeclRanges = DeclRanges.slice(1);
      if (!handleSpecialDeclAttribute(D, Toks)) {
        if (!passAttrNode(R))
          return false;
      }
    }
  }

  if (!DeclRanges.empty() &&
      !handleSpecialDeclAttribute(DeclRanges.front().first, Toks)) {
    if (!passAttrNode(DeclRanges.front().second))
      return false;
  }

  return true;
}

bool ModelASTWalker::shouldPassBraceStructureNode(BraceStmt *S) {
  return (!dyn_cast_or_null<AbstractFunctionDecl>(Parent.getAsDecl()) &&
          !dyn_cast_or_null<TopLevelCodeDecl>(Parent.getAsDecl()) &&
          !dyn_cast_or_null<CaseStmt>(Parent.getAsStmt()) &&
          S->getSourceRange().isValid());
}

bool ModelASTWalker::passTokenNodesUntil(SourceLoc Loc,
                                         PassNodesBehavior Behavior) {
  assert(Loc.isValid());
  unsigned I = 0;
  for (unsigned E = TokenNodes.size(); I != E; ++I) {
    SourceLoc TokLoc = TokenNodes[I].Range.getStart();
    if (SM.isBeforeInBuffer(Loc, TokLoc)) {
      break;
    }
    if (TokLoc == Loc && Behavior != IncludeNodeAtLocation) {
      if (Behavior == DisplaceNodeAtLocation) {
        // Skip past the node directly at the specified location, allowing the
        // caller to effectively replace it.
        ++I;
      }
      break;
    }
    if (!passNode(TokenNodes[I]))
      return false;
  }

  TokenNodes = TokenNodes.slice(I);
  return true;
}

bool ModelASTWalker::passNonTokenNode(const SyntaxNode &Node) {
  // Skip out of order non-token nodes.
  // Ideally this shouldn't happen, but the AST can contain overlapping nodes,
  // such as multiple PatternBindingDecl in code like: var a, b : Int. Which
  // would cause us to report the TypeRepr twice.
  if (!SM.isBeforeInBuffer(LastLoc, Node.Range.getStart()))
    return false;

  if (!passTokenNodesUntil(Node.Range.getStart(), DisplaceNodeAtLocation))
    return false;
  if (!passNode(Node))
    return false;
  return true;
}

bool ModelASTWalker::passNode(const SyntaxNode &Node) {
  assert(!SM.isBeforeInBuffer(Node.Range.getStart(), LastLoc));
  LastLoc = Node.Range.getStart();

  bool ShouldWalkSubTree = Walker.walkToNodePre(Node);

  if (ShouldWalkSubTree) {
    if (Node.isComment()) {
      if (!processComment(Node.Range))
        return false;
    } else if (Node.Kind == SyntaxNodeKind::DocCommentLine) {
      if (!findFieldsInDocCommentLine(Node))
        return false;
    } else if (Node.Kind == SyntaxNodeKind::DocCommentBlock) {
      if (!findFieldsInDocCommentBlock(Node))
        return false;
    } else if (Node.Kind == SyntaxNodeKind::CommentMarker) {
      if (!searchForURL(Node.Range))
        return false;
    }
  }

  return Walker.walkToNodePost(Node);
}

bool ModelASTWalker::pushStructureNode(const SyntaxStructureNode &Node,
                                       const ASTNodeType& ASTNode) {
  SubStructureStack.emplace_back(Node, ASTNode);

  if (!passTokenNodesUntil(Node.Range.getStart(), ExcludeNodeAtLocation))
    return false;
  if (!Walker.walkToSubStructurePre(Node))
    return false;

  return true;
}

bool ModelASTWalker::popStructureNode() {
  assert(!SubStructureStack.empty());
  SyntaxStructureNode Node = SubStructureStack.back().StructureNode;
  SubStructureStack.pop_back();

  // VarDecls are popped before we see their TypeRepr, so if we pass the token
  // nodes now they will not change from identifier to a type-identifier.
  if (!Node.hasSubstructure()) {
    if (!passTokenNodesUntil(Node.Range.getEnd(), IncludeNodeAtLocation))
      return false;
  }
  if (!Walker.walkToSubStructurePost(Node))
    return false;

  return true;
}

bool ModelASTWalker::isCurrentCallArgExpr(const Expr *E) {
  if (SubStructureStack.empty())
    return false;
  auto Current = SubStructureStack.back();

  if (Current.StructureNode.Kind ==
        SyntaxStructureKind::ObjectLiteralExpression &&
      cast<ObjectLiteralExpr>(Current.ASTNode.getAsExpr())->getArg() == E)
    return true;

  return Current.StructureNode.Kind == SyntaxStructureKind::CallExpression &&
         cast<CallExpr>(Current.ASTNode.getAsExpr())->getArg() == E;
}

bool ModelASTWalker::processComment(CharSourceRange Range) {
  StringRef Text = SM.extractText(Range, BufferID);
  SourceLoc Loc = Range.getStart();
  // Search for 'FIXME:' or 'TODO:'.
  while (1) {
    auto Pos = Text.find_first_of("FTM");
    if (Pos == StringRef::npos)
      return searchForURL(Range);

    Text = Text.substr(Pos);
    Loc = Loc.getAdvancedLoc(Pos);
    if (Text.startswith("FIXME:") || Text.startswith("TODO:") ||
        Text.startswith("MARK:"))
      break;
    Text = Text.substr(1);
    Loc = Loc.getAdvancedLoc(1);
  }

  auto NewLinePos = Text.find_first_of("\r\n");
  if (NewLinePos != StringRef::npos) {
    Text = Text.substr(0, NewLinePos);
  }

  CharSourceRange BeforeMarker{ SM, Range.getStart(), Loc };
  CharSourceRange Marker(Loc, Text.size());
  CharSourceRange AfterMarker{ SM, Marker.getEnd(), Range.getEnd() };

  if (!searchForURL(BeforeMarker))
    return false;

  SyntaxNode Node{ SyntaxNodeKind::CommentMarker, Marker };
  if (!passNode(Node))
    return false;

  return searchForURL(AfterMarker);  
}

bool ModelASTWalker::findUrlStartingLoc(StringRef Text,
                                        unsigned &Start,
                                        std::regex &Regex) {
#ifdef SWIFT_HAVE_WORKING_STD_REGEX
  static const auto MailToPosition = std::find(URLProtocols.begin(),
                                               URLProtocols.end(),
                                               "mailto");
  auto Index = Text.find(":");
  if (Index == StringRef::npos)
    return false;

  auto Lookback = [Text](unsigned Index, StringRef Name) {
    return Index >= Name.size() &&
      Text.substr(Index - Name.size(), Name.size()) == Name;
  };

  auto HasSlash = Text.substr(Index).startswith("://");

  if (HasSlash) {
    for (auto It = URLProtocols.begin(); It < URLProtocols.end(); ++ It) {
      if (Lookback(Index, *It)) {
        Regex = getURLRegex(*It);
        Start = Index - It->size();
        return true;
      }
    }
  } else {
    for (auto It = MailToPosition; It < URLProtocols.end(); ++ It) {
      if (Lookback(Index, *It)) {
        Regex = getURLRegex(*It);
        Start = Index - It->size();
        return true;
      }
    }
  }
#endif
  return false;
}

static CharSourceRange sanitizeUnpairedParenthesis(CharSourceRange Range) {
  auto Text = Range.str();
  if (Text.back() != ')') {
    return Range;
  }
  unsigned Pairs = 0;
  unsigned TrimLen = 0;
  for (char C : Text) {
    if (C == '(') {
      Pairs ++;
    } else if (C == ')') {
      if (Pairs == 0)
        TrimLen ++;
      else
        Pairs --;
    } else {
      TrimLen = 0;
    }
  }

  return CharSourceRange(Range.getStart(), Text.size() - TrimLen);
}

bool ModelASTWalker::searchForURL(CharSourceRange Range) {
  StringRef OrigText = SM.extractText(Range, BufferID);
  SourceLoc OrigLoc = Range.getStart();

  StringRef Text = OrigText;
  while (1) {
    std::match_results<StringRef::iterator> Matches;
    std::regex Regex;
    unsigned Start;
    if (findUrlStartingLoc(Text, Start, Regex) &&
        std::regex_search(Text.substr(Start).begin(),
                          Text.substr(Start).end(), Matches, Regex)) {
      auto &RxMatch = Matches[0];
      StringRef Match(RxMatch.first, RxMatch.second - RxMatch.first);
      SourceLoc Loc = OrigLoc.getAdvancedLoc(Match.data() - OrigText.data());
      CharSourceRange Range(Loc, Match.size());
      SyntaxNode Node{ SyntaxNodeKind::CommentURL,
                       sanitizeUnpairedParenthesis(Range) };
      if (!passNode(Node))
        return false;
      Text = Text.substr(Match.data() - Text.data() + Match.size());
    } else {
      auto Index = Text.find(':');
      if (Index == StringRef::npos)
        break;
      Text = Text.substr(Index + 1);
    }
  }
  return true;
}

namespace {
class DocFieldParser {
  const char *ptr;
  const char *end;

  bool advanceIf(char c) {
    if (ptr == end || c != *ptr)
      return false;
    ++ptr;
    return true;
  }
  bool advanceIf(llvm::function_ref<bool(char)> predicate) {
    if (ptr == end || !predicate(*ptr))
      return false;
    ++ptr;
    return true;
  }

public:
  DocFieldParser(StringRef text) : ptr(text.begin()), end(text.end()) {
    assert(text.rtrim().find('\n') == StringRef::npos &&
           "expected single line");
  }

  // Case-insensitively match one of the following patterns:
  // ^[ ]?- (parameter) [^:]*:
  // ^[ ]?- (Parameters):
  // ^[ ]*- (...MarkupSimpleFields.def...|returns):
  Optional<StringRef> parseFieldName() {
    unsigned numSpaces = 0;
    while (advanceIf(' '))
      ++numSpaces;
    if (!advanceIf('-') || !advanceIf(' '))
      return None;

    if (ptr == end || !clang::isIdentifierBody(*ptr))
      return None;
    const char *identStart = ptr++;
    while (advanceIf([](char c) { return clang::isIdentifierBody(c); }))
      ;
    StringRef ident(identStart, ptr - identStart);

    if (ident.equals_lower("parameter")) {
      if (numSpaces > 1 || !advanceIf(' '))
        return None;
      while (advanceIf([](char c) { return c != ':'; }))
        ;
      if (!advanceIf(':'))
        return None;
      return ident;

    } else if (advanceIf(':')) {
      if (ident.equals_lower("parameters") && numSpaces > 1)
        return None;
      auto lowerIdent = ident.lower();
      bool isField = llvm::StringSwitch<bool>(lowerIdent)
#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) .Case(#Keyword, true)
#include "swift/Markup/SimpleFields.def"
                         .Case("parameters", true)
                         .Case("returns", true)
                         .Default(false);
      if (isField)
        return ident;
    }

    return None;
  }
};
} // end anonymous namespace

Optional<SyntaxNode> ModelASTWalker::parseFieldNode(StringRef Text,
                                                    StringRef OrigText,
                                                    SourceLoc OrigLoc) {
  Optional<SyntaxNode> Node;
  DocFieldParser parser(Text);
  if (auto ident = parser.parseFieldName()) {
    auto loc = OrigLoc.getAdvancedLoc(ident->data() - OrigText.data());
    CharSourceRange range(loc, ident->size());
    Node = Optional<SyntaxNode>({SyntaxNodeKind::DocCommentField, range});
  }
  return Node;
}

bool ModelASTWalker::findFieldsInDocCommentLine(SyntaxNode Node) {
  auto OrigText = SM.extractText(Node.Range, BufferID);
  auto OrigLoc = Node.Range.getStart();

  auto Text = OrigText.drop_front(3); // Drop "///"
  if (Text.empty())
    return true;

  auto FieldNode = parseFieldNode(Text, OrigText, OrigLoc);
  if (FieldNode.hasValue())
    passNode(FieldNode.getValue());
  else
    searchForURL(Node.Range);
  return true;
}

bool ModelASTWalker::findFieldsInDocCommentBlock(SyntaxNode Node) {
  auto OrigText = SM.extractText(Node.Range, BufferID);
  auto OrigLoc = Node.Range.getStart();

  if (!OrigText.startswith("/**") &&
      !(LangOpts.Playground && OrigText.startswith("/*:")))
    return true;

  auto Text = OrigText.drop_front(3); // Drop "^/**" or "/*:"

  if (!Text.endswith("*/"))
    return true;

  Text = Text.drop_back(2); // Drop "*/"

  if (Text.empty())
    return true;

  llvm::SmallVector<StringRef, 8> RawLines;
  Text.split(RawLines, '\n');
  auto FirstNewLine = std::find_if(RawLines.begin(), RawLines.end(),
    [](StringRef Line) { return !Line.trim().empty(); });

  if (FirstNewLine == RawLines.end())
    return true;

  Text = Text.substr(FirstNewLine->data() - Text.data());
  if (Text.empty())
    return true;

  size_t Indent = Text.ltrim().data() - Text.data();
  SmallVector<StringRef, 10> Lines;
  Text.split(Lines, "\n");

  for (auto Line : Lines) {
    Line = Line.rtrim();
    if (Line.size() < Indent)
      continue;
    auto FieldNode = parseFieldNode(Line.drop_front(Indent), OrigText, OrigLoc);
    if (FieldNode.hasValue())
      passNode(FieldNode.getValue());
    else
      searchForURL(CharSourceRange(Node.Range.getStart().
        getAdvancedLoc(Line.data() - OrigText.data()),
                                   Line.size()));
  }

  std::match_results<StringRef::iterator> Matches;
  return true;
}
