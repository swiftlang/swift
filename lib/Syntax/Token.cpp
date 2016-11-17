//===--- Token.cpp - Token interface ----------------------------*- C++ -*-===//
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
//
//  This file defines the Token interface.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/RawComment.h"
#include "swift/Syntax/Token.h"

#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace swift::syntax;

void Token::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  for (unsigned i = 0; i < Indent; ++i)
    OS << ' ';

  OS << '(';
#define str(s) #s
#define DECL_KEYWORD(kind) case tok::kw_##kind: OS << "decl " str(kind); break;
#define SIL_KEYWORD(kind) case tok::kw_##kind: OS << "sil " << str(kind); break;
#define STMT_KEYWORD(kind) case tok::kw_##kind: OS << "stmt " << str(kind); break;
#define KEYWORD(kind) case tok::kw_##kind: OS << "keyword " << str(kind); break;
#define PUNCTUATOR(kind, text) case tok::kind: OS << "'" << str(text) << "'"; break;
#define POUND_KEYWORD(kind) case tok::pound_##kind: OS << '#' << str(kind); break;
#define POUND_CONFIG(kind) case tok::pound_##kind: OS << '#' << str(kind); break;
  switch (Kind) {
  #include "swift/Parse/Tokens.def"
#undef str
  case tok::unknown:
    OS << "unknown";
    break;
  case tok::eof:
    OS << "eof";
    break;
  case tok::code_complete:
    OS << "code_complete ";
    break;
  case tok::identifier:
    OS << "identifier ";
    break;
  case tok::oper_binary_unspaced:
    OS << "oper_binary_unspaced ";
    break;
  case tok::oper_binary_spaced:
    OS << "oper_binary_spaces";
    break;
  case tok::oper_postfix:
    OS << "oper_postfix";
    break;
  case tok::oper_prefix:
    OS << "oper_prefix";
    break;
  case tok::dollarident:
    OS << "dollar_indent ";
    break;
  case tok::integer_literal:
    OS << "integer_literal ";
    break;
  case tok::floating_literal:
    OS << "floating_literal ";
    break;
  case tok::string_literal:
    OS << "string_literal ";
    break;
  case tok::sil_local_name:
    OS << "sil_local_name ";
    break;
  case tok::comment:
    llvm_unreachable("These aren't tokens anymore!");
  case tok::NUM_TOKENS:
    llvm_unreachable("Not a valid token kind!");
  }

  OS << '"' << Text.str() << "\"\n";
  OS << ')';
}

/// Print the raw syntax tree with full formatting fideltiy.
void Token::print(llvm::raw_ostream &OS) const {
  for (auto Leader : LeadingTrivia)
    Leader.print(OS);

  OS << Text.str();

  for (auto Trailer : TrailingTrivia)
    Trailer.print(OS);
}

std::vector<SingleRawComment>
Token::getRawCommentPieces(SourceManager &SourceMgr) const {
  std::vector<SingleRawComment> RawCommentPieces;

  for (auto &T : LeadingTrivia) {
    if (T.getKind() > TriviaKind::DocComment) {
      if (!T.isGYBComment())
        RawCommentPieces.clear();
      continue;
    }

    if (T.getKind() == TriviaKind::DocComment)
      RawCommentPieces.push_back({ T.getCharSourceRange(), SourceMgr });
  }

  return RawCommentPieces;
}
