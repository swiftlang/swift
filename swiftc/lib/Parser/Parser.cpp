#include "swiftc/Parser/Parser.h"
#include "swiftc/Basic/SourceManager.h"
#include <sstream>

using namespace swiftc;

Parser::Parser(Lexer& lexer, DiagnosticEngine& diags)
    : Lex(lexer), Diags(diags) {
  // Prime the parser with the first token
  consumeToken();
}

std::vector<std::unique_ptr<Decl>> Parser::parseSourceFile() {
  std::vector<std::unique_ptr<Decl>> topLevelDecls;
  
  while (!is(TokenKind::Eof)) {
    // Skip any leading special tokens (comments, directives, etc.)
    while (is(TokenKind::Hash) || is(TokenKind::At) || is(TokenKind::Dollar) ||
           is(TokenKind::Backtick) || is(TokenKind::Backslash) || 
           is(TokenKind::LeftBraceBrace) || is(TokenKind::RightBraceBrace)) {
      consumeToken();
      if (is(TokenKind::Eof)) break;
    }
    
    if (is(TokenKind::Eof)) break;
    
    if (auto decl = parseTopLevelDecl()) {
      topLevelDecls.push_back(std::move(decl));
    } else {
      // Better error recovery - skip to next likely declaration start
      while (!is(TokenKind::Eof) && !canStartDecl(getCurrentToken().getKind())) {
        consumeToken();
      }
    }
  }
  
  return topLevelDecls;
}

std::unique_ptr<Decl> Parser::parseTopLevelDecl() {
  // Skip access modifiers and other declaration modifiers
  while (is(TokenKind::Public) || is(TokenKind::Private) || is(TokenKind::Internal) ||
         is(TokenKind::Fileprivate) || is(TokenKind::Open) || is(TokenKind::Static) ||
         is(TokenKind::Final) || is(TokenKind::Override) || is(TokenKind::Mutating) ||
         is(TokenKind::Nonmutating) || is(TokenKind::Lazy) || is(TokenKind::Weak) ||
         is(TokenKind::Unowned) || is(TokenKind::Required) || is(TokenKind::Convenience) ||
         is(TokenKind::Dynamic) || is(TokenKind::Optional)) {
    consumeToken();
  }
  
  switch (getCurrentToken().getKind()) {
  case TokenKind::Import:
    return parseImportDecl();
  case TokenKind::Let:
  case TokenKind::Var:
    return parseVarDecl();
  case TokenKind::Func:
    return parseFuncDecl();
  case TokenKind::Init:
    return parseFuncDecl(); // Treat init like a function for now
  case TokenKind::Deinit:
    return parseFuncDecl(); // Treat deinit like a function for now
  case TokenKind::Subscript:
    return parseFuncDecl(); // Treat subscript like a function for now
  case TokenKind::Class:
    return parseClassDecl();
  case TokenKind::Struct:
    return parseStructDecl();
  case TokenKind::Enum:
    return parseEnumDecl();
  case TokenKind::Protocol:
    return parseProtocolDecl();
  case TokenKind::Extension:
    return parseExtensionDecl();
  case TokenKind::Typealias:
    return parseImportDecl(); // Simplified typealias parsing
  case TokenKind::Operator:
  case TokenKind::Infix:
  case TokenKind::Prefix:
  case TokenKind::Postfix:
    return parseOperatorDecl();
  case TokenKind::Precedencegroup:
    return parsePrecedenceGroupDecl();
  case TokenKind::Associatedtype:
    return parseImportDecl(); // Simplified associatedtype parsing
  default:
    diagnoseUnexpectedToken("declaration");
    return nullptr;
  }
}

void Parser::consumeToken() {
  CurrentToken = Lex.lex();
}

bool Parser::consumeToken(TokenKind expectedKind) {
  if (is(expectedKind)) {
    consumeToken();
    return true;
  }
  
  std::string expected = getTokenKindName(expectedKind).str();
  diagnoseUnexpectedToken(expected);
  return false;
}

void Parser::diagnoseUnexpectedToken(StringRef expected) {
  std::string message = "unexpected token '" + getCurrentToken().getText().str() + "'";
  if (!expected.empty()) {
    message += ", expected " + expected.str();
  }
  Diags.diagnoseError(getCurrentToken().getLoc(), message);
}

std::unique_ptr<Decl> Parser::parseImportDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Import))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("module name");
    return nullptr;
  }
  
  StringRef moduleName = getCurrentToken().getText();
  SourceLoc endLoc = getCurrentToken().getLoc();
  consumeToken();
  
  return std::make_unique<ImportDecl>(SourceRange(startLoc, endLoc), moduleName);
}

std::unique_ptr<Decl> Parser::parseVarDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  bool isLet = is(TokenKind::Let);
  
  if (!consumeToken(isLet ? TokenKind::Let : TokenKind::Var))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("variable name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  std::unique_ptr<Type> type = nullptr;
  if (is(TokenKind::Colon)) {
    consumeToken();
    type = parseType();
  }
  
  std::unique_ptr<Expr> initializer = nullptr;
  if (is(TokenKind::Equal)) {
    consumeToken();
    initializer = parseExpr();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  
  return std::make_unique<VarDecl>(SourceRange(startLoc, endLoc), name,
                                   std::move(type), std::move(initializer), isLet);
}

std::unique_ptr<Decl> Parser::parseFuncDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Func))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("function name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  auto parameters = parseParameterList();
  
  std::unique_ptr<Type> returnType = nullptr;
  if (is(TokenKind::Arrow)) {
    consumeToken();
    returnType = parseType();
  }
  
  std::unique_ptr<Stmt> body = nullptr;
  if (is(TokenKind::LeftBrace)) {
    body = parseCompoundStmt();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  
  return std::make_unique<FuncDecl>(SourceRange(startLoc, endLoc), name,
                                    std::move(parameters), std::move(returnType),
                                    std::move(body));
}

std::unique_ptr<Decl> Parser::parseClassDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Class))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("class name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  if (!consumeToken(TokenKind::LeftBrace))
    return nullptr;
  
  std::vector<std::unique_ptr<Decl>> members;
  while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
    if (auto member = parseTopLevelDecl()) {
      members.push_back(std::move(member));
    } else {
      consumeToken(); // Skip invalid token
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBrace))
    return nullptr;
  
  return std::make_unique<ClassDecl>(SourceRange(startLoc, endLoc), name,
                                     std::move(members));
}

std::unique_ptr<Decl> Parser::parseStructDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Struct))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("struct name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  if (!consumeToken(TokenKind::LeftBrace))
    return nullptr;
  
  std::vector<std::unique_ptr<Decl>> members;
  while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
    if (auto member = parseTopLevelDecl()) {
      members.push_back(std::move(member));
    } else {
      consumeToken(); // Skip invalid token
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBrace))
    return nullptr;
  
  return std::make_unique<StructDecl>(SourceRange(startLoc, endLoc), name,
                                      std::move(members));
}

std::unique_ptr<Decl> Parser::parseEnumDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Enum))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("enum name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  // Parse enum body
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    
    std::vector<std::unique_ptr<Decl>> members;
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      // Handle case declarations
      if (is(TokenKind::Case)) {
        consumeToken();
        // Parse case name
        if (is(TokenKind::Identifier)) {
          StringRef caseName = getCurrentToken().getText();
          SourceLoc caseStart = getCurrentToken().getLoc();
          consumeToken();
          
          // Create a simplified case declaration (as a VarDecl)
          auto caseDecl = std::make_unique<VarDecl>(
            SourceRange(caseStart, getCurrentToken().getLoc()),
            caseName, nullptr, nullptr, true);
          members.push_back(std::move(caseDecl));
        }
      } else if (canStartDecl(getCurrentToken().getKind())) {
        if (auto member = parseTopLevelDecl()) {
          members.push_back(std::move(member));
        } else {
          consumeToken();
        }
      } else {
        consumeToken(); // Skip unknown tokens
      }
    }
    
    SourceLoc endLoc = getCurrentToken().getLoc();
    consumeToken(TokenKind::RightBrace);
    
    // Create a simplified enum declaration (as a ClassDecl for now)
    return std::make_unique<ClassDecl>(SourceRange(startLoc, endLoc), name, std::move(members));
  }
  
  return nullptr;
}

std::unique_ptr<Decl> Parser::parseProtocolDecl() {
  // Simplified protocol parsing for now
  consumeToken(); // consume 'protocol'
  if (is(TokenKind::Identifier)) consumeToken();
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      consumeToken();
    }
    consumeToken();
  }
  return nullptr; // TODO: Implement proper protocol declaration
}

std::unique_ptr<Decl> Parser::parseExtensionDecl() {
  // Simplified extension parsing for now
  consumeToken(); // consume 'extension'
  if (is(TokenKind::Identifier)) consumeToken();
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      consumeToken();
    }
    consumeToken();
  }
  return nullptr; // TODO: Implement proper extension declaration
}

std::unique_ptr<Decl> Parser::parseOperatorDecl() {
  // Simplified operator declaration parsing
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  // Consume operator declaration tokens
  if (is(TokenKind::Infix) || is(TokenKind::Prefix) || is(TokenKind::Postfix)) {
    consumeToken();
  }
  
  if (is(TokenKind::Operator)) {
    consumeToken();
  }
  
  // Skip operator name and precedence info
  while (!is(TokenKind::Eof) && !canStartDecl(getCurrentToken().getKind())) {
    consumeToken();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<ImportDecl>(SourceRange(startLoc, endLoc), "operator");
}

std::unique_ptr<Decl> Parser::parsePrecedenceGroupDecl() {
  // Simplified precedence group parsing
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  consumeToken(); // consume 'precedencegroup'
  
  if (is(TokenKind::Identifier)) {
    consumeToken(); // consume name
  }
  
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      consumeToken();
    }
    consumeToken();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<ImportDecl>(SourceRange(startLoc, endLoc), "precedencegroup");
}

std::unique_ptr<Stmt> Parser::parseStmt() {
  switch (getCurrentToken().getKind()) {
  case TokenKind::Return:
    return parseReturnStmt();
  case TokenKind::Break:
    return parseBreakStmt();
  case TokenKind::Continue:
    return parseContinueStmt();
  case TokenKind::If:
    return parseIfStmt();
  case TokenKind::While:
    return parseWhileStmt();
  case TokenKind::For:
    return parseForStmt();
  case TokenKind::Switch:
    return parseSwitchStmt();
  case TokenKind::Guard:
    return parseGuardStmt();
  case TokenKind::Do:
    return parseDoStmt();
  case TokenKind::LeftBrace:
    return parseCompoundStmt();
  default:
    return parseExprStmt();
  }
}

std::unique_ptr<Stmt> Parser::parseExprStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  auto expr = parseExpr();
  if (!expr)
    return nullptr;
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<ExprStmt>(SourceRange(startLoc, endLoc), std::move(expr));
}

std::unique_ptr<Stmt> Parser::parseReturnStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Return))
    return nullptr;
  
  std::unique_ptr<Expr> value = nullptr;
  if (canStartExpr(getCurrentToken().getKind())) {
    value = parseExpr();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<ReturnStmt>(SourceRange(startLoc, endLoc), std::move(value));
}

std::unique_ptr<Stmt> Parser::parseBreakStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  SourceLoc endLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Break))
    return nullptr;
  
  return std::make_unique<BreakStmt>(SourceRange(startLoc, endLoc));
}

std::unique_ptr<Stmt> Parser::parseContinueStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  SourceLoc endLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Continue))
    return nullptr;
  
  return std::make_unique<ContinueStmt>(SourceRange(startLoc, endLoc));
}

std::unique_ptr<Stmt> Parser::parseIfStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::If))
    return nullptr;
  
  auto condition = parseExpr();
  if (!condition)
    return nullptr;
  
  auto thenStmt = parseStmt();
  if (!thenStmt)
    return nullptr;
  
  std::unique_ptr<Stmt> elseStmt = nullptr;
  if (is(TokenKind::Else)) {
    consumeToken();
    elseStmt = parseStmt();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<IfStmt>(SourceRange(startLoc, endLoc),
                                  std::move(condition), std::move(thenStmt),
                                  std::move(elseStmt));
}

std::unique_ptr<Stmt> Parser::parseWhileStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::While))
    return nullptr;
  
  auto condition = parseExpr();
  if (!condition)
    return nullptr;
  
  auto body = parseStmt();
  if (!body)
    return nullptr;
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<WhileStmt>(SourceRange(startLoc, endLoc),
                                     std::move(condition), std::move(body));
}

std::unique_ptr<Stmt> Parser::parseCompoundStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::LeftBrace))
    return nullptr;
  
  std::vector<std::unique_ptr<Stmt>> statements;
  while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
    // Try to parse statements and declarations
    bool parsed = false;
    
    // Try declaration first
    if (canStartDecl(getCurrentToken().getKind())) {
      if (auto decl = parseTopLevelDecl()) {
        auto declStmt = std::make_unique<DeclStmt>(decl->getSourceRange(), std::move(decl));
        statements.push_back(std::move(declStmt));
        parsed = true;
      }
    }
    
    // Try statement if declaration failed
    if (!parsed && canStartStmt(getCurrentToken().getKind())) {
      if (auto stmt = parseStmt()) {
        statements.push_back(std::move(stmt));
        parsed = true;
      }
    }
    
    // If nothing worked, try to skip gracefully
    if (!parsed) {
      // For certain tokens, just skip them silently (like comments, attributes, etc.)
      if (is(TokenKind::Hash) || is(TokenKind::At) || is(TokenKind::Dollar) ||
          is(TokenKind::Backtick) || is(TokenKind::Backslash)) {
        consumeToken();
        parsed = true;
      } else {
        // For other unknown tokens, consume and continue
        consumeToken();
      }
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBrace))
    return nullptr;
  
  // Return a compound statement containing all statements
  return std::make_unique<CompoundStmt>(SourceRange(startLoc, endLoc), std::move(statements));
}

std::unique_ptr<Expr> Parser::parseExpr() {
  return parseBinaryExpr();
}

std::unique_ptr<Expr> Parser::parsePrimaryExpr() {
  switch (getCurrentToken().getKind()) {
  case TokenKind::IntegerLiteral: {
    SourceRange range = getCurrentToken().getRange();
    StringRef text = getCurrentToken().getText();
    
    // Parse integer value
    int64_t value = 0;
    try {
      std::istringstream iss(text.str());
      iss >> value;
    } catch (...) {
      value = 0; // Default value on parse error
    }
    
    consumeToken();
    return std::make_unique<IntegerLiteralExpr>(range, value);
  }
  
  case TokenKind::FloatingPointLiteral: {
    SourceRange range = getCurrentToken().getRange();
    StringRef text = getCurrentToken().getText();
    
    // Parse floating point value
    double value = 0.0;
    std::istringstream iss(text.str());
    iss >> value;
    
    consumeToken();
    return std::make_unique<FloatingPointLiteralExpr>(range, value);
  }
  
  case TokenKind::StringLiteral: {
    SourceRange range = getCurrentToken().getRange();
    StringRef text = getCurrentToken().getText();
    
    // Remove quotes from string literal
    StringRef value = text.substr(1, text.size() - 2);
    
    consumeToken();
    return std::make_unique<StringLiteralExpr>(range, value);
  }
  
  case TokenKind::True:
  case TokenKind::False: {
    SourceRange range = getCurrentToken().getRange();
    bool value = is(TokenKind::True);
    consumeToken();
    return std::make_unique<BooleanLiteralExpr>(range, value);
  }
  
  case TokenKind::Nil: {
    SourceRange range = getCurrentToken().getRange();
    consumeToken();
    return std::make_unique<NilLiteralExpr>(range);
  }
  
  case TokenKind::Identifier: {
    SourceRange range = getCurrentToken().getRange();
    StringRef name = getCurrentToken().getText();
    consumeToken();
    return std::make_unique<IdentifierExpr>(range, name);
  }
  
  case TokenKind::Self_: {
    SourceRange range = getCurrentToken().getRange();
    consumeToken();
    return std::make_unique<IdentifierExpr>(range, "self");
  }
  
  case TokenKind::Super: {
    SourceRange range = getCurrentToken().getRange();
    consumeToken();
    return std::make_unique<IdentifierExpr>(range, "super");
  }
  
  case TokenKind::LeftParen:
    return parseParenExpr();
  
  case TokenKind::LeftBracket:
    return parseArrayExpr();
  
  default:
    diagnoseUnexpectedToken("expression");
    return nullptr;
  }
}

std::unique_ptr<Expr> Parser::parsePostfixExpr() {
  auto expr = parsePrimaryExpr();
  if (!expr)
    return nullptr;
  
  while (true) {
    switch (getCurrentToken().getKind()) {
    case TokenKind::LeftParen:
      expr = parseCallExpr(std::move(expr));
      break;
    case TokenKind::Dot:
      expr = parseMemberRefExpr(std::move(expr));
      break;
    case TokenKind::LeftBracket:
      expr = parseSubscriptExpr(std::move(expr));
      break;
    default:
      return expr;
    }
    
    if (!expr)
      return nullptr;
  }
}

std::unique_ptr<Expr> Parser::parseBinaryExpr(int minPrec) {
  auto left = parsePostfixExpr();
  if (!left)
    return nullptr;
  
  while (true) {
    TokenKind opKind = getCurrentToken().getKind();
    int prec = getOperatorPrecedence(opKind);
    
    if (prec < minPrec)
      break;
    
    StringRef opName = getCurrentToken().getText();
    SourceLoc opLoc = getCurrentToken().getLoc();
    consumeToken();
    
    auto right = parseBinaryExpr(prec + 1);
    if (!right)
      return nullptr;
    
    SourceRange range(left->getStartLoc(), right->getEndLoc());
    left = std::make_unique<BinaryOperatorExpr>(range, std::move(left),
                                                opName, std::move(right));
  }
  
  return left;
}

std::unique_ptr<Expr> Parser::parseCallExpr(std::unique_ptr<Expr> base) {
  SourceLoc startLoc = base->getStartLoc();
  
  if (!consumeToken(TokenKind::LeftParen))
    return nullptr;
  
  auto arguments = parseArgumentList();
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightParen))
    return nullptr;
  
  return std::make_unique<CallExpr>(SourceRange(startLoc, endLoc),
                                    std::move(base), std::move(arguments));
}

std::unique_ptr<Expr> Parser::parseMemberRefExpr(std::unique_ptr<Expr> base) {
  SourceLoc startLoc = base->getStartLoc();
  
  if (!consumeToken(TokenKind::Dot))
    return nullptr;
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("member name");
    return nullptr;
  }
  
  StringRef memberName = getCurrentToken().getText();
  SourceLoc endLoc = getCurrentToken().getLoc();
  consumeToken();
  
  return std::make_unique<MemberRefExpr>(SourceRange(startLoc, endLoc),
                                         std::move(base), memberName);
}

std::unique_ptr<Expr> Parser::parseParenExpr() {
  if (!consumeToken(TokenKind::LeftParen))
    return nullptr;
  
  auto expr = parseExpr();
  if (!expr)
    return nullptr;
  
  if (!consumeToken(TokenKind::RightParen))
    return nullptr;
  
  return expr;
}

std::unique_ptr<Type> Parser::parseType() {
  return parseSimpleType();
}

std::unique_ptr<Type> Parser::parseSimpleType() {
  if (is(TokenKind::Identifier)) {
    SourceRange range = getCurrentToken().getRange();
    StringRef name = getCurrentToken().getText();
    consumeToken();
    
    auto type = std::make_unique<IdentifierType>(range, name);
    
    // Check for optional suffix
    if (is(TokenKind::Question)) {
      SourceLoc endLoc = getCurrentToken().getLoc();
      consumeToken();
      return std::make_unique<OptionalType>(SourceRange(range.getStart(), endLoc),
                                            std::move(type));
    }
    
    return type;
  }
  
  diagnoseUnexpectedToken("type");
  return nullptr;
}

std::vector<std::unique_ptr<ParamDecl>> Parser::parseParameterList() {
  std::vector<std::unique_ptr<ParamDecl>> parameters;
  
  if (!consumeToken(TokenKind::LeftParen))
    return parameters;
  
  if (!is(TokenKind::RightParen)) {
    do {
      auto param = parseParameter();
      if (param) {
        parameters.push_back(std::move(param));
      }
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  consumeToken(TokenKind::RightParen);
  return parameters;
}

std::unique_ptr<ParamDecl> Parser::parseParameter() {
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("parameter name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  if (!consumeToken(TokenKind::Colon))
    return nullptr;
  
  auto type = parseType();
  if (!type)
    return nullptr;
  
  return std::make_unique<ParamDecl>(name, name, std::move(type));
}

std::vector<std::unique_ptr<Expr>> Parser::parseArgumentList() {
  std::vector<std::unique_ptr<Expr>> arguments;
  
  if (!is(TokenKind::RightParen)) {
    do {
      auto arg = parseExpr();
      if (arg) {
        arguments.push_back(std::move(arg));
      }
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  return arguments;
}

int Parser::getOperatorPrecedence(TokenKind kind) {
  switch (kind) {
  case TokenKind::Star:
  case TokenKind::Slash:
  case TokenKind::Percent:
    return 150;
  case TokenKind::Plus:
  case TokenKind::Minus:
    return 140;
  case TokenKind::LessLess:
  case TokenKind::GreaterGreater:
    return 140;
  case TokenKind::Less:
  case TokenKind::LessEqual:
  case TokenKind::Greater:
  case TokenKind::GreaterEqual:
    return 130;
  case TokenKind::EqualEqual:
  case TokenKind::ExclaimEqual:
    return 130;
  case TokenKind::Amp:
    return 120;
  case TokenKind::Caret:
    return 120;
  case TokenKind::Pipe:
    return 120;
  case TokenKind::AmpAmp:
    return 120;
  case TokenKind::PipePipe:
    return 110;
  case TokenKind::QuestionQuestion:
    return 110;
  case TokenKind::DotDotDot:
  case TokenKind::DotDotLess:
    return 135; // Range operators
  case TokenKind::Equal:
  case TokenKind::PlusEqual:
  case TokenKind::MinusEqual:
  case TokenKind::StarEqual:
  case TokenKind::SlashEqual:
  case TokenKind::PercentEqual:
    return 90;
  default:
    return -1; // Not a binary operator
  }
}

bool Parser::canStartExpr(TokenKind kind) {
  switch (kind) {
  case TokenKind::Identifier:
  case TokenKind::IntegerLiteral:
  case TokenKind::FloatingPointLiteral:
  case TokenKind::StringLiteral:
  case TokenKind::True:
  case TokenKind::False:
  case TokenKind::Nil:
  case TokenKind::LeftParen:
  case TokenKind::LeftBracket:
  case TokenKind::Self_:
  case TokenKind::Super:
  case TokenKind::Try:
  case TokenKind::Exclaim:
  case TokenKind::Minus:
  case TokenKind::Plus:
    return true;
  default:
    return false;
  }
}

bool Parser::canStartStmt(TokenKind kind) {
  return canStartExpr(kind) || 
         kind == TokenKind::Return ||
         kind == TokenKind::Break ||
         kind == TokenKind::Continue ||
         kind == TokenKind::If ||
         kind == TokenKind::While ||
         kind == TokenKind::For ||
         kind == TokenKind::Switch ||
         kind == TokenKind::Guard ||
         kind == TokenKind::Do ||
         kind == TokenKind::LeftBrace ||
         canStartDecl(kind); // Allow declarations in statements
}

bool Parser::canStartDecl(TokenKind kind) {
  switch (kind) {
  case TokenKind::Import:
  case TokenKind::Let:
  case TokenKind::Var:
  case TokenKind::Func:
  case TokenKind::Init:
  case TokenKind::Deinit:
  case TokenKind::Class:
  case TokenKind::Struct:
  case TokenKind::Enum:
  case TokenKind::Protocol:
  case TokenKind::Extension:
  case TokenKind::Typealias:
  case TokenKind::Operator:
  case TokenKind::Infix:
  case TokenKind::Prefix:
  case TokenKind::Postfix:
  case TokenKind::Precedencegroup:
  case TokenKind::Associatedtype:
  case TokenKind::Subscript:
  // Access modifiers
  case TokenKind::Public:
  case TokenKind::Private:
  case TokenKind::Internal:
  case TokenKind::Fileprivate:
  case TokenKind::Open:
  case TokenKind::Static:
  case TokenKind::Final:
  case TokenKind::Override:
  case TokenKind::Mutating:
  case TokenKind::Nonmutating:
  case TokenKind::Lazy:
  case TokenKind::Weak:
  case TokenKind::Unowned:
  case TokenKind::Required:
  case TokenKind::Convenience:
  case TokenKind::Dynamic:
  case TokenKind::Optional:
    return true;
  default:
    return false;
  }
}

std::unique_ptr<Stmt> Parser::parseForStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::For))
    return nullptr;
  
  // Parse for-in loop: for variable in expression { body }
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("loop variable");
    return nullptr;
  }
  
  consumeToken(); // consume variable name
  
  if (!consumeToken(TokenKind::In))
    return nullptr;
  
  auto expr = parseExpr();
  if (!expr)
    return nullptr;
  
  auto body = parseStmt();
  if (!body)
    return nullptr;
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  // For now, return a simplified while statement
  return std::make_unique<WhileStmt>(SourceRange(startLoc, endLoc), std::move(expr), std::move(body));
}

std::unique_ptr<Stmt> Parser::parseSwitchStmt() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Switch))
    return nullptr;
  
  auto expr = parseExpr();
  if (!expr)
    return nullptr;
  
  if (!consumeToken(TokenKind::LeftBrace))
    return nullptr;
  
  // Parse case statements
  std::vector<std::unique_ptr<Stmt>> cases;
  while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
    if (is(TokenKind::Case)) {
      consumeToken();
      // Skip pattern for now
      while (!is(TokenKind::Colon) && !is(TokenKind::Eof)) {
        consumeToken();
      }
      if (is(TokenKind::Colon)) {
        consumeToken();
      }
      // Parse statements until next case or end
      while (!is(TokenKind::Case) && !is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
        if (auto stmt = parseStmt()) {
          cases.push_back(std::move(stmt));
        } else {
          consumeToken();
        }
      }
    } else {
      consumeToken(); // Skip unknown tokens
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  consumeToken(TokenKind::RightBrace);
  
  // For now, return a simplified expression statement
  return std::make_unique<ExprStmt>(SourceRange(startLoc, endLoc), std::move(expr));
}

std::unique_ptr<Stmt> Parser::parseGuardStmt() {
  // Simplified guard parsing
  consumeToken(); // consume 'guard'
  auto condition = parseExpr();
  if (is(TokenKind::Else)) {
    consumeToken();
    auto body = parseStmt();
    return std::make_unique<ExprStmt>(SourceRange(), std::move(condition));
  }
  return nullptr;
}

std::unique_ptr<Stmt> Parser::parseDoStmt() {
  // Simplified do parsing
  consumeToken(); // consume 'do'
  auto body = parseStmt();
  return body;
}

std::unique_ptr<Expr> Parser::parseUnaryExpr() {
  // Simplified unary expression parsing
  if (is(TokenKind::Exclaim) || is(TokenKind::Minus) || is(TokenKind::Plus)) {
    SourceLoc startLoc = getCurrentToken().getLoc();
    StringRef op = getCurrentToken().getText();
    consumeToken();
    
    auto operand = parsePostfixExpr();
    if (!operand)
      return nullptr;
    
    SourceRange range(startLoc, operand->getEndLoc());
    return std::make_unique<UnaryOperatorExpr>(range, op, std::move(operand), true);
  }
  
  return parsePostfixExpr();
}

std::unique_ptr<Expr> Parser::parseSubscriptExpr(std::unique_ptr<Expr> base) {
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  auto index = parseExpr();
  if (!index)
    return nullptr;
  
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  // For now, return the base expression
  return base;
}

std::unique_ptr<Expr> Parser::parseArrayExpr() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  std::vector<std::unique_ptr<Expr>> elements;
  if (!is(TokenKind::RightBracket)) {
    do {
      auto element = parseExpr();
      if (element) {
        elements.push_back(std::move(element));
      }
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  // For now, return a simple identifier
  return std::make_unique<IdentifierExpr>(SourceRange(startLoc, endLoc), "array");
}

std::unique_ptr<Expr> Parser::parseDictionaryExpr() {
  // Simplified dictionary parsing - treat like array for now
  return parseArrayExpr();
}

std::unique_ptr<Type> Parser::parseTupleType() {
  // Simplified tuple type parsing
  if (!consumeToken(TokenKind::LeftParen))
    return nullptr;
  
  std::vector<std::unique_ptr<Type>> elements;
  if (!is(TokenKind::RightParen)) {
    do {
      auto type = parseType();
      if (type) {
        elements.push_back(std::move(type));
      }
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  if (!consumeToken(TokenKind::RightParen))
    return nullptr;
  
  return std::make_unique<TupleType>(SourceRange(), std::move(elements));
}

std::unique_ptr<Type> Parser::parseFunctionType() {
  // Simplified function type parsing
  auto paramType = parseType();
  if (!paramType)
    return nullptr;
  
  if (!consumeToken(TokenKind::Arrow))
    return nullptr;
  
  auto returnType = parseType();
  if (!returnType)
    return nullptr;
  
  return std::make_unique<FunctionType>(SourceRange(), std::move(paramType), std::move(returnType));
}

std::unique_ptr<Type> Parser::parseArrayType() {
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  auto elementType = parseType();
  if (!elementType)
    return nullptr;
  
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  return std::make_unique<ArrayType>(SourceRange(), std::move(elementType));
}

std::unique_ptr<Type> Parser::parseDictionaryType() {
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  auto keyType = parseType();
  if (!keyType)
    return nullptr;
  
  if (!consumeToken(TokenKind::Colon))
    return nullptr;
  
  auto valueType = parseType();
  if (!valueType)
    return nullptr;
  
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  return std::make_unique<DictionaryType>(SourceRange(), std::move(keyType), std::move(valueType));
}

std::unique_ptr<Type> Parser::parseOptionalType(std::unique_ptr<Type> base) {
  if (!consumeToken(TokenKind::Question))
    return nullptr;
  
  return std::make_unique<OptionalType>(SourceRange(), std::move(base));
}