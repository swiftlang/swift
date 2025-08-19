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
    return parseTypeAliasDecl();
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
  
  // Parse generic parameters <T, U, ...>
  std::unique_ptr<GenericParamList> genericParams = nullptr;
  if (is(TokenKind::Less)) {
    genericParams = parseGenericParameterList();
  }
  
  auto parameters = parseParameterList();
  
  // Check for throws/rethrows
  bool isThrows = false;
  bool isRethrows = false;
  if (is(TokenKind::Throws)) {
    isThrows = true;
    consumeToken();
  } else if (is(TokenKind::Rethrows)) {
    isRethrows = true;
    consumeToken();
  }
  
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
                                    std::move(body), std::move(genericParams),
                                    isThrows, isRethrows);
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
  
  // Parse raw value type (e.g., enum Status: Int)
  std::unique_ptr<Type> rawType = nullptr;
  if (is(TokenKind::Colon)) {
    consumeToken();
    rawType = parseType();
  }
  
  // Parse enum body
  std::vector<std::unique_ptr<EnumCaseDecl>> cases;
  std::vector<std::unique_ptr<Decl>> members;
  
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      // Handle case declarations
      if (is(TokenKind::Case)) {
        consumeToken();
        
        // Parse case name
        if (is(TokenKind::Identifier)) {
          StringRef caseName = getCurrentToken().getText();
          SourceLoc caseStart = getCurrentToken().getLoc();
          consumeToken();
          
          // Parse associated values (Int) or (String, Int)
          std::vector<std::unique_ptr<Type>> associatedTypes;
          if (is(TokenKind::LeftParen)) {
            consumeToken();
            if (!is(TokenKind::RightParen)) {
              do {
                if (auto type = parseType()) {
                  associatedTypes.push_back(std::move(type));
                }
              } while (is(TokenKind::Comma) && (consumeToken(), true));
            }
            consumeToken(TokenKind::RightParen);
          }
          
          // Parse raw value (= 0, = 1)
          std::unique_ptr<Expr> rawValue = nullptr;
          if (is(TokenKind::Equal)) {
            consumeToken();
            rawValue = parseExpr();
          }
          
          auto enumCase = std::make_unique<EnumCaseDecl>(
            SourceRange(caseStart, getCurrentToken().getLoc()),
            caseName, std::move(associatedTypes), std::move(rawValue));
          cases.push_back(std::move(enumCase));
        }
      } else if (canStartDecl(getCurrentToken().getKind())) {
        // Parse other members (methods, computed properties, etc.)
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
    
    return std::make_unique<EnumDecl>(SourceRange(startLoc, endLoc), name,
                                      std::move(cases), std::move(rawType), std::move(members));
  }
  
  return nullptr;
}

std::unique_ptr<Decl> Parser::parseProtocolDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Protocol)) {
    return nullptr;
  }
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("protocol name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  // Skip inheritance for now
  if (is(TokenKind::Colon)) {
    consumeToken();
    while (!is(TokenKind::LeftBrace) && !is(TokenKind::Eof)) {
      consumeToken();
    }
  }
  
  // Parse protocol body
  std::vector<ProtocolRequirement> requirements;
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      // Parse protocol requirements
      if (is(TokenKind::Associatedtype)) {
        // Parse associatedtype declaration
        consumeToken(); // consume 'associatedtype'
        
        if (is(TokenKind::Identifier)) {
          StringRef typeName = getCurrentToken().getText();
          SourceRange typeRange = getCurrentToken().getRange();
          consumeToken();
          
          // Create an associated type declaration
          auto assocTypeDecl = std::make_unique<TypeAliasDecl>(typeRange, typeName, nullptr, true);
          requirements.emplace_back(ProtocolRequirement::AssociatedType, std::move(assocTypeDecl));
        }
      } else if (is(TokenKind::Func)) {
        // Parse function requirement
        if (auto funcDecl = parseFuncDecl()) {
          requirements.emplace_back(ProtocolRequirement::Method, std::move(funcDecl));
        }
      } else if (is(TokenKind::Var) || is(TokenKind::Let)) {
        // Parse property requirement
        if (auto varDecl = parseVarDecl()) {
          requirements.emplace_back(ProtocolRequirement::Property, std::move(varDecl));
        }
      } else if (is(TokenKind::Init)) {
        // Parse initializer requirement
        if (auto initDecl = parseFuncDecl()) { // Treat init as function for now
          requirements.emplace_back(ProtocolRequirement::Initializer, std::move(initDecl));
        }
      } else {
        // Skip unknown tokens
        consumeToken();
      }
    }
    
    consumeToken(TokenKind::RightBrace);
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  
  // Create a protocol declaration with requirements
  std::vector<std::unique_ptr<Type>> inheritedProtocols;
  return std::make_unique<ProtocolDecl>(SourceRange(startLoc, endLoc), name, 
                                        std::move(inheritedProtocols), std::move(requirements));
}

std::unique_ptr<Decl> Parser::parseExtensionDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Extension)) {
    return nullptr;
  }
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("type name");
    return nullptr;
  }
  
  StringRef typeName = getCurrentToken().getText();
  consumeToken();
  
  // Skip where clause if present
  if (is(TokenKind::Where)) {
    consumeToken();
    while (!is(TokenKind::LeftBrace) && !is(TokenKind::Eof)) {
      consumeToken();
    }
  }
  
  // Parse extension body (simplified - just skip contents)
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    int braceCount = 1;
    while (braceCount > 0 && !is(TokenKind::Eof)) {
      if (is(TokenKind::LeftBrace)) {
        braceCount++;
      } else if (is(TokenKind::RightBrace)) {
        braceCount--;
      }
      consumeToken();
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  
  // Create a basic extension declaration (simplified)
  auto extendedType = std::make_unique<IdentifierType>(SourceRange(), typeName);
  std::vector<std::unique_ptr<Type>> conformedProtocols;
  std::vector<std::unique_ptr<Decl>> members;
  return std::make_unique<ExtensionDecl>(SourceRange(startLoc, endLoc), 
                                         std::move(extendedType), std::move(conformedProtocols), std::move(members));
}

std::unique_ptr<Decl> Parser::parseOperatorDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  // Parse operator kind
  OperatorDecl::OperatorKind kind = OperatorDecl::Infix;
  if (is(TokenKind::Infix)) {
    kind = OperatorDecl::Infix;
    consumeToken();
  } else if (is(TokenKind::Prefix)) {
    kind = OperatorDecl::Prefix;
    consumeToken();
  } else if (is(TokenKind::Postfix)) {
    kind = OperatorDecl::Postfix;
    consumeToken();
  }
  
  if (!consumeToken(TokenKind::Operator)) {
    return nullptr;
  }
  
  // Parse operator name (can be multiple characters like **+)
  std::string operatorName;
  while (!is(TokenKind::Eof) && !is(TokenKind::Colon) && !canStartDecl(getCurrentToken().getKind())) {
    operatorName += getCurrentToken().getText().str();
    consumeToken();
  }
  
  std::string precedenceGroup;
  if (is(TokenKind::Colon)) {
    consumeToken();
    if (is(TokenKind::Identifier)) {
      precedenceGroup = getCurrentToken().getText().str();
      consumeToken();
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<OperatorDecl>(SourceRange(startLoc, endLoc), operatorName, kind, precedenceGroup);
}

std::unique_ptr<Decl> Parser::parsePrecedenceGroupDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Precedencegroup)) {
    return nullptr;
  }
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("precedence group name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  std::string higherThan, lowerThan;
  
  if (is(TokenKind::LeftBrace)) {
    consumeToken();
    
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      if (is(TokenKind::Identifier)) {
        StringRef attr = getCurrentToken().getText();
        consumeToken();
        
        if (is(TokenKind::Colon)) {
          consumeToken();
          if (is(TokenKind::Identifier)) {
            if (attr == "higherThan") {
              higherThan = getCurrentToken().getText().str();
            } else if (attr == "lowerThan") {
              lowerThan = getCurrentToken().getText().str();
            }
            consumeToken();
          }
        }
      } else {
        consumeToken();
      }
    }
    
    if (!consumeToken(TokenKind::RightBrace)) {
      return nullptr;
    }
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<PrecedenceGroupDecl>(SourceRange(startLoc, endLoc), name, higherThan, lowerThan);
}

std::unique_ptr<Decl> Parser::parseTypeAliasDecl() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::Typealias)) {
    return nullptr;
  }
  
  if (!is(TokenKind::Identifier)) {
    diagnoseUnexpectedToken("type alias name");
    return nullptr;
  }
  
  StringRef name = getCurrentToken().getText();
  consumeToken();
  
  std::unique_ptr<Type> underlyingType = nullptr;
  if (is(TokenKind::Equal)) {
    consumeToken();
    underlyingType = parseType();
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  return std::make_unique<TypeAliasDecl>(SourceRange(startLoc, endLoc), name, std::move(underlyingType), false);
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
  
  case TokenKind::LeftBrace:
    return parseClosureExpr();
  
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
    
    // Handle range operators specially
    if (opKind == TokenKind::DotDotLess) {
      left = std::make_unique<RangeExpr>(range, std::move(left), std::move(right), false);
    } else if (opKind == TokenKind::DotDotDot) {
      left = std::make_unique<RangeExpr>(range, std::move(left), std::move(right), true);
    } else {
      left = std::make_unique<BinaryOperatorExpr>(range, std::move(left),
                                                  opName, std::move(right));
    }
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
    
    std::unique_ptr<Type> type = std::make_unique<IdentifierType>(range, name);
    
    // Check for generic type arguments <T, U, ...>
    if (is(TokenKind::Less)) {
      consumeToken(); // consume '<'
      
      std::vector<std::unique_ptr<Type>> typeArguments;
      
      if (!is(TokenKind::Greater)) {
        do {
          auto argType = parseType();
          if (argType) {
            typeArguments.push_back(std::move(argType));
          }
        } while (is(TokenKind::Comma) && (consumeToken(), true));
      }
      
      if (!consumeToken(TokenKind::Greater)) {
        diagnoseUnexpectedToken("'>' to close generic type arguments");
        return nullptr;
      }
      
      SourceLoc endLoc = getCurrentToken().getLoc();
      type = std::make_unique<BoundGenericType>(
        SourceRange(range.getStart(), endLoc),
        std::move(type),
        std::move(typeArguments)
      );
    }
    
    // Check for optional suffix
    if (is(TokenKind::Question)) {
      SourceLoc endLoc = getCurrentToken().getLoc();
      consumeToken();
      return std::make_unique<OptionalType>(SourceRange(range.getStart(), endLoc),
                                            std::move(type));
    }
    
    return type;
  }
  
  // Handle tuple types (Int, String)
  if (is(TokenKind::LeftParen)) {
    return parseTupleType();
  }
  
  // Handle array types [Int]
  if (is(TokenKind::LeftBracket)) {
    return parseArrayType();
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
  // Handle external label or parameter name
  StringRef externalName = "";
  StringRef internalName = "";
  
  if (is(TokenKind::Identifier)) {
    externalName = getCurrentToken().getText();
    consumeToken();
    
    // Check if there's an internal name (external internal:)
    if (is(TokenKind::Identifier)) {
      internalName = getCurrentToken().getText();
      consumeToken();
    } else {
      internalName = externalName;
    }
  } else {
    diagnoseUnexpectedToken("parameter name");
    return nullptr;
  }
  
  if (!consumeToken(TokenKind::Colon))
    return nullptr;
  
  // Handle inout parameters
  bool isInout = false;
  if (is(TokenKind::Inout)) {
    isInout = true;
    consumeToken();
  }
  
  auto type = parseType();
  if (!type)
    return nullptr;
  
  // Handle default values
  std::unique_ptr<Expr> defaultValue = nullptr;
  if (is(TokenKind::Equal)) {
    consumeToken();
    defaultValue = parseExpr();
  }
  
  return std::make_unique<ParamDecl>(externalName, internalName, std::move(type), std::move(defaultValue));
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

std::unique_ptr<GenericParamList> Parser::parseGenericParameterList() {
  if (!consumeToken(TokenKind::Less))
    return nullptr;
  
  std::vector<std::unique_ptr<Type>> genericParams;
  std::vector<GenericConstraint> constraints;
  
  // Parse generic parameter names
  if (!is(TokenKind::Greater)) {
    do {
      if (is(TokenKind::Identifier)) {
        StringRef paramName = getCurrentToken().getText();
        SourceRange paramRange = getCurrentToken().getRange();
        consumeToken();
        
        // Create a generic type parameter
        auto genericParam = std::make_unique<GenericTypeParam>(paramRange, paramName);
        genericParams.push_back(std::move(genericParam));
        
        // Check for type constraints (T: Protocol)
        if (is(TokenKind::Colon)) {
          consumeToken();
          if (auto constraintType = parseType()) {
            auto subjectType = std::make_unique<GenericTypeParam>(paramRange, paramName);
            constraints.emplace_back(std::move(subjectType), std::move(constraintType), 
                                   GenericConstraint::Conformance);
          }
        }
      }
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  if (!consumeToken(TokenKind::Greater))
    return nullptr;
  
  return std::make_unique<GenericParamList>(std::move(genericParams), std::move(constraints));
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
  
  StringRef varName = getCurrentToken().getText();
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
  return std::make_unique<ForStmt>(SourceRange(startLoc, endLoc), varName, std::move(expr), std::move(body));
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
  SourceLoc startLoc = base->getStartLoc();
  
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  std::vector<std::unique_ptr<Expr>> indices;
  
  if (!is(TokenKind::RightBracket)) {
    do {
      auto index = parseExpr();
      if (index) {
        indices.push_back(std::move(index));
      }
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  return std::make_unique<SubscriptExpr>(SourceRange(startLoc, endLoc), 
                                         std::move(base), std::move(indices));
}

std::unique_ptr<Expr> Parser::parseArrayExpr() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  // Check if this is a dictionary literal [key: value, ...]
  if (!is(TokenKind::RightBracket)) {
    // Look ahead to see if we have a colon after the first expression
    // We'll need to implement proper lookahead here
    // For now, assume it's an array literal and parse accordingly
  }
  
  // Parse as array literal
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
  
  return std::make_unique<ArrayLiteralExpr>(SourceRange(startLoc, endLoc), std::move(elements));
}

std::unique_ptr<Expr> Parser::parseDictionaryExpr() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::LeftBracket))
    return nullptr;
  
  std::vector<DictionaryLiteralExpr::KeyValuePair> elements;
  
  if (!is(TokenKind::RightBracket)) {
    do {
      // Parse key
      auto key = parseExpr();
      if (!key) break;
      
      // Expect colon
      if (!consumeToken(TokenKind::Colon)) {
        diagnoseUnexpectedToken("':' in dictionary literal");
        break;
      }
      
      // Parse value
      auto value = parseExpr();
      if (!value) break;
      
      elements.emplace_back(std::move(key), std::move(value));
      
    } while (is(TokenKind::Comma) && (consumeToken(), true));
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  return std::make_unique<DictionaryLiteralExpr>(SourceRange(startLoc, endLoc), std::move(elements));
}

std::unique_ptr<Expr> Parser::parseClosureExpr() {
  SourceLoc startLoc = getCurrentToken().getLoc();
  
  if (!consumeToken(TokenKind::LeftBrace))
    return nullptr;
  
  std::vector<std::string> parameters;
  bool hasExplicitParameters = false;
  
  // Check for explicit parameters (parameter list followed by 'in')
  if (is(TokenKind::Identifier)) {
    // Look ahead to see if we have 'in' keyword
    // We'll need to implement proper lookahead here
    // For now, assume no explicit parameters
    
    // Check if we have 'in' keyword
    if (is(TokenKind::In)) {
      consumeToken(); // consume 'in'
      hasExplicitParameters = true;
    } else {
      // Reset - this wasn't a parameter list
      parameters.clear();
    }
  }
  
  // Parse closure body (simplified - parse as single expression)
  auto body = parseExpr();
  if (!body) {
    // If no expression, create a simple identifier
    body = std::make_unique<IdentifierExpr>(SourceRange(), "closure_body");
  }
  
  SourceLoc endLoc = getCurrentToken().getLoc();
  if (!consumeToken(TokenKind::RightBrace))
    return nullptr;
  
  return std::make_unique<ClosureExpr>(SourceRange(startLoc, endLoc), 
                                       std::move(parameters), std::move(body), hasExplicitParameters);
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
  
  auto firstType = parseType();
  if (!firstType)
    return nullptr;
  
  // Check if this is a dictionary type [Key: Value]
  if (is(TokenKind::Colon)) {
    consumeToken(); // consume ':'
    
    auto valueType = parseType();
    if (!valueType)
      return nullptr;
    
    if (!consumeToken(TokenKind::RightBracket))
      return nullptr;
    
    return std::make_unique<DictionaryType>(SourceRange(), std::move(firstType), std::move(valueType));
  }
  
  // Otherwise it's an array type [Element]
  if (!consumeToken(TokenKind::RightBracket))
    return nullptr;
  
  return std::make_unique<ArrayType>(SourceRange(), std::move(firstType));
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