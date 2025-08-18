#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Lexer/Token.h"

#include <llvm/Support/MemoryBuffer.h>
#include <iostream>

using namespace swiftc;

class RobustParser {
  Lexer& Lex;
  DiagnosticEngine& Diags;
  Token CurrentToken;
  int DeclarationCount = 0;

public:
  RobustParser(Lexer& lexer, DiagnosticEngine& diags) : Lex(lexer), Diags(diags) {
    CurrentToken = Lex.lex();
  }

  int parseSourceFile() {
    while (getCurrentToken().getKind() != TokenKind::Eof) {
      if (parseTopLevelConstruct()) {
        DeclarationCount++;
      }
    }
    return DeclarationCount;
  }

private:
  const Token& getCurrentToken() const { return CurrentToken; }
  
  void consumeToken() {
    CurrentToken = Lex.lex();
  }
  
  bool is(TokenKind kind) const {
    return getCurrentToken().getKind() == kind;
  }

  bool parseTopLevelConstruct() {
    // Skip any special tokens
    while (is(TokenKind::Hash) || is(TokenKind::At) || is(TokenKind::Dollar) ||
           is(TokenKind::Backtick) || is(TokenKind::Backslash) || 
           is(TokenKind::LeftBraceBrace) || is(TokenKind::RightBraceBrace)) {
      consumeToken();
      if (is(TokenKind::Eof)) return false;
    }
    
    // Check for declaration keywords
    TokenKind kind = getCurrentToken().getKind();
    
    if (kind == TokenKind::Import || kind == TokenKind::Let || kind == TokenKind::Var ||
        kind == TokenKind::Func || kind == TokenKind::Init || kind == TokenKind::Deinit ||
        kind == TokenKind::Class || kind == TokenKind::Struct || kind == TokenKind::Enum ||
        kind == TokenKind::Protocol || kind == TokenKind::Extension || 
        kind == TokenKind::Typealias || kind == TokenKind::Operator ||
        kind == TokenKind::Infix || kind == TokenKind::Prefix || kind == TokenKind::Postfix ||
        kind == TokenKind::Precedencegroup || kind == TokenKind::Associatedtype ||
        kind == TokenKind::Subscript) {
      
      return parseDeclaration();
    }
    
    // Check for access modifiers
    if (kind == TokenKind::Public || kind == TokenKind::Private || kind == TokenKind::Internal ||
        kind == TokenKind::Fileprivate || kind == TokenKind::Open || kind == TokenKind::Static ||
        kind == TokenKind::Final || kind == TokenKind::Override || kind == TokenKind::Mutating ||
        kind == TokenKind::Nonmutating || kind == TokenKind::Lazy || kind == TokenKind::Weak ||
        kind == TokenKind::Unowned || kind == TokenKind::Required || kind == TokenKind::Convenience ||
        kind == TokenKind::Dynamic || kind == TokenKind::Optional) {
      
      // Skip modifiers and try to parse the declaration
      while (getCurrentToken().getKind() != TokenKind::Eof && 
             (is(TokenKind::Public) || is(TokenKind::Private) || is(TokenKind::Internal) ||
              is(TokenKind::Fileprivate) || is(TokenKind::Open) || is(TokenKind::Static) ||
              is(TokenKind::Final) || is(TokenKind::Override) || is(TokenKind::Mutating) ||
              is(TokenKind::Nonmutating) || is(TokenKind::Lazy) || is(TokenKind::Weak) ||
              is(TokenKind::Unowned) || is(TokenKind::Required) || is(TokenKind::Convenience) ||
              is(TokenKind::Dynamic) || is(TokenKind::Optional))) {
        consumeToken();
      }
      
      return parseDeclaration();
    }
    
    // If we don't recognize it, skip to the next potential declaration
    consumeToken();
    return false;
  }

  bool parseDeclaration() {
    TokenKind kind = getCurrentToken().getKind();
    
    if (kind == TokenKind::Import) {
      return parseImportDecl();
    } else if (kind == TokenKind::Let || kind == TokenKind::Var) {
      return parseVarDecl();
    } else if (kind == TokenKind::Func || kind == TokenKind::Init || 
               kind == TokenKind::Deinit || kind == TokenKind::Subscript) {
      return parseFuncDecl();
    } else if (kind == TokenKind::Class || kind == TokenKind::Struct || 
               kind == TokenKind::Enum || kind == TokenKind::Protocol) {
      return parseTypeDecl();
    } else if (kind == TokenKind::Extension) {
      return parseExtensionDecl();
    } else if (kind == TokenKind::Typealias || kind == TokenKind::Associatedtype) {
      return parseTypealiasDecl();
    } else if (kind == TokenKind::Operator || kind == TokenKind::Infix ||
               kind == TokenKind::Prefix || kind == TokenKind::Postfix) {
      return parseOperatorDecl();
    } else if (kind == TokenKind::Precedencegroup) {
      return parsePrecedenceGroupDecl();
    }
    
    return false;
  }

  bool parseImportDecl() {
    consumeToken(); // consume 'import'
    if (is(TokenKind::Identifier)) {
      consumeToken(); // consume module name
    }
    return true;
  }

  bool parseVarDecl() {
    consumeToken(); // consume 'let' or 'var'
    if (is(TokenKind::Identifier)) {
      consumeToken(); // consume name
    }
    
    // Optional type annotation
    if (is(TokenKind::Colon)) {
      consumeToken();
      parseType();
    }
    
    // Optional initializer
    if (is(TokenKind::Equal)) {
      consumeToken();
      parseExpression();
    }
    
    return true;
  }

  bool parseFuncDecl() {
    consumeToken(); // consume 'func', 'init', 'deinit', or 'subscript'
    
    if (is(TokenKind::Identifier)) {
      consumeToken(); // consume name
    }
    
    // Parse parameter list
    if (is(TokenKind::LeftParen)) {
      parseParameterList();
    }
    
    // Parse return type
    if (is(TokenKind::Arrow)) {
      consumeToken();
      parseType();
    }
    
    // Parse body
    if (is(TokenKind::LeftBrace)) {
      parseBlock();
    }
    
    return true;
  }

  bool parseTypeDecl() {
    consumeToken(); // consume type keyword
    
    if (is(TokenKind::Identifier)) {
      consumeToken(); // consume name
    }
    
    // Parse generic parameters
    if (is(TokenKind::Less)) {
      parseGenericParameters();
    }
    
    // Parse inheritance
    if (is(TokenKind::Colon)) {
      consumeToken();
      parseType();
    }
    
    // Parse body
    if (is(TokenKind::LeftBrace)) {
      parseTypeBody();
    }
    
    return true;
  }

  bool parseExtensionDecl() {
    consumeToken(); // consume 'extension'
    if (is(TokenKind::Identifier)) {
      consumeToken();
    }
    if (is(TokenKind::LeftBrace)) {
      parseTypeBody();
    }
    return true;
  }

  bool parseTypealiasDecl() {
    consumeToken(); // consume 'typealias' or 'associatedtype'
    if (is(TokenKind::Identifier)) {
      consumeToken();
    }
    if (is(TokenKind::Equal)) {
      consumeToken();
      parseType();
    }
    return true;
  }

  bool parseOperatorDecl() {
    // Skip operator declaration completely
    while (!is(TokenKind::Eof) && !isStartOfNextDeclaration()) {
      consumeToken();
    }
    return true;
  }

  bool parsePrecedenceGroupDecl() {
    consumeToken(); // consume 'precedencegroup'
    if (is(TokenKind::Identifier)) {
      consumeToken();
    }
    if (is(TokenKind::LeftBrace)) {
      parseBlock();
    }
    return true;
  }

  void parseParameterList() {
    if (!is(TokenKind::LeftParen)) return;
    
    consumeToken(); // consume '('
    
    while (!is(TokenKind::RightParen) && !is(TokenKind::Eof)) {
      // Skip parameter content
      if (is(TokenKind::LeftParen)) {
        parseParameterList(); // Nested parentheses
      } else {
        consumeToken();
      }
    }
    
    if (is(TokenKind::RightParen)) {
      consumeToken();
    }
  }

  void parseGenericParameters() {
    if (!is(TokenKind::Less)) return;
    
    consumeToken(); // consume '<'
    int angleCount = 1;
    
    while (!is(TokenKind::Eof) && angleCount > 0) {
      TokenKind kind = getCurrentToken().getKind();
      consumeToken();
      
      if (kind == TokenKind::Less) angleCount++;
      else if (kind == TokenKind::Greater) angleCount--;
    }
  }

  void parseType() {
    if (is(TokenKind::Identifier)) {
      consumeToken();
    } else if (is(TokenKind::LeftParen)) {
      parseParameterList(); // Tuple type
    } else if (is(TokenKind::LeftBracket)) {
      consumeToken();
      parseType(); // Array element type
      if (is(TokenKind::Colon)) {
        consumeToken();
        parseType(); // Dictionary value type
      }
      if (is(TokenKind::RightBracket)) {
        consumeToken();
      }
    }
    
    // Handle optional types
    while (is(TokenKind::Question) || is(TokenKind::Exclaim)) {
      consumeToken();
    }
  }

  void parseExpression() {
    // Very simplified expression parsing
    while (!is(TokenKind::Eof) && !isEndOfExpression()) {
      if (is(TokenKind::LeftParen)) {
        parseParameterList();
      } else if (is(TokenKind::LeftBrace)) {
        parseBlock();
      } else if (is(TokenKind::LeftBracket)) {
        consumeToken();
        parseExpression();
        if (is(TokenKind::RightBracket)) consumeToken();
      } else {
        consumeToken();
      }
    }
  }

  void parseBlock() {
    if (!is(TokenKind::LeftBrace)) return;
    
    consumeToken(); // consume '{'
    
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      if (isStartOfNextDeclaration()) {
        parseTopLevelConstruct();
      } else {
        consumeToken();
      }
    }
    
    if (is(TokenKind::RightBrace)) {
      consumeToken();
    }
  }

  void parseTypeBody() {
    if (!is(TokenKind::LeftBrace)) return;
    
    consumeToken(); // consume '{'
    
    while (!is(TokenKind::RightBrace) && !is(TokenKind::Eof)) {
      if (parseTopLevelConstruct()) {
        // Successfully parsed something
      } else {
        consumeToken(); // Skip unknown token
      }
    }
    
    if (is(TokenKind::RightBrace)) {
      consumeToken();
    }
  }

  bool isStartOfNextDeclaration() {
    TokenKind kind = getCurrentToken().getKind();
    return kind == TokenKind::Import || kind == TokenKind::Let || kind == TokenKind::Var ||
           kind == TokenKind::Func || kind == TokenKind::Init || kind == TokenKind::Deinit ||
           kind == TokenKind::Class || kind == TokenKind::Struct || kind == TokenKind::Enum ||
           kind == TokenKind::Protocol || kind == TokenKind::Extension || 
           kind == TokenKind::Typealias || kind == TokenKind::Operator ||
           kind == TokenKind::Infix || kind == TokenKind::Prefix || kind == TokenKind::Postfix ||
           kind == TokenKind::Precedencegroup || kind == TokenKind::Associatedtype ||
           kind == TokenKind::Subscript || kind == TokenKind::Public || kind == TokenKind::Private;
  }

  bool isEndOfExpression() {
    TokenKind kind = getCurrentToken().getKind();
    return kind == TokenKind::RightBrace || kind == TokenKind::RightParen || 
           kind == TokenKind::RightBracket || kind == TokenKind::Semicolon ||
           kind == TokenKind::Comma || isStartOfNextDeclaration();
  }
};

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <file>\n";
    return 1;
  }

  // Create diagnostic engine
  DiagnosticEngine diags;
  
  // Create source manager
  SourceManager sourceMgr;
  
  // Read input file
  auto fileOrError = llvm::MemoryBuffer::getFile(argv[1]);
  if (std::error_code ec = fileOrError.getError()) {
    std::cerr << "Error reading file: " << ec.message() << "\n";
    return 1;
  }
  
  auto buffer = std::move(fileOrError.get());
  SourceLoc startLoc = sourceMgr.addSourceFile(std::move(buffer), argv[1]);
  
  // Lexical analysis
  StringRef sourceText = sourceMgr.getBuffer(startLoc);
  Lexer lexer(sourceText, startLoc, diags);
  
  // Robust parsing
  RobustParser parser(lexer, diags);
  
  std::cout << "Robust parsing: " << argv[1] << "\n";
  
  int declCount = parser.parseSourceFile();
  
  std::cout << "Parsing completed!\n";
  std::cout << "Found " << declCount << " top-level constructs.\n";
  
  if (diags.hasErrors()) {
    std::cout << "Note: " << diags.getDiagnostics().size() << " diagnostics generated during parsing.\n";
  }
  
  return 0;
}