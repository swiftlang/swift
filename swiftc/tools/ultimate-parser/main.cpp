#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Lexer/Token.h"

#include <llvm/Support/MemoryBuffer.h>
#include <iostream>

using namespace swiftc;

class UltimateParser {
  Lexer& Lex;
  Token CurrentToken;
  int ConstructCount = 0;

public:
  UltimateParser(Lexer& lexer) : Lex(lexer) {
    CurrentToken = Lex.lex();
  }

  int parseSourceFile() {
    while (getCurrentToken().getKind() != TokenKind::Eof) {
      if (parseAnyConstruct()) {
        ConstructCount++;
      }
    }
    return ConstructCount;
  }

private:
  const Token& getCurrentToken() const { return CurrentToken; }
  
  void consumeToken() {
    CurrentToken = Lex.lex();
  }
  
  bool is(TokenKind kind) const {
    return getCurrentToken().getKind() == kind;
  }

  bool parseAnyConstruct() {
    TokenKind kind = getCurrentToken().getKind();
    
    // Count any major declaration or statement keyword as a construct
    if (kind == TokenKind::Import || kind == TokenKind::Let || kind == TokenKind::Var ||
        kind == TokenKind::Func || kind == TokenKind::Init || kind == TokenKind::Deinit ||
        kind == TokenKind::Class || kind == TokenKind::Struct || kind == TokenKind::Enum ||
        kind == TokenKind::Protocol || kind == TokenKind::Extension || 
        kind == TokenKind::Typealias || kind == TokenKind::Operator ||
        kind == TokenKind::Infix || kind == TokenKind::Prefix || kind == TokenKind::Postfix ||
        kind == TokenKind::Precedencegroup || kind == TokenKind::Associatedtype ||
        kind == TokenKind::Subscript) {
      
      consumeToken(); // consume the keyword
      
      // Skip to the end of this construct
      skipToEndOfConstruct();
      return true;
    }
    
    // Skip access modifiers and try again
    if (kind == TokenKind::Public || kind == TokenKind::Private || kind == TokenKind::Internal ||
        kind == TokenKind::Fileprivate || kind == TokenKind::Open || kind == TokenKind::Static ||
        kind == TokenKind::Final || kind == TokenKind::Override || kind == TokenKind::Mutating ||
        kind == TokenKind::Nonmutating || kind == TokenKind::Lazy || kind == TokenKind::Weak ||
        kind == TokenKind::Unowned || kind == TokenKind::Required || kind == TokenKind::Convenience ||
        kind == TokenKind::Dynamic || kind == TokenKind::Optional) {
      
      // Skip modifiers
      while (getCurrentToken().getKind() != TokenKind::Eof && isModifier(getCurrentToken().getKind())) {
        consumeToken();
      }
      
      // Try to parse the actual declaration
      return parseAnyConstruct();
    }
    
    // If we don't recognize it, just skip this token
    consumeToken();
    return false;
  }

  void skipToEndOfConstruct() {
    int braceDepth = 0;
    int parenDepth = 0;
    int bracketDepth = 0;
    
    while (getCurrentToken().getKind() != TokenKind::Eof) {
      TokenKind kind = getCurrentToken().getKind();
      
      // Track nesting depth
      if (kind == TokenKind::LeftBrace) braceDepth++;
      else if (kind == TokenKind::RightBrace) braceDepth--;
      else if (kind == TokenKind::LeftParen) parenDepth++;
      else if (kind == TokenKind::RightParen) parenDepth--;
      else if (kind == TokenKind::LeftBracket) bracketDepth++;
      else if (kind == TokenKind::RightBracket) bracketDepth--;
      
      consumeToken();
      
      // If we're back at top level and see a new declaration, stop
      if (braceDepth == 0 && parenDepth == 0 && bracketDepth == 0) {
        if (isDeclarationKeyword(kind)) {
          // Put the token back by not consuming it
          return;
        }
      }
      
      // Safety: if braces go negative, we've probably finished the construct
      if (braceDepth < 0) {
        return;
      }
    }
  }

  bool isModifier(TokenKind kind) {
    return kind == TokenKind::Public || kind == TokenKind::Private || kind == TokenKind::Internal ||
           kind == TokenKind::Fileprivate || kind == TokenKind::Open || kind == TokenKind::Static ||
           kind == TokenKind::Final || kind == TokenKind::Override || kind == TokenKind::Mutating ||
           kind == TokenKind::Nonmutating || kind == TokenKind::Lazy || kind == TokenKind::Weak ||
           kind == TokenKind::Unowned || kind == TokenKind::Required || kind == TokenKind::Convenience ||
           kind == TokenKind::Dynamic || kind == TokenKind::Optional;
  }

  bool isDeclarationKeyword(TokenKind kind) {
    return kind == TokenKind::Import || kind == TokenKind::Let || kind == TokenKind::Var ||
           kind == TokenKind::Func || kind == TokenKind::Init || kind == TokenKind::Deinit ||
           kind == TokenKind::Class || kind == TokenKind::Struct || kind == TokenKind::Enum ||
           kind == TokenKind::Protocol || kind == TokenKind::Extension || 
           kind == TokenKind::Typealias || kind == TokenKind::Operator ||
           kind == TokenKind::Infix || kind == TokenKind::Prefix || kind == TokenKind::Postfix ||
           kind == TokenKind::Precedencegroup || kind == TokenKind::Associatedtype ||
           kind == TokenKind::Subscript || isModifier(kind);
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
  
  // Ultimate parsing
  UltimateParser parser(lexer);
  
  std::cout << "Ultimate parsing: " << argv[1] << "\n";
  
  int constructCount = parser.parseSourceFile();
  
  std::cout << "Parsing completed!\n";
  std::cout << "Found " << constructCount << " top-level constructs.\n";
  
  return 0;
}