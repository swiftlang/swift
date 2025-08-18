#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Lexer/Token.h"

#include <llvm/Support/MemoryBuffer.h>
#include <iostream>

using namespace swiftc;

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
  
  std::cout << "Tokenizing: " << argv[1] << "\n";
  std::cout << "Source: " << sourceText.str() << "\n\n";
  
  // Lex all tokens
  Token token;
  do {
    token = lexer.lex();
    std::cout << "Token: " << getTokenKindName(token.getKind()).str() 
              << " [" << token.getText().str() << "]\n";
  } while (token.getKind() != TokenKind::Eof);
  
  if (diags.hasErrors()) {
    std::cerr << "\nLexing errors occurred:\n";
    for (const auto& diag : diags.getDiagnostics()) {
      std::cerr << "Error: " << diag.Message << "\n";
    }
    return 1;
  }
  
  std::cout << "\nLexing completed successfully!\n";
  return 0;
}