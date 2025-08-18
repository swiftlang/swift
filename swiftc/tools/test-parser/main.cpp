#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Parser/Parser.h"

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
  
  // Parsing
  Parser parser(lexer, diags);
  
  std::cout << "Parsing: " << argv[1] << "\n";
  std::cout << "Source: " << sourceText.str() << "\n\n";
  
  auto topLevelDecls = parser.parseSourceFile();
  
  if (diags.hasErrors()) {
    std::cerr << "Parse errors occurred:\n";
    for (const auto& diag : diags.getDiagnostics()) {
      auto [line, col] = sourceMgr.getLineAndColumn(diag.Location);
      std::cerr << sourceMgr.getFilename(diag.Location).str() << ":" << line << ":" << col << ": ";
      
      switch (diag.Level) {
      case DiagnosticLevel::Error:
        std::cerr << "error: ";
        break;
      case DiagnosticLevel::Warning:
        std::cerr << "warning: ";
        break;
      case DiagnosticLevel::Note:
        std::cerr << "note: ";
        break;
      }
      
      std::cerr << diag.Message << "\n";
    }
    return 1;
  }
  
  std::cout << "Parsing completed successfully!\n";
  std::cout << "Found " << topLevelDecls.size() << " top-level declarations.\n";
  
  return 0;
}