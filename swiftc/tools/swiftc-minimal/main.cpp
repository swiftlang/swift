#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Parser/Parser.h"
#include "swiftc/AST/ASTNode.h"
#include "swiftc/AST/Decl.h"

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>

#include <iostream>
#include <memory>

using namespace swiftc;
using namespace llvm;

static cl::opt<std::string> InputFilename(cl::Positional, 
                                         cl::desc("<input file>"), 
                                         cl::Required);

static cl::opt<bool> DumpAST("dump-ast", 
                            cl::desc("Dump the AST"));

static cl::opt<bool> DumpTokens("dump-tokens", 
                               cl::desc("Dump lexer tokens"));

static cl::opt<bool> Verbose("v", 
                            cl::desc("Verbose output"));

int main(int argc, char **argv) {
    cl::ParseCommandLineOptions(argc, argv, "Swift Compiler (Minimal)\n");
    
    // Initialize diagnostics
    DiagnosticEngine Diags;
    SourceManager SM;
    
    if (Verbose) {
        llvm::outs() << "🚀 SwiftC Minimal Compiler Starting...\n";
        llvm::outs() << "📁 Input file: " << InputFilename << "\n";
    }
    
    // Read input file
    auto FileOrErr = MemoryBuffer::getFile(InputFilename);
    if (std::error_code EC = FileOrErr.getError()) {
        llvm::errs() << "❌ Error reading file '" << InputFilename << "': " << EC.message() << "\n";
        return 1;
    }
    
    std::unique_ptr<MemoryBuffer> Buffer = std::move(FileOrErr.get());
    
    // Add source file to source manager
    SourceLoc StartLoc = SM.addSourceFile(std::move(Buffer), InputFilename);
    
    if (Verbose) {
        llvm::outs() << "✅ File loaded successfully\n";
        llvm::outs() << "🔍 Starting lexical analysis...\n";
    }
    
    // Create lexer
    auto FileBuffer = SM.getSourceFile(StartLoc);
    if (!FileBuffer) {
        llvm::errs() << "❌ Failed to get source buffer\n";
        return 1;
    }
    
    Lexer lexer(FileBuffer->getBuffer(), StartLoc, Diags);
    
    if (DumpTokens) {
        llvm::outs() << "🔤 TOKENS:\n";
        llvm::outs() << "========================================\n";
        
        Token tok;
        int tokenCount = 0;
        do {
            tok = lexer.lex();
            tokenCount++;
            
            llvm::outs() << "Token " << tokenCount << ": ";
            
            switch (tok.getKind()) {
                case TokenKind::Eof:
                    llvm::outs() << "EOF";
                    break;
                case TokenKind::Identifier:
                    llvm::outs() << "IDENTIFIER('" << tok.getText() << "')";
                    break;
                case TokenKind::KeywordLet:
                    llvm::outs() << "KEYWORD_LET";
                    break;
                case TokenKind::KeywordVar:
                    llvm::outs() << "KEYWORD_VAR";
                    break;
                case TokenKind::KeywordFunc:
                    llvm::outs() << "KEYWORD_FUNC";
                    break;
                case TokenKind::KeywordClass:
                    llvm::outs() << "KEYWORD_CLASS";
                    break;
                case TokenKind::KeywordStruct:
                    llvm::outs() << "KEYWORD_STRUCT";
                    break;
                case TokenKind::KeywordEnum:
                    llvm::outs() << "KEYWORD_ENUM";
                    break;
                case TokenKind::KeywordProtocol:
                    llvm::outs() << "KEYWORD_PROTOCOL";
                    break;
                case TokenKind::KeywordIf:
                    llvm::outs() << "KEYWORD_IF";
                    break;
                case TokenKind::KeywordElse:
                    llvm::outs() << "KEYWORD_ELSE";
                    break;
                case TokenKind::KeywordFor:
                    llvm::outs() << "KEYWORD_FOR";
                    break;
                case TokenKind::KeywordWhile:
                    llvm::outs() << "KEYWORD_WHILE";
                    break;
                case TokenKind::KeywordReturn:
                    llvm::outs() << "KEYWORD_RETURN";
                    break;
                case TokenKind::IntegerLiteral:
                    llvm::outs() << "INTEGER_LITERAL(" << tok.getText() << ")";
                    break;
                case TokenKind::FloatingLiteral:
                    llvm::outs() << "FLOAT_LITERAL(" << tok.getText() << ")";
                    break;
                case TokenKind::StringLiteral:
                    llvm::outs() << "STRING_LITERAL(" << tok.getText() << ")";
                    break;
                case TokenKind::LeftParen:
                    llvm::outs() << "LEFT_PAREN";
                    break;
                case TokenKind::RightParen:
                    llvm::outs() << "RIGHT_PAREN";
                    break;
                case TokenKind::LeftBrace:
                    llvm::outs() << "LEFT_BRACE";
                    break;
                case TokenKind::RightBrace:
                    llvm::outs() << "RIGHT_BRACE";
                    break;
                case TokenKind::Equal:
                    llvm::outs() << "EQUAL";
                    break;
                case TokenKind::Plus:
                    llvm::outs() << "PLUS";
                    break;
                case TokenKind::Minus:
                    llvm::outs() << "MINUS";
                    break;
                case TokenKind::Star:
                    llvm::outs() << "STAR";
                    break;
                case TokenKind::Slash:
                    llvm::outs() << "SLASH";
                    break;
                case TokenKind::Semicolon:
                    llvm::outs() << "SEMICOLON";
                    break;
                case TokenKind::Colon:
                    llvm::outs() << "COLON";
                    break;
                case TokenKind::Comma:
                    llvm::outs() << "COMMA";
                    break;
                case TokenKind::Dot:
                    llvm::outs() << "DOT";
                    break;
                default:
                    llvm::outs() << "UNKNOWN(" << tok.getText() << ")";
                    break;
            }
            
            llvm::outs() << "\n";
            
        } while (tok.getKind() != TokenKind::Eof);
        
        llvm::outs() << "========================================\n";
        llvm::outs() << "✅ Lexical analysis completed: " << (tokenCount-1) << " tokens\n";
    }
    
    if (DumpAST) {
        llvm::outs() << "🌳 ABSTRACT SYNTAX TREE:\n";
        llvm::outs() << "========================================\n";
        
        // Reset lexer for parsing
        Lexer parserLexer(FileBuffer->getBuffer(), StartLoc, Diags);
        Parser parser(parserLexer, Diags);
        
        if (Verbose) {
            llvm::outs() << "🔍 Starting syntactic analysis...\n";
        }
        
        try {
            auto decls = parser.parseTopLevelDecls();
            
            llvm::outs() << "📊 Parsed " << decls.size() << " top-level declarations:\n\n";
            
            for (size_t i = 0; i < decls.size(); ++i) {
                auto& decl = decls[i];
                llvm::outs() << "Declaration " << (i+1) << ":\n";
                
                if (auto varDecl = llvm::dyn_cast<VarDecl>(decl.get())) {
                    llvm::outs() << "  Type: Variable Declaration\n";
                    llvm::outs() << "  Name: " << varDecl->getName() << "\n";
                    llvm::outs() << "  Is Let: " << (varDecl->isLet() ? "true" : "false") << "\n";
                } else if (auto funcDecl = llvm::dyn_cast<FuncDecl>(decl.get())) {
                    llvm::outs() << "  Type: Function Declaration\n";
                    llvm::outs() << "  Name: " << funcDecl->getName() << "\n";
                    llvm::outs() << "  Parameters: " << funcDecl->getParameters().size() << "\n";
                } else if (auto classDecl = llvm::dyn_cast<ClassDecl>(decl.get())) {
                    llvm::outs() << "  Type: Class Declaration\n";
                    llvm::outs() << "  Name: " << classDecl->getName() << "\n";
                } else if (auto structDecl = llvm::dyn_cast<StructDecl>(decl.get())) {
                    llvm::outs() << "  Type: Struct Declaration\n";
                    llvm::outs() << "  Name: " << structDecl->getName() << "\n";
                } else {
                    llvm::outs() << "  Type: Other Declaration\n";
                }
                
                llvm::outs() << "\n";
            }
            
        } catch (const std::exception& e) {
            llvm::errs() << "❌ Parse error: " << e.what() << "\n";
        }
        
        llvm::outs() << "========================================\n";
        llvm::outs() << "✅ Syntactic analysis completed\n";
    }
    
    // Check for diagnostics
    if (Diags.hasErrors()) {
        llvm::errs() << "❌ Compilation failed with " << Diags.getDiagnostics().size() << " diagnostic(s):\n";
        for (const auto& diag : Diags.getDiagnostics()) {
            llvm::errs() << "  ";
            switch (diag.Level) {
                case DiagnosticLevel::Note:
                    llvm::errs() << "📝 Note: ";
                    break;
                case DiagnosticLevel::Warning:
                    llvm::errs() << "⚠️  Warning: ";
                    break;
                case DiagnosticLevel::Error:
                    llvm::errs() << "❌ Error: ";
                    break;
            }
            llvm::errs() << diag.Message << "\n";
        }
        return 1;
    }
    
    if (Verbose) {
        llvm::outs() << "✅ Compilation completed successfully!\n";
        llvm::outs() << "🎉 SwiftC Minimal Compiler finished\n";
    }
    
    return 0;
}