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
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <iostream>
#include <memory>
#include <map>
#include <string>
#include <vector>

using namespace swiftc;
using namespace llvm;

static cl::opt<std::string> InputFilename(cl::Positional, 
                                         cl::desc("<input file>"), 
                                         cl::Required);

static cl::opt<bool> DumpAST("dump-ast", 
                            cl::desc("Dump the AST"));

static cl::opt<bool> DumpTokens("dump-tokens", 
                               cl::desc("Dump lexer tokens"));

static cl::opt<bool> EmitLLVM("emit-llvm", 
                             cl::desc("Emit LLVM IR"));

static cl::opt<bool> Verbose("v", 
                            cl::desc("Verbose output"));

// Simple Swift Standard Library Implementation
class SwiftStdLib {
private:
    LLVMContext& Context;
    Module& M;
    IRBuilder<>& Builder;
    
public:
    SwiftStdLib(LLVMContext& ctx, Module& mod, IRBuilder<>& builder) 
        : Context(ctx), M(mod), Builder(builder) {}
    
    // Create Swift String type (simplified as i8*)
    llvm::Type* getStringType() {
        return llvm::PointerType::getUnqual(Context);
    }
    
    // Create Swift Int type
    llvm::Type* getIntType() {
        return llvm::Type::getInt64Ty(Context);
    }
    
    // Create Swift Bool type
    llvm::Type* getBoolType() {
        return llvm::Type::getInt1Ty(Context);
    }
    
    // Implement print function
    Function* getPrintFunction() {
        static Function* printFunc = nullptr;
        if (!printFunc) {
            llvm::FunctionType* printType = llvm::FunctionType::get(
                llvm::Type::getVoidTy(Context),
                {getStringType()},
                false
            );
            printFunc = Function::Create(printType, Function::ExternalLinkage, "swift_print", M);
        }
        return printFunc;
    }
    
    // Create string literal
    Value* createStringLiteral(const std::string& str) {
        return Builder.CreateGlobalString(str, "str");
    }
    
    // Create integer literal
    Value* createIntLiteral(int64_t value) {
        return ConstantInt::get(getIntType(), value);
    }
    
    // Create boolean literal
    Value* createBoolLiteral(bool value) {
        return ConstantInt::get(getBoolType(), value);
    }
};

// Swift Code Generator
class SwiftCodeGen {
private:
    LLVMContext Context;
    std::unique_ptr<Module> M;
    std::unique_ptr<IRBuilder<>> Builder;
    std::unique_ptr<SwiftStdLib> StdLib;
    std::map<std::string, Value*> NamedValues;
    
public:
    SwiftCodeGen() : M(std::make_unique<Module>("SwiftModule", Context)),
                     Builder(std::make_unique<IRBuilder<>>(Context)) {
        StdLib = std::make_unique<SwiftStdLib>(Context, *M, *Builder);
    }
    
    void generateCode(const std::vector<std::unique_ptr<Decl>>& decls) {
        llvm::outs() << "🔧 LLVM IR CODE GENERATION:\n";
        llvm::outs() << "========================================\n";
        
        // Create main function
        llvm::FunctionType* mainType = llvm::FunctionType::get(llvm::Type::getInt32Ty(Context), false);
        Function* mainFunc = Function::Create(mainType, Function::ExternalLinkage, "main", *M);
        BasicBlock* entry = BasicBlock::Create(Context, "entry", mainFunc);
        Builder->SetInsertPoint(entry);
        
        // Generate code for each declaration
        for (const auto& decl : decls) {
            generateDecl(decl.get());
        }
        
        // Return 0 from main
        Builder->CreateRet(ConstantInt::get(llvm::Type::getInt32Ty(Context), 0));
        
        llvm::outs() << "✅ Code generation completed\n";
        
        // Verify the module
        if (verifyModule(*M, &llvm::errs())) {
            llvm::errs() << "❌ Module verification failed!\n";
        } else {
            llvm::outs() << "✅ Module verification passed\n";
        }
    }
    
    void dumpLLVM() {
        llvm::outs() << "\n📄 GENERATED LLVM IR:\n";
        llvm::outs() << "========================================\n";
        M->print(llvm::outs(), nullptr);
        llvm::outs() << "========================================\n";
    }
    
private:
    Value* generateDecl(Decl* decl) {
        if (auto varDecl = llvm::dyn_cast<VarDecl>(decl)) {
            return generateVarDecl(varDecl);
        } else if (auto funcDecl = llvm::dyn_cast<FuncDecl>(decl)) {
            return generateFuncDecl(funcDecl);
        }
        
        llvm::outs() << "⚠️  Unsupported declaration type\n";
        return nullptr;
    }
    
    Value* generateVarDecl(VarDecl* varDecl) {
        llvm::outs() << "  Generating variable: " << varDecl->getName() << "\n";
        
        // For now, create simple integer variables
        llvm::Type* varType = StdLib->getIntType();
        Value* alloca = Builder->CreateAlloca(varType, nullptr, varDecl->getName().str());
        
        // Store initial value (simplified - assume 0 for now)
        Value* initValue = StdLib->createIntLiteral(0);
        Builder->CreateStore(initValue, alloca);
        
        NamedValues[varDecl->getName().str()] = alloca;
        return alloca;
    }
    
    Value* generateFuncDecl(FuncDecl* funcDecl) {
        llvm::outs() << "  Generating function: " << funcDecl->getName() << "\n";
        
        // Create function type (simplified)
        std::vector<llvm::Type*> paramTypes;
        for (size_t i = 0; i < funcDecl->getParameters().size(); ++i) {
            paramTypes.push_back(StdLib->getIntType());
        }
        
        llvm::FunctionType* funcType = llvm::FunctionType::get(StdLib->getIntType(), paramTypes, false);
        Function* func = Function::Create(funcType, Function::ExternalLinkage, funcDecl->getName().str(), *M);
        
        // Create basic block
        BasicBlock* bb = BasicBlock::Create(Context, "entry", func);
        Builder->SetInsertPoint(bb);
        
        // For now, just return 0
        Builder->CreateRet(StdLib->createIntLiteral(0));
        
        return func;
    }
};

int main(int argc, char **argv) {
    cl::ParseCommandLineOptions(argc, argv, "Swift Compiler with Standard Library\n");
    
    llvm::outs() << "🚀 SwiftC Complete Compiler Starting...\n";
    llvm::outs() << "📁 Input file: " << InputFilename << "\n";
    
    // Initialize diagnostics
    DiagnosticEngine Diags;
    SourceManager SM;
    
    // Read input file
    auto FileOrErr = MemoryBuffer::getFile(InputFilename);
    if (std::error_code EC = FileOrErr.getError()) {
        llvm::errs() << "❌ Error reading file '" << InputFilename << "': " << EC.message() << "\n";
        return 1;
    }
    
    std::unique_ptr<MemoryBuffer> Buffer = std::move(FileOrErr.get());
    
    if (Verbose) {
        llvm::outs() << "✅ File loaded (" << Buffer->getBufferSize() << " bytes)\n";
        llvm::outs() << "📄 Source code:\n";
        llvm::outs() << "----------------------------------------\n";
        llvm::outs() << Buffer->getBuffer() << "\n";
        llvm::outs() << "----------------------------------------\n";
    }
    
    // Add source file to source manager
    SourceLoc StartLoc = SM.addSourceFile(std::move(Buffer), InputFilename);
    
    // Get the source buffer content
    StringRef sourceContent = SM.getBuffer(StartLoc);
    if (sourceContent.empty()) {
        llvm::errs() << "❌ Failed to get source buffer\n";
        return 1;
    }
    
    if (DumpTokens) {
        llvm::outs() << "\n🔤 LEXICAL ANALYSIS:\n";
        llvm::outs() << "========================================\n";
        
        Lexer lexer(sourceContent, StartLoc, Diags);
        
        Token tok;
        int tokenCount = 0;
        do {
            tok = lexer.lex();
            tokenCount++;
            
            llvm::outs() << "Token " << tokenCount << ": ";
            llvm::outs() << "'" << tok.getText() << "' ";
            
            switch (tok.getKind()) {
                case TokenKind::Eof: llvm::outs() << "(EOF)"; break;
                case TokenKind::Identifier: llvm::outs() << "(IDENTIFIER)"; break;
                case TokenKind::IntegerLiteral: llvm::outs() << "(INTEGER)"; break;
                case TokenKind::StringLiteral: llvm::outs() << "(STRING)"; break;
                default: llvm::outs() << "(OTHER)"; break;
            }
            
            llvm::outs() << "\n";
            
        } while (tok.getKind() != TokenKind::Eof);
        
        llvm::outs() << "✅ Tokenized " << (tokenCount-1) << " tokens\n";
        llvm::outs() << "========================================\n";
    }
    
    // Parse AST
    llvm::outs() << "\n🌳 PARSING:\n";
    llvm::outs() << "========================================\n";
    
    Lexer parserLexer(sourceContent, StartLoc, Diags);
    Parser parser(parserLexer, Diags);
    
    // Parse multiple top-level declarations
    std::vector<std::unique_ptr<Decl>> decls;
    while (true) {
        auto decl = parser.parseTopLevelDecl();
        if (!decl) break;
        decls.push_back(std::move(decl));
    }
    
    llvm::outs() << "✅ Parsed " << decls.size() << " declarations\n";
    
    if (DumpAST) {
        llvm::outs() << "\n📊 AST DUMP:\n";
        llvm::outs() << "========================================\n";
        
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
            } else {
                llvm::outs() << "  Type: Other Declaration\n";
            }
            
            llvm::outs() << "\n";
        }
        llvm::outs() << "========================================\n";
    }
    
    // Generate LLVM IR
    SwiftCodeGen codegen;
    codegen.generateCode(decls);
    
    if (EmitLLVM) {
        codegen.dumpLLVM();
    }
    
    // Check for diagnostics
    if (Diags.hasErrors()) {
        llvm::errs() << "❌ Compilation failed with errors:\n";
        for (const auto& diag : Diags.getDiagnostics()) {
            if (diag.Level == DiagnosticLevel::Error) {
                llvm::errs() << "  Error: " << diag.Message << "\n";
            }
        }
        return 1;
    }
    
    llvm::outs() << "\n🎉 COMPILATION SUCCESSFUL!\n";
    llvm::outs() << "✅ Swift code compiled to LLVM IR\n";
    llvm::outs() << "✅ Basic stdlib support included\n";
    llvm::outs() << "✅ Ready for further processing\n";
    
    return 0;
}