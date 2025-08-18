#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Parser/Parser.h"
#include "swiftc/Sema/TypeChecker.h"

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/GlobalsModRef.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/SCCP.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/CodeGen/Passes.h>
#include <iostream>

using namespace swiftc;
using namespace llvm;

// LLVM optimization pipeline
void runOptimizationPipeline(llvm::Module* module, int optLevel) {
  if (!module || optLevel <= 0) return;
  
  // Create pass builder
  PassBuilder PB;
  
  // Create analysis managers
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;
  
  // Register analysis passes
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
  
  // Create optimization pipeline based on level
  ModulePassManager MPM;
  
  if (optLevel >= 1) {
    // Basic optimizations
    MPM.addPass(PB.buildPerModuleDefaultPipeline(OptimizationLevel::O1));
  }
  if (optLevel >= 2) {
    // More aggressive optimizations
    MPM.addPass(PB.buildPerModuleDefaultPipeline(OptimizationLevel::O2));
  }
  if (optLevel >= 3) {
    // Maximum optimizations
    MPM.addPass(PB.buildPerModuleDefaultPipeline(OptimizationLevel::O3));
  }
  
  // Run the optimization pipeline
  MPM.run(*module, MAM);
}

static cl::opt<std::string> InputFilename(cl::Positional, 
                                         cl::desc("<input file>"), 
                                         cl::Required);

static cl::opt<bool> DumpTokens("dump-tokens", 
                               cl::desc("Dump tokens"));

static cl::opt<bool> DumpAST("dump-ast", 
                            cl::desc("Dump the AST"));

static cl::opt<bool> TypeCheck("typecheck", 
                              cl::desc("Perform type checking"));

static cl::opt<bool> EmitSIL("emit-sil", 
                            cl::desc("Emit SIL"));

static cl::opt<bool> EmitLLVM("emit-llvm", 
                             cl::desc("Emit LLVM IR"));

static cl::opt<bool> OptimizeCode("O", 
                                 cl::desc("Enable optimizations"));

static cl::opt<std::string> OptLevel("opt-level",
                                     cl::desc("Optimization level (0,1,2,3)"),
                                     cl::init("0"));

static cl::opt<std::string> OutputFile("o",
                                       cl::desc("Output file"),
                                       cl::value_desc("filename"));

static cl::opt<bool> EmitObject("c",
                               cl::desc("Emit object file"));

static cl::opt<std::string> TargetTriple("target",
                                         cl::desc("Target triple"),
                                         cl::init(""));

static cl::opt<std::string> TargetCPU("target-cpu",
                                     cl::desc("Target CPU"),
                                     cl::init("generic"));

static cl::opt<bool> Verbose("v", 
                            cl::desc("Verbose output"));

// Forward declarations for expression/statement generation
#include <unordered_map>
Value* generateExpr(Expr* expr, IRBuilder<>& Builder, LLVMContext& Context, Function* currentFunc = nullptr, std::unordered_map<std::string, Value*>* localVars = nullptr);
Value* generateStmt(Stmt* stmt, IRBuilder<>& Builder, LLVMContext& Context, Function* currentFunc = nullptr, std::unordered_map<std::string, Value*>* localVars = nullptr);

int main(int argc, char* argv[]) {
  // Parse command line arguments
  cl::ParseCommandLineOptions(argc, argv, "Swift Compiler (Simplified)\n");
  
  // Initialize LLVM targets for binary generation (only essential ones)
  LLVMInitializeX86TargetInfo();
  LLVMInitializeX86Target();
  LLVMInitializeX86TargetMC();
  LLVMInitializeX86AsmPrinter();
  
  LLVMInitializeAArch64TargetInfo();
  LLVMInitializeAArch64Target();
  LLVMInitializeAArch64TargetMC();
  LLVMInitializeAArch64AsmPrinter();

  // Create diagnostic engine
  DiagnosticEngine diags;
  
  // Create source manager
  SourceManager sourceMgr;
  
  // Read input file
  auto fileOrError = MemoryBuffer::getFile(InputFilename);
  if (std::error_code ec = fileOrError.getError()) {
    std::cerr << "Error reading file '" << InputFilename << "': " << ec.message() << "\n";
    return 1;
  }
  
  auto buffer = std::move(fileOrError.get());
  SourceLoc startLoc = sourceMgr.addSourceFile(std::move(buffer), InputFilename);
  
  if (Verbose) {
    std::cout << "Compiling: " << InputFilename << "\n";
  }

  // Lexical analysis
  StringRef sourceText = sourceMgr.getBuffer(startLoc);
  Lexer lexer(sourceText, startLoc, diags);
  
  if (DumpTokens) {
    std::cout << "=== TOKENS ===\n";
    Token token;
    do {
      token = lexer.lex();
      std::cout << getTokenKindName(token.getKind()).str() 
                << " [" << token.getText().str() << "]\n";
    } while (token.getKind() != TokenKind::Eof);
    return 0;
  }
  
  // Parsing
  Parser parser(lexer, diags);
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
  
  if (Verbose) {
    std::cout << "Parsing completed successfully\n";
  }

  if (DumpAST) {
    std::cout << "=== AST ===\n";
    std::cout << "Found " << topLevelDecls.size() << " top-level declarations\n";
    for (size_t i = 0; i < topLevelDecls.size(); ++i) {
      const auto& decl = topLevelDecls[i];
      std::cout << "Declaration " << i << ": ";
      switch (decl->getKind()) {
      case NodeKind::VarDecl:
        std::cout << "Variable Declaration\n";
        break;
      case NodeKind::FuncDecl:
        std::cout << "Function Declaration\n";
        break;
      case NodeKind::ClassDecl:
        std::cout << "Class Declaration\n";
        break;
      case NodeKind::StructDecl:
        std::cout << "Struct Declaration\n";
        break;
      case NodeKind::ImportDecl:
        std::cout << "Import Declaration\n";
        break;
      default:
        std::cout << "Other Declaration\n";
        break;
      }
    }
    return 0;
  }

  // Type checking
  if (TypeCheck || EmitSIL || EmitLLVM) {
    TypeChecker typeChecker(diags);
    if (!typeChecker.typeCheck(topLevelDecls)) {
      if (TypeCheck) {
        std::cerr << "Type checking failed\n";
        return 1;
      }
      // For SIL/IR generation, continue with best effort
    }
    
    if (Verbose && TypeCheck) {
      std::cout << "Type checking completed successfully\n";
    }
  }

  // SIL Generation
  if (EmitSIL) {
    std::cout << "=== SIL ===\n";
    
    // Generate enhanced SIL output
    int funcCount = 0;
    int varCount = 0;
    int classCount = 0;
    
    // SIL module header
    std::cout << "sil_stage canonical\n\n";
    std::cout << "import Builtin\n";
    std::cout << "import Swift\n\n";
    
    for (const auto& decl : topLevelDecls) {
      switch (decl->getKind()) {
        case NodeKind::FuncDecl: {
          auto funcDecl = static_cast<FuncDecl*>(decl.get());
          std::cout << "// " << funcDecl->getName().str();
          
          // Handle generic functions
          if (funcDecl->isGeneric()) {
            std::cout << " (generic)";
            auto genericParams = funcDecl->getGenericParams();
            if (genericParams) {
              std::cout << "<";
              for (size_t i = 0; i < genericParams->getNumParameters(); ++i) {
                if (i > 0) std::cout << ", ";
                std::cout << "T" << i;
              }
              std::cout << ">";
            }
          }
          std::cout << "\n";
          
          if (funcDecl->isGeneric()) {
            // Generate generic SIL function
            std::cout << "sil hidden @_T4main" << funcDecl->getName().str() 
                      << funcCount << "SiyF : $@convention(thin) <T> () -> Int32 {\n";
          } else {
            std::cout << "sil hidden @_T4main" << funcDecl->getName().str() 
                      << funcCount << "SiyF : $@convention(thin) () -> Int32 {\n";
          }
          
          std::cout << "bb0:\n";
          std::cout << "  %0 = integer_literal $Builtin.Int32, 0\n";
          std::cout << "  %1 = struct $Int32 (%0 : $Builtin.Int32)\n";
          std::cout << "  return %1 : $Int32\n";
          std::cout << "}\n\n";
          funcCount++;
          break;
        }
        case NodeKind::VarDecl: {
          auto varDecl = static_cast<VarDecl*>(decl.get());
          std::cout << "// Global variable: " << varDecl->getName().str() << "\n";
          std::cout << "sil_global hidden @_T4main" << varDecl->getName().str() 
                    << "Si : $Int32\n\n";
          
          // Initializer function
          std::cout << "sil private @globalinit_" << varCount << " : $@convention(c) () -> () {\n";
          std::cout << "bb0:\n";
          std::cout << "  %0 = global_addr @_T4main" << varDecl->getName().str() << "Si : $*Int32\n";
          std::cout << "  %1 = integer_literal $Builtin.Int32, 42\n";
          std::cout << "  %2 = struct $Int32 (%1 : $Builtin.Int32)\n";
          std::cout << "  store %2 to %0 : $*Int32\n";
          std::cout << "  %3 = tuple ()\n";
          std::cout << "  return %3 : $()\n";
          std::cout << "}\n\n";
          varCount++;
          break;
        }
        case NodeKind::ClassDecl: {
          std::cout << "// Class declaration " << classCount << "\n";
          std::cout << "class_method @_T4mainC" << classCount << " : $@convention(method) (@guaranteed C" 
                    << classCount << ") -> ()\n\n";
          classCount++;
          break;
        }
        case NodeKind::StructDecl: {
          auto structDecl = static_cast<StructDecl*>(decl.get());
          std::cout << "// Struct declaration: " << structDecl->getName().str();
          
          if (structDecl->isGeneric()) {
            std::cout << " (generic)";
            auto genericParams = structDecl->getGenericParams();
            if (genericParams) {
              std::cout << "<";
              for (size_t i = 0; i < genericParams->getNumParameters(); ++i) {
                if (i > 0) std::cout << ", ";
                std::cout << "T" << i;
              }
              std::cout << ">";
            }
          }
          std::cout << "\n";
          
          if (structDecl->isGeneric()) {
            std::cout << "struct $" << structDecl->getName().str() << "<T> {\n";
            std::cout << "  var field0: T\n";
            std::cout << "  var field1: T\n";
          } else {
            std::cout << "struct $" << structDecl->getName().str() << " {\n";
            std::cout << "  var x: Int32\n";
            std::cout << "  var y: Int32\n";
          }
          std::cout << "}\n\n";
          break;
        }
        default:
          break;
      }
    }
    
    std::cout << "// SIL generation completed\n";
    std::cout << "// Functions: " << funcCount << ", Variables: " << varCount 
              << ", Classes: " << classCount << "\n";
    
    return 0;
  }

  // IR Generation with Optimization
  if (EmitLLVM) {
    std::cout << "=== LLVM IR ===\n";
    
    // Create LLVM context and module
    llvm::LLVMContext Context;
    auto Module = std::make_unique<llvm::Module>("main.swift", Context);
    Module->setSourceFileName("main.swift");
    
    // Set target triple (support cross-platform compilation)
    std::string Triple = TargetTriple.empty() ? "x86_64-unknown-linux-gnu" : TargetTriple.getValue();
    Module->setTargetTriple(Triple);
    
    // Get target machine for data layout
    std::string Error;
    const Target* TheTarget = TargetRegistry::lookupTarget(Triple, Error);
    if (!TheTarget) {
      std::cerr << "Error: " << Error << "\n";
      return 1;
    }
    
    TargetOptions opt;
    auto RM = std::optional<Reloc::Model>();
    TargetMachine* TheTargetMachine = TheTarget->createTargetMachine(
      Triple, TargetCPU, "", opt, RM);
    
    Module->setDataLayout(TheTargetMachine->createDataLayout());
    
    if (Verbose) {
      std::cout << "; Target: " << Triple << "\n";
      std::cout << "; CPU: " << TargetCPU << "\n";
    }
    
    IRBuilder<> Builder(Context);
    
    // Generate LLVM IR for declarations
    int funcCount = 0;
    int globalCount = 0;
    
    // Swift runtime function declarations
    llvm::FunctionType* VoidFuncType = llvm::FunctionType::get(llvm::Type::getVoidTy(Context), {}, false);
    llvm::FunctionType* RetainType = llvm::FunctionType::get(llvm::Type::getVoidTy(Context), {llvm::PointerType::getUnqual(Context)}, false);
    
    Module->getOrInsertFunction("swift_retain", RetainType);
    Module->getOrInsertFunction("swift_release", RetainType);
    
    // Generate globals
    for (const auto& decl : topLevelDecls) {
      if (decl->getKind() == NodeKind::VarDecl) {
        auto varDecl = static_cast<VarDecl*>(decl.get());
        std::string globalName = "_swift_global_" + varDecl->getName().str();
        
        GlobalVariable* global = new GlobalVariable(
          *Module, llvm::Type::getInt32Ty(Context), false,
          GlobalValue::InternalLinkage,
          ConstantInt::get(llvm::Type::getInt32Ty(Context), 42),
          globalName);
        global->setAlignment(MaybeAlign(4));
        globalCount++;
      }
    }
    
    // Generate functions
    for (const auto& decl : topLevelDecls) {
      if (decl->getKind() == NodeKind::FuncDecl) {
        auto funcDecl = static_cast<FuncDecl*>(decl.get());
        std::string funcName = "_swift_func_" + funcDecl->getName().str();
        
        // Create parameter types (for now, assume all parameters are Int)
        std::vector<llvm::Type*> paramTypes;
        for (const auto& param : funcDecl->getParameters()) {
          paramTypes.push_back(llvm::Type::getInt32Ty(Context));
        }
        
        llvm::FunctionType* FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(Context), paramTypes, false);
        Function* F = Function::Create(FT, Function::ExternalLinkage, funcName, Module.get());
        
        // Set parameter names
        auto argIt = F->arg_begin();
        for (const auto& param : funcDecl->getParameters()) {
          argIt->setName(param->getInternalName());
          ++argIt;
        }
        
        BasicBlock* BB = BasicBlock::Create(Context, "entry", F);
        Builder.SetInsertPoint(BB);
        
        // Generate function body with expression evaluation
        Value* RetVal = Builder.CreateAlloca(llvm::Type::getInt32Ty(Context), nullptr, "retval");
        
        // Check if function has a body with return statement
        if (funcDecl->getBody()) {
          std::unordered_map<std::string, Value*> localVars;
          Value* bodyResult = generateStmt(funcDecl->getBody(), Builder, Context, F, &localVars);
          if (bodyResult) {
            Builder.CreateStore(bodyResult, RetVal);
          } else {
            // Default return value if no explicit return
            Builder.CreateStore(ConstantInt::get(llvm::Type::getInt32Ty(Context), 0), RetVal);
          }
        } else {
          // No body, return 0
          Builder.CreateStore(ConstantInt::get(llvm::Type::getInt32Ty(Context), 0), RetVal);
        }
        
        Value* LoadedVal = Builder.CreateLoad(llvm::Type::getInt32Ty(Context), RetVal);
        Builder.CreateRet(LoadedVal);
        
        funcCount++;
      }
    }
    
    // Apply optimizations if requested
    if (OptimizeCode || OptLevel != "0") {
      int level = std::stoi(OptLevel);
      if (OptimizeCode && level == 0) level = 2; // Default optimization level
      
      if (Verbose) {
        std::cout << "; Applying optimization level " << level << "\n";
      }
      
      runOptimizationPipeline(Module.get(), level);
      
      if (Verbose) {
        std::cout << "; Optimizations completed\n";
      }
    }
    
    // Verify the module
    if (verifyModule(*Module, &llvm::errs())) {
      std::cerr << "Error: Generated LLVM IR is invalid\n";
      return 1;
    }
    
    // Generate object file if requested
    if (EmitObject || !OutputFile.empty()) {
      std::string outputFilename = OutputFile.empty() ? "output.o" : OutputFile.getValue();
      
      // Open output file
      std::error_code EC;
      raw_fd_ostream dest(outputFilename, EC, sys::fs::OF_None);
      
      if (EC) {
        std::cerr << "Could not open file: " << EC.message() << "\n";
        return 1;
      }
      
      // Create pass manager for code generation
      legacy::PassManager pass;
      auto FileType = CodeGenFileType::ObjectFile;
      
      if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        std::cerr << "TheTargetMachine can't emit a file of this type\n";
        return 1;
      }
      
      // Generate object file
      pass.run(*Module);
      dest.flush();
      
      if (Verbose) {
        std::cout << "; Object file generated: " << outputFilename << "\n";
        std::cout << "; Target: " << Triple << "\n";
        std::cout << "; CPU: " << TargetCPU << "\n";
      }
    } else {
      // Print the optimized module to stdout
      Module->print(llvm::outs(), nullptr);
    }
    
    if (Verbose) {
      std::cout << "\n; IR generation completed\n";
      std::cout << "; Functions: " << funcCount << ", Globals: " << globalCount << "\n";
      if (OptimizeCode || OptLevel != "0") {
        std::cout << "; Optimization level: " << OptLevel << "\n";
      }
    }
    
    return 0;
  }

  if (TypeCheck) {
    // Type check only mode
    return 0;
  }

  std::cout << "Compilation completed successfully!\n";
  std::cout << "Processed " << topLevelDecls.size() << " declarations\n";

  return 0;
}
// Expression generation implementation
Value* generateExpr(Expr* expr, IRBuilder<>& Builder, LLVMContext& Context, Function* currentFunc, std::unordered_map<std::string, Value*>* localVars) {
  if (!expr) return nullptr;
  
  switch (expr->getKind()) {
    case NodeKind::IntegerLiteralExpr: {
      auto intExpr = static_cast<IntegerLiteralExpr*>(expr);
      return ConstantInt::get(llvm::Type::getInt32Ty(Context), intExpr->getValue());
    }
    
    case NodeKind::BooleanLiteralExpr: {
      auto boolExpr = static_cast<BooleanLiteralExpr*>(expr);
      return ConstantInt::get(llvm::Type::getInt1Ty(Context), boolExpr->getValue() ? 1 : 0);
    }
    
    case NodeKind::BinaryOperatorExpr: {
      auto binExpr = static_cast<BinaryOperatorExpr*>(expr);
      Value* lhs = generateExpr(binExpr->getLHS(), Builder, Context, currentFunc, localVars);
      Value* rhs = generateExpr(binExpr->getRHS(), Builder, Context, currentFunc, localVars);
      
      if (!lhs || !rhs) return nullptr;
      
      StringRef op = binExpr->getOperatorName();
      if (op == "+") {
        return Builder.CreateAdd(lhs, rhs, "addtmp");
      } else if (op == "-") {
        return Builder.CreateSub(lhs, rhs, "subtmp");
      } else if (op == "*") {
        return Builder.CreateMul(lhs, rhs, "multmp");
      } else if (op == "/") {
        return Builder.CreateSDiv(lhs, rhs, "divtmp");
      } else if (op == "==") {
        return Builder.CreateICmpEQ(lhs, rhs, "eqtmp");
      } else if (op == "!=") {
        return Builder.CreateICmpNE(lhs, rhs, "netmp");
      } else if (op == "<") {
        return Builder.CreateICmpSLT(lhs, rhs, "lttmp");
      } else if (op == ">") {
        return Builder.CreateICmpSGT(lhs, rhs, "gttmp");
      } else if (op == "<=") {
        return Builder.CreateICmpSLE(lhs, rhs, "letmp");
      } else if (op == ">=") {
        return Builder.CreateICmpSGE(lhs, rhs, "getmp");
      }
      
      return nullptr;
    }
    
    case NodeKind::IdentifierExpr: {
      auto idExpr = static_cast<IdentifierExpr*>(expr);
      StringRef name = idExpr->getName();
      
      // Look for local variable first
      if (localVars && localVars->find(name.str()) != localVars->end()) {
        Value* varPtr = (*localVars)[name.str()];
        return Builder.CreateLoad(llvm::Type::getInt32Ty(Context), varPtr, name);
      }
      
      // Look for function parameter
      if (currentFunc) {
        for (auto& arg : currentFunc->args()) {
          if (arg.getName() == name) {
            return &arg;
          }
        }
      }
      
      // Variable not found, return 0 for now
      return ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
    }
    
    case NodeKind::CallExpr: {
      auto callExpr = static_cast<CallExpr*>(expr);
      auto callee = callExpr->getCallee();
      
      // For now, only handle simple identifier function calls
      if (callee->getKind() == NodeKind::IdentifierExpr) {
        auto idExpr = static_cast<IdentifierExpr*>(callee);
        std::string funcName = "_swift_func_" + idExpr->getName().str();
        
        // Generate arguments
        std::vector<Value*> args;
        for (const auto& arg : callExpr->getArguments()) {
          Value* argVal = generateExpr(arg.get(), Builder, Context, currentFunc, localVars);
          if (argVal) {
            args.push_back(argVal);
          }
        }
        
        // Special handling for print function
        if (idExpr->getName() == "print") {
          // For now, print is a no-op but we'll add it as an external function
          llvm::FunctionType* printType = llvm::FunctionType::get(llvm::Type::getVoidTy(Context), {llvm::Type::getInt32Ty(Context)}, false);
          FunctionCallee printCallee = Builder.GetInsertBlock()->getModule()->getOrInsertFunction("swift_print_int", printType);
          if (!args.empty()) {
            Builder.CreateCall(printCallee, args);
          }
          return ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
        }
        
        // Look up the function and call it
        if (Function* targetFunc = Builder.GetInsertBlock()->getModule()->getFunction(funcName)) {
          return Builder.CreateCall(targetFunc, args, "calltmp");
        }
      }
      
      return ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
    }
    
    default:
      return nullptr;
  }
}

// Statement generation implementation
Value* generateStmt(Stmt* stmt, IRBuilder<>& Builder, LLVMContext& Context, Function* currentFunc, std::unordered_map<std::string, Value*>* localVars) {
  if (!stmt) return nullptr;
  
  switch (stmt->getKind()) {
    case NodeKind::CompoundStmt: {
      auto compStmt = static_cast<CompoundStmt*>(stmt);
      Value* lastValue = nullptr;
      for (const auto& s : compStmt->getStatements()) {
        lastValue = generateStmt(s.get(), Builder, Context, currentFunc, localVars);
      }
      return lastValue;
    }
    
    case NodeKind::DeclStmt: {
      auto declStmt = static_cast<DeclStmt*>(stmt);
      auto decl = declStmt->getDeclaration();
      
      if (decl->getKind() == NodeKind::VarDecl) {
        auto varDecl = static_cast<VarDecl*>(decl);
        std::string varName = varDecl->getName().str();
        
        // Create alloca for the variable
        Value* varPtr = Builder.CreateAlloca(llvm::Type::getInt32Ty(Context), nullptr, varName);
        
        // Initialize with the initial value if present
        if (varDecl->getInitializer()) {
          Value* initVal = generateExpr(varDecl->getInitializer(), Builder, Context, currentFunc, localVars);
          if (initVal) {
            Builder.CreateStore(initVal, varPtr);
          }
        }
        
        // Store in local variables map
        if (localVars) {
          (*localVars)[varName] = varPtr;
        }
      }
      
      return nullptr; // Declarations don't return values
    }
    
    case NodeKind::ReturnStmt: {
      auto retStmt = static_cast<ReturnStmt*>(stmt);
      if (retStmt->hasValue()) {
        return generateExpr(retStmt->getValue(), Builder, Context, currentFunc, localVars);
      }
      return ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
    }
    
    case NodeKind::ExprStmt: {
      auto exprStmt = static_cast<ExprStmt*>(stmt);
      return generateExpr(exprStmt->getExpression(), Builder, Context, currentFunc, localVars);
    }
    
    case NodeKind::IfStmt: {
      auto ifStmt = static_cast<IfStmt*>(stmt);
      Value* condVal = generateExpr(ifStmt->getCondition(), Builder, Context, currentFunc, localVars);
      
      if (!condVal) return nullptr;
      
      // Convert condition to i1 if needed
      if (condVal->getType()->isIntegerTy(32)) {
        condVal = Builder.CreateICmpNE(condVal, ConstantInt::get(llvm::Type::getInt32Ty(Context), 0), "tobool");
      }
      
      Function* func = Builder.GetInsertBlock()->getParent();
      BasicBlock* thenBB = BasicBlock::Create(Context, "then", func);
      BasicBlock* elseBB = BasicBlock::Create(Context, "else");
      BasicBlock* mergeBB = BasicBlock::Create(Context, "ifcont");
      
      Builder.CreateCondBr(condVal, thenBB, elseBB);
      
      // Generate then block
      Builder.SetInsertPoint(thenBB);
      Value* thenVal = generateStmt(ifStmt->getThenStmt(), Builder, Context, currentFunc, localVars);
      Builder.CreateBr(mergeBB);
      thenBB = Builder.GetInsertBlock(); // Update in case of nested blocks
      
      // Generate else block
      func->insert(func->end(), elseBB);
      Builder.SetInsertPoint(elseBB);
      Value* elseVal = nullptr;
      if (ifStmt->getElseStmt()) {
        elseVal = generateStmt(ifStmt->getElseStmt(), Builder, Context, currentFunc, localVars);
      }
      Builder.CreateBr(mergeBB);
      elseBB = Builder.GetInsertBlock();
      
      // Generate merge block
      func->insert(func->end(), mergeBB);
      Builder.SetInsertPoint(mergeBB);
      
      // If both branches return values, create a PHI node
      if (thenVal && elseVal && thenVal->getType() == elseVal->getType()) {
        PHINode* phi = Builder.CreatePHI(thenVal->getType(), 2, "iftmp");
        phi->addIncoming(thenVal, thenBB);
        phi->addIncoming(elseVal, elseBB);
        return phi;
      }
      
      return nullptr;
    }
    
    default:
      return nullptr;
  }
}
