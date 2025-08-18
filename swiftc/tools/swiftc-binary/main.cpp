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
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <iostream>

using namespace swiftc;
using namespace llvm;

static cl::opt<std::string> InputFilename(cl::Positional, 
                                         cl::desc("<input file>"), 
                                         cl::Required);

static cl::opt<std::string> OutputFile("o",
                                       cl::desc("Output file"),
                                       cl::value_desc("filename"));

static cl::opt<bool> EmitLLVM("emit-llvm", 
                             cl::desc("Emit LLVM IR"));

static cl::opt<bool> EmitObject("c",
                               cl::desc("Emit object file"));

static cl::opt<bool> EmitExecutable("executable",
                                   cl::desc("Emit executable"));

static cl::opt<bool> TypeCheck("typecheck", 
                              cl::desc("Perform type checking only"));

static cl::opt<std::string> TargetTriple("target",
                                         cl::desc("Target triple"),
                                         cl::init(""));

static cl::opt<bool> Verbose("v", 
                            cl::desc("Verbose output"));

int main(int argc, char* argv[]) {
  // Parse command line arguments
  cl::ParseCommandLineOptions(argc, argv, "Swift Compiler - Binary Generation\n");
  
  // Initialize ALL LLVM targets for complete platform support
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

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

  // Type checking
  TypeChecker typeChecker(diags);
  if (!typeChecker.typeCheck(topLevelDecls)) {
    std::cerr << "Type checking failed\n";
    return 1;
  }
  
  if (Verbose) {
    std::cout << "Type checking completed successfully\n";
  }

  // If only type checking was requested, stop here
  if (TypeCheck) {
    std::cout << "Type checking completed successfully!\n";
    std::cout << "Processed " << topLevelDecls.size() << " declarations\n";
    return 0;
  }

  // IR Generation
  llvm::LLVMContext Context;
  auto Module = std::make_unique<llvm::Module>(InputFilename, Context);
  Module->setSourceFileName(InputFilename);
  
  // Set target triple
  std::string Triple = TargetTriple.empty() ? "x86_64-unknown-linux-gnu" : TargetTriple.getValue();
  Module->setTargetTriple(Triple);
  
  // Get target machine
  std::string Error;
  const Target* TheTarget = TargetRegistry::lookupTarget(Triple, Error);
  if (!TheTarget) {
    std::cerr << "Error: " << Error << "\n";
    return 1;
  }
  
  TargetOptions opt;
  auto RM = std::optional<Reloc::Model>();
  std::unique_ptr<TargetMachine> TheTargetMachine(
    TheTarget->createTargetMachine(Triple, "generic", "", opt, RM));
  
  Module->setDataLayout(TheTargetMachine->createDataLayout());
  
  if (Verbose) {
    std::cout << "Target: " << Triple << "\n";
  }

  IRBuilder<> Builder(Context);
  
  // Generate LLVM IR
  int funcCount = 0;
  int globalCount = 0;
  
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
      
      llvm::FunctionType* FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(Context), {}, false);
      Function* F = Function::Create(FT, Function::ExternalLinkage, funcName, Module.get());
      
      BasicBlock* BB = BasicBlock::Create(Context, "entry", F);
      Builder.SetInsertPoint(BB);
      
      // Simple function body that returns 42
      Value* RetVal = Builder.CreateAlloca(llvm::Type::getInt32Ty(Context), nullptr, "retval");
      Builder.CreateStore(ConstantInt::get(llvm::Type::getInt32Ty(Context), 42), RetVal);
      Value* LoadedVal = Builder.CreateLoad(llvm::Type::getInt32Ty(Context), RetVal);
      Builder.CreateRet(LoadedVal);
      
      funcCount++;
    }
  }
  
  // Add a main function if none exists
  bool hasMain = false;
  for (const auto& decl : topLevelDecls) {
    if (decl->getKind() == NodeKind::FuncDecl) {
      auto funcDecl = static_cast<FuncDecl*>(decl.get());
      if (funcDecl->getName() == "main") {
        hasMain = true;
        break;
      }
    }
  }
  
  if (!hasMain && EmitExecutable) {
    // Create a simple main function for executable generation
    llvm::FunctionType* MainFT = llvm::FunctionType::get(llvm::Type::getInt32Ty(Context), {}, false);
    Function* MainF = Function::Create(MainFT, Function::ExternalLinkage, "main", Module.get());
    
    BasicBlock* MainBB = BasicBlock::Create(Context, "entry", MainF);
    Builder.SetInsertPoint(MainBB);
    Builder.CreateRet(ConstantInt::get(llvm::Type::getInt32Ty(Context), 0));
    
    if (Verbose) {
      std::cout << "Generated main function for executable\n";
    }
  }
  
  // Verify the module
  if (verifyModule(*Module, &llvm::errs())) {
    std::cerr << "Error: Generated LLVM IR is invalid\n";
    return 1;
  }
  
  if (Verbose) {
    std::cout << "IR generation completed: " << funcCount << " functions, " << globalCount << " globals\n";
  }

  // Output handling
  if (EmitLLVM) {
    // Emit LLVM IR
    if (!OutputFile.empty()) {
      std::error_code EC;
      raw_fd_ostream dest(OutputFile.getValue(), EC, sys::fs::OF_None);
      if (EC) {
        std::cerr << "Could not open file: " << EC.message() << "\n";
        return 1;
      }
      Module->print(dest, nullptr);
      dest.flush();
      if (Verbose) {
        std::cout << "LLVM IR written to: " << OutputFile.getValue() << "\n";
      }
    } else {
      Module->print(llvm::outs(), nullptr);
    }
    return 0;
  }
  
  // Object file or executable generation
  if (EmitObject || EmitExecutable || !OutputFile.empty()) {
    std::string outputFilename;
    
    if (!OutputFile.empty()) {
      outputFilename = OutputFile.getValue();
    } else if (EmitExecutable) {
      outputFilename = "a.out";
    } else {
      outputFilename = "output.o";
    }
    
    // Open output file
    std::error_code EC;
    raw_fd_ostream dest(outputFilename, EC, sys::fs::OF_None);
    
    if (EC) {
      std::cerr << "Could not open file: " << EC.message() << "\n";
      return 1;
    }
    
    // Create pass manager for code generation
    legacy::PassManager pass;
    auto FileType = EmitExecutable ? CodeGenFileType::ObjectFile : CodeGenFileType::ObjectFile;
    
    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
      std::cerr << "TheTargetMachine can't emit a file of this type\n";
      return 1;
    }
    
    // Generate object file
    pass.run(*Module);
    dest.flush();
    
    if (Verbose) {
      std::cout << "Generated: " << outputFilename << "\n";
      std::cout << "Target: " << Triple << "\n";
      std::cout << "Functions: " << funcCount << ", Globals: " << globalCount << "\n";
    }
    
    // If generating executable, link the object file
    if (EmitExecutable && outputFilename.ends_with(".o")) {
      std::string execName = OutputFile.empty() ? "a.out" : OutputFile.getValue();
      std::string linkCmd = "gcc -o " + execName + " " + outputFilename;
      
      if (Verbose) {
        std::cout << "Linking: " << linkCmd << "\n";
      }
      
      int linkResult = system(linkCmd.c_str());
      if (linkResult == 0) {
        if (Verbose) {
          std::cout << "Executable created: " << execName << "\n";
        }
        // Clean up intermediate object file
        std::remove(outputFilename.c_str());
      } else {
        std::cerr << "Linking failed\n";
        return 1;
      }
    }
    
    return 0;
  }

  std::cout << "Compilation completed successfully!\n";
  std::cout << "Processed " << topLevelDecls.size() << " declarations\n";

  return 0;
}