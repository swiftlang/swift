#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Parser/Parser.h"
#include "swiftc/Sema/TypeChecker.h"
#include "swiftc/SIL/SILGen.h"
#include "swiftc/SIL/SILType.h"
#include "swiftc/AST/Type.h"
#include "swiftc/IRGen/IRGen.h"

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/IR/LegacyPassManager.h>

#include <iostream>
#include <memory>
#include <optional>

using namespace swiftc;
using namespace llvm;

static cl::opt<std::string> InputFilename(cl::Positional, 
                                         cl::desc("<input file>"), 
                                         cl::Required);

static cl::opt<std::string> OutputFilename("o", 
                                          cl::desc("Output filename"), 
                                          cl::value_desc("filename"),
                                          cl::init("a.out"));

static cl::opt<bool> EmitLLVM("emit-llvm", 
                             cl::desc("Emit LLVM IR instead of object code"));

static cl::opt<bool> EmitAST("dump-ast", 
                            cl::desc("Dump the AST"));

static cl::opt<bool> EmitSIL("emit-sil", 
                            cl::desc("Emit SIL"));

static cl::opt<bool> Verbose("v", 
                            cl::desc("Verbose output"));

int main(int argc, char* argv[]) {
  // Parse command line arguments
  cl::ParseCommandLineOptions(argc, argv, "Swift Compiler\n");

  // Initialize LLVM - just x86_64 and AArch64 for simplicity
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
    errs() << "Error reading file '" << InputFilename << "': " << ec.message() << "\n";
    return 1;
  }
  
  auto buffer = std::move(fileOrError.get());
  SourceLoc startLoc = sourceMgr.addSourceFile(std::move(buffer), InputFilename);
  
  if (Verbose) {
    outs() << "Compiling: " << InputFilename << "\n";
  }

  // Lexical analysis
  StringRef sourceText = sourceMgr.getBuffer(startLoc);
  Lexer lexer(sourceText, startLoc, diags);
  
  // Parsing
  Parser parser(lexer, diags);
  auto topLevelDecls = parser.parseSourceFile();
  
  if (diags.hasErrors()) {
    errs() << "Parse errors occurred\n";
    for (const auto& diag : diags.getDiagnostics()) {
      auto [line, col] = sourceMgr.getLineAndColumn(diag.Location);
      errs() << sourceMgr.getFilename(diag.Location) << ":" << line << ":" << col << ": ";
      
      switch (diag.Level) {
      case DiagnosticLevel::Error:
        errs() << "error: ";
        break;
      case DiagnosticLevel::Warning:
        errs() << "warning: ";
        break;
      case DiagnosticLevel::Note:
        errs() << "note: ";
        break;
      }
      
      errs() << diag.Message << "\n";
    }
    return 1;
  }
  
  if (EmitAST) {
    outs() << "AST dump not implemented yet\n";
    return 0;
  }

  // Semantic analysis
  TypeChecker typeChecker(diags);
  if (!typeChecker.typeCheck(topLevelDecls)) {
    errs() << "Type checking failed\n";
    return 1;
  }
  
  if (Verbose) {
    outs() << "Type checking completed successfully\n";
  }

  // SIL generation
  SILGen silGen(diags);
  auto silModule = silGen.generateSIL(topLevelDecls);
  
  if (EmitSIL) {
    outs() << "SIL dump not implemented yet\n";
    return 0;
  }

  // LLVM IR generation
  LLVMContext context;
  IRGen irGen(diags, context, InputFilename);
  auto llvmModule = irGen.generateIR(*silModule);
  
  if (EmitLLVM) {
    llvmModule->print(outs(), nullptr);
    return 0;
  }

  // Verify the module
  if (verifyModule(*llvmModule, &errs())) {
    errs() << "Module verification failed\n";
    return 1;
  }

  // Get target triple
  std::string targetTriple = "x86_64-unknown-linux-gnu"; // Simplified for now
  llvmModule->setTargetTriple(targetTriple);

  // Get target
  std::string error;
  const Target* target = TargetRegistry::lookupTarget(targetTriple, error);
  if (!target) {
    errs() << "Error: " << error << "\n";
    return 1;
  }

  // Create target machine
  std::string cpu = "generic";
  std::string features = "";
  TargetOptions opt;
  std::optional<llvm::Reloc::Model> relocModel;
  auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, relocModel);

  llvmModule->setDataLayout(targetMachine->createDataLayout());

  // Open output file
  std::error_code ec;
  raw_fd_ostream dest(OutputFilename, ec, sys::fs::OF_None);
  if (ec) {
    errs() << "Could not open file: " << ec.message() << "\n";
    return 1;
  }

  // Generate object code
  legacy::PassManager pass;
  auto fileType = llvm::CodeGenFileType::ObjectFile;

  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
    errs() << "Target machine can't emit a file of this type\n";
    return 1;
  }

  pass.run(*llvmModule);
  dest.flush();

  if (Verbose) {
    outs() << "Generated: " << OutputFilename << "\n";
  }

  return 0;
}