//===--- IRGenTest.cpp - Tests for LLVM IR generation --------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/IRGen/IRGen.h"
#include "swiftc/SIL/SILFunction.h"
#include "swiftc/AST/Decl.h"
#include <gtest/gtest.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>

using namespace swiftc;

class IRGenTest : public ::testing::Test {
protected:
  std::unique_ptr<llvm::LLVMContext> Context;
  std::unique_ptr<IRGenerator> IRGen;
  
  void SetUp() override {
    Context = std::make_unique<llvm::LLVMContext>();
    IRGen = std::make_unique<IRGenerator>(*Context);
  }
};

TEST_F(IRGenTest, BasicModuleGeneration) {
  auto module = IRGen->createModule("TestModule");
  
  EXPECT_NE(module, nullptr);
  EXPECT_EQ(module->getName(), "TestModule");
  EXPECT_TRUE(llvm::verifyModule(*module, &llvm::errs()));
}

TEST_F(IRGenTest, SimpleFunctionGeneration) {
  auto module = IRGen->createModule("TestModule");
  
  // Generate IR for a simple function: func test() -> Int { return 42 }
  SourceRange range(SourceLoc(1), SourceLoc(50));
  
  auto returnType = std::make_unique<IdentifierType>("Int", range);
  auto returnExpr = std::make_unique<IntegerLiteralExpr>(42, range);
  auto returnStmt = std::make_unique<ReturnStmt>(std::move(returnExpr), range);
  
  std::vector<std::unique_ptr<Stmt>> stmts;
  stmts.push_back(std::move(returnStmt));
  auto body = std::make_unique<CompoundStmt>(std::move(stmts), range);
  
  auto funcDecl = std::make_unique<FuncDecl>("test", std::vector<std::unique_ptr<ParamDecl>>{}, 
                                           std::move(returnType), std::move(body), range);
  
  auto llvmFunc = IRGen->generateFunction(*module, funcDecl.get());
  
  EXPECT_NE(llvmFunc, nullptr);
  EXPECT_EQ(llvmFunc->getName(), "test");
  EXPECT_TRUE(llvm::verifyFunction(*llvmFunc, &llvm::errs()));
}

TEST_F(IRGenTest, FunctionWithParameters) {
  auto module = IRGen->createModule("TestModule");
  
  // Generate IR for: func add(a: Int, b: Int) -> Int { return a + b }
  SourceRange range(SourceLoc(1), SourceLoc(100));
  
  std::vector<std::unique_ptr<ParamDecl>> params;
  params.push_back(std::make_unique<ParamDecl>("a", "Int", range));
  params.push_back(std::make_unique<ParamDecl>("b", "Int", range));
  
  auto returnType = std::make_unique<IdentifierType>("Int", range);
  
  // Create return a + b
  auto aRef = std::make_unique<DeclRefExpr>("a", range);
  auto bRef = std::make_unique<DeclRefExpr>("b", range);
  auto addExpr = std::make_unique<BinaryExpr>(std::move(aRef), "+", std::move(bRef), range);
  auto returnStmt = std::make_unique<ReturnStmt>(std::move(addExpr), range);
  
  std::vector<std::unique_ptr<Stmt>> stmts;
  stmts.push_back(std::move(returnStmt));
  auto body = std::make_unique<CompoundStmt>(std::move(stmts), range);
  
  auto funcDecl = std::make_unique<FuncDecl>("add", std::move(params), 
                                           std::move(returnType), std::move(body), range);
  
  auto llvmFunc = IRGen->generateFunction(*module, funcDecl.get());
  
  EXPECT_NE(llvmFunc, nullptr);
  EXPECT_EQ(llvmFunc->getName(), "add");
  EXPECT_EQ(llvmFunc->arg_size(), 2u);
  EXPECT_TRUE(llvm::verifyFunction(*llvmFunc, &llvm::errs()));
}

TEST_F(IRGenTest, ARCInstructions) {
  auto module = IRGen->createModule("TestModule");
  
  // Test that ARC instructions are properly generated
  // This would involve creating objects and verifying retain/release calls are inserted
  
  SourceRange range(SourceLoc(1), SourceLoc(50));
  auto returnType = std::make_unique<IdentifierType>("Void", range);
  auto body = std::make_unique<CompoundStmt>(std::vector<std::unique_ptr<Stmt>>{}, range);
  
  auto funcDecl = std::make_unique<FuncDecl>("testARC", std::vector<std::unique_ptr<ParamDecl>>{}, 
                                           std::move(returnType), std::move(body), range);
  
  auto llvmFunc = IRGen->generateFunction(*module, funcDecl.get());
  
  EXPECT_NE(llvmFunc, nullptr);
  EXPECT_TRUE(llvm::verifyFunction(*llvmFunc, &llvm::errs()));
  
  // In a real implementation, we would check for swift_retain/swift_release calls
}

TEST_F(IRGenTest, CrossPlatformCodeGeneration) {
  auto module = IRGen->createModule("CrossPlatformTest");
  
  // Test that code generation works for different target platforms
  std::vector<std::string> targets = {
    "x86_64-unknown-linux-gnu",
    "aarch64-apple-darwin",
    "wasm32-unknown-unknown",
    "riscv64-unknown-linux-gnu"
  };
  
  for (const auto& target : targets) {
    IRGen->setTargetTriple(target);
    
    SourceRange range(SourceLoc(1), SourceLoc(30));
    auto returnType = std::make_unique<IdentifierType>("Int", range);
    auto returnExpr = std::make_unique<IntegerLiteralExpr>(42, range);
    auto returnStmt = std::make_unique<ReturnStmt>(std::move(returnExpr), range);
    
    std::vector<std::unique_ptr<Stmt>> stmts;
    stmts.push_back(std::move(returnStmt));
    auto body = std::make_unique<CompoundStmt>(std::move(stmts), range);
    
    auto funcDecl = std::make_unique<FuncDecl>("test_" + target, std::vector<std::unique_ptr<ParamDecl>>{}, 
                                             std::move(returnType), std::move(body), range);
    
    auto llvmFunc = IRGen->generateFunction(*module, funcDecl.get());
    
    EXPECT_NE(llvmFunc, nullptr) << "Failed to generate function for target: " << target;
    EXPECT_TRUE(llvm::verifyFunction(*llvmFunc, &llvm::errs())) << "Invalid IR for target: " << target;
  }
}

TEST_F(IRGenTest, OptimizationPipeline) {
  auto module = IRGen->createModule("OptimizationTest");
  
  // Generate a function that should be optimizable
  SourceRange range(SourceLoc(1), SourceLoc(100));
  
  // func redundant() -> Int { let x = 1; let y = 2; return x + y }
  auto xInit = std::make_unique<IntegerLiteralExpr>(1, range);
  auto yInit = std::make_unique<IntegerLiteralExpr>(2, range);
  
  auto xDecl = std::make_unique<VarDecl>("x", std::move(xInit), true, range);
  auto yDecl = std::make_unique<VarDecl>("y", std::move(yInit), true, range);
  
  auto xRef = std::make_unique<DeclRefExpr>("x", range);
  auto yRef = std::make_unique<DeclRefExpr>("y", range);
  auto addExpr = std::make_unique<BinaryExpr>(std::move(xRef), "+", std::move(yRef), range);
  auto returnStmt = std::make_unique<ReturnStmt>(std::move(addExpr), range);
  
  std::vector<std::unique_ptr<Stmt>> stmts;
  stmts.push_back(std::move(xDecl));
  stmts.push_back(std::move(yDecl));
  stmts.push_back(std::move(returnStmt));
  auto body = std::make_unique<CompoundStmt>(std::move(stmts), range);
  
  auto returnType = std::make_unique<IdentifierType>("Int", range);
  auto funcDecl = std::make_unique<FuncDecl>("redundant", std::vector<std::unique_ptr<ParamDecl>>{}, 
                                           std::move(returnType), std::move(body), range);
  
  auto llvmFunc = IRGen->generateFunction(*module, funcDecl.get());
  EXPECT_NE(llvmFunc, nullptr);
  
  // Apply optimizations
  IRGen->optimizeModule(*module);
  
  EXPECT_TRUE(llvm::verifyModule(*module, &llvm::errs()));
}