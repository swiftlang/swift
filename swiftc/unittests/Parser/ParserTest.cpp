//===--- ParserTest.cpp - Tests for Parser -------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Parser/Parser.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/AST/Decl.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Stmt.h"
#include <gtest/gtest.h>
#include <llvm/Support/MemoryBuffer.h>

using namespace swiftc;

class ParserTest : public ::testing::Test {
protected:
  SourceManager SM;
  DiagnosticEngine Diag;
  
  std::vector<std::unique_ptr<Decl>> parse(const std::string& source, const std::string& filename = "test.swift") {
    auto buffer = llvm::MemoryBuffer::getMemBuffer(source, filename);
    SourceLoc startLoc = SM.addSourceFile(std::move(buffer), filename);
    
    Lexer lexer(SM, Diag, startLoc);
    Parser parser(lexer, Diag);
    
    return parser.parseSourceFile();
  }
  
  void expectNoDiagnostics() {
    EXPECT_FALSE(Diag.hasErrors()) << "Unexpected parse errors";
    if (Diag.hasErrors()) {
      for (const auto& diag : Diag.getDiagnostics()) {
        std::cerr << "Diagnostic: " << diag.Message << std::endl;
      }
    }
  }
  
  void expectErrors() {
    EXPECT_TRUE(Diag.hasErrors()) << "Expected parse errors but got none";
  }
  
  void SetUp() override {
    Diag.clear();
  }
};

TEST_F(ParserTest, EmptyFile) {
  auto decls = parse("");
  EXPECT_TRUE(decls.empty());
  expectNoDiagnostics();
}

TEST_F(ParserTest, SimpleVariableDeclaration) {
  auto decls = parse("let x = 42");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
  
  auto* varDecl = dynamic_cast<VarDecl*>(decls[0].get());
  EXPECT_NE(varDecl, nullptr);
  EXPECT_EQ(varDecl->getName(), "x");
  EXPECT_TRUE(varDecl->isLet());
}

TEST_F(ParserTest, SimpleFunctionDeclaration) {
  auto decls = parse("func greet() -> String { return \"Hello\" }");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
  
  auto* funcDecl = dynamic_cast<FuncDecl*>(decls[0].get());
  EXPECT_NE(funcDecl, nullptr);
  EXPECT_EQ(funcDecl->getName(), "greet");
}

TEST_F(ParserTest, FunctionWithParameters) {
  auto decls = parse("func add(a: Int, b: Int) -> Int { return a + b }");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
  
  auto* funcDecl = dynamic_cast<FuncDecl*>(decls[0].get());
  EXPECT_NE(funcDecl, nullptr);
  EXPECT_EQ(funcDecl->getName(), "add");
  EXPECT_EQ(funcDecl->getParameters().size(), 2u);
}

TEST_F(ParserTest, SimpleClassDeclaration) {
  auto decls = parse(R"(
    class MyClass {
        let property: String
        
        init(property: String) {
            self.property = property
        }
    }
  )");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
  
  auto* classDecl = dynamic_cast<ClassDecl*>(decls[0].get());
  EXPECT_NE(classDecl, nullptr);
  EXPECT_EQ(classDecl->getName(), "MyClass");
}

TEST_F(ParserTest, SimpleStructDeclaration) {
  auto decls = parse(R"(
    struct Point {
        let x: Double
        let y: Double
        
        func distance() -> Double {
            return sqrt(x * x + y * y)
        }
    }
  )");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
  
  auto* structDecl = dynamic_cast<StructDecl*>(decls[0].get());
  EXPECT_NE(structDecl, nullptr);
  EXPECT_EQ(structDecl->getName(), "Point");
}

TEST_F(ParserTest, MultipleDeclarations) {
  auto decls = parse(R"(
    let x = 42
    var y = "hello"
    func test() {}
    class TestClass {}
  )");
  
  EXPECT_EQ(decls.size(), 4u);
  expectNoDiagnostics();
  
  EXPECT_NE(dynamic_cast<VarDecl*>(decls[0].get()), nullptr);
  EXPECT_NE(dynamic_cast<VarDecl*>(decls[1].get()), nullptr);
  EXPECT_NE(dynamic_cast<FuncDecl*>(decls[2].get()), nullptr);
  EXPECT_NE(dynamic_cast<ClassDecl*>(decls[3].get()), nullptr);
}

TEST_F(ParserTest, ParseErrors) {
  // Test various syntax errors
  parse("let = 42");  // Missing identifier
  expectErrors();
  
  Diag.clear();
  parse("func }");     // Invalid function syntax
  expectErrors();
  
  Diag.clear();
  parse("class {");    // Missing class name
  expectErrors();
}

TEST_F(ParserTest, NestedStructures) {
  auto decls = parse(R"(
    class OuterClass {
        struct InnerStruct {
            let value: Int
        }
        
        func method() {
            let inner = InnerStruct(value: 42)
        }
    }
  )");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
  
  auto* classDecl = dynamic_cast<ClassDecl*>(decls[0].get());
  EXPECT_NE(classDecl, nullptr);
}

TEST_F(ParserTest, ControlFlowStatements) {
  auto decls = parse(R"(
    func testControlFlow() {
        if true {
            return
        }
        
        while false {
            break
        }
        
        for i in 0..<10 {
            continue
        }
    }
  )");
  
  EXPECT_EQ(decls.size(), 1u);
  expectNoDiagnostics();
}