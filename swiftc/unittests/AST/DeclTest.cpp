//===--- DeclTest.cpp - Tests for declaration AST nodes ------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/AST/Decl.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Type.h"
#include "swiftc/Basic/SourceLoc.h"
#include <gtest/gtest.h>

using namespace swiftc;

class DeclTest : public ::testing::Test {
protected:
  SourceRange makeRange(uint32_t start, uint32_t end) {
    return SourceRange(SourceLoc(start), SourceLoc(end));
  }
};

TEST_F(DeclTest, VarDecl) {
  SourceRange range = makeRange(100, 110);
  auto initializer = std::make_unique<IntegerLiteralExpr>(42, makeRange(106, 108));
  
  auto varDecl = std::make_unique<VarDecl>("x", std::move(initializer), true, range);
  
  EXPECT_EQ(varDecl->getKind(), NodeKind::VarDecl);
  EXPECT_EQ(varDecl->getName(), "x");
  EXPECT_TRUE(varDecl->isLet());
  EXPECT_FALSE(varDecl->isVar());
  EXPECT_NE(varDecl->getInitializer(), nullptr);
  EXPECT_TRUE(varDecl->isa<Decl>());
}

TEST_F(DeclTest, VarDeclMutable) {
  SourceRange range = makeRange(200, 210);
  auto initializer = std::make_unique<StringLiteralExpr>("hello", makeRange(206, 213));
  
  auto varDecl = std::make_unique<VarDecl>("message", std::move(initializer), false, range);
  
  EXPECT_EQ(varDecl->getKind(), NodeKind::VarDecl);
  EXPECT_EQ(varDecl->getName(), "message");
  EXPECT_FALSE(varDecl->isLet());
  EXPECT_TRUE(varDecl->isVar());
  EXPECT_NE(varDecl->getInitializer(), nullptr);
}

TEST_F(DeclTest, FuncDecl) {
  SourceRange range = makeRange(300, 350);
  
  std::vector<std::unique_ptr<ParamDecl>> params;
  params.push_back(std::make_unique<ParamDecl>("x", "Int", makeRange(310, 315)));
  params.push_back(std::make_unique<ParamDecl>("y", "Int", makeRange(317, 322)));
  
  auto returnType = std::make_unique<IdentifierType>("Int", makeRange(327, 330));
  auto body = std::make_unique<CompoundStmt>(std::vector<std::unique_ptr<Stmt>>{}, makeRange(332, 350));
  
  auto funcDecl = std::make_unique<FuncDecl>("add", std::move(params), 
                                           std::move(returnType), std::move(body), range);
  
  EXPECT_EQ(funcDecl->getKind(), NodeKind::FuncDecl);
  EXPECT_EQ(funcDecl->getName(), "add");
  EXPECT_EQ(funcDecl->getParameters().size(), 2u);
  EXPECT_NE(funcDecl->getReturnType(), nullptr);
  EXPECT_NE(funcDecl->getBody(), nullptr);
  EXPECT_TRUE(funcDecl->isa<Decl>());
}

TEST_F(DeclTest, ClassDecl) {
  SourceRange range = makeRange(400, 450);
  
  std::vector<std::unique_ptr<Decl>> members;
  members.push_back(std::make_unique<VarDecl>("property", nullptr, true, makeRange(410, 420)));
  
  auto classDecl = std::make_unique<ClassDecl>("MyClass", std::move(members), range);
  
  EXPECT_EQ(classDecl->getKind(), NodeKind::ClassDecl);
  EXPECT_EQ(classDecl->getName(), "MyClass");
  EXPECT_EQ(classDecl->getMembers().size(), 1u);
  EXPECT_TRUE(classDecl->isa<Decl>());
  EXPECT_TRUE(classDecl->isa<NominalTypeDecl>());
}

TEST_F(DeclTest, StructDecl) {
  SourceRange range = makeRange(500, 550);
  
  std::vector<std::unique_ptr<Decl>> members;
  members.push_back(std::make_unique<VarDecl>("x", nullptr, true, makeRange(510, 515)));
  members.push_back(std::make_unique<VarDecl>("y", nullptr, true, makeRange(520, 525)));
  
  auto structDecl = std::make_unique<StructDecl>("Point", std::move(members), range);
  
  EXPECT_EQ(structDecl->getKind(), NodeKind::StructDecl);
  EXPECT_EQ(structDecl->getName(), "Point");
  EXPECT_EQ(structDecl->getMembers().size(), 2u);
  EXPECT_TRUE(structDecl->isa<Decl>());
  EXPECT_TRUE(structDecl->isa<NominalTypeDecl>());
}

TEST_F(DeclTest, EnumDecl) {
  SourceRange range = makeRange(600, 650);
  
  std::vector<std::unique_ptr<EnumCaseDecl>> cases;
  cases.push_back(std::make_unique<EnumCaseDecl>("success", nullptr, makeRange(610, 617)));
  cases.push_back(std::make_unique<EnumCaseDecl>("failure", nullptr, makeRange(620, 627)));
  
  auto enumDecl = std::make_unique<EnumDecl>("Result", std::move(cases), range);
  
  EXPECT_EQ(enumDecl->getKind(), NodeKind::EnumDecl);
  EXPECT_EQ(enumDecl->getName(), "Result");
  EXPECT_EQ(enumDecl->getCases().size(), 2u);
  EXPECT_TRUE(enumDecl->isa<Decl>());
  EXPECT_TRUE(enumDecl->isa<NominalTypeDecl>());
}

TEST_F(DeclTest, ProtocolDecl) {
  SourceRange range = makeRange(700, 750);
  
  std::vector<std::unique_ptr<Decl>> requirements;
  auto funcReq = std::make_unique<FuncDecl>("method", std::vector<std::unique_ptr<ParamDecl>>{}, 
                                          nullptr, nullptr, makeRange(710, 720));
  requirements.push_back(std::move(funcReq));
  
  auto protocolDecl = std::make_unique<ProtocolDecl>("MyProtocol", std::move(requirements), range);
  
  EXPECT_EQ(protocolDecl->getKind(), NodeKind::ProtocolDecl);
  EXPECT_EQ(protocolDecl->getName(), "MyProtocol");
  EXPECT_EQ(protocolDecl->getRequirements().size(), 1u);
  EXPECT_TRUE(protocolDecl->isa<Decl>());
}

TEST_F(DeclTest, ImportDecl) {
  SourceRange range = makeRange(800, 815);
  auto importDecl = std::make_unique<ImportDecl>("Foundation", range);
  
  EXPECT_EQ(importDecl->getKind(), NodeKind::ImportDecl);
  EXPECT_EQ(importDecl->getModuleName(), "Foundation");
  EXPECT_TRUE(importDecl->isa<Decl>());
}

TEST_F(DeclTest, ParamDecl) {
  SourceRange range = makeRange(900, 910);
  auto paramDecl = std::make_unique<ParamDecl>("name", "String", range);
  
  EXPECT_EQ(paramDecl->getName(), "name");
  EXPECT_EQ(paramDecl->getTypeName(), "String");
  EXPECT_TRUE(paramDecl->isa<Decl>());
}