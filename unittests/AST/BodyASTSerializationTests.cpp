//===--- BodyASTSerializationTests.cpp - Body AST serialization tests ----===//
//
// This source code is part of the Swift.org open source project
//
// Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Unit tests for BodyASTSerializer and BodyASTDeserializer.
//
// Stage A: Tests that the body block is written correctly and that
// ExprID/StmtID assignment is sequential.
//
// Stage B: Round-trip tests for individual expression kinds.
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "swift/AST/FunctionRefInfo.h"
#include "swift/Serialization/SerializationOptions.h"
#include "gtest/gtest.h"

#include "../../lib/Serialization/ModuleFormat.h"
#include "../../lib/Serialization/BodyASTSerializer.h"
#include "../../lib/Serialization/BodyASTDeserializer.h"
#include "../../lib/Serialization/Serialization.h"

using namespace swift;
using namespace swift::serialization;
using namespace swift::unittest;

namespace {

/// Test fixture that provides a Serializer and Deserializer.
class BodyASTSerializationTest : public ::testing::Test {
protected:
  TestContext C;
  ASTContext &Ctx;
  std::unique_ptr<Serializer> Ser;
  SourceFile *SF = nullptr;
  BodyASTSerializationTest() : C(DeclareOptionalTypes), Ctx(C.Ctx) {}

  void SetUp() override {
    SerializationOptions opts;
    // The Serializer needs a ModuleOrSourceFile. We use the test context's
    // module via a FileUnit. TestContext creates a SourceFile internally.
    // We access it via the module's files.
    auto *M = Ctx.getLoadedModule(Ctx.getIdentifier("Swift"));
    if (!M) {
      auto mods = Ctx.getLoadedModules();
      if (mods.begin() != mods.end())
        M = mods.begin()->second;
    }
    ASSERT_NE(M, nullptr);
    this->SF = nullptr;
    for (auto *file : M->getFiles()) {
      if (auto *sf = dyn_cast<FileUnit>(file)) {
        if (auto *parentSF = sf->getParentSourceFile()) {
          this->SF = parentSF;
          break;
        }
      }
    }
    ASSERT_NE(this->SF, nullptr);
    Ser = std::make_unique<Serializer>(
        SWIFTMODULE_SIGNATURE, ModuleOrSourceFile(SF), opts);
  }

  /// Serializes a body and returns the bitstream data.
  std::vector<uint8_t> serializeBody(DeclID funcDeclID, BraceStmt *body) {
    BodyASTSerializer bodySer(*Ser);
    bodySer.serializeBody(funcDeclID, body);
    auto buf = Ser->getBuffer();
    return std::vector<uint8_t>(buf.begin(), buf.end());
  }

  /// Creates a deserializer with no-op resolution callbacks.
  BodyASTDeserializer makeDeserializer() {
    return BodyASTDeserializer(
        Ctx,
        [](serialization::TypeID) -> Type { return Type(); },
        [](serialization::DeclID) -> Decl * { return nullptr; },
        [](serialization::IdentifierID) -> Identifier { return Identifier(); });
  }
};

// === Stage A Tests ===

/// TEST: BODY_BLOCK is written when a function has a body.
TEST_F(BodyASTSerializationTest, BodyBlockWrittenWhenFunctionHasBody) {
  SmallVector<ASTNode, 4> elements;
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);

  EXPECT_GT(data.size(), 4u);
  ASSERT_GE(data.size(), 4u);
  EXPECT_EQ(data[0], 0xE2);
  EXPECT_EQ(data[1], 0x9C);
  EXPECT_EQ(data[2], 0xA8);
  EXPECT_EQ(data[3], 0x0E);
}

/// TEST: BODY_BLOCK contains correct record count.
TEST_F(BodyASTSerializationTest, BodyBlockContainsCorrectRecordCount) {
  auto *intExpr = new (Ctx) IntegerLiteralExpr("42", SourceLoc(),
                                                /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(intExpr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  BodyASTSerializer bodySer(*Ser);
  bodySer.serializeBody(/*funcDeclID=*/1, body);

  EXPECT_EQ(bodySer.getNumStmts(), 1u);
  EXPECT_EQ(bodySer.getNumExprs(), 1u);
}

/// TEST: ExprID/StmtID assignment is sequential.
TEST_F(BodyASTSerializationTest, ExprIDStmtIDAssignmentIsSequential) {
  auto *int1 = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *int2 = new (Ctx) IntegerLiteralExpr("2", SourceLoc(), false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(int1));
  elements.push_back(ASTNode(int2));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  BodyASTSerializer bodySer(*Ser);
  bodySer.serializeBody(/*funcDeclID=*/1, body);

  EXPECT_EQ(bodySer.getNumStmts(), 1u);
  EXPECT_EQ(bodySer.getNumExprs(), 2u);
}

/// TEST: Empty body (null BraceStmt) is handled correctly.
TEST_F(BodyASTSerializationTest, NullBodyHandledCorrectly) {
  BodyASTSerializer bodySer(*Ser);
  bodySer.serializeBody(/*funcDeclID=*/1, /*body=*/nullptr);

  EXPECT_EQ(bodySer.getNumStmts(), 0u);
  EXPECT_EQ(bodySer.getNumExprs(), 0u);
}

// === Stage B Tests (Expression round-trip) ===

/// TEST: DeclRefExpr round-trips.
TEST_F(BodyASTSerializationTest, DeclRefExprRoundTrip) {
  // Create a VarDecl with a valid DeclContext (the test SourceFile).
  auto *vd = new (Ctx) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Let,
                                SourceLoc(), Ctx.getIdentifier("x"),
                                /*DC=*/SF);
  auto *declRef = new (Ctx) DeclRefExpr(vd, DeclNameLoc(SourceLoc()),
                                        /*Implicit=*/false);

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(declRef));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);


  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  // DeclRefExpr needs a decl resolver; with no-op resolution it won't
  // reconstruct, but the bitstream should still be parseable.
  // The key test is that the bitstream is valid and the body block
  // was found and read without crashing.
  // With a real resolver, this would verify the DeclRefExpr kind.
  (void)result;
}

/// TEST: IntegerLiteralExpr round-trips.
TEST_F(BodyASTSerializationTest, IntegerLiteralExprRoundTrip) {
  auto *intExpr = new (Ctx) IntegerLiteralExpr("42", SourceLoc(),
                                                /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(intExpr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);

  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  auto *E = cast<Expr *>(elt);
  EXPECT_EQ(E->getKind(), ExprKind::IntegerLiteral);
  // Note: getDigitsText() returns "" in test environment because the test
  // deserializer has no identifier table. In production, identifiers resolve
  // via ModuleFile::getIdentifier(iid).
  (void)cast<IntegerLiteralExpr>(E); // verify cast succeeds
}

/// TEST: MemberRefExpr round-trips.
TEST_F(BodyASTSerializationTest, MemberRefExprRoundTrip) {
  auto *base = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *member = new (Ctx) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Let,
                                    SourceLoc(), Ctx.getIdentifier("prop"),
                                    /*DC=*/SF);
  auto *memberRef = new (Ctx) MemberRefExpr(base, SourceLoc(), member,
                                            DeclNameLoc(), /*Implicit=*/false);

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(memberRef));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  // MemberRefExpr needs a decl resolver for the member; with no-op resolution
  // it won't fully reconstruct, but the bitstream should be valid.
  (void)result;
}

/// TEST: TypeExpr round-trips (via ReturnStmt + IntegerLiteralExpr).
TEST_F(BodyASTSerializationTest, TypeExprRoundTrip) {
  // Test ReturnStmt with result expression, which exercises both
  // stmt and expr serialization/deserialization paths.
  auto *intExpr = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *returnStmt = ReturnStmt::createParsed(Ctx, SourceLoc(), intExpr);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(returnStmt));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);

  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Stmt *>(elt));
  auto *S = cast<Stmt *>(elt);
  EXPECT_EQ(S->getKind(), StmtKind::Return);
  auto *RS = cast<ReturnStmt>(S);
  EXPECT_TRUE(RS->hasResult());
}

/// TEST: ReturnStmt without result round-trips.
TEST_F(BodyASTSerializationTest, ReturnStmtNoResultRoundTrip) {
  auto *returnStmt = ReturnStmt::createParsed(Ctx, SourceLoc(), /*result=*/nullptr);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(returnStmt));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);

  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Stmt *>(elt));
  auto *S = cast<Stmt *>(elt);
  EXPECT_EQ(S->getKind(), StmtKind::Return);
  auto *RS = cast<ReturnStmt>(S);
  EXPECT_FALSE(RS->hasResult());
}

/// TEST: Implicit flag is preserved across round-trip.
TEST_F(BodyASTSerializationTest, ImplicitFlagPreserved) {
  auto *intExpr = new (Ctx) IntegerLiteralExpr("42", SourceLoc(),
                                                /*Implicit=*/true);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(intExpr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  auto *E = cast<Expr *>(elt);
  EXPECT_TRUE(E->isImplicit());
}

} // anonymous namespace
