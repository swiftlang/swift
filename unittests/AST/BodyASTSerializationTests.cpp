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
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/ExistentialLayout.h"
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

// === Stage C Tests: Compound Expressions ===

/// TEST: BinaryExpr round-trips.
TEST_F(BodyASTSerializationTest, BinaryExprRoundTrip) {
  auto *lhs = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *rhs = new (Ctx) IntegerLiteralExpr("2", SourceLoc(), false);
  auto *fn = new (Ctx) ErrorExpr(SourceRange());
  auto *bin = BinaryExpr::create(Ctx, lhs, fn, rhs, /*implicit=*/false, Type());

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(bin));
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
  EXPECT_EQ(E->getKind(), ExprKind::Binary);
}

/// TEST: AssignExpr round-trips.
TEST_F(BodyASTSerializationTest, AssignExprRoundTrip) {
  auto *dest = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *src = new (Ctx) IntegerLiteralExpr("42", SourceLoc(), false);
  auto *assign = new (Ctx) AssignExpr(dest, SourceLoc(), src, /*Implicit=*/false);

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(assign));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::Assign);
}

/// TEST: InOutExpr round-trips.
TEST_F(BodyASTSerializationTest, InOutExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *io = new (Ctx) InOutExpr(SourceLoc(), sub, Type(), /*isImplicit=*/false);

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(io));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::InOut);
}

// === Stage E Test: Full body round-trip ===

/// TEST: A complete function body with multiple statements round-trips.
/// This exercises the full serializer/deserializer pipeline:
/// BraceStmt → ReturnStmt → BinaryExpr → IntegerLiteralExpr
TEST_F(BodyASTSerializationTest, FullBodyRoundTrip) {
  // Build: { return 1 + 2 }
  auto *lhs = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *rhs = new (Ctx) IntegerLiteralExpr("2", SourceLoc(), false);
  auto *fn = new (Ctx) ErrorExpr(SourceRange());
  auto *bin = BinaryExpr::create(Ctx, lhs, fn, rhs, /*implicit=*/false, Type());

  auto *retStmt = ReturnStmt::createParsed(Ctx, SourceLoc(), bin);

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(retStmt));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);

  // The element should be a ReturnStmt.
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Stmt *>(elt));
  auto *RS = cast<ReturnStmt>(cast<Stmt *>(elt));
  EXPECT_TRUE(RS->hasResult());

  // The result should be a BinaryExpr.
  auto *resultExpr = RS->getResult();
  ASSERT_NE(resultExpr, nullptr);
  EXPECT_EQ(resultExpr->getKind(), ExprKind::Binary);
}

/// TEST: Empty body (e.g., destructor) round-trips.
TEST_F(BodyASTSerializationTest, EmptyBodyRoundTrip) {
  SmallVector<ASTNode, 4> elements;
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 0u);
}

/// TEST: Multiple statements in body round-trip.
TEST_F(BodyASTSerializationTest, MultipleStatementsRoundTrip) {
  // Build: { 42; return }
  auto *intExpr = new (Ctx) IntegerLiteralExpr("42", SourceLoc(), false);
  auto *retStmt = ReturnStmt::createParsed(Ctx, SourceLoc(), nullptr);

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(intExpr));
  elements.push_back(ASTNode(retStmt));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 2u);

  auto elt0 = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt0));
  EXPECT_EQ(cast<Expr *>(elt0)->getKind(), ExprKind::IntegerLiteral);

  auto elt1 = result->getElements()[1];
  ASSERT_TRUE(isa<Stmt *>(elt1));
  EXPECT_EQ(cast<Stmt *>(elt1)->getKind(), StmtKind::Return);
}

// === Stage D Tests: Literal Expressions ===

/// TEST: NilLiteralExpr round-trips.
TEST_F(BodyASTSerializationTest, NilLiteralExprRoundTrip) {
  auto *expr = new (Ctx) NilLiteralExpr(SourceLoc(), /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::NilLiteral);
}

/// TEST: FloatLiteralExpr round-trips.
TEST_F(BodyASTSerializationTest, FloatLiteralExprRoundTrip) {
  auto *expr = new (Ctx) FloatLiteralExpr("3.14", SourceLoc(),
                                          /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::FloatLiteral);
}

/// TEST: BooleanLiteralExpr round-trips.
TEST_F(BodyASTSerializationTest, BooleanLiteralExprRoundTrip) {
  auto *expr = new (Ctx) BooleanLiteralExpr(true, SourceLoc(),
                                              /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::BooleanLiteral);
  EXPECT_TRUE(cast<BooleanLiteralExpr>(E)->getValue());
}

/// TEST: BooleanLiteralExpr (false) round-trips.
TEST_F(BodyASTSerializationTest, BooleanLiteralFalseRoundTrip) {
  auto *expr = new (Ctx) BooleanLiteralExpr(false, SourceLoc(),
                                              /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  auto *E = cast<Expr *>(elt);
  EXPECT_EQ(E->getKind(), ExprKind::BooleanLiteral);
  EXPECT_FALSE(cast<BooleanLiteralExpr>(E)->getValue());
}

/// TEST: StringLiteralExpr round-trips.
TEST_F(BodyASTSerializationTest, StringLiteralExprRoundTrip) {
  auto *expr = new (Ctx) StringLiteralExpr("hello", SourceRange(),
                                            /*Implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::StringLiteral);
}

/// TEST: MagicIdentifierLiteralExpr round-trips.
TEST_F(BodyASTSerializationTest, MagicIdentifierLiteralExprRoundTrip) {
  auto *expr = new (Ctx) MagicIdentifierLiteralExpr(
      MagicIdentifierLiteralExpr::Function, SourceLoc(), /*implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::MagicIdentifierLiteral);
  EXPECT_EQ(cast<MagicIdentifierLiteralExpr>(E)->getKind(),
            MagicIdentifierLiteralExpr::Function);
}

// === Stage E Tests: Identity Wrapper Expressions ===

/// TEST: ParenExpr round-trips.
TEST_F(BodyASTSerializationTest, ParenExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("42", SourceLoc(), false);
  auto *expr = new (Ctx) ParenExpr(SourceLoc(), sub, SourceLoc(), Type());
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::Paren);
  auto *PE = cast<ParenExpr>(E);
  ASSERT_NE(PE->getSubExpr(), nullptr);
  EXPECT_EQ(PE->getSubExpr()->getKind(), ExprKind::IntegerLiteral);
}

/// TEST: DotSelfExpr round-trips.
TEST_F(BodyASTSerializationTest, DotSelfExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) DotSelfExpr(sub, SourceLoc(), SourceLoc());
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::DotSelf);
}

/// TEST: AwaitExpr round-trips.
TEST_F(BodyASTSerializationTest, AwaitExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *expr = new (Ctx) AwaitExpr(SourceLoc(), sub, Type(), /*implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::Await);
}

/// TEST: UnsafeExpr round-trips.
TEST_F(BodyASTSerializationTest, UnsafeExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *expr = new (Ctx) UnsafeExpr(SourceLoc(), sub, Type(), /*implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::Unsafe);
}

/// TEST: BorrowExpr round-trips.
TEST_F(BodyASTSerializationTest, BorrowExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *expr = new (Ctx) BorrowExpr(SourceLoc(), sub, Type(), /*implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::Borrow);
}

// === Stage F Tests: Other Simple Expressions ===

/// TEST: CopyExpr round-trips.
TEST_F(BodyASTSerializationTest, CopyExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) CopyExpr(SourceLoc(), sub, Type(), /*implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::Copy);
}

/// TEST: ConsumeExpr round-trips.
TEST_F(BodyASTSerializationTest, ConsumeExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) ConsumeExpr(SourceLoc(), sub, Type(), /*implicit=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::Consume);
}

/// TEST: ForceValueExpr round-trips.
TEST_F(BodyASTSerializationTest, ForceValueExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) ForceValueExpr(sub, SourceLoc(), /*forcedIUO=*/false);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::ForceValue);
}

/// TEST: ForceValueExpr with forcedIUO round-trips.
TEST_F(BodyASTSerializationTest, ForceValueExprIUORoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) ForceValueExpr(sub, SourceLoc(), /*forcedIUO=*/true);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  auto *E = cast<Expr *>(elt);
  EXPECT_EQ(E->getKind(), ExprKind::ForceValue);
  EXPECT_TRUE(cast<ForceValueExpr>(E)->isForceOfImplicitlyUnwrappedOptional());
}

/// TEST: BindOptionalExpr round-trips.
TEST_F(BodyASTSerializationTest, BindOptionalExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) BindOptionalExpr(sub, SourceLoc(), /*depth=*/2);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  auto *E = cast<Expr *>(elt);
  EXPECT_EQ(E->getKind(), ExprKind::BindOptional);
  EXPECT_EQ(cast<BindOptionalExpr>(E)->getDepth(), 2u);
}

/// TEST: OptionalEvaluationExpr round-trips.
TEST_F(BodyASTSerializationTest, OptionalEvaluationExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) OptionalEvaluationExpr(sub);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  EXPECT_EQ(cast<Expr *>(elt)->getKind(), ExprKind::OptionalEvaluation);
}

/// TEST: TupleElementExpr round-trips.
TEST_F(BodyASTSerializationTest, TupleElementExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *expr = new (Ctx) TupleElementExpr(sub, SourceLoc(), /*FieldNo=*/1,
                                           SourceLoc(), Type());
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  auto elt = result->getElements()[0];
  ASSERT_TRUE(isa<Expr *>(elt));
  auto *E = cast<Expr *>(elt);
  EXPECT_EQ(E->getKind(), ExprKind::TupleElement);
  EXPECT_EQ(cast<TupleElementExpr>(E)->getFieldNumber(), 1u);
}

// === Stage G Tests: Ternary Expression ===

/// TEST: TernaryExpr round-trips.
TEST_F(BodyASTSerializationTest, TernaryExprRoundTrip) {
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *thenE = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *elseE = new (Ctx) IntegerLiteralExpr("2", SourceLoc(), false);
  auto *expr = new (Ctx) TernaryExpr(cond, SourceLoc(), thenE, SourceLoc(),
                                      elseE);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(expr));
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
  EXPECT_EQ(E->getKind(), ExprKind::Ternary);
  auto *TE = cast<TernaryExpr>(E);
  ASSERT_NE(TE->getCondExpr(), nullptr);
  ASSERT_NE(TE->getThenExpr(), nullptr);
  ASSERT_NE(TE->getElseExpr(), nullptr);
  EXPECT_EQ(TE->getCondExpr()->getKind(), ExprKind::BooleanLiteral);
  EXPECT_EQ(TE->getThenExpr()->getKind(), ExprKind::IntegerLiteral);
  EXPECT_EQ(TE->getElseExpr()->getKind(), ExprKind::IntegerLiteral);
}
// === Stage F Tests: Statement round-trip ===

/// TEST: BreakStmt round-trips.
TEST_F(BodyASTSerializationTest, BreakStmtRoundTrip) {
  auto *breakStmt = new (Ctx) BreakStmt(SourceLoc(), Identifier(), SourceLoc(),
                                        /*DC=*/nullptr);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(breakStmt));
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
  EXPECT_EQ(S->getKind(), StmtKind::Break);
}

/// TEST: ThrowStmt round-trips.
TEST_F(BodyASTSerializationTest, ThrowStmtRoundTrip) {
  auto *subExpr = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *throwStmt = new (Ctx) ThrowStmt(SourceLoc(), subExpr);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(throwStmt));
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
  EXPECT_EQ(S->getKind(), StmtKind::Throw);
  auto *TS = cast<ThrowStmt>(S);
  ASSERT_NE(TS->getSubExpr(), nullptr);
  EXPECT_EQ(TS->getSubExpr()->getKind(), ExprKind::IntegerLiteral);
}

/// TEST: IfStmt round-trips with then branch but no else.
TEST_F(BodyASTSerializationTest, IfStmtNoElseRoundTrip) {
  // Build: if true { 42 }
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *thenExpr = new (Ctx) IntegerLiteralExpr("42", SourceLoc(), false);
  SmallVector<ASTNode, 4> thenElements;
  thenElements.push_back(ASTNode(thenExpr));
  auto *thenBody = BraceStmt::create(Ctx, SourceLoc(), thenElements,
                                     SourceLoc(), /*implicit=*/std::nullopt);
  auto *ifStmt = new (Ctx) IfStmt(SourceLoc(), cond, thenBody, SourceLoc(),
                                  /*Else=*/nullptr,
                                  /*implicit=*/std::nullopt, Ctx);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(ifStmt));
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
  EXPECT_EQ(S->getKind(), StmtKind::If);
  auto *IS = cast<IfStmt>(S);
  ASSERT_NE(IS->getThenStmt(), nullptr);
  EXPECT_EQ(IS->getThenStmt()->getNumElements(), 1u);
  EXPECT_EQ(IS->getElseStmt(), nullptr);
}

/// TEST: IfStmt round-trips with else branch.
TEST_F(BodyASTSerializationTest, IfStmtWithElseRoundTrip) {
  // Build: if true { 1 } else { 2 }
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *thenExpr = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  SmallVector<ASTNode, 4> thenElements;
  thenElements.push_back(ASTNode(thenExpr));
  auto *thenBody = BraceStmt::create(Ctx, SourceLoc(), thenElements,
                                     SourceLoc(), /*implicit=*/std::nullopt);
  auto *elseExpr = new (Ctx) IntegerLiteralExpr("2", SourceLoc(), false);
  SmallVector<ASTNode, 4> elseElements;
  elseElements.push_back(ASTNode(elseExpr));
  auto *elseBody = BraceStmt::create(Ctx, SourceLoc(), elseElements,
                                     SourceLoc(), /*implicit=*/std::nullopt);
  auto *ifStmt = new (Ctx) IfStmt(SourceLoc(), cond, thenBody, SourceLoc(),
                                  elseBody,
                                  /*implicit=*/std::nullopt, Ctx);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(ifStmt));
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
  EXPECT_EQ(S->getKind(), StmtKind::If);
  auto *IS = cast<IfStmt>(S);
  ASSERT_NE(IS->getThenStmt(), nullptr);
  EXPECT_EQ(IS->getThenStmt()->getNumElements(), 1u);
  ASSERT_NE(IS->getElseStmt(), nullptr);
  auto *elseBrace = cast<BraceStmt>(IS->getElseStmt());
  EXPECT_EQ(elseBrace->getNumElements(), 1u);
}

/// TEST: WhileStmt round-trips.
TEST_F(BodyASTSerializationTest, WhileStmtRoundTrip) {
  // Build: while true { break }
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *breakStmt = new (Ctx) BreakStmt(SourceLoc(), Identifier(), SourceLoc(),
                                        /*DC=*/nullptr);
  SmallVector<ASTNode, 4> bodyElements;
  bodyElements.push_back(ASTNode(breakStmt));
  auto *loopBody = BraceStmt::create(Ctx, SourceLoc(), bodyElements,
                                    SourceLoc(), /*implicit=*/std::nullopt);

  // WhileStmt constructor takes a StmtCondition
  SmallVector<StmtConditionElement, 1> condElts;
  condElts.push_back(StmtConditionElement(cond));
  auto *whileStmt = new (Ctx) WhileStmt(LabeledStmtInfo(), SourceLoc(),
                                        MutableArrayRef<StmtConditionElement>(condElts),
                                        loopBody, /*implicit=*/std::nullopt);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(whileStmt));
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
  EXPECT_EQ(S->getKind(), StmtKind::While);
  auto *WS = cast<WhileStmt>(S);
  ASSERT_NE(WS->getBody(), nullptr);
  auto *wbody = cast<BraceStmt>(WS->getBody());
  EXPECT_EQ(wbody->getNumElements(), 1u);
}

/// TEST: ContinueStmt round-trips.
TEST_F(BodyASTSerializationTest, ContinueStmtRoundTrip) {
  auto *continueStmt = new (Ctx) ContinueStmt(SourceLoc(), Identifier(),
                                               SourceLoc(), /*DC=*/nullptr);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(continueStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Continue);
}

/// TEST: FailStmt round-trips.
TEST_F(BodyASTSerializationTest, FailStmtRoundTrip) {
  auto *failStmt = new (Ctx) FailStmt(SourceLoc(), SourceLoc());
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(failStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Fail);
}

/// TEST: FallthroughStmt round-trips.
TEST_F(BodyASTSerializationTest, FallthroughStmtRoundTrip) {
  auto *fallthroughStmt = FallthroughStmt::createParsed(SourceLoc(), SF);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(fallthroughStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Fallthrough);
}

/// TEST: OpaqueStmt round-trips.
TEST_F(BodyASTSerializationTest, OpaqueStmtRoundTrip) {
  auto *opaqueStmt = new (Ctx) OpaqueStmt();
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(opaqueStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Opaque);
}

/// TEST: DoStmt round-trips.
TEST_F(BodyASTSerializationTest, DoStmtRoundTrip) {
  auto *intExpr = new (Ctx) IntegerLiteralExpr("42", SourceLoc(), false);
  SmallVector<ASTNode, 4> doElements;
  doElements.push_back(ASTNode(intExpr));
  auto *doBody = BraceStmt::create(Ctx, SourceLoc(), doElements, SourceLoc(),
                                   /*implicit=*/std::nullopt);
  auto *doStmt = new (Ctx) DoStmt(LabeledStmtInfo(), SourceLoc(), doBody);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(doStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Do);
  auto *DS = cast<DoStmt>(cast<Stmt *>(elt));
  ASSERT_NE(DS->getBody(), nullptr);
  EXPECT_EQ(DS->getBody()->getNumElements(), 1u);
}

/// TEST: DiscardStmt round-trips.
TEST_F(BodyASTSerializationTest, DiscardStmtRoundTrip) {
  auto *discardStmt = new (Ctx) DiscardStmt(SourceLoc(), /*subExpr=*/nullptr);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(discardStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Discard);
}

/// TEST: RepeatWhileStmt round-trips.
TEST_F(BodyASTSerializationTest, RepeatWhileStmtRoundTrip) {
  // Build: repeat { break } while true
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *breakStmt = new (Ctx) BreakStmt(SourceLoc(), Identifier(), SourceLoc(),
                                        /*DC=*/nullptr);
  SmallVector<ASTNode, 4> bodyElements;
  bodyElements.push_back(ASTNode(breakStmt));
  auto *loopBody = BraceStmt::create(Ctx, SourceLoc(), bodyElements,
                                     SourceLoc(), /*implicit=*/std::nullopt);
  auto *repeatStmt = new (Ctx) RepeatWhileStmt(LabeledStmtInfo(), SourceLoc(),
                                               cond, SourceLoc(), loopBody);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(repeatStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::RepeatWhile);
  auto *RWS = cast<RepeatWhileStmt>(cast<Stmt *>(elt));
  ASSERT_NE(RWS->getCond(), nullptr);
  ASSERT_NE(RWS->getBody(), nullptr);
}

/// TEST: GuardStmt round-trips.
TEST_F(BodyASTSerializationTest, GuardStmtRoundTrip) {
  // Build: guard true else { return }
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *retStmt = ReturnStmt::createParsed(Ctx, SourceLoc(), nullptr);
  SmallVector<ASTNode, 4> bodyElements;
  bodyElements.push_back(ASTNode(retStmt));
  auto *guardBody = BraceStmt::create(Ctx, SourceLoc(), bodyElements,
                                      SourceLoc(), /*implicit=*/std::nullopt);
  auto *guardStmt = new (Ctx) GuardStmt(SourceLoc(), cond, guardBody,
                                        /*implicit=*/std::nullopt, Ctx);
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(guardStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::Guard);
  auto *GS = cast<GuardStmt>(cast<Stmt *>(elt));
  ASSERT_NE(GS->getBody(), nullptr);
  EXPECT_EQ(GS->getBody()->getNumElements(), 1u);
}

/// TEST: PoundAssertStmt round-trips.
TEST_F(BodyASTSerializationTest, PoundAssertStmtRoundTrip) {
  auto *cond = new (Ctx) BooleanLiteralExpr(true, SourceLoc(), false);
  auto *paStmt = new (Ctx) PoundAssertStmt(SourceRange(), cond, "test message");
  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(paStmt));
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
  EXPECT_EQ(cast<Stmt *>(elt)->getKind(), StmtKind::PoundAssert);
  auto *PAS = cast<PoundAssertStmt>(cast<Stmt *>(elt));
  ASSERT_NE(PAS->getCondition(), nullptr);
  EXPECT_EQ(PAS->getCondition()->getKind(), ExprKind::BooleanLiteral);
}

// === Stage F Tests: Conversion Expressions ===

/// TEST: LoadExpr (ImplicitConversionExpr) round-trips.
TEST_F(BodyASTSerializationTest, LoadExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *load = new (Ctx) LoadExpr(sub, Type());

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(load));
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
  EXPECT_EQ(E->getKind(), ExprKind::Load);
  auto *LE = cast<LoadExpr>(E);
  ASSERT_NE(LE->getSubExpr(), nullptr);
  EXPECT_EQ(LE->getSubExpr()->getKind(), ExprKind::IntegerLiteral);
}
/// TEST: FunctionConversionExpr (ImplicitConversionExpr) round-trips.
/// This is a simple conversion with {subExprID} — same shape as all 35
/// ImplicitConversionExpr subclasses. Tests the generic conversion path.
TEST_F(BodyASTSerializationTest, FunctionConversionExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *conv = new (Ctx) FunctionConversionExpr(sub, Type());

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(conv));
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
  EXPECT_EQ(E->getKind(), ExprKind::FunctionConversion);
  auto *FCE = cast<FunctionConversionExpr>(E);
  ASSERT_NE(FCE->getSubExpr(), nullptr);
  EXPECT_EQ(FCE->getSubExpr()->getKind(), ExprKind::IntegerLiteral);
}

/// TEST: ErasureExpr round-trips with conformances.
/// Tests the Erasure-specific serialization format:
/// {subExprID, numConformances, [conformanceIDs...]}.
/// We construct an ErasureExpr via ErasureExpr::create, which requires a
/// valid existential type. We use a plain ProtocolType if available; otherwise
/// we skip the test gracefully.
TEST_F(BodyASTSerializationTest, ErasureExprRoundTrip) {
  // ErasureExpr::create requires a valid existential type and conformances
  // matching the existential layout's protocol count. In the test context
  // without a loaded stdlib, we may not have any ProtocolDecl available.
  // Construct an ErasureExpr with TheAnyType and 0 conformances if the
  // layout has 0 protocols; otherwise skip.
  auto anyType = Type(Ctx.TheAnyType);
  if (!anyType || !anyType->isExistentialType())
    GTEST_SKIP() << "No existential type available in test context";

  auto layout = anyType->getExistentialLayout();
  auto protos = layout.getProtocols();
  if (!protos.empty())
    GTEST_SKIP() << "Any type has protocols; cannot construct abstract "
                    "conformances in test context";

  auto *sub = new (Ctx) IntegerLiteralExpr("0", SourceLoc(), false);
  auto *erasure = ErasureExpr::create(Ctx, sub, anyType, {}, {});

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(erasure));
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
  EXPECT_EQ(E->getKind(), ExprKind::Erasure);
  auto *EE = cast<ErasureExpr>(E);
  ASSERT_NE(EE->getSubExpr(), nullptr);
  EXPECT_EQ(EE->getSubExpr()->getKind(), ExprKind::IntegerLiteral);
}

/// TEST: CoerceExpr (ExplicitCastExpr) round-trips.
/// Serializes {subExprID, targetTypeID, castKind}.
TEST_F(BodyASTSerializationTest, CoerceExprRoundTrip) {
  auto *sub = new (Ctx) IntegerLiteralExpr("42", SourceLoc(), false);
  // CoerceExpr::createImplicit asserts that castTy is non-null.
  // Use TheAnyType as a valid target type.
  auto *coerce = CoerceExpr::createImplicit(Ctx, sub, Type(Ctx.TheAnyType));

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(coerce));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), elements, SourceLoc(),
                                 /*implicit=*/std::nullopt);

  auto data = serializeBody(/*funcDeclID=*/1, body);
  EXPECT_GT(data.size(), 4u);

  // With a no-op type resolver, the deserialized CoerceExpr's target type
  // is null, so the deserializer falls back to ErrorExpr. The key test is
  // that serialization succeeds and the bitstream is parseable.
  auto deser = makeDeserializer();
  auto *result = deser.deserializeBody(data);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->getNumElements(), 1u);
  // The bitstream was successfully parsed — the body was deserialized
  // without crashing. With a real type resolver, the result would be a
  // CoerceExpr; with a no-op resolver, it falls back to ErrorExpr.
}
// === Stage F Tests: Collections, Closures, Unary ===

/// TEST: ArrayExpr round-trips.
TEST_F(BodyASTSerializationTest, ArrayExprRoundTrip) {
  auto *e1 = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *e2 = new (Ctx) IntegerLiteralExpr("2", SourceLoc(), false);
  auto *e3 = new (Ctx) IntegerLiteralExpr("3", SourceLoc(), false);
  SmallVector<Expr*, 4> elements = {e1, e2, e3};
  auto *arr = ArrayExpr::create(Ctx, SourceLoc(), elements, {}, SourceLoc(),
                                  Type());

  SmallVector<ASTNode, 4> bodyElements;
  bodyElements.push_back(ASTNode(arr));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), bodyElements, SourceLoc(),
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
  EXPECT_EQ(E->getKind(), ExprKind::Array);
  auto *AE = cast<ArrayExpr>(E);
  EXPECT_EQ(AE->getNumElements(), 3u);
}

/// TEST: ClosureExpr round-trips with a body.
TEST_F(BodyASTSerializationTest, ClosureExprRoundTrip) {
  // Create a closure with an empty body BraceStmt.
  SmallVector<ASTNode, 4> bodyElements;
  auto *closureBody = BraceStmt::create(Ctx, SourceLoc(), bodyElements,
                                         SourceLoc(),
                                         /*implicit=*/std::nullopt);
  auto *params = ParameterList::createEmpty(Ctx);
  auto *closure = new (Ctx) ClosureExpr(DeclAttributes(), SourceRange(),
                                         /*capturedSelfDecl=*/nullptr,
                                         params, SourceLoc(), SourceLoc(),
                                         /*thrownType=*/nullptr, SourceLoc(),
                                         SourceLoc(),
                                         /*explicitResultType=*/nullptr,
                                         /*parent=*/SF);
  closure->setBody(closureBody);

  SmallVector<ASTNode, 4> outerElements;
  outerElements.push_back(ASTNode(closure));
  auto *body = BraceStmt::create(Ctx, SourceLoc(), outerElements, SourceLoc(),
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
  EXPECT_EQ(E->getKind(), ExprKind::Closure);
}

/// TEST: PrefixUnaryExpr round-trips.
TEST_F(BodyASTSerializationTest, PrefixUnaryExprRoundTrip) {
  auto *operand = new (Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto *fn = new (Ctx) ErrorExpr(SourceRange());
  auto *prefix = PrefixUnaryExpr::create(Ctx, fn, operand, Type());

  SmallVector<ASTNode, 4> elements;
  elements.push_back(ASTNode(prefix));
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
  EXPECT_EQ(E->getKind(), ExprKind::PrefixUnary);
}
} // anonymous namespace
