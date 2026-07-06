//===--- GenericSignatureTests.cpp - Tests for generic signature --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Regression tests for the AST cache generic_signature fix.
//
// The per-file AST cache deserializes protocols without computing the generic
// signature (it's lazy). The SnapshotDeserializer force-computes
// getGenericSignature() during deserialization so that the ASTDumper renders
// "generic_signature" instead of "parsed_generic_params".
//
// These tests verify that GenericContext::hasComputedGenericSignature()
// correctly reports false before the signature is set, and true after.
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {

TEST(GenericSignatureTest, StructNotComputedByDefault) {
  TestContext C;

  auto *structural = C.makeNominal<StructDecl>("MyStruct");

  // A freshly-created struct has not computed its generic signature.
  EXPECT_FALSE(structural->hasComputedGenericSignature())
      << "StructDecl should not have a computed generic signature before "
         "setGenericSignature() is called";
}

TEST(GenericSignatureTest, StructComputedAfterSet) {
  TestContext C;

  auto *structural = C.makeNominal<StructDecl>("MyStruct");

  // Setting a null generic signature still marks it as "computed" — this
  // mirrors what getGenericSignature() does when it resolves to an empty
  // signature for a non-generic context.
  structural->setGenericSignature(GenericSignature());

  EXPECT_TRUE(structural->hasComputedGenericSignature())
      << "StructDecl should have a computed generic signature after "
         "setGenericSignature() is called";
}

} // end anonymous namespace
