//===--- TypeMatchTests.cpp - Tests for TypeBase::matches -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

TEST(TypeMatch, IdenticalTypes) {
  TestContext C;

  auto check = [](Type ty) {
    return ty->matches(ty, TypeMatchOptions()) &&
           ty->matches(ty, TypeMatchFlags::AllowOverride);
  };

  EXPECT_TRUE(check(C.Ctx.TheEmptyTupleType));
  EXPECT_TRUE(check(C.Ctx.TheRawPointerType));

  Type voidToVoidFn = FunctionType::get({}, C.Ctx.TheEmptyTupleType);
  EXPECT_TRUE(check(voidToVoidFn));

  Type ptrToPtrFn = FunctionType::get({}, C.Ctx.TheRawPointerType);
  EXPECT_TRUE(check(ptrToPtrFn));

  auto *someStruct = C.makeNominal<StructDecl>("MyStruct");
  Type structTy = someStruct->getDeclaredInterfaceType();
  EXPECT_TRUE(check(structTy));

  Type structToStructFn = FunctionType::get(
      {FunctionType::Param(structTy)}, structTy);
  EXPECT_TRUE(check(structToStructFn));
}

TEST(TypeMatch, UnrelatedTypes) {
  TestContext C;

  auto check = [](Type base, Type derived) {
    return derived->matches(base, TypeMatchOptions()) &&
           derived->matches(base, TypeMatchFlags::AllowOverride);
  };

  EXPECT_FALSE(check(C.Ctx.TheEmptyTupleType, C.Ctx.TheRawPointerType));
  EXPECT_FALSE(check(C.Ctx.TheRawPointerType, C.Ctx.TheEmptyTupleType));

  Type voidToVoidFn = FunctionType::get({}, C.Ctx.TheEmptyTupleType);
  EXPECT_FALSE(check(voidToVoidFn, C.Ctx.TheEmptyTupleType));
  EXPECT_FALSE(check(C.Ctx.TheEmptyTupleType, voidToVoidFn));

  Type ptrToPtrFn = FunctionType::get({}, C.Ctx.TheRawPointerType);
  EXPECT_FALSE(check(ptrToPtrFn, voidToVoidFn));
  EXPECT_FALSE(check(voidToVoidFn, ptrToPtrFn));

  auto *someStruct = C.makeNominal<StructDecl>("MyStruct");
  Type structTy = someStruct->getDeclaredInterfaceType();
  EXPECT_FALSE(check(structTy, C.Ctx.TheEmptyTupleType));
  EXPECT_FALSE(check(C.Ctx.TheEmptyTupleType, structTy));
  EXPECT_FALSE(check(structTy, voidToVoidFn));
  EXPECT_FALSE(check(voidToVoidFn, structTy));

  Type structToStructFn = FunctionType::get(
      FunctionType::Param(structTy), structTy);
  EXPECT_FALSE(check(structToStructFn, structTy));
  EXPECT_FALSE(check(structTy, structToStructFn));
  EXPECT_FALSE(check(structToStructFn, voidToVoidFn));
  EXPECT_FALSE(check(voidToVoidFn, structToStructFn));

  auto *anotherStruct = C.makeNominal<StructDecl>("AnotherStruct");
  Type anotherStructTy = anotherStruct->getDeclaredInterfaceType();
  EXPECT_FALSE(check(structTy, anotherStructTy));
  EXPECT_FALSE(check(anotherStructTy, structTy));

  Type anotherStructToAnotherStructFn = FunctionType::get(
      FunctionType::Param(anotherStructTy),
      anotherStructTy);
  EXPECT_FALSE(check(anotherStructToAnotherStructFn, structToStructFn));
  EXPECT_FALSE(check(structToStructFn, anotherStructToAnotherStructFn));

  Type S2ASFn = FunctionType::get(
      FunctionType::Param(structTy),
      anotherStructTy);
  EXPECT_FALSE(check(S2ASFn, structToStructFn));
  EXPECT_FALSE(check(structToStructFn, S2ASFn));
  EXPECT_FALSE(check(S2ASFn, anotherStructToAnotherStructFn));
  EXPECT_FALSE(check(anotherStructToAnotherStructFn, S2ASFn));
}

TEST(TypeMatch, Classes) {
  TestContext C;

  auto check = [](Type base, Type derived) {
    return derived->matches(base, TypeMatchFlags::AllowOverride) &&
           !derived->matches(base, TypeMatchOptions());
  };

  auto *baseClass = C.makeNominal<ClassDecl>("Base");
  Type baseTy = baseClass->getDeclaredInterfaceType();

  auto *subClass = C.makeNominal<ClassDecl>("Sub");
  subClass->setSuperclass(baseTy);
  Type subTy = subClass->getDeclaredInterfaceType();

  EXPECT_TRUE(check(baseTy, subTy));
  EXPECT_FALSE(check(subTy, baseTy));

  auto *otherClass = C.makeNominal<ClassDecl>("Other");
  Type otherTy = otherClass->getDeclaredInterfaceType();
  EXPECT_FALSE(check(otherTy, baseTy));
  EXPECT_FALSE(check(baseTy, otherTy));
  EXPECT_FALSE(check(otherTy, subTy));
  EXPECT_FALSE(check(subTy, otherTy));

  Type baseToVoid = FunctionType::get(
      FunctionType::Param(baseTy),
      C.Ctx.TheEmptyTupleType);
  Type subToVoid = FunctionType::get(
      FunctionType::Param(subTy),
      C.Ctx.TheEmptyTupleType);
  EXPECT_FALSE(check(baseToVoid, subToVoid));
  EXPECT_TRUE(check(subToVoid, baseToVoid));

  Type voidToBase = FunctionType::get({}, baseTy);
  Type voidToSub = FunctionType::get({}, subTy);
  EXPECT_FALSE(check(voidToSub, voidToBase));
  EXPECT_TRUE(check(voidToBase, voidToSub));

  Type baseToBase = FunctionType::get(
      FunctionType::Param(baseTy), baseTy);
  Type subToSub = FunctionType::get(
      FunctionType::Param(subTy), subTy);
  EXPECT_FALSE(check(baseToBase, subToSub));
  EXPECT_FALSE(check(subToSub, baseToBase));
}

TEST(TypeMatch, Optionals) {
  TestContext C{DeclareOptionalTypes};

  auto check = [](Type base, Type derived) {
    return derived->matches(base, TypeMatchFlags::AllowOverride) &&
           !derived->matches(base, TypeMatchOptions());
  };

  auto *baseClass = C.makeNominal<ClassDecl>("Base");
  Type baseTy = baseClass->getDeclaredInterfaceType();
  Type optTy = OptionalType::get(baseTy);

  EXPECT_FALSE(check(baseTy, optTy));
  EXPECT_TRUE(check(optTy, baseTy));

  Type baseToVoid = FunctionType::get(
      FunctionType::Param(baseTy),
      C.Ctx.TheEmptyTupleType);
  Type optToVoid = FunctionType::get(
      FunctionType::Param(optTy),
      C.Ctx.TheEmptyTupleType);
  EXPECT_TRUE(check(baseToVoid, optToVoid));
  EXPECT_FALSE(check(optToVoid, baseToVoid));

  Type voidToBase = FunctionType::get({}, baseTy);
  Type voidToOpt = FunctionType::get({}, optTy);
  EXPECT_FALSE(check(voidToBase, voidToOpt));
  EXPECT_TRUE(check(voidToOpt, voidToBase));

  Type baseToBase = FunctionType::get(
      FunctionType::Param(baseTy),
      baseTy);
  Type optToOpt = FunctionType::get(
      FunctionType::Param(optTy),
      optTy);
  EXPECT_FALSE(check(baseToBase, optToOpt));
  EXPECT_FALSE(check(optToOpt, baseToBase));
}

TEST(TypeMatch, OptionalMismatch) {
  TestContext C{DeclareOptionalTypes};

  auto check = [](Type base, Type derived) {
    return derived->matches(base, TypeMatchFlags::AllowOverride) &&
           !derived->matches(base, TypeMatchOptions());
  };
  auto checkOpt = [](Type base, Type derived) {
    return derived->matches(base,
                            TypeMatchFlags::AllowTopLevelOptionalMismatch);
  };
  auto checkOptOverride = [](Type base, Type derived) {
    TypeMatchOptions matchMode = TypeMatchFlags::AllowOverride;
    matchMode |= TypeMatchFlags::AllowTopLevelOptionalMismatch;
    return derived->matches(base, matchMode);
  };

  auto *baseClass = C.makeNominal<ClassDecl>("Base");
  Type baseTy = baseClass->getDeclaredInterfaceType();
  Type optTy = OptionalType::get(baseTy);

  Type baseToVoid = FunctionType::get(
      FunctionType::Param(baseTy),
      C.Ctx.TheEmptyTupleType);
  Type optToVoid = FunctionType::get(
      FunctionType::Param(optTy),
      C.Ctx.TheEmptyTupleType);
  EXPECT_TRUE(check(baseToVoid, optToVoid));
  EXPECT_TRUE(checkOpt(baseToVoid, optToVoid));
  EXPECT_TRUE(checkOptOverride(baseToVoid, optToVoid));
  EXPECT_FALSE(check(optToVoid, baseToVoid));
  EXPECT_TRUE(checkOpt(optToVoid, baseToVoid));
  EXPECT_TRUE(checkOptOverride(optToVoid, baseToVoid));

  Type voidToBase = FunctionType::get({}, baseTy);
  Type voidToOpt = FunctionType::get({}, optTy);
  EXPECT_FALSE(check(voidToBase, voidToOpt));
  EXPECT_TRUE(checkOpt(voidToBase, voidToOpt));
  EXPECT_TRUE(checkOptOverride(voidToBase, voidToOpt));
  EXPECT_TRUE(check(voidToOpt, voidToBase));
  EXPECT_TRUE(checkOpt(voidToOpt, voidToBase));
  EXPECT_TRUE(checkOptOverride(voidToOpt, voidToBase));

  Type baseToBase = FunctionType::get(
      FunctionType::Param(baseTy),
      baseTy);
  Type optToOpt = FunctionType::get(
      FunctionType::Param(optTy),
      optTy);
  EXPECT_FALSE(check(baseToBase, optToOpt));
  EXPECT_TRUE(checkOpt(baseToBase, optToOpt));
  EXPECT_TRUE(checkOptOverride(baseToBase, optToOpt));
  EXPECT_FALSE(check(optToOpt, baseToBase));
  EXPECT_TRUE(checkOpt(optToOpt, baseToBase));
  EXPECT_TRUE(checkOptOverride(optToOpt, baseToBase));

  auto *subClass = C.makeNominal<ClassDecl>("Sub");
  subClass->setSuperclass(baseTy);
  Type subTy = subClass->getDeclaredInterfaceType();
  Type optSubTy = OptionalType::get(subTy);

  EXPECT_FALSE(check(baseTy, optSubTy));
  EXPECT_FALSE(checkOpt(baseTy, optSubTy));
  EXPECT_TRUE(checkOptOverride(baseTy, optSubTy));
  EXPECT_TRUE(check(optTy, subTy));
  EXPECT_FALSE(checkOpt(optTy, subTy));
  EXPECT_TRUE(checkOptOverride(optTy, subTy));
  EXPECT_TRUE(check(optTy, optSubTy));
  EXPECT_FALSE(checkOpt(optTy, optSubTy));
  EXPECT_TRUE(checkOptOverride(optTy, optSubTy));

  EXPECT_FALSE(check(optSubTy, baseTy));
  EXPECT_FALSE(checkOpt(optSubTy, baseTy));
  EXPECT_FALSE(checkOptOverride(optSubTy, baseTy));
  EXPECT_FALSE(check(subTy, optTy));
  EXPECT_FALSE(checkOpt(subTy, optTy));
  EXPECT_FALSE(checkOptOverride(subTy, optTy));
  EXPECT_FALSE(check(optSubTy, optTy));
  EXPECT_FALSE(checkOpt(optSubTy, optTy));
  EXPECT_FALSE(checkOptOverride(optSubTy, optTy));
}

TEST(TypeMatch, OptionalMismatchTuples) {
  TestContext C{DeclareOptionalTypes};

  auto checkOverride = [](Type base, Type derived) {
    return derived->matches(base, TypeMatchFlags::AllowOverride) &&
           !derived->matches(base, TypeMatchOptions());
  };

  auto *baseClass = C.makeNominal<ClassDecl>("Base");
  Type baseTy = baseClass->getDeclaredInterfaceType();
  Type optTy = OptionalType::get(baseTy);

  Type baseBaseTuple = TupleType::get({baseTy, baseTy}, C.Ctx);
  Type optOptTuple = TupleType::get({optTy, optTy}, C.Ctx);
  Type baseOptTuple = TupleType::get({baseTy, optTy}, C.Ctx);
  Type optBaseTuple = TupleType::get({optTy, baseTy}, C.Ctx);

  EXPECT_FALSE(checkOverride(baseBaseTuple, optOptTuple));
  EXPECT_FALSE(checkOverride(baseBaseTuple, baseOptTuple));
  EXPECT_FALSE(checkOverride(baseBaseTuple, optBaseTuple));

  EXPECT_TRUE(checkOverride(optOptTuple, baseBaseTuple));
  EXPECT_TRUE(checkOverride(optOptTuple, baseOptTuple));
  EXPECT_TRUE(checkOverride(optOptTuple, optBaseTuple));

  EXPECT_TRUE(checkOverride(baseOptTuple, baseBaseTuple));
  EXPECT_FALSE(checkOverride(baseOptTuple, optOptTuple));
  EXPECT_FALSE(checkOverride(baseOptTuple, optBaseTuple));

  EXPECT_TRUE(checkOverride(optBaseTuple, baseBaseTuple));
  EXPECT_FALSE(checkOverride(optBaseTuple, optOptTuple));
  EXPECT_FALSE(checkOverride(optBaseTuple, baseOptTuple));

  auto checkOpt = [](Type base, Type derived) {
    return derived->matches(base,
                            TypeMatchFlags::AllowTopLevelOptionalMismatch);
  };

  EXPECT_TRUE(checkOpt(baseBaseTuple, optOptTuple));
  EXPECT_TRUE(checkOpt(baseBaseTuple, baseOptTuple));
  EXPECT_TRUE(checkOpt(baseBaseTuple, optBaseTuple));

  EXPECT_TRUE(checkOpt(optOptTuple, baseBaseTuple));
  EXPECT_TRUE(checkOpt(optOptTuple, baseOptTuple));
  EXPECT_TRUE(checkOpt(optOptTuple, optBaseTuple));

  EXPECT_TRUE(checkOpt(baseOptTuple, baseBaseTuple));
  EXPECT_TRUE(checkOpt(baseOptTuple, optOptTuple));
  EXPECT_TRUE(checkOpt(baseOptTuple, optBaseTuple));

  EXPECT_TRUE(checkOpt(optBaseTuple, baseBaseTuple));
  EXPECT_TRUE(checkOpt(optBaseTuple, optOptTuple));
  EXPECT_TRUE(checkOpt(optBaseTuple, baseOptTuple));

  Type optOfTuple = OptionalType::get(baseBaseTuple);
  EXPECT_TRUE(checkOverride(optOfTuple, baseBaseTuple));
  EXPECT_FALSE(checkOverride(baseBaseTuple, optOfTuple));
  EXPECT_TRUE(checkOpt(optOfTuple, baseBaseTuple));
  EXPECT_TRUE(checkOpt(baseBaseTuple, optOfTuple));
}

TEST(TypeMatch, OptionalMismatchFunctions) {
  TestContext C{DeclareOptionalTypes};

  auto checkOverride = [](Type base, Type derived) {
    return derived->matches(base, TypeMatchFlags::AllowOverride) &&
           !derived->matches(base, TypeMatchOptions());
  };
  auto checkOpt = [](Type base, Type derived) {
    return derived->matches(base,
                            TypeMatchFlags::AllowTopLevelOptionalMismatch);
  };

  Type voidToVoid = FunctionType::get({}, C.Ctx.TheEmptyTupleType);
  Type optVoidToVoid = OptionalType::get(voidToVoid);
  EXPECT_TRUE(checkOverride(optVoidToVoid, voidToVoid));
  EXPECT_TRUE(checkOpt(optVoidToVoid, voidToVoid));
  EXPECT_FALSE(checkOverride(voidToVoid, optVoidToVoid));
  EXPECT_TRUE(checkOpt(voidToVoid, optVoidToVoid));
}

TEST(TypeMatch, NoEscapeMismatchFunctions) {
  TestContext C{DeclareOptionalTypes};

  // Note the reversed names here: parameters must be contravariant for the
  // functions that take them to be covariant.
  auto checkOverride = [](Type paramOfDerived, Type paramOfBase) {
    return paramOfBase->matches(paramOfDerived, TypeMatchFlags::AllowOverride);
  };
  auto checkMismatch = [](Type paramOfDerived, Type paramOfBase) {
    return paramOfBase->matches(
        paramOfDerived,
        TypeMatchFlags::IgnoreNonEscapingForOptionalFunctionParam);
  };

  Type voidToVoidFn = FunctionType::get({}, C.Ctx.TheEmptyTupleType);
  Type nonescapingVoidToVoidFn =
      FunctionType::get({}, C.Ctx.TheEmptyTupleType,
                        FunctionType::ExtInfo().withNoEscape());
  Type optVoidToVoidFn = OptionalType::get(voidToVoidFn);
  Type optNonescapingVoidToVoidFn = OptionalType::get(nonescapingVoidToVoidFn);

  EXPECT_FALSE(checkOverride(nonescapingVoidToVoidFn, voidToVoidFn));
  EXPECT_FALSE(checkMismatch(nonescapingVoidToVoidFn, voidToVoidFn));
  EXPECT_FALSE(checkOverride(voidToVoidFn, nonescapingVoidToVoidFn));
  EXPECT_FALSE(checkMismatch(voidToVoidFn, nonescapingVoidToVoidFn));

  EXPECT_FALSE(checkOverride(nonescapingVoidToVoidFn, optVoidToVoidFn));
  EXPECT_FALSE(checkMismatch(nonescapingVoidToVoidFn, optVoidToVoidFn));
  EXPECT_FALSE(checkOverride(optVoidToVoidFn, nonescapingVoidToVoidFn));
  EXPECT_FALSE(checkMismatch(optVoidToVoidFn, nonescapingVoidToVoidFn));

  EXPECT_FALSE(checkOverride(optNonescapingVoidToVoidFn, voidToVoidFn));
  EXPECT_FALSE(checkMismatch(optNonescapingVoidToVoidFn, voidToVoidFn));
  EXPECT_FALSE(checkOverride(voidToVoidFn, optNonescapingVoidToVoidFn));
  EXPECT_FALSE(checkMismatch(voidToVoidFn, optNonescapingVoidToVoidFn));

  EXPECT_FALSE(checkOverride(optNonescapingVoidToVoidFn, optVoidToVoidFn));
  EXPECT_FALSE(checkMismatch(optNonescapingVoidToVoidFn, optVoidToVoidFn));
  EXPECT_FALSE(checkOverride(optVoidToVoidFn, optNonescapingVoidToVoidFn));
  EXPECT_TRUE(checkMismatch(optVoidToVoidFn, optNonescapingVoidToVoidFn));
}
