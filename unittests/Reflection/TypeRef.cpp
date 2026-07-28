//===--- TypeRef.cpp - TypeRef tests --------------------------------------===//
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

#include "swift/RemoteInspection/TypeRefBuilder.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Demangling/Demangler.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace reflection;

static const std::string ABC = "ABC";
static const std::string ABCD = "ABCD";
static const std::string XYZ = "XYZ";
static const std::string Empty = "";
static const std::string MyClass = "MyClass";
static const std::string NotMyClass = "NotMyClass";
static const std::string MyModule = "MyModule";
static const std::string Shmodule = "Shmodule";
static const std::string MyProtocol = "MyProtocol";
static const std::string Shmrotocol = "Shmrotocol";

static const TypeRefDecl ABC_decl = {"ABC"};
static const TypeRefDecl ABCD_decl = {"ABCD"};
static const TypeRefDecl XYZ_decl = {"XYZ"};
static const TypeRefDecl Empty_decl = {""};
static const TypeRefDecl MyClass_decl = {"MyClass"};
static const TypeRefDecl NotMyClass_decl = {"NotMyClass"};
static const TypeRefDecl MyModule_decl = {"MyModule"};
static const TypeRefDecl Shmodule_decl = {"Shmodule"};
static const TypeRefDecl MyProtocol_decl = {"MyProtocol"};
static const TypeRefDecl Shmrotocol_decl = {"Shmrotocol"};

using Param = remote::FunctionParam<const TypeRef *>;

TEST(TypeRefTest, UniqueBuiltinTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto BI1 = Builder.createBuiltinType(ABC, ABC);
  auto BI2 = Builder.createBuiltinType(ABC, ABC);
  auto BI3 = Builder.createBuiltinType(ABCD, ABCD);

  EXPECT_EQ(BI1, BI2);
  EXPECT_NE(BI2, BI3);
}

TEST(TypeRefTest, UniqueNominalTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(ABC_decl, nullptr);
  auto N2 = Builder.createNominalType(ABC_decl, nullptr);
  auto N3 = Builder.createNominalType(ABCD_decl, nullptr);

  EXPECT_EQ(N1, N2);
  EXPECT_NE(N2, N3);

  auto N4 = Builder.createNominalType(ABC_decl, N1);
  auto N5 = Builder.createNominalType(ABC_decl, N1);

  EXPECT_EQ(N4, N5);
}

TEST(TypeRefTest, UniqueBoundGenericTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto GTP00 = Builder.createGenericTypeParameterType(0, 0);
  auto GTP01 = Builder.createGenericTypeParameterType(0, 1);

  auto BG1 = Builder.createBoundGenericType(ABC_decl, {}, nullptr);
  auto BG2 = Builder.createBoundGenericType(ABC_decl, {}, nullptr);
  auto BG3 = Builder.createBoundGenericType(ABCD_decl, {}, nullptr);

  EXPECT_EQ(BG1, BG2);
  EXPECT_NE(BG2, BG3);

  std::vector<const TypeRef *> GenericParams { GTP00, GTP01 };

  auto BG4 = Builder.createBoundGenericType(ABC_decl, GenericParams, nullptr);
  auto BG5 = Builder.createBoundGenericType(ABC_decl, GenericParams, nullptr);
  auto BG6 = Builder.createBoundGenericType(ABCD_decl, GenericParams, nullptr);

  EXPECT_EQ(BG4, BG5);
  EXPECT_NE(BG5, BG6);
  EXPECT_NE(BG5, BG1);
}

TEST(TypeRefTest, UniqueTupleTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(ABC_decl, nullptr);
  auto N2 = Builder.createNominalType(XYZ_decl, nullptr);

  std::vector<const TypeRef *> Void;
  auto Void1 = Builder.createTupleType(Void, ArrayRef<StringRef>());
  auto Void2 = Builder.createTupleType(Void, ArrayRef<StringRef>());

  EXPECT_EQ(Void1, Void2);

  std::vector<const TypeRef *> Elements1 { N1, N2 };
  std::vector<const TypeRef *> Elements2 { N1, N2, N2 };

  auto T1 = Builder.createTupleType(Elements1, ArrayRef<StringRef>());
  auto T2 = Builder.createTupleType(Elements1, ArrayRef<StringRef>());
  auto T3 = Builder.createTupleType(Elements2, ArrayRef<StringRef>());

  EXPECT_EQ(T1, T2);
  EXPECT_NE(T2, T3);
  EXPECT_NE(T1, Void1);
}

TEST(TypeRefTest, UniqueFunctionTypeRef) {

  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  std::vector<const TypeRef *> Void;
  auto VoidResult = Builder.createTupleType(Void, ArrayRef<StringRef>());
  Param Param1 = Builder.createNominalType(ABC_decl, nullptr);
  Param Param2 = Builder.createNominalType(XYZ_decl, nullptr);

  std::vector<Param> VoidParams;
  std::vector<Param> Parameters1{Param1, Param2};
  std::vector<Param> Parameters2{Param1, Param1};

  auto Result =
      Builder.createTupleType({Param1.getType(), Param2.getType()},
                              ArrayRef<StringRef>());

  auto F1 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags(), ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F2 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags(), ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F3 = Builder.createFunctionType(
      Parameters2, Result, FunctionTypeFlags(), ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);

  EXPECT_EQ(F1, F2);
  EXPECT_NE(F2, F3);

  auto F4 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withThrows(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F5 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withThrows(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);

  EXPECT_EQ(F4, F5);
  EXPECT_NE(F4, F1);

  // Test parameter with and without inout/shared/variadic and/or label.
  ParameterFlags paramFlags;
  auto inoutFlags = paramFlags.withOwnership(ParameterOwnership::InOut);
  auto variadicFlags = paramFlags.withVariadic(true);
  auto sharedFlags = paramFlags.withOwnership(ParameterOwnership::Shared);
  auto ownedFlags = paramFlags.withOwnership(ParameterOwnership::Owned);

  auto F6 = Builder.createFunctionType(
      {Param1.withFlags(inoutFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F6_1 = Builder.createFunctionType(
      {Param1.withFlags(inoutFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_EQ(F6, F6_1);

  auto F7 = Builder.createFunctionType(
      {Param1.withFlags(variadicFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F7_1 = Builder.createFunctionType(
      {Param1.withFlags(variadicFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_EQ(F7, F7_1);

  auto F8 = Builder.createFunctionType(
      {Param1.withFlags(sharedFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F8_1 = Builder.createFunctionType(
      {Param1.withFlags(sharedFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_EQ(F8, F8_1);

  auto F9 = Builder.createFunctionType(
      {Param1.withFlags(ownedFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F9_1 = Builder.createFunctionType(
      {Param1.withFlags(ownedFlags)}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_EQ(F9, F9_1);

  auto F10 = Builder.createFunctionType(
      {Param1}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F10_1 = Builder.createFunctionType(
      {Param1.withLabel("foo")}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_NE(F10, F10_1);

  EXPECT_NE(F6, F7);
  EXPECT_NE(F6, F8);
  EXPECT_NE(F6, F9);
  EXPECT_NE(F6, F10);
  EXPECT_NE(F7, F8);
  EXPECT_NE(F7, F9);
  EXPECT_NE(F7, F10);
  EXPECT_NE(F8, F9);
  EXPECT_NE(F8, F10);
  EXPECT_NE(F9, F10);

  auto VoidVoid1 =
      Builder.createFunctionType(VoidParams, VoidResult, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto VoidVoid2 =
      Builder.createFunctionType(VoidParams, VoidResult, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);

  EXPECT_EQ(VoidVoid1, VoidVoid2);
  EXPECT_NE(VoidVoid1, F1);

  // Test escaping.
  auto F11 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withEscaping(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F12 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withEscaping(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F13 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withEscaping(false),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_EQ(F11, F12);
  EXPECT_NE(F11, F13);

  // Test sendable.
  auto F14 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withSendable(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F15 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withSendable(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  auto F16 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withSendable(false),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);
  EXPECT_EQ(F14, F15);
  EXPECT_NE(F14, F16);

  // Test differentiable.
  auto F17 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withDifferentiable(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::Reverse, nullptr, nullptr);
  auto F18 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withDifferentiable(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::Reverse, nullptr, nullptr);
  auto F19 = Builder.createFunctionType(
      Parameters1, Result, FunctionTypeFlags().withDifferentiable(false),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::Reverse, nullptr, nullptr);
  EXPECT_EQ(F17, F18);
  EXPECT_NE(F17, F19);

  // Test differentiable with @noDerivative.
  {
    auto parameters = Parameters1;
    parameters[1].setNoDerivative();
    auto f1 = Builder.createFunctionType(
        parameters, Result, FunctionTypeFlags().withDifferentiable(true),
        ExtendedFunctionTypeFlags(),
        FunctionMetadataDifferentiabilityKind::Reverse, nullptr, nullptr);
    auto f2 = Builder.createFunctionType(
        parameters, Result, FunctionTypeFlags().withDifferentiable(true),
        ExtendedFunctionTypeFlags(),
        FunctionMetadataDifferentiabilityKind::Reverse, nullptr, nullptr);
    auto f3 = Builder.createFunctionType(
        Parameters1, Result, FunctionTypeFlags().withDifferentiable(true),
        ExtendedFunctionTypeFlags(),
        FunctionMetadataDifferentiabilityKind::Reverse, nullptr, nullptr);
    EXPECT_EQ(f1, f2);
    EXPECT_NE(f1, f3);
  }
}

TEST(TypeRefTest, UniqueProtocolTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  TypeRefBuilder::BuiltProtocolDecl P1 = std::make_pair(ABC, false);
  TypeRefBuilder::BuiltProtocolDecl P2 = std::make_pair(ABC, false);
  TypeRefBuilder::BuiltProtocolDecl P3 = std::make_pair(ABCD, false);
  TypeRefBuilder::BuiltProtocolDecl P4 = std::make_pair(XYZ, false);

  EXPECT_EQ(P1, P2);
  EXPECT_NE(P2, P3);
  EXPECT_NE(P2, P3);
  EXPECT_NE(P3, P4);

  auto PC1 = Builder.createProtocolCompositionType({P1, P2}, nullptr, false);
  auto PC2 = Builder.createProtocolCompositionType({P1, P2}, nullptr, false);
  auto PC3 =
    Builder.createProtocolCompositionType({P1, P2, P2}, nullptr, false);
  auto Any = Builder.createProtocolCompositionType({}, nullptr, false);

  EXPECT_EQ(PC1, PC2);
  EXPECT_NE(PC2, PC3);
  EXPECT_NE(PC1, Any);
}

TEST(TypeRefTest, UniqueMetatypeTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(ABC_decl, nullptr);
  auto M1 = Builder.createMetatypeType(N1, std::nullopt);
  auto M2 = Builder.createMetatypeType(N1, std::nullopt);
  auto MM3 = Builder.createMetatypeType(M1, std::nullopt);
  auto M4 = Builder.createMetatypeType(N1, Demangle::ImplMetatypeRepresentation::Thick);

  EXPECT_EQ(M1, M2);
  EXPECT_NE(M2, MM3);
  EXPECT_NE(M1, M4);
}

TEST(TypeRefTest, UniqueExistentialMetatypeTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(ABC_decl, nullptr);
  auto M1 = Builder.createExistentialMetatypeType(N1);
  auto M2 = Builder.createExistentialMetatypeType(N1);
  auto MM3 = Builder.createExistentialMetatypeType(M1);

  EXPECT_EQ(M1, M2);
  EXPECT_NE(M2, MM3);
}

TEST(TypeRefTest, UniqueGenericTypeParameterTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto GTP00 = Builder.createGenericTypeParameterType(0, 0);
  auto GTP00_2 = Builder.createGenericTypeParameterType(0, 0);
  auto GTP01 = Builder.createGenericTypeParameterType(0, 1);
  auto GTP10 = Builder.createGenericTypeParameterType(1, 0);

  EXPECT_EQ(GTP00, GTP00_2);
  EXPECT_NE(GTP00, GTP01);
  EXPECT_NE(GTP01, GTP10);
}

TEST(TypeRefTest, UniqueDependentMemberTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(ABC_decl, nullptr);
  auto N2 = Builder.createNominalType(XYZ_decl, nullptr);
  TypeRefBuilder::BuiltProtocolDecl P1 = std::make_pair(ABC, false);
  TypeRefBuilder::BuiltProtocolDecl P2 = std::make_pair(ABCD, false);

  auto DM1 = Builder.createDependentMemberType("Index", N1, P1);
  auto DM2 = Builder.createDependentMemberType("Index", N1, P1);
  auto DM3 = Builder.createDependentMemberType("Element", N1, P1);
  auto DM4 = Builder.createDependentMemberType("Index", N2, P1);
  auto DM5 = Builder.createDependentMemberType("Index", N2, P2);

  EXPECT_EQ(DM1, DM2);
  EXPECT_NE(DM2, DM3);
  EXPECT_NE(DM2, DM4);
  EXPECT_NE(DM4, DM5);
}

TEST(TypeRefTest, UniqueForeignClassTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto UN1 = Builder.getUnnamedForeignClassType();
  auto UN2 = Builder.getUnnamedForeignClassType();
  auto FC1 = Builder.createForeignClassType(ABC);
  auto FC2 = Builder.createForeignClassType(ABC);
  auto FC3 = Builder.createForeignClassType(ABCD);

  EXPECT_EQ(UN1, UN2);
  EXPECT_EQ(FC1, FC2);
  EXPECT_NE(FC2, FC3);
}

TEST(TypeRefTest, UniqueObjCClassTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto UN1 = Builder.getUnnamedObjCClassType();
  auto UN2 = Builder.getUnnamedObjCClassType();
  auto FC1 = Builder.createObjCClassType(ABC);
  auto FC2 = Builder.createObjCClassType(ABC);
  auto FC3 = Builder.createObjCClassType(ABCD);

  EXPECT_EQ(UN1, UN2);
  EXPECT_EQ(FC1, FC2);
  EXPECT_NE(FC2, FC3);
}

TEST(TypeRefTest, UniqueOpaqueTypeRef) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto Op = OpaqueTypeRef::get();
  auto Op1 = Builder.getOpaqueType();
  auto Op2 = Builder.getOpaqueType();

  EXPECT_EQ(Op, Op1);
  EXPECT_EQ(Op1, Op2);
}

TEST(TypeRefTest, UniqueUnownedStorageType) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(MyClass_decl, nullptr);
  auto N2 = Builder.createNominalType(NotMyClass_decl, nullptr);
  auto RS1 = Builder.createUnownedStorageType(N1);
  auto RS2 = Builder.createUnownedStorageType(N1);
  auto RS3 = Builder.createUnownedStorageType(N2);

  EXPECT_EQ(RS1, RS2);
  EXPECT_NE(RS2, RS3);
}

TEST(TypeRefTest, UniqueWeakStorageType) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(MyClass_decl, nullptr);
  auto N2 = Builder.createNominalType(NotMyClass_decl, nullptr);
  auto RS1 = Builder.createWeakStorageType(N1);
  auto RS2 = Builder.createWeakStorageType(N1);
  auto RS3 = Builder.createWeakStorageType(N2);

  EXPECT_EQ(RS1, RS2);
  EXPECT_NE(RS2, RS3);
}

TEST(TypeRefTest, UniqueUnmanagedStorageType) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N1 = Builder.createNominalType(MyClass_decl, nullptr);
  auto N2 = Builder.createNominalType(NotMyClass_decl, nullptr);
  auto RS1 = Builder.createUnmanagedStorageType(N1);
  auto RS2 = Builder.createUnmanagedStorageType(N1);
  auto RS3 = Builder.createUnmanagedStorageType(N2);

  EXPECT_EQ(RS1, RS2);
  EXPECT_NE(RS2, RS3);
}

// Tests that a concrete ABC<Int, Int> is the exact same typeref
// (with the same pointer) as an ABC<T, U> that got substituted
// with T : Int and U : Int.
TEST(TypeRefTest, UniqueAfterSubstitution) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  TypeRefDecl MangledIntName("Si");
  auto NominalInt = Builder.createNominalType(MangledIntName,
                                              /*parent*/ nullptr);
  std::vector<const TypeRef *> ConcreteArgs { NominalInt, NominalInt };

  TypeRefDecl MangledName("ABC");

  auto ConcreteBG = Builder.createBoundGenericType(MangledName,
                                                   ConcreteArgs,
                                                   /*parent*/ nullptr);

  auto GTP00 = Builder.createGenericTypeParameterType(0, 0);
  auto GTP01 = Builder.createGenericTypeParameterType(0, 1);
  std::vector<const TypeRef *> GenericParams { GTP00, GTP01 };

  auto Unbound = Builder.createBoundGenericType(MangledName, GenericParams,
                                                /*parent*/ nullptr);

  GenericArgumentMap Subs;
  Subs[{0,0}] = NominalInt;
  Subs[{0,1}] = NominalInt;

  auto SubstitutedBG = Unbound->subst(Builder, Subs);

  EXPECT_EQ(ConcreteBG, SubstitutedBG);
}

// Make sure subst() and isConcrete() walk into parent types
TEST(TypeRefTest, NestedTypes) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto GTP00 = Builder.createGenericTypeParameterType(0, 0);

  TypeRefDecl ParentName("parent");
  std::vector<const TypeRef *> ParentArgs { GTP00 };
  auto Parent = Builder.createBoundGenericType(ParentName, ParentArgs,
                                               /*parent*/ nullptr);

  TypeRefDecl ChildName("child");
  auto Child = Builder.createNominalType(ChildName, Parent);

  EXPECT_FALSE(Child->isConcrete());

  TypeRefDecl SubstName("subst");
  auto SubstArg = Builder.createNominalType(SubstName, /*parent*/ nullptr);

  std::vector<const TypeRef *> SubstParentArgs { SubstArg };
  auto SubstParent = Builder.createBoundGenericType(ParentName,
                                                    SubstParentArgs,
                                                    /*parent*/ nullptr);
  auto SubstChild = Builder.createNominalType(ChildName, SubstParent);

  GenericArgumentMap Subs;
  Subs[{0,0}] = SubstArg;

  EXPECT_TRUE(Child->isConcreteAfterSubstitutions(Subs));
  EXPECT_EQ(SubstChild, Child->subst(Builder, Subs));
}

// A BoundGenericTypeRef whose mangled name fails to demangle must not crash
// when demangled back into a node tree; getDemangling() should fail gracefully
// by returning null. Remote Mirror can encounter such names when inspecting
// corrupt or foreign process memory (rdar://181289066).
TEST(TypeRefTest, DemangleBoundGenericInvalidMangledName) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);
  Demangle::Demangler Dem;

  auto Int = Builder.createNominalType(TypeRefDecl("Si"), /*parent*/ nullptr);
  std::vector<const TypeRef *> Args{Int};

  // An empty mangled name does not demangle to a type.
  auto BG = Builder.createBoundGenericType(TypeRefDecl(""), Args,
                                           /*parent*/ nullptr);

  EXPECT_EQ(nullptr, BG->getDemangling(Dem));
}

// A BoundGenericTypeRef with a generic argument that fails to demangle must
// likewise fail gracefully rather than crash (rdar://181289066).
TEST(TypeRefTest, DemangleBoundGenericInvalidArgument) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);
  Demangle::Demangler Dem;

  // A nominal argument with an empty mangled name does not demangle.
  auto BadArg = Builder.createNominalType(TypeRefDecl(""), /*parent*/ nullptr);
  std::vector<const TypeRef *> Args{BadArg};

  auto BG = Builder.createBoundGenericType(TypeRefDecl("Si"), Args,
                                           /*parent*/ nullptr);

  EXPECT_EQ(nullptr, BG->getDemangling(Dem));
}

// The remaining container visitors in DemanglingForTypeRef must also propagate
// a failed sub-demangling as null rather than tripping Node::addChild's
// assertion (rdar://181289066).
TEST(TypeRefTest, DemangleContainersWithInvalidElement) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);
  Demangle::Demangler Dem;

  // A nominal with an empty mangled name does not demangle.
  auto BadArg = Builder.createNominalType(TypeRefDecl(""), /*parent*/ nullptr);

  // Labels must match the element count so the element is actually visited.
  StringRef Labels[] = {StringRef()};
  auto Tuple = Builder.createTupleType({BadArg}, Labels);
  EXPECT_EQ(nullptr, Tuple->getDemangling(Dem));

  auto Meta = Builder.createMetatypeType(BadArg, std::nullopt);
  EXPECT_EQ(nullptr, Meta->getDemangling(Dem));

  auto ExistentialMeta = Builder.createExistentialMetatypeType(BadArg);
  EXPECT_EQ(nullptr, ExistentialMeta->getDemangling(Dem));
}

TEST(TypeRefTest, DeriveSubstitutions) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto GTP00 = Builder.createGenericTypeParameterType(0, 0);
  auto GTP01 = Builder.createGenericTypeParameterType(0, 1);

  TypeRefDecl NominalName("nominal");
  std::vector<const TypeRef *> NominalArgs { GTP00 };
  auto Nominal = Builder.createBoundGenericType(NominalName, NominalArgs,
                                               /*parent*/ nullptr);

  auto Result = Builder.createTupleType({GTP00, GTP01},
                                        ArrayRef<StringRef>());
  auto Func = Builder.createFunctionType(
      {Nominal}, Result, FunctionTypeFlags(),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr, nullptr);

  TypeRefDecl SubstOneName("subst1");
  auto SubstOne = Builder.createNominalType(SubstOneName, /*parent*/ nullptr);

  TypeRefDecl SubstTwoName("subst2");
  auto SubstTwo = Builder.createNominalType(SubstTwoName, /*parent*/ nullptr);

  GenericArgumentMap Subs;
  Subs[{0,0}] = SubstOne;
  Subs[{0,1}] = SubstTwo;

  auto Subst = Func->subst(Builder, Subs);

  GenericArgumentMap DerivedSubs;
  EXPECT_TRUE(TypeRef::deriveSubstitutions(DerivedSubs, Func, Subst));

  auto ResultOne = DerivedSubs[{0,0}];
  auto ResultTwo = DerivedSubs[{0,1}];
  EXPECT_EQ(SubstOne, ResultOne);
  EXPECT_EQ(SubstTwo, ResultTwo);
}

// TypeRef::deepEquals() must compare structurally rather than by pointer, so
// that TypeRefs built by two independent TypeRefBuilders (which do not share
// a uniquing table) can still be recognized as equal.
TEST(TypeRefTest, EqualsAcrossBuilders) {
  TypeRefBuilder Builder1(TypeRefBuilder::ForTesting);
  TypeRefBuilder Builder2(TypeRefBuilder::ForTesting);

  auto N1 = Builder1.createNominalType(ABC_decl, nullptr);
  auto N2 = Builder2.createNominalType(ABC_decl, nullptr);
  auto N3 = Builder2.createNominalType(ABCD_decl, nullptr);

  EXPECT_NE(N1, N2);
  EXPECT_TRUE(TypeRef::deepEquals(N1, N2));
  EXPECT_TRUE(N1->deepEquals(*N2));
  EXPECT_FALSE(N2->deepEquals(*N3));
  EXPECT_FALSE(TypeRef::deepEquals(N1, N3));

  auto GTP00_1 = Builder1.createGenericTypeParameterType(0, 0);
  auto GTP00_2 = Builder2.createGenericTypeParameterType(0, 0);
  std::vector<const TypeRef *> Args1{N1, GTP00_1};
  std::vector<const TypeRef *> Args2{N2, GTP00_2};

  auto BG1 = Builder1.createBoundGenericType(ABC_decl, Args1, nullptr);
  auto BG2 = Builder2.createBoundGenericType(ABC_decl, Args2, nullptr);
  EXPECT_TRUE(TypeRef::deepEquals(BG1, BG2));

  auto F1 = Builder1.createFunctionType(
      {N1}, N1, FunctionTypeFlags(), ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr,
      nullptr);
  auto F2 = Builder2.createFunctionType(
      {N2}, N2, FunctionTypeFlags(), ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr,
      nullptr);
  auto F3 = Builder2.createFunctionType(
      {N2}, N2, FunctionTypeFlags().withThrows(true),
      ExtendedFunctionTypeFlags(),
      FunctionMetadataDifferentiabilityKind::NonDifferentiable, nullptr,
      nullptr);
  EXPECT_TRUE(TypeRef::deepEquals(F1, F2));
  EXPECT_FALSE(TypeRef::deepEquals(F1, F3));

  EXPECT_TRUE(TypeRef::deepEquals(nullptr, nullptr));
  EXPECT_FALSE(TypeRef::deepEquals(N1, nullptr));
  EXPECT_FALSE(TypeRef::deepEquals(nullptr, N1));
  EXPECT_FALSE(TypeRef::deepEquals(N1, GTP00_1));
}

// deepEquals must be reflexive (a value equals itself) and must distinguish
// TypeRefs of different kinds even when they carry similar payloads.
TEST(TypeRefTest, DeepEqualsReflexiveAndKindMismatch) {
  TypeRefBuilder Builder(TypeRefBuilder::ForTesting);

  auto N = Builder.createNominalType(ABC_decl, nullptr);
  auto BI = Builder.createBuiltinType(ABC, ABC);
  auto FC = Builder.createForeignClassType(ABC);
  auto OC = Builder.createObjCClassType(ABC);

  EXPECT_TRUE(N->deepEquals(*N));
  EXPECT_TRUE(BI->deepEquals(*BI));
  EXPECT_TRUE(TypeRef::deepEquals(N, N));

  EXPECT_FALSE(TypeRef::deepEquals(N, BI));
  EXPECT_FALSE(TypeRef::deepEquals(N, FC));
  EXPECT_FALSE(TypeRef::deepEquals(FC, OC));
  EXPECT_FALSE(TypeRef::deepEquals(BI, OC));
}

// Structural comparison for the leaf/name-carrying kinds: Builtin, ObjCClass,
// ObjCProtocol, ForeignClass, Integer, and GenericTypeParameter.
TEST(TypeRefTest, DeepEqualsLeafKinds) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  // Builtin: equal iff mangled name matches.
  EXPECT_TRUE(TypeRef::deepEquals(B1.createBuiltinType(ABC, ABC),
                                  B2.createBuiltinType(ABC, ABC)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createBuiltinType(ABC, ABC),
                                   B2.createBuiltinType(ABCD, ABCD)));

  // ObjC class.
  EXPECT_TRUE(TypeRef::deepEquals(B1.createObjCClassType(MyClass),
                                  B2.createObjCClassType(MyClass)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createObjCClassType(MyClass),
                                   B2.createObjCClassType(NotMyClass)));

  // ObjC protocol.
  EXPECT_TRUE(TypeRef::deepEquals(B1.createObjCProtocolType(MyProtocol),
                                  B2.createObjCProtocolType(MyProtocol)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createObjCProtocolType(MyProtocol),
                                   B2.createObjCProtocolType(Shmrotocol)));

  // Foreign class.
  EXPECT_TRUE(TypeRef::deepEquals(B1.createForeignClassType(ABC),
                                  B2.createForeignClassType(ABC)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createForeignClassType(ABC),
                                   B2.createForeignClassType(ABCD)));

  // Integer: equal iff value matches (including sign).
  EXPECT_TRUE(TypeRef::deepEquals(B1.createIntegerType(42),
                                  B2.createIntegerType(42)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createIntegerType(42),
                                   B2.createIntegerType(43)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createIntegerType(1),
                                   B2.createNegativeIntegerType(-1)));

  // Generic type parameter: equal iff both depth and index match.
  EXPECT_TRUE(TypeRef::deepEquals(B1.createGenericTypeParameterType(1, 2),
                                  B2.createGenericTypeParameterType(1, 2)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createGenericTypeParameterType(1, 2),
                                   B2.createGenericTypeParameterType(1, 3)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createGenericTypeParameterType(1, 2),
                                   B2.createGenericTypeParameterType(0, 2)));
}

// Opaque type refs are singletons with no payload; any two compare equal.
TEST(TypeRefTest, DeepEqualsOpaque) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  EXPECT_TRUE(TypeRef::deepEquals(B1.getOpaqueType(), B2.getOpaqueType()));
  EXPECT_TRUE(TypeRef::deepEquals(B1.getOpaqueType(), OpaqueTypeRef::get()));
}

// Tuples compare their labels and their elements element-wise.
TEST(TypeRefTest, DeepEqualsTuple) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  auto A1 = B1.createNominalType(ABC_decl, nullptr);
  auto X1 = B1.createNominalType(XYZ_decl, nullptr);
  auto A2 = B2.createNominalType(ABC_decl, nullptr);
  auto X2 = B2.createNominalType(XYZ_decl, nullptr);

  StringRef Labels[] = {"first", "second"};
  StringRef OtherLabels[] = {"first", "third"};

  auto T1 = B1.createTupleType({A1, X1}, Labels);
  auto T2 = B2.createTupleType({A2, X2}, Labels);
  EXPECT_TRUE(TypeRef::deepEquals(T1, T2));

  // Same elements, different labels.
  auto T3 = B2.createTupleType({A2, X2}, OtherLabels);
  EXPECT_FALSE(TypeRef::deepEquals(T1, T3));

  // Same labels, different element order.
  auto T4 = B2.createTupleType({X2, A2}, Labels);
  EXPECT_FALSE(TypeRef::deepEquals(T1, T4));

  // Different arity.
  StringRef OneLabel[] = {"first"};
  auto T5 = B2.createTupleType({A2}, OneLabel);
  EXPECT_FALSE(TypeRef::deepEquals(T1, T5));

  // Empty tuples (Void) are equal.
  EXPECT_TRUE(TypeRef::deepEquals(
      B1.createTupleType({}, ArrayRef<StringRef>()),
      B2.createTupleType({}, ArrayRef<StringRef>())));
}

// Metatype compares its instance type and its abstract-ness; existential
// metatype compares only its instance type.
TEST(TypeRefTest, DeepEqualsMetatypes) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  auto N1 = B1.createNominalType(ABC_decl, nullptr);
  auto M1 = B2.createNominalType(ABC_decl, nullptr);
  auto Other = B2.createNominalType(XYZ_decl, nullptr);

  EXPECT_TRUE(TypeRef::deepEquals(B1.createMetatypeType(N1, std::nullopt),
                                  B2.createMetatypeType(M1, std::nullopt)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createMetatypeType(N1, std::nullopt),
                                   B2.createMetatypeType(Other, std::nullopt)));

  // wasAbstract differs (Thick -> abstract, default -> not abstract).
  EXPECT_FALSE(TypeRef::deepEquals(
      B1.createMetatypeType(N1, std::nullopt),
      B2.createMetatypeType(M1, Demangle::ImplMetatypeRepresentation::Thick)));

  EXPECT_TRUE(
      TypeRef::deepEquals(B1.createExistentialMetatypeType(N1),
                          B2.createExistentialMetatypeType(M1)));
  EXPECT_FALSE(
      TypeRef::deepEquals(B1.createExistentialMetatypeType(N1),
                          B2.createExistentialMetatypeType(Other)));
}

// Dependent member types compare member name, protocol, and base type.
TEST(TypeRefTest, DeepEqualsDependentMember) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  auto Base1 = B1.createNominalType(ABC_decl, nullptr);
  auto Base2 = B2.createNominalType(ABC_decl, nullptr);
  auto OtherBase = B2.createNominalType(XYZ_decl, nullptr);
  TypeRefBuilder::BuiltProtocolDecl P1 = std::make_pair(MyProtocol, false);
  TypeRefBuilder::BuiltProtocolDecl P2 = std::make_pair(Shmrotocol, false);

  EXPECT_TRUE(TypeRef::deepEquals(
      B1.createDependentMemberType("Element", Base1, P1),
      B2.createDependentMemberType("Element", Base2, P1)));

  // Different member name.
  EXPECT_FALSE(TypeRef::deepEquals(
      B1.createDependentMemberType("Element", Base1, P1),
      B2.createDependentMemberType("Index", Base2, P1)));

  // Different protocol.
  EXPECT_FALSE(TypeRef::deepEquals(
      B1.createDependentMemberType("Element", Base1, P1),
      B2.createDependentMemberType("Element", Base2, P2)));

  // Different base type.
  EXPECT_FALSE(TypeRef::deepEquals(
      B1.createDependentMemberType("Element", Base1, P1),
      B2.createDependentMemberType("Element", OtherBase, P1)));
}

// Protocol compositions compare their protocol list, superclass, and the
// explicit-AnyObject / class-bound bit.
TEST(TypeRefTest, DeepEqualsProtocolComposition) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  TypeRefBuilder::BuiltProtocolDecl P = std::make_pair(MyProtocol, false);
  TypeRefBuilder::BuiltProtocolDecl Q = std::make_pair(Shmrotocol, false);

  auto PC1 = B1.createProtocolCompositionType({P, Q}, nullptr, false);
  auto PC2 = B2.createProtocolCompositionType({P, Q}, nullptr, false);
  EXPECT_TRUE(TypeRef::deepEquals(PC1, PC2));

  // Different protocol set.
  auto PC3 = B2.createProtocolCompositionType({P}, nullptr, false);
  EXPECT_FALSE(TypeRef::deepEquals(PC1, PC3));

  // Different class-bound bit.
  auto PC4 = B2.createProtocolCompositionType({P, Q}, nullptr, true);
  EXPECT_FALSE(TypeRef::deepEquals(PC1, PC4));

  // Different superclass (nullptr vs. present).
  auto Super = B2.createNominalType(MyClass_decl, nullptr);
  auto PC5 = B2.createProtocolCompositionType({P, Q}, Super, false);
  EXPECT_FALSE(TypeRef::deepEquals(PC1, PC5));

  // Empty composition (Any) equal across builders.
  EXPECT_TRUE(TypeRef::deepEquals(
      B1.createProtocolCompositionType({}, nullptr, false),
      B2.createProtocolCompositionType({}, nullptr, false)));
}

// Each reference-storage flavor (weak/unowned/unmanaged) compares its
// referent type, and the flavors are distinct from one another.
TEST(TypeRefTest, DeepEqualsReferenceStorage) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  auto N1 = B1.createNominalType(MyClass_decl, nullptr);
  auto N2 = B2.createNominalType(MyClass_decl, nullptr);
  auto Other = B2.createNominalType(NotMyClass_decl, nullptr);

  EXPECT_TRUE(TypeRef::deepEquals(B1.createWeakStorageType(N1),
                                  B2.createWeakStorageType(N2)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createWeakStorageType(N1),
                                   B2.createWeakStorageType(Other)));

  EXPECT_TRUE(TypeRef::deepEquals(B1.createUnownedStorageType(N1),
                                  B2.createUnownedStorageType(N2)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createUnownedStorageType(N1),
                                   B2.createUnownedStorageType(Other)));

  EXPECT_TRUE(TypeRef::deepEquals(B1.createUnmanagedStorageType(N1),
                                  B2.createUnmanagedStorageType(N2)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createUnmanagedStorageType(N1),
                                   B2.createUnmanagedStorageType(Other)));

  // Different storage flavors wrapping the same referent are different kinds.
  EXPECT_FALSE(TypeRef::deepEquals(B1.createWeakStorageType(N1),
                                   B2.createUnownedStorageType(N2)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createUnownedStorageType(N1),
                                   B2.createUnmanagedStorageType(N2)));
}

// Builtin.FixedArray compares its size and element operands; Builtin.Borrow
// compares its referent.
TEST(TypeRefTest, DeepEqualsBuiltinFixedArrayAndBorrow) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  auto Elt1 = B1.createNominalType(ABC_decl, nullptr);
  auto Elt2 = B2.createNominalType(ABC_decl, nullptr);
  auto OtherElt = B2.createNominalType(XYZ_decl, nullptr);

  auto FA1 = B1.createBuiltinFixedArrayType(B1.createIntegerType(4), Elt1);
  auto FA2 = B2.createBuiltinFixedArrayType(B2.createIntegerType(4), Elt2);
  EXPECT_TRUE(TypeRef::deepEquals(FA1, FA2));

  // Different count.
  auto FA3 = B2.createBuiltinFixedArrayType(B2.createIntegerType(5), Elt2);
  EXPECT_FALSE(TypeRef::deepEquals(FA1, FA3));

  // Different element type.
  auto FA4 = B2.createBuiltinFixedArrayType(B2.createIntegerType(4), OtherElt);
  EXPECT_FALSE(TypeRef::deepEquals(FA1, FA4));

  // Builtin.Borrow compares its referent.
  EXPECT_TRUE(TypeRef::deepEquals(B1.createBuiltinBorrowType(Elt1),
                                  B2.createBuiltinBorrowType(Elt2)));
  EXPECT_FALSE(TypeRef::deepEquals(B1.createBuiltinBorrowType(Elt1),
                                   B2.createBuiltinBorrowType(OtherElt)));
}

// SILBox-with-layout compares its fields (type and mutability), its
// substitutions, and its requirements.
TEST(TypeRefTest, DeepEqualsSILBoxWithLayout) {
  using Field = TypeRefBuilder::BuiltSILBoxField;
  using Subst = TypeRefBuilder::BuiltSubstitution;
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  auto A1 = B1.createNominalType(ABC_decl, nullptr);
  auto A2 = B2.createNominalType(ABC_decl, nullptr);
  auto X1 = B1.createNominalType(XYZ_decl, nullptr);
  auto X2 = B2.createNominalType(XYZ_decl, nullptr);
  auto GTP1 = B1.createGenericTypeParameterType(0, 0);
  auto GTP2 = B2.createGenericTypeParameterType(0, 0);

  auto makeBox = [](TypeRefBuilder &B, const TypeRef *fieldTy, bool mutableField,
                    const TypeRef *param, const TypeRef *arg) {
    llvm::SmallVector<Field, 1> Fields{Field(fieldTy, mutableField)};
    llvm::SmallVector<Subst, 1> Subs{Subst(param, arg)};
    llvm::SmallVector<TypeRefBuilder::BuiltRequirement, 0> Reqs;
    return B.createSILBoxTypeWithLayout(Fields, Subs, Reqs, {});
  };

  auto Box1 = makeBox(B1, A1, /*mutable*/ true, GTP1, X1);
  auto Box2 = makeBox(B2, A2, /*mutable*/ true, GTP2, X2);
  EXPECT_TRUE(TypeRef::deepEquals(Box1, Box2));

  // Different field mutability.
  auto Box3 = makeBox(B2, A2, /*mutable*/ false, GTP2, X2);
  EXPECT_FALSE(TypeRef::deepEquals(Box1, Box3));

  // Different field type.
  auto Box4 = makeBox(B2, X2, /*mutable*/ true, GTP2, X2);
  EXPECT_FALSE(TypeRef::deepEquals(Box1, Box4));

  // Different substitution argument.
  auto Box5 = makeBox(B2, A2, /*mutable*/ true, GTP2, A2);
  EXPECT_FALSE(TypeRef::deepEquals(Box1, Box5));
}

// deepEquals must recurse into nested structure and only report equal when the
// whole tree matches; a difference buried deep in the tree must be detected.
TEST(TypeRefTest, DeepEqualsRecursesIntoNestedStructure) {
  TypeRefBuilder B1(TypeRefBuilder::ForTesting);
  TypeRefBuilder B2(TypeRefBuilder::ForTesting);

  // Build Outer<Inner<ABC>> in each builder, then perturb the innermost leaf.
  auto build = [](TypeRefBuilder &B, const TypeRefDecl &leaf) {
    auto Leaf = B.createNominalType(leaf, nullptr);
    auto Inner = B.createBoundGenericType(XYZ_decl, {Leaf}, nullptr);
    return B.createBoundGenericType(ABC_decl, {Inner}, nullptr);
  };

  auto Tree1 = build(B1, ABC_decl);
  auto Tree2 = build(B2, ABC_decl);
  EXPECT_TRUE(TypeRef::deepEquals(Tree1, Tree2));

  // A leaf difference three levels deep must make the trees unequal.
  auto Tree3 = build(B2, ABCD_decl);
  EXPECT_FALSE(TypeRef::deepEquals(Tree1, Tree3));

  // A nominal type's parent chain participates in the comparison.
  auto Parent1 = B1.createNominalType(MyModule_decl, nullptr);
  auto Nested1 = B1.createNominalType(MyClass_decl, Parent1);
  auto Parent2 = B2.createNominalType(MyModule_decl, nullptr);
  auto Nested2 = B2.createNominalType(MyClass_decl, Parent2);
  auto OtherParent = B2.createNominalType(Shmodule_decl, nullptr);
  auto NestedOther = B2.createNominalType(MyClass_decl, OtherParent);

  EXPECT_TRUE(TypeRef::deepEquals(Nested1, Nested2));
  EXPECT_FALSE(TypeRef::deepEquals(Nested1, NestedOther));
  // Same leaf name, but one has a parent and the other does not.
  auto NoParent = B2.createNominalType(MyClass_decl, nullptr);
  EXPECT_FALSE(TypeRef::deepEquals(Nested1, NoParent));
}

// Verify that the MultiPayloadEnumDescriptor spare-bit-mask accessors defend
// against malformed __swift5_mpenum records supplied by an inspected process,
// rather than trusting the record's declared mask size.
// rdar://181865069, rdar://181864074
TEST(TypeRefTest, MultiPayloadEnumSpareBitMaskBounds) {
  // Records are laid out as: int32 TypeName; uint32 contents[SizeInWords].
  //   contents[0] = (SizeInWords << 16) | flags
  //   contents[1] = (maskByteOffset << 16) | maskByteCount
  //   contents[2...] = mask bytes
  auto makeRecord = [](std::vector<uint32_t> &storage, uint32_t sizeInWords,
                       uint32_t flags,
                       uint32_t maskByteCount) -> const MultiPayloadEnumDescriptor * {
    storage.assign(1 + sizeInWords, 0);
    storage[0] = 0; // TypeName relative pointer (unused here).
    storage[1] = (sizeInWords << 16) | (flags & 0xffff);
    if (sizeInWords >= 2)
      storage[2] = maskByteCount & 0xffff;
    return reinterpret_cast<const MultiPayloadEnumDescriptor *>(storage.data());
  };

  // A record claiming a 0xFFFF-byte mask but declaring only two content words
  // (leaving zero bytes of room for the mask) must clamp the count to 0.
  {
    std::vector<uint32_t> storage;
    auto *desc = makeRecord(storage, /*sizeInWords=*/2, /*flags=*/1,
                            /*maskByteCount=*/0xFFFF);
    EXPECT_TRUE(desc->usesPayloadSpareBits());
    EXPECT_EQ(desc->getPayloadSpareBitMaskByteCount(), 0u);
    EXPECT_NE(desc->getPayloadSpareBits(), nullptr);
  }

  // With extra content words the count is clamped to the room the record
  // actually describes ((SizeInWords - 2) * 4 bytes).
  {
    std::vector<uint32_t> storage;
    auto *desc = makeRecord(storage, /*sizeInWords=*/4, /*flags=*/1,
                            /*maskByteCount=*/0xFFFF);
    EXPECT_EQ(desc->getPayloadSpareBitMaskByteCount(), 8u);
  }

  // A well-formed count is returned unchanged.
  {
    std::vector<uint32_t> storage;
    auto *desc = makeRecord(storage, /*sizeInWords=*/3, /*flags=*/1,
                            /*maskByteCount=*/4);
    EXPECT_EQ(desc->getPayloadSpareBitMaskByteCount(), 4u);
    EXPECT_NE(desc->getPayloadSpareBits(), nullptr);
  }

  // A record that claims to use spare bits but declares too few words to even
  // hold the mask-count word must not read contents[1] or hand back a pointer.
  {
    std::vector<uint32_t> storage;
    auto *desc = makeRecord(storage, /*sizeInWords=*/1, /*flags=*/1,
                            /*maskByteCount=*/0);
    EXPECT_EQ(desc->getPayloadSpareBitMaskByteCount(), 0u);
    EXPECT_EQ(desc->getPayloadSpareBitMaskByteOffset(), 0u);
    EXPECT_EQ(desc->getPayloadSpareBits(), nullptr);
  }
}

// A reflection section whose size is smaller than a single record header must
// terminate iteration immediately rather than reading the header past the end
// of the section buffer. rdar://181867993
TEST(TypeRefTest, ReflectionSectionUndersizedRecord) {
  // Back the section with a generous buffer so the test itself never reads out
  // of bounds, but tell the section it is only 4 bytes long -- fewer than
  // sizeof(FieldDescriptor).
  alignas(16) char buffer[64] = {0};
  remote::RemoteAddress addr(reinterpret_cast<uintptr_t>(buffer),
                             remote::RemoteAddress::DefaultAddressSpace);
  RemoteRef<void> ref(addr, buffer);

  FieldSection section(ref, /*Size=*/4);
  EXPECT_TRUE(section.begin() == section.end());
  for (auto record : section) {
    (void)record;
    ADD_FAILURE() << "undersized field section should yield no records";
  }
}
