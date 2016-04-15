//===--- OverrideTests.cpp - Tests for overriding logic -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Host.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
/// Helper class used to set the LangOpts target before initializing the
/// ASTContext.
///
/// \see TestContext
class TestContextBase {
public:
  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  SourceManager SourceMgr;
  DiagnosticEngine Diags;

  TestContextBase() : Diags(SourceMgr) {
    LangOpts.Target = llvm::Triple(llvm::sys::getProcessTriple());
  }
};

enum ShouldDeclareOptionalTypes : bool {
  DoNotDeclareOptionalTypes,
  DeclareOptionalTypes
};

/// Owns an ASTContext and the associated types.
class TestContext : public TestContextBase {
  SourceFile *FileForLookups;

  void declareOptionalType(Identifier name) {
    auto wrapped = new (Ctx) GenericTypeParamDecl(File,
                                                  Ctx.getIdentifier("Wrapped"),
                                                  SourceLoc(), /*depth*/0,
                                                  /*index*/0);
    auto params = GenericParamList::create(Ctx, SourceLoc(), wrapped,
                                           SourceLoc());
    auto decl = new (Ctx) EnumDecl(SourceLoc(), name, SourceLoc(),
                                   /*inherited*/{}, params, File);
    wrapped->setDeclContext(decl);
    FileForLookups->Decls.push_back(decl);
  }

public:
  ASTContext Ctx;
  DerivedFileUnit *File;

  TestContext(ShouldDeclareOptionalTypes optionals = DoNotDeclareOptionalTypes)
      : Ctx(LangOpts, SearchPathOpts, SourceMgr, Diags) {
    auto stdlibID = Ctx.getIdentifier(STDLIB_NAME);
    auto *module = ModuleDecl::create(stdlibID, Ctx);
    Ctx.LoadedModules[stdlibID] = module;
    File = new (Ctx) DerivedFileUnit(*module);

    if (optionals == DeclareOptionalTypes) {
      using ImplicitModuleImportKind = SourceFile::ImplicitModuleImportKind;
      FileForLookups = new (Ctx) SourceFile(*module, SourceFileKind::Library,
                                            /*buffer*/None,
                                            ImplicitModuleImportKind::None);
      module->addFile(*FileForLookups);

      declareOptionalType(Ctx.getIdentifier("Optional"));
      declareOptionalType(Ctx.getIdentifier("ImplicitlyUnwrappedOptional"));
    }
  }

  template <typename Nominal>
  Nominal *makeNominal(StringRef name,
                       GenericParamList *genericParams = nullptr) {
    auto result = new (Ctx) Nominal(SourceLoc(), Ctx.getIdentifier(name),
                                    SourceLoc(), /*inherited*/{},
                                    genericParams, File);
    result->setAccessibility(Accessibility::Internal);
    return result;
  }
};
} // end anonymous namespace

TEST(Override, IdenticalTypes) {
  TestContext C;

  auto check = [&C](Type ty) {
    return ty->canOverride(ty, OverrideMatchMode::Strict, /*resolver*/nullptr);
  };

  EXPECT_TRUE(check(C.Ctx.TheEmptyTupleType));
  EXPECT_TRUE(check(C.Ctx.TheRawPointerType));

  Type voidToVoidFn = FunctionType::get(C.Ctx.TheEmptyTupleType,
                                        C.Ctx.TheEmptyTupleType);
  EXPECT_TRUE(check(voidToVoidFn));

  Type ptrToPtrFn = FunctionType::get(C.Ctx.TheRawPointerType,
                                      C.Ctx.TheRawPointerType);
  EXPECT_TRUE(check(ptrToPtrFn));

  auto *someStruct = C.makeNominal<StructDecl>("MyStruct");
  Type structTy = someStruct->getDeclaredInterfaceType();
  EXPECT_TRUE(check(structTy));

  Type structToStructFn = FunctionType::get(structTy, structTy);
  EXPECT_TRUE(check(structToStructFn));
}

TEST(Override, UnrelatedTypes) {
  TestContext C;

  auto check = [&C](Type base, Type derived) {
    return derived->canOverride(base, OverrideMatchMode::Strict,
                                /*resolver*/nullptr);
  };

  EXPECT_FALSE(check(C.Ctx.TheEmptyTupleType, C.Ctx.TheRawPointerType));
  EXPECT_FALSE(check(C.Ctx.TheRawPointerType, C.Ctx.TheEmptyTupleType));

  Type voidToVoidFn = FunctionType::get(C.Ctx.TheEmptyTupleType,
                                        C.Ctx.TheEmptyTupleType);
  EXPECT_FALSE(check(voidToVoidFn, C.Ctx.TheEmptyTupleType));
  EXPECT_FALSE(check(C.Ctx.TheEmptyTupleType, voidToVoidFn));

  Type ptrToPtrFn = FunctionType::get(C.Ctx.TheRawPointerType,
                                      C.Ctx.TheRawPointerType);
  EXPECT_FALSE(check(ptrToPtrFn, voidToVoidFn));
  EXPECT_FALSE(check(voidToVoidFn, ptrToPtrFn));

  auto *someStruct = C.makeNominal<StructDecl>("MyStruct");
  Type structTy = someStruct->getDeclaredInterfaceType();
  EXPECT_FALSE(check(structTy, C.Ctx.TheEmptyTupleType));
  EXPECT_FALSE(check(C.Ctx.TheEmptyTupleType, structTy));
  EXPECT_FALSE(check(structTy, voidToVoidFn));
  EXPECT_FALSE(check(voidToVoidFn, structTy));

  Type structToStructFn = FunctionType::get(structTy, structTy);
  EXPECT_FALSE(check(structToStructFn, structTy));
  EXPECT_FALSE(check(structTy, structToStructFn));
  EXPECT_FALSE(check(structToStructFn, voidToVoidFn));
  EXPECT_FALSE(check(voidToVoidFn, structToStructFn));

  auto *anotherStruct = C.makeNominal<StructDecl>("AnotherStruct");
  Type anotherStructTy = anotherStruct->getDeclaredInterfaceType();
  EXPECT_FALSE(check(structTy, anotherStructTy));
  EXPECT_FALSE(check(anotherStructTy, structTy));

  Type anotherStructToAnotherStructFn = FunctionType::get(anotherStructTy,
                                                          anotherStructTy);
  EXPECT_FALSE(check(anotherStructToAnotherStructFn, structToStructFn));
  EXPECT_FALSE(check(structToStructFn, anotherStructToAnotherStructFn));

  Type S2ASFn = FunctionType::get(structTy, anotherStructTy);
  EXPECT_FALSE(check(S2ASFn, structToStructFn));
  EXPECT_FALSE(check(structToStructFn, S2ASFn));
  EXPECT_FALSE(check(S2ASFn, anotherStructToAnotherStructFn));
  EXPECT_FALSE(check(anotherStructToAnotherStructFn, S2ASFn));
}

TEST(Override, Classes) {
  TestContext C;

  auto check = [&C](Type base, Type derived) {
    return derived->canOverride(base, OverrideMatchMode::Strict,
                                /*resolver*/nullptr);
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

  Type baseToVoid = FunctionType::get(baseTy, C.Ctx.TheEmptyTupleType);
  Type subToVoid = FunctionType::get(subTy, C.Ctx.TheEmptyTupleType);
  EXPECT_FALSE(check(baseToVoid, subToVoid));
  EXPECT_TRUE(check(subToVoid, baseToVoid));

  Type voidToBase = FunctionType::get(C.Ctx.TheEmptyTupleType, baseTy);
  Type voidToSub = FunctionType::get(C.Ctx.TheEmptyTupleType, subTy);
  EXPECT_FALSE(check(voidToSub, voidToBase));
  EXPECT_TRUE(check(voidToBase, voidToSub));

  Type baseToBase = FunctionType::get(baseTy, baseTy);
  Type subToSub = FunctionType::get(subTy, subTy);
  EXPECT_FALSE(check(baseToBase, subToSub));
  EXPECT_FALSE(check(subToSub, baseToBase));
}

TEST(Override, Optionals) {
  TestContext C{DeclareOptionalTypes};

  auto check = [&C](Type base, Type derived) {
    return derived->canOverride(base, OverrideMatchMode::Strict,
                                /*resolver*/nullptr);
  };

  auto *baseClass = C.makeNominal<ClassDecl>("Base");
  Type baseTy = baseClass->getDeclaredInterfaceType();
  Type optTy = OptionalType::get(baseTy);

  EXPECT_FALSE(check(baseTy, optTy));
  EXPECT_TRUE(check(optTy, baseTy));

  Type baseToVoid = FunctionType::get(baseTy, C.Ctx.TheEmptyTupleType);
  Type optToVoid = FunctionType::get(optTy, C.Ctx.TheEmptyTupleType);
  EXPECT_TRUE(check(baseToVoid, optToVoid));
  EXPECT_FALSE(check(optToVoid, baseToVoid));

  Type voidToBase = FunctionType::get(C.Ctx.TheEmptyTupleType, baseTy);
  Type voidToOpt = FunctionType::get(C.Ctx.TheEmptyTupleType, optTy);
  EXPECT_FALSE(check(voidToBase, voidToOpt));
  EXPECT_TRUE(check(voidToOpt, voidToBase));

  Type baseToBase = FunctionType::get(baseTy, baseTy);
  Type optToOpt = FunctionType::get(optTy, optTy);
  EXPECT_FALSE(check(baseToBase, optToOpt));
  EXPECT_FALSE(check(optToOpt, baseToBase));
}

TEST(Override, IUONearMatch) {
  TestContext C{DeclareOptionalTypes};

  auto check = [&C](Type base, Type derived) {
    return derived->canOverride(base, OverrideMatchMode::Strict,
                                /*resolver*/nullptr);
  };
  auto checkIUO = [&C](Type base, Type derived) {
    return derived->canOverride(base,
                                OverrideMatchMode::AllowNonOptionalForIUOParam,
                                /*resolver*/nullptr);
  };

  auto *baseClass = C.makeNominal<ClassDecl>("Base");
  Type baseTy = baseClass->getDeclaredInterfaceType();
  Type optTy = ImplicitlyUnwrappedOptionalType::get(baseTy);

  Type baseToVoid = FunctionType::get(baseTy, C.Ctx.TheEmptyTupleType);
  Type optToVoid = FunctionType::get(optTy, C.Ctx.TheEmptyTupleType);
  EXPECT_TRUE(check(baseToVoid, optToVoid));
  EXPECT_TRUE(checkIUO(baseToVoid, optToVoid));
  EXPECT_FALSE(check(optToVoid, baseToVoid));
  EXPECT_TRUE(checkIUO(optToVoid, baseToVoid));

  Type voidToBase = FunctionType::get(C.Ctx.TheEmptyTupleType, baseTy);
  Type voidToOpt = FunctionType::get(C.Ctx.TheEmptyTupleType, optTy);
  EXPECT_FALSE(check(voidToBase, voidToOpt));
  EXPECT_FALSE(checkIUO(voidToBase, voidToOpt));
  EXPECT_TRUE(check(voidToOpt, voidToBase));
  EXPECT_TRUE(checkIUO(voidToOpt, voidToBase));

  Type baseToBase = FunctionType::get(baseTy, baseTy);
  Type optToOpt = FunctionType::get(optTy, optTy);
  EXPECT_FALSE(check(baseToBase, optToOpt));
  EXPECT_FALSE(checkIUO(baseToBase, optToOpt));
  EXPECT_FALSE(check(optToOpt, baseToBase));
  EXPECT_TRUE(checkIUO(optToOpt, baseToBase));
}
