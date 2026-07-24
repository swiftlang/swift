//===--- CxxDirectViewTests.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Unit tests for the "direct view" classifier (importer::isDirectViewType) and
// the corresponding safety of imported non-escapable C++ types.
//
//===----------------------------------------------------------------------===//

#include "CxxInteropTestHarness.h"
#include "../../lib/ClangImporter/ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {

/// Import \p cxxSource and return whether the C++ type named \p name is a
/// direct view (runs `importer::isDirectViewType` on the clang decl).
bool isDirectView(StringRef cxxSource, StringRef name) {
  bool result = false;
  compile(cxxSource).checkCppDecl(name, [&](const clang::Decl *d,
                                            ASTContext &ctx) {
    result = importer::isDirectViewType(d, ctx);
  });
  return result;
}

/// Import \p cxxSource and return the explicit safety of the imported Swift
/// decl named \p name.
ExplicitSafety importedSafety(StringRef cxxSource, StringRef name) {
  ExplicitSafety result = ExplicitSafety::Unspecified;
  import(cxxSource).checkDecl(name, [&](ValueDecl *d) {
    result = d->getExplicitSafety();
  });
  return result;
}

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// isDirectViewType classifier
//===----------------------------------------------------------------------===//

TEST(CxxDirectView, PointerToSelfContainedIsDirectView) {
  EXPECT_TRUE(isDirectView("struct S { int *p; };", "S"));
}

TEST(CxxDirectView, StringRefLikeStructIsDirectView) {
  EXPECT_TRUE(
      isDirectView("struct StringRef { char *p; unsigned long n; };", "StringRef"));
}

TEST(CxxDirectView, MultipleDirectViewMembersIsDirectView) {
  EXPECT_TRUE(
      isDirectView("struct StringView { const char *begin, *end; };", "StringView"));
}

TEST(CxxDirectView, DoublePointerIsNotDirectView) {
  EXPECT_FALSE(isDirectView("struct S { int **pp; };", "S"));
}

TEST(CxxDirectView, StructStoringPointerToViewIsNotDirectView) {
  EXPECT_FALSE(isDirectView("struct StringRef { char *p; unsigned long n; };\n"
                            "struct Holder { StringRef *ref; };",
                            "Holder"));
}

TEST(CxxDirectView, SpanOfSelfContainedIsDirectView) {
  EXPECT_TRUE(isDirectView(
      "template <class X> struct Span { X *data; unsigned long n; };\n"
      "template struct Span<int>;\n"
      "typedef Span<int> SpanInt;",
      "SpanInt"));
}

TEST(CxxDirectView, SpanOfViewIsNotDirectView) {
  EXPECT_FALSE(isDirectView(
      "struct View { int *p; };\n"
      "template <class X> struct Span { X *data; unsigned long n; };\n"
      "template struct Span<View>;\n"
      "typedef Span<View> SpanView;",
      "SpanView"));
}

TEST(CxxDirectView, PointerToSelfContainedRecordIsDirectView) {
  EXPECT_TRUE(isDirectView(
      "struct " SWIFT_TEST_SELF_CONTAINED " Owned { char *storage; };\n"
      "struct View { Owned *owned; };",
      "View"));
}

TEST(CxxDirectView, NestedDirectViewStructByValueIsDirectView) {
  EXPECT_TRUE(isDirectView("struct StringRef { char *p; unsigned long n; };\n"
                           "struct Nested { StringRef sv; };",
                           "Nested"));
}

TEST(CxxDirectView, EmptyStructIsDirectView) {
  EXPECT_TRUE(isDirectView("struct Empty {};", "Empty"));
}

TEST(CxxDirectView, DirectViewBaseIsDirectView) {
  EXPECT_TRUE(isDirectView("struct Base { int *p; };\n"
                           "struct Derived : Base {};",
                           "Derived"));
}

TEST(CxxDirectView, NonDirectViewBaseIsNotDirectView) {
  EXPECT_FALSE(isDirectView("struct Base { int **pp; };\n"
                            "struct Derived : Base {};",
                            "Derived"));
}

TEST(CxxDirectView, SharedByValueSubobjectIsDirectView) {
  // Both fields have the same record type, exercising the visited-set
  // de-duplication in the classifier.
  EXPECT_TRUE(isDirectView("struct Leaf { int *p; };\n"
                           "struct S { Leaf a; Leaf b; };",
                           "S"));
}

TEST(CxxDirectView, ReferenceToSelfContainedIsDirectView) {
  EXPECT_TRUE(isDirectView("struct S { int &r; };", "S"));
}

TEST(CxxDirectView, FunctionPointerFieldIsNotDirectView) {
  EXPECT_FALSE(isDirectView("struct S { void (*fn)(); };", "S"));
}

TEST(CxxDirectView, SelfReferentialStructIsNotDirectView) {
  // `Node *` points to a non-self-contained type (Node has pointer members), so
  // it is not a direct view.
  EXPECT_FALSE(isDirectView("struct Node { Node *next; };", "Node"));
}

//===----------------------------------------------------------------------===//
// Imported safety of non-escapable types
//===----------------------------------------------------------------------===//

TEST(CxxDirectView, DirectViewImportedAsSafe) {
  EXPECT_EQ(importedSafety(
                "struct " SWIFT_TEST_NONESCAPABLE " View { const int *p; };",
                "View"),
            ExplicitSafety::Safe);
}

TEST(CxxDirectView, NonDirectViewImportedAsUnsafe) {
  EXPECT_EQ(importedSafety("struct " SWIFT_TEST_NONESCAPABLE " V { int *p; };\n"
                           "struct " SWIFT_TEST_NONESCAPABLE " VV { V **pp; };",
                           "VV"),
            ExplicitSafety::Unsafe);
}

TEST(CxxDirectView, NonDirectViewFieldMakesEnclosingViewUnsafe) {
  // Exercises the classifier recursing into a by-value non-escapable field: the
  // inner view is not a direct view (double pointer), so the outer view is
  // unsafe too.
  EXPECT_EQ(importedSafety(
                "struct V { int *p; };\n"
                "struct " SWIFT_TEST_NONESCAPABLE " Inner { V **pp; };\n"
                "struct " SWIFT_TEST_NONESCAPABLE " Outer { Inner i; };",
                "Outer"),
            ExplicitSafety::Unsafe);
}

TEST(CxxDirectView, ExplicitlyUnsafeSubobjectMakesViewUnsafe) {
  // `Owned` is self-contained (import_owned) but explicitly marked unsafe, so a
  // view containing it must not be silently imported as safe.
  EXPECT_EQ(importedSafety(
                "struct " SWIFT_TEST_SELF_CONTAINED " " SWIFT_TEST_UNSAFE
                " Owned { char *p; };\n"
                "struct " SWIFT_TEST_NONESCAPABLE " V { Owned o; };",
                "V"),
            ExplicitSafety::Unsafe);
}
