//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Strings.h"
#include "llvm/ADT/StringRef.h"

#include "gtest/gtest.h"

/// Helper class to conveniently construct demangle tree hierarchies.
class NodeBuilder {
  using NodePointer = swift::Demangle::NodePointer;
  using Kind = swift::Demangle::Node::Kind;
  
  swift::Demangle::Demangler &m_dem;

public:
  NodeBuilder(swift::Demangle::Demangler &dem) : m_dem(dem) {
#ifndef NDEBUG
    m_dem.disableAssertionsForUnitTest = true;
#endif
  }
  NodePointer Node(Kind kind, StringRef text) {
    return m_dem.createNode(kind, text);
  }
  NodePointer NodeWithIndex(Kind kind, swift::Demangle::Node::IndexType index) {
    return m_dem.createNode(kind, index);
  }
  NodePointer Node(Kind kind, NodePointer child0 = nullptr,
                   NodePointer child1 = nullptr,
                   NodePointer child2 = nullptr,
                   NodePointer child3 = nullptr) {
    NodePointer node = m_dem.createNode(kind);

    if (child0)
      node->addChild(child0, m_dem);
    if (child1)
      node->addChild(child1, m_dem);
    if (child2)
      node->addChild(child2, m_dem);
    if (child3)
      node->addChild(child3, m_dem);
    return node;
  }
  NodePointer IntType() {
    return Node(Node::Kind::Type,
                Node(Node::Kind::Structure,
                     Node(Node::Kind::Module, swift::STDLIB_NAME),
                     Node(Node::Kind::Identifier, "Int")));
  }
  NodePointer GlobalTypeMangling(NodePointer type) {
    assert(type && type->getKind() == Node::Kind::Type);
    return Node(Node::Kind::Global, Node(Node::Kind::TypeMangling, type));
  }
  NodePointer GlobalType(NodePointer type) {
    assert(type && type->getKind() != Node::Kind::Type &&
           type->getKind() != Node::Kind::TypeMangling &&
           type->getKind() != Node::Kind::Global);
    return GlobalTypeMangling(Node(Node::Kind::Type, type));
  }

  ManglingErrorOr<StringRef> remangle(NodePointer node) {
    return mangleNode(
        node,
        [](SymbolicReferenceKind, const void *) -> NodePointer {
          return nullptr;
        },
        m_dem);
  }
  std::string remangleResult(NodePointer node) {
    return remangle(node).result().str();
  }
  bool remangleSuccess(NodePointer node) {
    return remangle(node).isSuccess();
  }
};

TEST(TestSwiftRemangler, DependentGenericConformanceRequirement) {
  using namespace swift::Demangle;
  using Kind = swift::Demangle::Node::Kind;
  Demangler dem;
  NodeBuilder b(dem);
  {
    // Well-formed.
    NodePointer n = b.GlobalType(b.Node(
        Kind::DependentGenericType,
        b.Node(Kind::DependentGenericType,
               b.Node(Kind::DependentGenericSignature,
                      b.NodeWithIndex(Kind::DependentGenericParamCount, 1),
                      b.Node(Kind::DependentGenericConformanceRequirement,
                             b.Node(Kind::Type,
                                    b.Node(Kind::DependentGenericParamType,
                                           b.NodeWithIndex(Kind::Index, 0),
                                           b.NodeWithIndex(Kind::Index, 0))),
                             b.Node(Kind::Type,
                                    b.Node(Kind::Protocol,
                                           b.Node(Kind::Module, "M"),
                                           b.Node(Kind::Identifier, "B"))))),
               b.IntType())));
    ASSERT_EQ(b.remangleResult(n), "$sSi1M1BRzluuD");
  }
  {
    // Malformed.
    NodePointer n = b.GlobalType(b.Node(
        Kind::DependentGenericType,
        b.Node(Kind::DependentGenericType,
               b.Node(Kind::DependentGenericSignature,
                      b.NodeWithIndex(Kind::DependentGenericParamCount, 1),
                      b.Node(Kind::DependentGenericConformanceRequirement,
                             b.Node(Kind::Type,
                                    b.Node(Kind::DependentGenericParamType,
                                           b.NodeWithIndex(Kind::Index, 0),
                                           b.NodeWithIndex(Kind::Index, 0))))),
               b.IntType())));
    ASSERT_FALSE(b.remangleSuccess(n));
  }
}

TEST(TestSwiftRemangler, IdentifierExpansionLimit) {
  using namespace swift::Demangle;

  // Word substitution lets each one-letter reference re-append a whole
  // previously-seen word. 2048 references to a 2048-character word is exactly
  // the identifier length limit.
  std::string Word(2048, 'x');
  std::string Mangled =
      "$s0" + std::to_string(Word.size()) + Word + std::string(2048, 'a') +
      "03fooV";

  Demangler Dem;
  ASSERT_EQ(Dem.demangleSymbol(Mangled), nullptr);
  ASSERT_FALSE(Dem.isTooComplex());

  // One reference fewer stays under the limit and demangles.
  std::string Under =
      "$s0" + std::to_string(Word.size()) + Word + std::string(2047, 'a') +
      "03fooV";

  Demangler UnderDem;
  ASSERT_NE(UnderDem.demangleSymbol(Under), nullptr);
}

TEST(TestSwiftRemangler, AllocateWithMisalignedSlabEnd) {
  using namespace swift::Demangle;

  // A char allocation big enough to force a new slab leaves the slab's end
  // misaligned relative to Node. Allocating a Node afterwards must not read
  // or write past the end of that slab.
  NodeFactory Factory;
  size_t Size = 100 * sizeof(Node) * 2 + 1;
  char *Chars = Factory.Allocate<char>(Size);
  memset(Chars, 'x', Size);

  NodePointer N = Factory.createNode(Node::Kind::Identifier, "a");
  ASSERT_EQ(N->getText(), "a");
}

TEST(TestSwiftRemangler, TooComplexIsReportedNotFatal) {
  using namespace swift::Demangle;

  // A factory that has hit a size limit reports failure from both the
  // demangler and the remangler rather than raising a fatal error.
  Demangler Dem;
  NodeBuilder b(Dem);
  NodePointer n = b.GlobalTypeMangling(b.IntType());
  ASSERT_TRUE(b.remangleSuccess(n));

  Dem.setTooComplex();
  ASSERT_TRUE(Dem.isTooComplex());
  ASSERT_FALSE(b.remangleSuccess(n));

  // clear() resets the failure so the factory can be reused.
  Dem.clear();
  ASSERT_FALSE(Dem.isTooComplex());
}
