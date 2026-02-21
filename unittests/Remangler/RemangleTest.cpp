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
