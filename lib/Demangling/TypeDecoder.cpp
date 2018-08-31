//===--- TypeDecoder.cpp - Type decoding from a demangling tree -----------===//
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
//
//  This file implements support routines for the TypeDecoder class template.
//
//===----------------------------------------------------------------------===//
#include "swift/Demangling/TypeDecoder.h"

using namespace swift;
using namespace Demangle;

NodePointer Demangle::stripGenericArgsFromContextNode(NodePointer node,
                                                      NodeFactory &factory) {
  switch (node->getKind()) {
  case Demangle::Node::Kind::BoundGenericClass:
  case Demangle::Node::Kind::BoundGenericEnum:
  case Demangle::Node::Kind::BoundGenericStructure:
  case Demangle::Node::Kind::BoundGenericOtherNominalType:
    // Bound generic types have a 'Type' node under them, whose child is
    // the non-generic reference. If we don't see that structure, do nothing.
    if (node->getNumChildren() < 2 ||
        node->getChild(0)->getKind() != Demangle::Node::Kind::Type ||
        node->getChild(0)->getNumChildren() < 1)
      return node;

    // Strip generic arguments from that child, then return it.
    return stripGenericArgsFromContextNode(node->getChild(0)->getChild(0),
                                          factory);

  case Demangle::Node::Kind::Class:
  case Demangle::Node::Kind::Enum:
  case Demangle::Node::Kind::Structure:
  case Demangle::Node::Kind::OtherNominalType: {
    if (node->getNumChildren() < 2)
      return node;

    auto newContext = stripGenericArgsFromContextNode(node->getChild(0),
                                                      factory);
    if (newContext == node->getChild(0)) return node;

    auto newNode = factory.createNode(node->getKind());
    newNode->addChild(newContext, factory);
    for (unsigned i = 1, n = node->getNumChildren(); i != n; ++i)
      newNode->addChild(node->getChild(i), factory);
    return newNode;
  }
      
  case Demangle::Node::Kind::Extension: {
    // Strip generic arguments from the extended type.
    if (node->getNumChildren() < 2)
      return node;
    
    auto newExtended = stripGenericArgsFromContextNode(node->getChild(1),
                                                       factory);
    if (newExtended == node->getChild(1)) return node;
    
    auto newNode = factory.createNode(Node::Kind::Extension);
    newNode->addChild(node->getChild(0), factory);
    newNode->addChild(newExtended, factory);
    if (node->getNumChildren() == 3)
      newNode->addChild(node->getChild(2), factory);
    return newNode;
  }

  case Demangle::Node::Kind::Module:
    // Modules terminate the recursion.
    return node;

  default:
    // FIXME: Handle local contexts.
    return node;
  }
}
