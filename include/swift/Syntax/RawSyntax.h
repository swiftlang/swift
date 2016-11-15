//===--- Trivia.h - Swift Raw Syntax Nodes ----------------------*- C++ -*-===//
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
//
// This file defines the RawSyntax class - the immutable, persistent backing
// for syntactic information in the AST. These are raw containers of child
// relationships and the layout of a particular node. For example:
//
// func foo() {
//   // body ...
// }
//
// This function declaration is laid out as following:
// - func token
// - foo identifier token
// - child: argument list
// - child: brace statement
//
// And would have two children.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_RAWSYNTAX_H
#define SWIFT_SYNTAX_RAWSYNTAX_H

namespace swift {
namespace syntax {

struct RawSyntax;
using RawSyntaxRef = llvm::IntrusiveRefCntPtr<RawSyntax>;

enum class RawSyntaxLayoutKind {
  /// The layout element is a terminal token.
  Token,

  /// The layout element is a child syntax node.
  ChildIndex,
};

struct RawSyntaxLayout {
  const RawSyntaxLayoutKind Kind;
private:
  union {
    syntax::Token Tok;
    size_t ChildIndex;
  } Data;

public:
  const syntax::Token &getToken() const {
    assert(Kind == RawSyntaxLayoutKind::Token);
    return Data.Tok;
  }

  size_t getChildIndex() const {
    assert(Kind == RawSyntaxLayoutKind::ChildIndex);
    return Data.ChildIndex;
  }
};

struct RawSyntax : public llvm::ThreadSafeRefCountedBase<RawSyntax> {
  const std::vector<RawSyntaxRef> Children;
  const std::vector<RawSyntaxLayout> Layout;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_RAWSYNTAX_H
