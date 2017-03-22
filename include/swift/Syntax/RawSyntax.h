//===--- RawSyntax.h - Swift Raw Syntax Interface ---------------*- C++ -*-===//
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
// This file defines the RawSyntax type.
//
// These are the "backbone or "skeleton" of the Syntax tree, providing
// the recursive structure, child relationships, kind of node, etc.
//
// They are reference-counted and strictly immutable, so can be shared freely
// among Syntax nodes and have no specific identity. They could even in theory
// be shared for expressions like 1 + 1 + 1 + 1 - you don't need 7 syntax nodes
// to expressSwiftTypeConverter that at this layer.
//
// These are internal implementation ONLY - do not expose anything involving
// RawSyntax publically. Clients of lib/Syntax should not be aware that they
// exist.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_RAWSYNTAX_H
#define SWIFT_SYNTAX_RAWSYNTAX_H

#include "swift/Syntax/References.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>

using llvm::cast;
using llvm::StringRef;

#ifndef NDEBUG
#define syntax_assert_child_kind(Raw, Cursor, ExpectedKind)                    \
  (assert(Raw->getChild(Cursor)->Kind == ExpectedKind));
#else
#define syntax_assert_child_kind(Raw, Cursor, Kind) ((void)0);
#endif

#ifndef NDEBUG
#define syntax_assert_child_token(Raw, Cursor, TokenKind)                      \
  (assert(cast<TokenSyntax>(Raw->getChild(Cursor))->getTokenKind() == TokenKind));
#else
#define syntax_assert_child_token(Raw, Cursor, TokenKind) ((void)0);
#endif

#ifndef NDEBUG
#define syntax_assert_child_token_text(Raw, Cursor, TokenKind, Text)           \
  (assert(cast<TokenSyntax>(Raw->getChild(Cursor))->getTokenKind() ==          \
          TokenKind));                                                         \
  (assert(cast<TokenSyntax>(Raw->getChild(Cursor))->getText() == Text));
#else
#define syntax_assert_child_token_text(Raw, Cursor, TokenKind, Text) ((void)0);
#endif

#ifndef NDEBUG
#define syntax_assert_token_is(Tok, Kind, Text)                                \
  {                                                                            \
    assert(Tok->getTokenKind() == Kind);                                       \
    assert(Tok->getText() == Text);                                            \
  }
#else
#define syntax_assert_token_is(Tok, Kind, Text) ((void)0);
#endif

namespace swift {
namespace syntax {

using CursorIndex = uint32_t;

/// Get a numeric index suitable for array/vector indexing
/// from a syntax node's Cursor enum value.
template <typename CursorType> constexpr CursorIndex cursorIndex(CursorType C) {
  return static_cast<CursorIndex>(C);
}

/// An absolute position in a source file as text - the absolute offset from
/// the start, line, and column.
class AbsolutePosition {
  uintptr_t Offset = 0;
  uint32_t Line = 1;
  uint32_t Column = 1;

public:
  /// Add some number of columns to the position.
  void addColumns(uint32_t Columns) {
    Column += Columns;
    Offset += Columns;
  }

  /// Add some number of newlines to the position, resetting the column.
  void addNewlines(uint32_t NewLines) {
    Line += NewLines;
    Column = 1;
    Offset += NewLines;
  }

  /// Use some text as a reference for adding to the absolute position,
  /// taking note of newlines, etc.
  void addText(StringRef Str) {
    for (auto C : Str) {
      addCharacter(C);
    }
  }

  /// Use some character as a reference for adding to the absolute position,
  /// taking note of newlines, etc.
  void addCharacter(char C) {
    if (C == '\n') {
      addNewlines(1);
    } else {
      addColumns(1);
    }
  }

  /// Get the line number of this position.
  uint32_t getLine() const { return Line; }

  /// Get the column number of this position.
  uint32_t getColumn() const { return Column; }

  /// Get the line and column number of this position.
  std::pair<uint32_t, uint32_t> getLineAndColumn() const {
    return {Line, Column};
  }

  /// Get the absolute offset of this position, suitable for indexing into a
  /// buffer if applicable.
  uintptr_t getOffset() const { return Offset; }

  /// Print the line and column as "l:c" to the given output stream.
  void printLineAndColumn(llvm::raw_ostream &OS) const;

  /// Dump a description of this position to the given output stream
  /// for debugging.
  void dump(llvm::raw_ostream &OS = llvm::errs()) const;
};

enum class SyntaxKind {
  Token,
#define SYNTAX(Id, Parent) Id,
#define SYNTAX_COLLECTION(Id, Element) Id,
#define MISSING_SYNTAX(Id, Parent) Id,
#define SYNTAX_RANGE(Id, First, Last) First_##Id = First, Last_##Id = Last,
#include "swift/Syntax/SyntaxKinds.def"
};

/// An indicator of whether a Syntax node was found or written in the source.
///
/// This is not an 'implicit' bit.
enum class SourcePresence {
  /// The syntax was authored by a human and found, or was generated.
  Present,

  /// The syntax was expected or optional, but not found in the source.
  Missing,
};

/// RawSyntax - the strictly immutable, shared backing nodes for all syntax.
///
/// This is implementation detail - do not expose it in public API.
struct RawSyntax : public llvm::ThreadSafeRefCountedBase<RawSyntax> {

  using LayoutList = std::vector<RC<RawSyntax>>;

  /// The kind of syntax this node represents.
  const SyntaxKind Kind;

  /// The "layout" of the node - representing the children, or the terms
  /// in the production of the grammar.
  const LayoutList Layout;

  /// Whether this piece of syntax was actually present in the source.
  const SourcePresence Presence;

  /// Create a piece of raw syntax.
  RawSyntax(const SyntaxKind Kind, const std::vector<RC<RawSyntax>> Layout,
            const SourcePresence Presence)
      : Kind(Kind), Layout(Layout), Presence(Presence) {}

  /// Returns a raw syntax node of the given Kind, specified Layout,
  /// and source presence.
  static RC<RawSyntax> make(const SyntaxKind Kind, const LayoutList Layout,
                            const SourcePresence Presence) {
    return RC<RawSyntax>{new RawSyntax{Kind, Layout, Presence}};
  }

  /// Returns a raw syntax node of the given Kind, marked as missing.
  static RC<RawSyntax> missing(const SyntaxKind Kind) {
    return RC<RawSyntax>{new RawSyntax{Kind, {}, SourcePresence::Missing}};
  }

  /// Get a child based on a particular node's "Cursor", indicating
  /// the position of the terms in the production of the Swift grammar.
  template <typename CursorType> RC<RawSyntax> getChild(CursorType C) const {
    return Layout[cursorIndex(C)];
  }

  /// Returns true if the node is "missing" in the source (i.e. it was
  /// expected (or optional) but not written.
  bool isMissing() const { return Presence == SourcePresence::Missing; }

  /// Returns true if the node is "present" in the source.
  bool isPresent() const {
    return Presence == SourcePresence::Present;
  }

  /// Returns true if this raw syntax node is some kind of declaration.
  bool isDecl() const {
    return Kind >= SyntaxKind::First_Decl && Kind <= SyntaxKind::Last_Decl;
  }

  /// Returns true if this raw syntax node is some kind of type syntax.
  bool isType() const {
    return Kind >= SyntaxKind::First_Type && Kind <= SyntaxKind::Last_Type;
  }

  /// Returns true if this raw syntax node is some kind of statement.
  bool isStmt() const {
    return Kind >= SyntaxKind::First_Stmt && Kind <= SyntaxKind::Last_Stmt;
  }

  /// Returns true if this raw syntax node is some kind of expression.
  bool isExpr() const {
    return Kind >= SyntaxKind::First_Expr && Kind <= SyntaxKind::Last_Expr;
  }

  /// Return true if this raw syntax node is a token.
  bool isToken() const {
    return Kind == SyntaxKind::Token;
  }

  bool isUnknown() const {
    return Kind == SyntaxKind::Unknown ||
           Kind == SyntaxKind::UnknownDecl ||
           Kind == SyntaxKind::UnknownExpr ||
           Kind == SyntaxKind::UnknownStmt;
  }

  /// Get the absolute position of this raw syntax: its offset, line,
  /// and column.
  AbsolutePosition getAbsolutePosition(RC<RawSyntax> Root) const;

  /// Return a new raw syntax node with the given new layout element appended
  /// to the end of the node's layout.
  RC<RawSyntax> append(RC<RawSyntax> NewLayoutElement) const;

  /// Return a new raw syntax node with the given new layout element replacing
  /// another at some cursor position.
  template <typename CursorType>
  RC<RawSyntax>
  replaceChild(CursorType C, RC<RawSyntax> NewLayoutElement) const {
    LayoutList NewLayout;

    std::copy(Layout.begin(), Layout.begin() + cursorIndex(C),
              std::back_inserter(NewLayout));

    NewLayout.push_back(NewLayoutElement);

    std::copy(Layout.begin() + cursorIndex(C) + 1, Layout.end(),
              std::back_inserter(NewLayout));

    return RawSyntax::make(Kind, NewLayout, Presence);
  }

  /// Print this piece of syntax recursively.
  void print(llvm::raw_ostream &OS) const;

  /// Dump this piece of syntax recursively for debugging or testing.
  void dump() const;

  /// Dump this piece of syntax recursively.
  void dump(llvm::raw_ostream &OS, unsigned Indent) const;

private:
  bool accumulateAbsolutePosition(AbsolutePosition &Pos,
                                  const RawSyntax *UpToTargetNode) const;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_RAWSYNTAX_H
