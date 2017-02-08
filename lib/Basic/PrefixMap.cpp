//===--- PrefixMap.cpp - Out-of-line helpers for the PrefixMap structure --===//
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

#include "swift/Basic/PrefixMap.h"
#include "swift/Basic/QuotedString.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

#if __has_attribute(may_alias) || LLVM_GNUC_PREREQ(3, 0, 0)
#define LLVM_MAY_ALIAS __attribute__((may_alias))
#else
#define LLVM_MAY_ALIAS
#endif

namespace {

enum class ChildKind { Left, Right, Further, Root };

// We'd like the dump routine to be present in all builds, but it's
// a pretty large amount of code, most of which is not sensitive to the
// actual key and value data.  If we try to have a common implementation,
// we're left with the problem of describing the layout of a node when
// that's technically instantiation-specific.  Redefining the struct here
// is technically an aliasing violation, but we can just tell the compilers
// that actually use TBAA that this is okay.
typedef struct _Node Node LLVM_MAY_ALIAS;
struct _Node {
  // If you change the layout in the header, you'll need to change it here.
  // (This comment is repeated there.)
  Node *Left, *Right, *Further;
};

class TreePrinter {
  llvm::raw_ostream &Out;
  void (&PrintNodeData)(llvm::raw_ostream &out, void *node);
  SmallString<40> Indent;
public:
  TreePrinter(llvm::raw_ostream &out,
              void (&printNodeData)(llvm::raw_ostream &out, void *node))
    : Out(out), PrintNodeData(printNodeData) {}

  struct IndentScope {
    TreePrinter *Printer;
    size_t OldLength;
    IndentScope(TreePrinter *printer, StringRef indent)
        : Printer(printer), OldLength(printer->Indent.size()) {
      Printer->Indent += indent;
    }
    ~IndentScope() { Printer->Indent.resize(OldLength); }
  };

  void print(Node *node, ChildKind childKind) {
    // The top-level indents here create the line to the node we're
    // trying to print.

    if (node->Left) {
      IndentScope ms(this, (childKind == ChildKind::Left ||
                            childKind == ChildKind::Root) ? "  " : "| ");
      print(node->Left, ChildKind::Left);
    }

    {
      Out << Indent;
      if (childKind == ChildKind::Root) {
        Out << "+- ";
      } else if (childKind == ChildKind::Left) {
        Out << "/- ";
      } else if (childKind == ChildKind::Right) {
        Out << "\\- ";
      } else if (childKind == ChildKind::Further) {
        Out << "\\-> ";
      }
      PrintNodeData(Out, node);
      Out << '\n';
    }

    if (node->Further || node->Right) {
      IndentScope ms(this, (childKind == ChildKind::Right ||
                            childKind == ChildKind::Further ||
                            childKind == ChildKind::Root) ? "  " : "| ");

      if (node->Further) {
        // Further indent, and include the line to the right child if
        // there is one.
        IndentScope is(this, node->Right ? "|   " : "    ");
        print(node->Further, ChildKind::Further);
      }

      if (node->Right) {
        print(node->Right, ChildKind::Right);
      }
    }
  }
};

} // end anonymous namespace

void swift::printOpaquePrefixMap(raw_ostream &out, void *_root,
                         void (*printNodeData)(raw_ostream &out, void *node)) {
  auto root = reinterpret_cast<Node*>(_root);
  if (!root) {
    out << "(empty)\n";
    return;
  }
  TreePrinter(out, *printNodeData).print(root, ChildKind::Root);
}

void PrefixMapKeyPrinter<char>::print(raw_ostream &out, ArrayRef<char> key) {
  out << QuotedString(StringRef(key.data(), key.size()));
};

void PrefixMapKeyPrinter<unsigned char>::print(raw_ostream &out,
                                               ArrayRef<unsigned char> key) {
  out << '\'';
  for (auto byte : key) {
    if (byte < 16) out << '0';
    out.write_hex(byte);
  }
  out << '\'';
}
