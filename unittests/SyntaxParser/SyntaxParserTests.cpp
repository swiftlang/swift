//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift-c/SyntaxParser/SwiftSyntaxParser.h"
#include "swift/Basic/LLVM.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include "gtest/gtest.h"
#include <vector>

using namespace swift;
using namespace swift::syntax;
using namespace serialization;

static swiftparse_client_node_t
parse(StringRef source, swiftparse_node_handler_t node_handler,
      swiftparse_get_syntaxkind_t get_syntaxkind,
      swiftparse_get_tokenkind_t get_tokenkind,
      swiftparse_is_present_t is_present,
      swiftparse_node_lookup_t node_lookup) {
  swiftparse_parser_t parser = swiftparse_parser_create();
  swiftparse_parser_set_required_callbacks(parser, node_handler, get_syntaxkind,
                                           get_tokenkind, is_present);
  swiftparse_parser_set_node_lookup(parser, node_lookup);
  swiftparse_client_node_t top = swiftparse_parse_string(parser, source.data(), source.size());
  swiftparse_parser_dispose(parser);
  return top;
}

static bool containsChild(swiftparse_layout_data_t layout_data, void *child) {
  for (size_t i = 0; i < layout_data.nodes_count; i++) {
    if (layout_data.nodes[i] == child) {
      return true;
    }
  }
  return false;
}

struct SyntaxNode {
  swiftparse_syntax_kind_t SyntaxKind;
  swiftparse_token_kind_t TokenKind;
  bool IsPresent;
};

TEST(SwiftSyntaxParserTests, IncrementalParsing) {
  StringRef source1 =
  "func t1() { }\n"
  "func t2() { }\n"
  "func t3() { }\n";

  StringRef source2 =
  "func t1renamed() { }\n"
  "func t2() { }\n"
  "func t3() { }\n";

  swiftparse_syntax_kind_t token = getNumericValue(SyntaxKind::Token);
  swiftparse_syntax_kind_t functionDecl = getNumericValue(SyntaxKind::FunctionDecl);
  swiftparse_syntax_kind_t codeBlockItem = getNumericValue(SyntaxKind::CodeBlockItem);
  swiftparse_syntax_kind_t codeBlockItemList = getNumericValue(SyntaxKind::CodeBlockItemList);

  // Allocate all SyntaxNodes in a bump allocator so we can free them when the
  // test finished running.
  __block llvm::BumpPtrAllocator NodeAllocator;

  // As we parse the nodes, these pointer are pouplated to point to the
  // corresponding SyntaxNodes
  __block SyntaxNode *t1Token = nullptr;
  __block SyntaxNode *t1Func = nullptr;
  __block SyntaxNode *t1CodeBlockItem = nullptr;
  __block SyntaxNode *t2Token = nullptr;
  __block SyntaxNode *t2Func = nullptr;
  __block SyntaxNode *t2CodeBlockItem = nullptr;
  __block SyntaxNode *t3Token = nullptr;
  __block SyntaxNode *t3Func = nullptr;
  __block SyntaxNode *t3CodeBlockItem = nullptr;

  // Find the t1/t2/t3 tokens in the source
  size_t t1TokenOffset = StringRef(source1).find("t1");
  size_t t2TokenOffset = StringRef(source1).find("t2");
  size_t t3TokenOffset = StringRef(source1).find("t3");

  // The length of the t2/t3 code block items
  size_t t2CodeBlockItemLength = 14;
  size_t t3CodeBlockItemLength = 14;

  // Collect the node ids of the code block items in this list and verify that
  // t2 and t3 get reused after the edit from source1 to source2.
  __block std::vector<void *> codeBlockItemIds;

  swiftparse_node_handler_t nodeHandler = ^swiftparse_client_node_t(
      const swiftparse_syntax_node_t *raw_node) {
      // Create a SyntaxNode
      SyntaxNode *node;
      if (raw_node->kind == token) {
        node = new (NodeAllocator) SyntaxNode{
            raw_node->kind, raw_node->token_data.kind, raw_node->present};
      } else {
        node = new (NodeAllocator)
            SyntaxNode{raw_node->kind, /*token_kind=*/0, raw_node->present};
      }

      // Check if it's a node we care about and update the corresponding pointer
      if (raw_node->kind == token) {
        if (raw_node->token_data.range.offset == t1TokenOffset) {
          t1Token = node;
        } else if (raw_node->token_data.range.offset == t2TokenOffset) {
          t2Token = node;
        } else if (raw_node->token_data.range.offset == t3TokenOffset) {
          t3Token = node;
        }
      } else if (raw_node->kind == functionDecl) {
        if (containsChild(raw_node->layout_data, t1Token)) {
          t1Func = node;
        } else if (containsChild(raw_node->layout_data, t2Token)) {
          t2Func = node;
        } else if (containsChild(raw_node->layout_data, t3Token)) {
          t3Func = node;
        }
      } else if (raw_node->kind == codeBlockItem) {
        if (containsChild(raw_node->layout_data, t1Func)) {
          t1CodeBlockItem = node;
        } else if (containsChild(raw_node->layout_data, t2Func)) {
          t2CodeBlockItem = node;
        } else if (containsChild(raw_node->layout_data, t3Func)) {
          t3CodeBlockItem = node;
        }
      } else if (raw_node->kind == codeBlockItemList) {
        for (unsigned i = 0, e = raw_node->layout_data.nodes_count;
             i != e; ++i) {
          codeBlockItemIds.push_back(raw_node->layout_data.nodes[i]);
        }
      }
      return node;
    };
  swiftparse_get_syntaxkind_t getSyntaxKind =
      ^swiftparse_syntax_kind_t(swiftparse_client_node_t node) {
        return static_cast<SyntaxNode *>(node)->SyntaxKind;
      };
  swiftparse_get_tokenkind_t getTokenKind =
      ^swiftparse_token_kind_t(swiftparse_client_node_t node) {
        auto synNode = static_cast<SyntaxNode *>(node);
        assert(synNode->SyntaxKind == token);
        return synNode->TokenKind;
      };
  swiftparse_is_present_t isPresent = ^bool(swiftparse_client_node_t node) {
    return static_cast<SyntaxNode *>(node)->IsPresent;
  };
  parse(source1, nodeHandler, getSyntaxKind, getTokenKind, isPresent,
        /*node_lookup=*/nullptr);
  ASSERT_NE(t2CodeBlockItemLength, size_t(0));
  EXPECT_EQ(codeBlockItemIds, (std::vector<void *>{t1CodeBlockItem, t2CodeBlockItem, t3CodeBlockItem}));

  codeBlockItemIds.clear();
  size_t t2CodeBlockItemOffset = StringRef(source2).find("\nfunc t2");
  size_t t3CodeBlockItemOffset = StringRef(source2).find("\nfunc t3");
  swiftparse_node_lookup_t nodeLookup =
    ^swiftparse_lookup_result_t(size_t offset, swiftparse_syntax_kind_t kind) {
      if (kind == codeBlockItem) {
        if (offset == t2CodeBlockItemOffset) {
          return { t2CodeBlockItemLength, t2CodeBlockItem };
        } else if (offset == t3CodeBlockItemOffset) {
          return { t3CodeBlockItemLength, t3CodeBlockItem };
        }
      }
      return {0, nullptr};
    };

  parse(source2, nodeHandler, getSyntaxKind, getTokenKind, isPresent,
        nodeLookup);
  // Assert that t2 and t3 get reused.
  EXPECT_EQ(codeBlockItemIds[1], t2CodeBlockItem);
  EXPECT_EQ(codeBlockItemIds[2], t3CodeBlockItem);
}
