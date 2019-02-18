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
#include "llvm/ADT/StringRef.h"
#include <vector>
#include "gtest/gtest.h"

using namespace swift;

static swiftparse_client_node_t
parse(const char *source, swiftparse_node_handler_t node_handler,
      swiftparse_node_lookup_t node_lookup) {
  swiftparse_parser_t parser = swiftparse_parser_create();
  swiftparse_parser_set_node_handler(parser, node_handler);
  swiftparse_parser_set_node_lookup(parser, node_lookup);
  swiftparse_client_node_t top = swiftparse_parse_string(parser, source);
  swiftparse_parser_dispose(parser);
  return top;
}

TEST(SwiftSyntaxParserTests, IncrementalParsing) {
  const char *source1 =
  "func t1() { }\n"
  "func t2() { }\n";
  const char *source2 =
  "func t1renamed() { }\n"
  "func t2() { }\n";

  // FIXME: Use the syntax kind directly instead of the serialization number.
  swiftparse_syntax_kind_t codeBlockItemList = 163;
  swiftparse_syntax_kind_t codeBlockItem = 92;

  // Assign id numbers to codeBlockItem nodes and collect the ids that are
  // listed as members of a codeBlockItemList node into a vector.
  // When we reparse, check that we got the parser to resuse the node id from
  // the previous parse.

  __block std::vector<int> nodeids;
  __block int idcounter = 0;
  size_t t2Offset = StringRef(source1).find("\nfunc t2");
  __block int t2NodeId = 0;
  __block size_t t2NodeLength = 0;
  swiftparse_node_handler_t nodeHandler =
    ^swiftparse_client_node_t(const swiftparse_syntax_node_t *raw_node) {
      if (raw_node->kind == codeBlockItem) {
        int nodeid = ++idcounter;
        if (raw_node->range.offset == t2Offset) {
          t2NodeId = nodeid;
          t2NodeLength = raw_node->range.length;
        }
        return (void*)(intptr_t)nodeid;
      }
      if (raw_node->kind == codeBlockItemList) {
        for (unsigned i = 0, e = raw_node->layout_data.nodes_count;
             i != e; ++i) {
          nodeids.push_back((int)(intptr_t)raw_node->layout_data.nodes[i]);
        }
      }
      return nullptr;
    };
  parse(source1, nodeHandler, nullptr);
  EXPECT_EQ(t2NodeId, 2);
  ASSERT_NE(t2NodeLength, size_t(0));
  EXPECT_EQ(nodeids, (std::vector<int>{1, 2}));

  nodeids.clear();
  idcounter = 1000;
  t2Offset = StringRef(source2).find("\nfunc t2");
  swiftparse_node_lookup_t nodeLookup =
    ^swiftparse_lookup_result_t(size_t offset, swiftparse_syntax_kind_t kind) {
      if (offset == t2Offset && kind == codeBlockItem) {
        return { t2NodeLength, (void*)(intptr_t)t2NodeId };
      } else {
        return {0, nullptr};
      }
    };

  parse(source2, nodeHandler, nodeLookup);
  EXPECT_EQ(nodeids, (std::vector<int>{1001, 2}));
}
