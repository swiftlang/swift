//===--- swift-syntax-parser-test.cpp - Test util for C parser library ----===//
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
//
// Testing utility for the C API of the parser library.
//
//===----------------------------------------------------------------------===//

#include "swift-c/SyntaxParser/SwiftSyntaxParser.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Timer.h"

using namespace swift;
using namespace llvm;

enum class ActionType {
  DumpTree,
  Time,
};

namespace options {

static cl::opt<ActionType>
Action(cl::desc("Action (required):"),
       cl::init(ActionType::DumpTree),
       cl::values(
        clEnumValN(ActionType::DumpTree,
                   "dump-tree",
                   "Parse the source file and dump syntax tree"),
        clEnumValN(ActionType::Time,
                   "time",
                   "Time parsing, use '-n' to specify number of invocations")));

static cl::list<std::string>
Filename(cl::Positional, cl::desc("source file"), cl::Required);

static cl::opt<unsigned>
NumParses("n", cl::desc("number of invocations"), cl::init(1));

}

namespace {
struct SPNode {
  swiftparse_syntax_kind_t kind;
  StringRef nodeText;

  Optional<swiftparse_token_kind_t> tokKind;
  StringRef leadingTriviaText;
  StringRef tokenText;
  StringRef trailingTriviaText;

  std::vector<std::unique_ptr<SPNode>> members;

  LLVM_DUMP_METHOD void dump() {
    dump(errs());
  }

  void dump(raw_ostream &OS) {
    // FIXME: Return the syntax/token kinds directly from the C API, instead
    // of the serialization number, and print the kind identifier for each,
    // instead of the number.
    if (tokKind.hasValue()) {
      OS << "<t" << (unsigned)tokKind.getValue() << '>';
      OS << leadingTriviaText << '|' << tokenText << '|' << trailingTriviaText;
      OS << "</t" << (unsigned)tokKind.getValue() << '>';
    } else {
      OS << "<s" << (unsigned)kind << '>';
      for (const auto &mn : members) {
        if (mn) {
          mn->dump(OS);
        } else {
          OS << "<NULL/>";
        }
      }
      OS << "</s" << (unsigned)kind << '>';
    }
  }
};
}

static std::unique_ptr<SPNode>
convertClientNode(swiftparse_client_node_t client_node) {
  return std::unique_ptr<SPNode>((SPNode*)client_node);
}

static size_t trivialLen(ArrayRef<swiftparse_trivia_piece_t> trivia) {
  size_t len = 0;
  for (const auto &piece : trivia) {
    len += piece.length;
  }
  return len;
}

static swiftparse_client_node_t
makeNode(const swiftparse_syntax_node_t *raw_node, StringRef source) {
  SPNode *node = new SPNode();
  node->kind = raw_node->kind;
  auto range = raw_node->range;
  node->nodeText = source.substr(range.offset, range.length);
  if (raw_node->kind == 0) {
    node->tokKind = raw_node->token_data.kind;
    size_t leadingTriviaLen =
      trivialLen(makeArrayRef(raw_node->token_data.leading_trivia,
                              raw_node->token_data.leading_trivia_count));
    size_t trailingTriviaLen =
      trivialLen(makeArrayRef(raw_node->token_data.trailing_trivia,
                              raw_node->token_data.trailing_trivia_count));
    node->leadingTriviaText = node->nodeText.take_front(leadingTriviaLen);
    node->tokenText =
      node->nodeText.substr(leadingTriviaLen,
                            range.length-leadingTriviaLen-trailingTriviaLen);
    node->trailingTriviaText = node->nodeText.take_back(trailingTriviaLen);
  } else {
    for (unsigned i = 0, e = raw_node->layout_data.nodes_count; i != e; ++i) {
      auto subnode = convertClientNode(raw_node->layout_data.nodes[i]);
      node->members.push_back(std::move(subnode));
    }
  }
  return node;
}

static swiftparse_client_node_t
parse(const char *source, swiftparse_node_handler_t node_handler) {
  swiftparse_parser_t parser = swiftparse_parser_create();
  swiftparse_parser_set_node_handler(parser, node_handler);
  swiftparse_client_node_t top = swiftparse_parse_string(parser, source);
  swiftparse_parser_dispose(parser);
  return top;
}

static int dumpTree(const char *source) {
  swiftparse_node_handler_t nodeHandler =
    ^swiftparse_client_node_t(const swiftparse_syntax_node_t *raw_node) {
      return makeNode(raw_node, source);
    };

  std::unique_ptr<SPNode> top = convertClientNode(parse(source, nodeHandler));
  top->dump(outs());

  return 0;
}

static void printTimeRecord(unsigned numInvoks, const TimeRecord &total,
                            raw_ostream &OS) {
  if (total.getUserTime())
    OS << "   ---User Time---";
  if (total.getSystemTime())
    OS << "   --System Time--";
  if (total.getProcessTime())
    OS << "   --User+System--";
  OS << "   ---Wall Time---";
  if (total.getMemUsed())
    OS << "  ---Mem---";
  OS << '\n';

  auto printVal = [&](double Val) {
    OS << format("  %8.5f        ", Val);
  };

  printVal(total.getUserTime()/numInvoks);
  printVal(total.getSystemTime()/numInvoks);
  printVal(total.getProcessTime()/numInvoks);
  printVal(total.getWallTime()/numInvoks);

  OS << '\n';
}

static int timeParsing(const char *source, unsigned numInvoks) {
  swiftparse_node_handler_t nodeHandler =
    ^swiftparse_client_node_t(const swiftparse_syntax_node_t *raw_node) {
      return nullptr;
    };

  Timer timer;
  timer.startTimer();
  for (unsigned i = 0; i != numInvoks; ++i) {
    parse(source, nodeHandler);
  }
  timer.stopTimer();

  printTimeRecord(numInvoks, timer.getTotalTime(), outs());
  return 0;
}

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  cl::ParseCommandLineOptions(argc, argv, "Swift Syntax Parser Test\n");

  StringRef fname = options::Filename[0];
  auto fileBufOrErr = MemoryBuffer::getFile(fname);
  if (!fileBufOrErr) {
    errs() << "error opening file '" << fname << "': "
                 << fileBufOrErr.getError().message();
    return 1;
  }
  StringRef source = fileBufOrErr.get()->getBuffer();

  switch (options::Action) {
  case ActionType::DumpTree:
    return dumpTree(source.data());
  case ActionType::Time:
    return timeParsing(source.data(), options::NumParses);
  }
}
