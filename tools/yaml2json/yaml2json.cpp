//===------------ yaml2json.cpp - YAML to JSON conversion -----------------===//
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
// This file will convert the provided YAML file to JSON and print the resulting
// JSON to stdout.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/JSONSerialization.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace swift;

/// Prints an error to the console and exits with a non-zero exit code.
LLVM_ATTRIBUTE_NORETURN
void show_error(std::string msg) {
  llvm::outs() << "error: " << msg << "\n";
  exit(-1);
}

/// Wraps a json::Output and converts the nodes in a YAML stream to
/// JSON nodes.
class YAML2JSON {
  /// The underlying JSON output that will translate these values.
  json::Output jsonOut;

  /// The stream the JSON output is outputting to.
  raw_ostream &os;

  /// The input YAML stream.
  yaml::Stream &stream;

  /// Outputs a key and value as a JSON pair.
  void outputKeyValueNode(yaml::KeyValueNode *node) {
    bool useDefault = false;
    void *context = nullptr;
    auto stringNode = dyn_cast<yaml::ScalarNode>(node->getKey());
    llvm::SmallString<24> scratch;
    jsonOut.preflightKey(stringNode->getValue(scratch).str().c_str(),
                         /*Required=*/true,
                         /*SameAsDefault=*/true,
                         /*UseDefault=*/useDefault, context);
    jsonOut.postflightKey(context);
    output(node->getValue());
  }

  /// Outputs a YAML map to a JSON object.
  void outputMappingNode(yaml::MappingNode *node) {
    jsonOut.beginObject();
    for (auto &child : *node) {
      outputKeyValueNode(&child);
    }
    jsonOut.endObject();
  }

  /// Outputs a YAML sequence to a JSON array.
  void outputSequenceNode(yaml::SequenceNode *node) {
    jsonOut.beginArray();
    unsigned index = 0;
    void *context = nullptr;
    for (auto &child : *node) {
      jsonOut.preflightElement(index, context);
      output(&child);
      jsonOut.postflightElement(context);
      ++index;
    }
    jsonOut.endArray();
  }

  // LLVM's YAML parser makes no distinction between numeric/bool/null values
  // and strings. Check to see if we've got an explicitly-quoted string here
  // and only require quotes if it's explicitly quoted, non-numeric,
  // non-boolean, and non-null.
  // This function is very basic and doesn't handle things like hexadecimal
  // numbers which are otherwise supported by YAML.
  bool mustQuoteRawString(StringRef string) {
    if (string.startswith("\"")) return true;
    if (string == "true") return false;
    if (string == "false") return false;
    if (string == "null") return false;

    auto cStr = string.str().c_str();
    char *endPtr = nullptr;

    // Check if the value is an integer
    (void)strtol(cStr, &endPtr, 10);
    if (endPtr == &cStr[string.size()]) return false;

    endPtr = nullptr;

    // Check if the value is a floating-point number
    (void)strtod(cStr, &endPtr);
    if (endPtr == &cStr[string.size()]) return false;

    return true;
  }

  /// Outputs a YAML string to a JSON string.
  void outputScalarNode(yaml::ScalarNode *node) {
    llvm::SmallString<24> scratch;
    auto string = node->getValue(scratch);
    jsonOut.scalarString(string, mustQuoteRawString(node->getRawValue()));
  }

  /// Outputs a YAML block string to a JSON string.
  void outputBlockScalarNode(yaml::BlockScalarNode *node) {
    auto string = node->getValue();
    jsonOut.scalarString(string, /*mustQuote=*/true);
  }

  /// Outputs a YAML `null` to a JSON `null`.
  void outputNullNode(yaml::NullNode *node) {
    StringRef str = "null";
    jsonOut.scalarString(str, /*mustQuote=*/false);
  }

  /// Determines the underlying node structure and outputs it.
  void output(const yaml::Node *node) {
    assert(node && "node must not be null");
    if (auto n = dyn_cast<yaml::MappingNode>(node)) {
      return outputMappingNode(const_cast<yaml::MappingNode *>(n));
    }
    if (auto n = dyn_cast<yaml::ScalarNode>(node)) {
      return outputScalarNode(const_cast<yaml::ScalarNode *>(n));
    }
    if (auto n = dyn_cast<yaml::BlockScalarNode>(node)) {
      return outputBlockScalarNode(const_cast<yaml::BlockScalarNode *>(n));
    }
    if (auto n = dyn_cast<yaml::SequenceNode>(node)) {
      return outputSequenceNode(const_cast<yaml::SequenceNode *>(n));
    }
    if (auto n = dyn_cast<yaml::NullNode>(node)) {
      return outputNullNode(const_cast<yaml::NullNode *>(n));
    }
    if (auto n = dyn_cast<yaml::AliasNode>(node)) {
      show_error("cannot serialize aliases as JSON");
    }
    llvm_unreachable("unknown node type");
  }
public:
  /// Creates a YAML2JSON to output the root of a YAML
  YAML2JSON(raw_ostream &os, yaml::Stream &stream, bool prettyPrint = false)
    : jsonOut(os, prettyPrint), os(os), stream(stream) {}

  /// Converts the input YAML stream to JSON on the provided output stream.
  void output() {
    auto iter = stream.begin();
    if (iter == stream.end()) {
      show_error("invalid yaml");
    }
    output(iter->getRoot());
    os << "\n";
  }
};

int main(int argc, char **argv) {
  cl::opt<std::string> inputFilename(cl::Positional,
     cl::desc("The YAML file to convert to JSON"));
  cl::opt<bool> prettyPrint("pretty-print",
     cl::desc("Pretty-print the resulting JSON"));

  cl::ParseCommandLineOptions(argc, argv);

  auto bufOrErr = MemoryBuffer::getFile(inputFilename);
  if (auto error = bufOrErr.getError()) {
    show_error(error.message());
  }
  auto buf = std::move(bufOrErr.get());

  llvm::SourceMgr sm;
  yaml::Stream stream(buf->getBuffer(), sm);

  YAML2JSON yaml2json(llvm::outs(), stream, prettyPrint);
  yaml2json.output();

  return 0;
}
