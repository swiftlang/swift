//== ParseTestSpecification.cpp - Parsing for test instructions -*- C++ -*- ==//
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

#include "swift/SILOptimizer/Utils/ParseTestSpecification.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace swift::test;

namespace {

// Helpers: Finding marker instructions

void findAndDeleteTraceValues(SILFunction *function,
                              SmallVectorImpl<SILValue> &values) {
  InstructionDeleter deleter;
  for (auto &block : *function) {
    for (SILInstruction *inst : deleter.updatingRange(&block)) {
      if (auto *debugValue = dyn_cast<DebugValueInst>(inst)) {
        if (!debugValue->hasTrace())
          continue;
        values.push_back(debugValue->getOperand());
        deleter.forceDelete(debugValue);
      }
    }
  }
}

// Helpers: Looking up subobjects by index

SILInstruction *getInstruction(SILFunction *function, unsigned long index) {
  unsigned long i = 0;
  for (auto &block : *function) {
    for (auto &instruction : block) {
      if (i == index) {
        return &instruction;
      }
      ++i;
    }
  }
  llvm_unreachable("bad index!?");
}

SILInstruction *getInstruction(SILBasicBlock *block, unsigned long index) {
  unsigned long i = 0;
  for (auto &instruction : *block) {
    if (i == index) {
      return &instruction;
    }
    ++i;
  }
  llvm_unreachable("bad index!?");
}

SILBasicBlock *getBlock(SILFunction *function, unsigned long index) {
  auto iterator = function->begin();
  for (unsigned long i = 0; i < index; ++i) {
    iterator = std::next(iterator);
  }
  return &*iterator;
}

SILFunction *getFunction(SILModule *module, unsigned long long index) {
  auto &list = module->getFunctionList();
  auto iter = list.begin();
  for (unsigned long i = 0; i < index; ++i) {
    iter = std::next(iter);
  }
  return &*iter;
}

// Parsing:

class ParseTestSpecification;

class ParseArgumentSpecification {
  ParseTestSpecification &outer;
  StringRef specification;

  SILFunction *getFunction();
  SILValue getTraceValue(unsigned index, SILFunction *function);

public:
  ParseArgumentSpecification(ParseTestSpecification &outer,
                             StringRef specification)
      : outer(outer), specification(specification) {}

  Argument parse() {
    auto argument = parseArgument();
    demandEmpty();
    return argument;
  }

private:
  // String parsing helpers:

  bool empty() { return specification.empty(); }

  bool peekPrefix(StringRef prefix) { return specification.startswith(prefix); }

  bool consumePrefix(StringRef prefix) {
    return specification.consume_front(prefix);
  }

  bool consumeAll(StringRef remainder) {
    if (specification.size() == remainder.size())
      return specification.consume_front(remainder);
    return false;
  }

  bool consumeThrough(char separator, StringRef &substring) {
    unsigned long index = 0;
    if ((index = specification.find(separator)) == StringRef::npos)
      return false;
    substring = specification.slice(0, index);
    specification = specification.slice(index + 1, specification.size());
    return true;
  }

  void demandPrefix(StringRef prefix) {
    if (consumePrefix(prefix))
      return;
    llvm_unreachable("failed to find prefix in specification");
  }

  void demandEmpty() {
    assert(empty() && "specification unexpectedly inhabited?!");
  }

  // Argument parsing: Primitives

  Optional<BoolArgument> parseBool() {
    if (consumeAll("true")) {
      return BoolArgument{true};
    } else if (consumeAll("false")) {
      return BoolArgument{false};
    }
    return llvm::None;
  }

  Optional<UIntArgument> parseUInt() {
    unsigned long long value;
    if (llvm::consumeUnsignedInteger(specification, /*radix=*/10, value))
      return llvm::None;
    return UIntArgument{value};
  }

  Optional<StringArgument> parseString() {
    auto retval = StringArgument{specification};
    specification = specification.drop_front(specification.size());
    return retval;
  }

  Optional<TaggedUnion<unsigned long long, StringRef>> parseSubscript() {
    if (!consumePrefix("["))
      return llvm::None;
    unsigned long long index = 0;
    if (!llvm::consumeUnsignedInteger(specification, /*radix=*/10, index)) {
      demandPrefix("]");
      return {index};
    }
    StringRef string;
    if (!consumeThrough(']', string))
      llvm_unreachable("missing ] to end subscript operation!?");
    return {string};
  }

  // Argument parsing: References

  SILValue parseTraceComponent(SILFunction *within) {
    if (!consumePrefix("trace"))
      return SILValue();
    if (empty() || peekPrefix("."))
      return getTraceValue(0, within);
    if (auto subscript = parseSubscript()) {
      auto index = subscript->get<unsigned long long>();
      return getTraceValue(index, within);
    }
    llvm_unreachable("bad suffix after 'trace'!?");
  }

  Optional<Argument> parseTraceReference(SILFunction *within) {
    auto trace = parseTraceComponent(within);
    if (!trace)
      return llvm::None;
    if (!consumePrefix("."))
      return ValueArgument{trace};
    auto *instruction = trace.getDefiningInstruction();
    assert(instruction &&
           "attempting to access operand of non-instruction value!?");
    if (auto arg = parseOperandReference(instruction))
      return *arg;
    llvm_unreachable("bad suffix after 'trace'!?");
  }

  Operand *parseOperandComponent(SILInstruction *within) {
    if (!consumePrefix("operand"))
      return nullptr;
    if (empty()) {
      auto &operand = within->getOperandRef(0);
      return &operand;
    }
    if (auto subscript = parseSubscript()) {
      auto index = subscript->get<unsigned long long>();
      auto &operand = within->getOperandRef(index);
      return &operand;
    }
    llvm_unreachable("bad suffix after 'operand'!?");
  }

  Optional<Argument> parseOperandReference(SILInstruction *within) {
    auto *operand = parseOperandComponent(within);
    if (!operand)
      return llvm::None;
    return OperandArgument{operand};
  }

  SILInstruction *parseInstructionComponent(
      TaggedUnion<SILFunction *, SILBasicBlock *> within) {
    if (!consumePrefix("instruction"))
      return nullptr;
    auto getInstructionAtIndex = [within](unsigned index) {
      if (within.isa<SILFunction *>()) {
        auto *function = within.get<SILFunction *>();
        return getInstruction(function, index);
      }
      auto *block = within.get<SILBasicBlock *>();
      return getInstruction(block, index);
    };
    if (empty() || peekPrefix(".")) {
      return getInstructionAtIndex(0);
    }
    if (auto subscript = parseSubscript()) {
      auto index = subscript->get<unsigned long long>();
      return getInstructionAtIndex(index);
    }
    llvm_unreachable("bad suffix after 'instruction'!?");
  }

  Optional<Argument> parseInstructionReference(
      TaggedUnion<SILFunction *, SILBasicBlock *> within) {
    auto *instruction = parseInstructionComponent(within);
    if (!instruction)
      return llvm::None;
    if (!consumePrefix("."))
      return InstructionArgument{instruction};
    if (auto arg = parseOperandReference(instruction))
      return arg;
    llvm_unreachable("unhandled suffix after 'instruction'!?");
  }

  SILBasicBlock *parseBlockComponent(SILFunction *within) {
    if (!consumePrefix("block"))
      return nullptr;
    if (empty() || peekPrefix(".")) {
      auto *block = getBlock(within, 0);
      return block;
    }
    if (auto subscript = parseSubscript()) {
      auto index = subscript->get<unsigned long long>();
      auto *block = getBlock(within, index);
      return block;
    }
    llvm_unreachable("bad suffix after 'block'!?");
  }

  Optional<Argument> parseBlockReference(SILFunction *within) {
    auto *block = parseBlockComponent(within);
    if (!block)
      return llvm::None;
    if (!consumePrefix("."))
      return BlockArgument{block};
    if (auto arg = parseInstructionReference(block))
      return *arg;
    llvm_unreachable("unhandled suffix after 'block'!?");
  }

  SILFunction *parseFunctionComponent(SILModule *within) {
    if (!consumePrefix("function"))
      return nullptr;
    if (empty() || peekPrefix(".")) {
      return getFunction();
    }
    if (auto subscript = parseSubscript()) {
      if (subscript->isa<unsigned long long>()) {
        auto index = subscript->get<unsigned long long>();
        auto *fn = ::getFunction(within, index);
        return fn;
      } else {
        auto name = subscript->get<StringRef>();
        auto *specified = getFunction()->getModule().lookUpFunction(name);
        if (!specified)
          llvm_unreachable("unknown function name!?");
        return specified;
      }
    }
    llvm_unreachable("bad suffix after 'function'!?");
  }

  Optional<Argument> parseFunctionReference(SILModule *within) {
    auto *function = parseFunctionComponent(within);
    if (!function)
      return llvm::None;
    if (!consumePrefix("."))
      return FunctionArgument{function};
    if (auto arg = parseInstructionReference(function))
      return *arg;
    if (auto arg = parseTraceReference(function))
      return *arg;
    if (auto arg = parseBlockReference(function))
      return *arg;
    llvm_unreachable("unhandled suffix after 'function'!?");
  }

  Optional<Argument> parseReference() {
    if (!consumePrefix("@"))
      return llvm::None;
    if (auto arg = parseTraceReference(getFunction()))
      return *arg;
    if (auto arg = parseOperandReference(getInstruction(getFunction(), 0)))
      return *arg;
    if (auto arg = parseInstructionReference(getFunction()))
      return *arg;
    if (auto arg = parseBlockReference(getFunction()))
      return *arg;
    if (auto arg = parseFunctionReference(&getFunction()->getModule()))
      return *arg;
    return llvm::None;
  }

  Argument parseArgument() {
    if (auto arg = parseBool())
      return *arg;
    if (auto arg = parseUInt())
      return *arg;
    if (auto arg = parseReference())
      return *arg;
    // Parse strings last--everything parses as a string.
    if (auto arg = parseString())
      return *arg;
    llvm_unreachable("bad argument specification");
  }
};

using TraceValueMap =
    llvm::SmallDenseMap<SILFunction *, llvm::SmallVector<SILValue, 4>, 32>;
/// A global map from functions to the trace instructions that they once
/// contained.
static TraceValueMap traceValues;

class ParseTestSpecification {
  friend class ParseArgumentSpecification;
  SILFunction *function;
  SmallVectorImpl<StringRef> &components;

  SILValue getTraceValue(unsigned index, SILFunction *function) {
    auto iterator = traceValues.find(function);
    if (iterator == traceValues.end()) {
      // llvm::SmallVector<SILValue, 4> values;
      auto &values = traceValues[function];
      findAndDeleteTraceValues(function, values);
      // auto pair = traceValues.insert({function, values});
      // assert(pair.second);
      iterator = traceValues.find(function);
    }
    return iterator->getSecond()[index];
  }

public:
  ParseTestSpecification(SILFunction *function,
                         SmallVectorImpl<StringRef> &components)
      : function(function), components(components) {}

  void parse(StringRef specification, Arguments &arguments) {
    specification.split(components, " ");
    for (unsigned long index = 0, size = components.size(); index < size;
         ++index) {
      auto component = components[index];
      ParseArgumentSpecification parser(*this, component);
      auto argument = parser.parse();
      arguments.storage.push_back(argument);
    }
  }
};

SILFunction *ParseArgumentSpecification::getFunction() {
  return outer.function;
}
SILValue ParseArgumentSpecification::getTraceValue(unsigned index,
                                                   SILFunction *function) {
  return outer.getTraceValue(index, function);
}

} // anonymous namespace

// API

void swift::test::getTestSpecifications(
    SILFunction *function, SmallVectorImpl<std::string> &specifications) {
  InstructionDeleter deleter;
  for (auto &block : *function) {
    for (SILInstruction *inst : deleter.updatingRange(&block)) {
      if (auto *tsi = dyn_cast<TestSpecificationInst>(inst)) {
        auto ref = tsi->getArgumentsSpecification();
        specifications.push_back(std::string(ref.begin(), ref.end()));
        deleter.forceDelete(tsi);
      }
    }
  }
}

void swift::test::parseTestArgumentsFromSpecification(
    SILFunction *function, StringRef specification, Arguments &arguments,
    SmallVectorImpl<StringRef> &argumentStrings) {
  ParseTestSpecification parser(function, argumentStrings);
  parser.parse(specification, arguments);
}
