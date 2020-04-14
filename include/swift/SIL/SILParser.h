//===--- SILParser.h - SIL Undef Value Representation -----------*- C++ -*-===//
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

#ifndef SWIFT_SIL_PARSER_H
#define SWIFT_SIL_PARSER_H

#include "swift/AST/DeclContext.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Parser.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {
namespace syntax {

//===----------------------------------------------------------------------===//
// General types for parsing
//===----------------------------------------------------------------------===//

// These are both StringRefs used to represent operand ids such as %abc or %0.
using ValueID = StringRef;

struct SILParserOperand {
  ValueID value;
  // TODO: the type parsing should follow a similar model, eventually.
  SILType type;

  SILParserOperand(ValueID val, SILType ty) : value(val), type(ty) {}
};

// A list of parser operand ids. This is used primarily for keeping track of the
// result(s) of a sil instruction.
using SILParserValues = SmallVector<ValueID, 4>;

// Similar to SILParserValues but includes a type for each value.
struct SILParserOperands : SmallVector<SILParserOperand, 8> {};

// An unsigned integer where each bit position corresponds to a different
// attribute.
// TODO: define and implement this.
using SILParserAttributes = uint32_t;

// A location range that corisponds to the start of an instruction and the end
// of an instruction. The Reader will exit early if it runs into an error and
// let the checker diagnose the problem. Knowing where the reader got to (end)
// will allow for more accurate diagnosis of the error and location printing.
struct SourceLocRange {
  SourceLoc begin;
  SourceLoc end;
};

// SILParserResult holds all needed information about any instruction that we
// read/parse. The reader will return this type and the checker and emitter will
// take this type and use it to check or emit sil instructions.
struct SILParserResult {
  SILInstructionKind kind = SILInstructionKind(0);
  SILParserValues results;
  SILParserOperands operands;
  SourceLocRange loc;
  SILParserAttributes attrs;
};

// TODO: move this back to ParseSIL.
struct UnresolvedValueName {
  StringRef Name;
  SourceLoc NameLoc;

  bool isUndef() const { return Name == "undef"; }
};

//===----------------------------------------------------------------------===//
// SIL Reader definition
//===----------------------------------------------------------------------===//

// Provides default implementation for each visitor method. This default
// implementation will be called if one has not been provided by the visitor.
class ReadSILBase {
public:
  // The default implementation of the visitor is to set the parser result to
  // `0` so that we know that we cannot handle this type of instruction.
#define FULL_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE)                   \
  void read##ID(SILParserResult &out) { \
    out.kind = SILInstructionKind(0); \
  }
#include "swift/SIL/SILNodes.def"
};

// The main sil "Parser" class. Reads and visits sil instructions. Reads the sil
// instructions into a SILParserResult which can be checked and emitted.
// TODO: Parser should be private inheritance
class ReadSIL : public ReadSILBase, public Parser {
  bool readSingleID(SILParserValues &instResults);

public:
  // Entry point for the visitor. Reads a sil instruction and visits it.
  SILParserResult read();

  /// Old methods moved to this parser.
  /// Parse the top-level SIL decls into the SIL module.
  void parseTopLevelSIL();

  ReadSIL(unsigned bufferID, SourceFile &SF, SILParserTUStateBase *SIL)
      : Parser(bufferID, SF, SIL,
               /*SPActions*/ nullptr, /*PersistentState*/ nullptr) {}
};

//===----------------------------------------------------------------------===//
// SIL Checker definition
//===----------------------------------------------------------------------===//

// Provides default implementation for each visitor method. This default
// implementation will be called if one has not been provided by the visitor.
class CheckSILBase {
public:
  // The default implementation is to return true, or "no errors". This is
  // nessisary so that we do not fail when bailing to the old implementation (in
  // SILParser).
#define FULL_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE)                   \
  bool check##ID(SILParserResult) { return true; }
#include "swift/SIL/SILNodes.def"
};

// A visitor for checking sil instructions as parser results. The job of this
// class is to make sure that errors do not occor when emitting the instruction.
// The Emitter should be allowed to make any assumptions it needs to about
// operands, results, types, and the existance of those. This checker should,
// therefore, check that there are the correct number of operands, results,
// attributes, types, etc. and that all those things exist and have already been
// parsed and/or emitted.
class CheckSIL : public CheckSILBase {
public:
  // Entry point for the visitor.
  bool check(SILParserResult);
};

//===----------------------------------------------------------------------===//
// SIL Emitter definition
//===----------------------------------------------------------------------===//

// Provides default implementation for each visitor method. This default
// implementation will be called if one has not been provided by the visitor.
class EmitSILBase {
public:
  // The default implementation is to return a nullptr. This is the only time a
  // nullptr will be returned from this method and signifies to the caller that
  // it should fallback to the old parser.
#define FULL_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE)                   \
  SILInstruction *emit##ID(SILBuilder &, SILParserResult) { return nullptr; }
#include "swift/SIL/SILNodes.def"
};

// A visitor for emitting sil instructions based on a *valid* SILParserResult.
class EmitSIL : public EmitSILBase {
public:
  // The main entry point for this visitor. It is required that a sil
  // instruction can be created given the information in SILParserResult.
  // Therefore, this method should never return nullptr. The only exception is
  // to signal that an implementation for the given sil instruction has not yet
  // been created so, the caller should revert to the old parser. "builder" will
  // be used to create this sil instruction.
  SILInstruction *emit(SILBuilder &builder, SILParserResult);
};

} // namespace syntax
} // namespace swift

#endif
