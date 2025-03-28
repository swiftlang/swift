//===--- ParseSILSupport.h - Interface with ParseSIL ------------*- C++ -*-===//
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

#ifndef SWIFT_PARSER_PARSESILSUPPORT_H
#define SWIFT_PARSER_PARSESILSUPPORT_H

#include "llvm/Support/PrettyStackTrace.h"

namespace swift {
  class Parser;

  /// Interface between the Parse and ParseSIL libraries, to avoid circular
  /// dependencies.
  class SILParserStateBase {
    virtual void anchor();
  protected:
    SILParserStateBase() = default;
    virtual ~SILParserStateBase() = default;
  public:
    virtual bool parseDeclSIL(Parser &P) = 0;
    virtual bool parseDeclSILStage(Parser &P) = 0;
    virtual bool parseSILVTable(Parser &P) = 0;
    virtual bool parseSILMoveOnlyDeinit(Parser &P) = 0;
    virtual bool parseSILGlobal(Parser &P) = 0;
    virtual bool parseSILWitnessTable(Parser &P) = 0;
    virtual bool parseSILDefaultWitnessTable(Parser &P) = 0;
    virtual bool parseSILDefaultOverrideTable(Parser &P) = 0;
    virtual bool parseSILDifferentiabilityWitness(Parser &P) = 0;
    virtual bool parseSILCoverageMap(Parser &P) = 0;
    virtual bool parseSILProperty(Parser &P) = 0;
    virtual bool parseSILScope(Parser &P) = 0;
  };
} // end namespace swift

#endif // SWIFT_PARSER_PARSESILSUPPORT_H

