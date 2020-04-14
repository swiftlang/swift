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

#include "swift/SIL/SILParser.h"
#include "llvm/Support/PrettyStackTrace.h"

namespace swift {
  /// Interface between the Parse and ParseSIL libraries, to avoid circular
  /// dependencies.
  class SILParserTUStateBase {
    virtual void anchor();
  protected:
    SILParserTUStateBase() = default;
    virtual ~SILParserTUStateBase() = default;
  public:
    virtual bool parseDeclSIL(ReadSIL &P) = 0;
    virtual bool parseDeclSILStage(ReadSIL &P) = 0;
    virtual bool parseSILVTable(ReadSIL &P) = 0;
    virtual bool parseSILGlobal(ReadSIL &P) = 0;
    virtual bool parseSILWitnessTable(ReadSIL &P) = 0;
    virtual bool parseSILDefaultWitnessTable(ReadSIL &P) = 0;
    virtual bool parseSILDifferentiabilityWitness(ReadSIL &P) = 0;
    virtual bool parseSILCoverageMap(ReadSIL &P) = 0;
    virtual bool parseSILProperty(ReadSIL &P) = 0;
    virtual bool parseSILScope(ReadSIL &P) = 0;
  };
} // end namespace swift

#endif // SWIFT_PARSER_PARSESILSUPPORT_H

