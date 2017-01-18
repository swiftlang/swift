//===--- TypeCheckerDebugConsumer.h - Type checker debug log consumer -----===//
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

#ifndef SWIFT_AST_TYPE_CHECKER_DEBUG_CONSUMER_H
#define SWIFT_AST_TYPE_CHECKER_DEBUG_CONSUMER_H

#include "llvm/Support/raw_ostream.h"
#include <string>

namespace swift {

/// \brief A consumer of type checker debug output.
class TypeCheckerDebugConsumer {
public:
  virtual ~TypeCheckerDebugConsumer();

  virtual raw_ostream &getStream() = 0;
};

/// \brief A consumer of type checker debug output that dumps the information
/// to stderr.
class StderrTypeCheckerDebugConsumer : public TypeCheckerDebugConsumer {
public:
  raw_ostream &getStream() override {
    return llvm::errs();
  }
};

/// \brief A base class for a custom consumer of type checker debug output.
class CapturingTypeCheckerDebugConsumer : public TypeCheckerDebugConsumer {
  raw_ostream *Log;

public:
  CapturingTypeCheckerDebugConsumer();
  ~CapturingTypeCheckerDebugConsumer();

  raw_ostream &getStream() override {
    return *Log;
  }

  virtual void handleMessage(StringRef Message) = 0;
};

} // namespace swift

#endif // SWIFT_AST_TYPE_CHECKER_DEBUG_CONSUMER_H

