//===--- XMLValidator.h - XML validation ------------------------*- C++ -*-===//
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

#ifndef SWIFT_IDE_TEST_XML_VALIDATOR_H
#define SWIFT_IDE_TEST_XML_VALIDATOR_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class XMLValidator {
  struct Implementation;
  Implementation *Impl;

public:
  XMLValidator();
  ~XMLValidator();

  void setSchema(StringRef FileName);

  enum class ErrorCode {
    Valid,
    NotCompiledIn,
    NoSchema,
    BadSchema,
    NotWellFormed,
    NotValid,
    InternalError,
  };
  struct Status {
    ErrorCode Code;
    std::string Message;
  };

  Status validate(const std::string &XML);
};

} // end namespace swift

#endif // SWIFT_IDE_TEST_XML_VALIDATOR_H

