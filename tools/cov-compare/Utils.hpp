//===----------- Utils.hpp - Tools for analyzing llvm profdata ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef Utils_hpp
#define Utils_hpp

#include <stdio.h>
#include <string>
#include <system_error>
#include "llvm/Support/raw_ostream.h"

namespace covcompare {
  std::string extractSymbol(std::string symbol);
  
  /// Attempts to demangle a C++ symbol, returning
  /// the mangled symbol if it fails.
  std::string demangled(std::string symbol);
  
  /// Prints an error message and exits.
  void exitWithErrorCode(std::error_code error);
  
  /// Formats a double as xxx.xx%
  std::string formattedDouble(double n);
  
  /// Either reads the provided file or returns stdout if the file is empty.
  std::unique_ptr<llvm::raw_ostream> streamForFile(std::string file);
}

#endif /* Utils_hpp */
