//===--- Sema.h - Swift Language Semantic Analysis --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Sema interface which implement hooks invoked by the 
// parser to build the AST.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_H
#define SWIFT_SEMA_H

namespace llvm {
}

namespace swift {
class Sema {

  Sema(const Sema&);           // DO NOT IMPLEMENT
  void operator=(const Sema&); // DO NOT IMPLEMENT
public:
  Sema();
};

} // end namespace swift

#endif
