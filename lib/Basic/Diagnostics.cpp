//===- Diagnostics.cpp - Diagnostic Definitions -----------------*- C++ -*-===//
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
//  This file defines all of the diagnostics emitted by Swift.
//
//===----------------------------------------------------------------------===//
#include "swift/Basic/Diagnostics.h"

using namespace swift;

// \brief Define all of the 
#define DIAG(KIND,ID,Category,Options,Text,...) \
  Diag<__VA_ARGS__> swift::diags::ID = { DiagID::ID };
#include "swift/Basic/Diagnostics.def"  
