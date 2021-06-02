//===--- CPPTool.cpp ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Calls a method from one of the swift libraries.
///
//===----------------------------------------------------------------------===//

#include <cstdio>

extern "C" {
  int doSomething();
}

int main(int argc, char *argv[]) {
  printf("doSomething: %d\n", doSomething());
  return 0;
}
