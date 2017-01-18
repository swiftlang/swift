//===--- messages.h - Remote reflection testing messages ------------------===//
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

static const char *REQUEST_INSTANCE_KIND = "k\n";
static const char *REQUEST_INSTANCE_ADDRESS = "i\n";
static const char *REQUEST_REFLECTION_INFO = "r\n";
static const char *REQUEST_READ_BYTES = "b\n";
static const char *REQUEST_SYMBOL_ADDRESS = "s\n";
static const char *REQUEST_STRING_LENGTH = "l\n";
static const char *REQUEST_POINTER_SIZE = "p\n";
static const char *REQUEST_DONE = "d\n";

typedef enum InstanceKind {
  None,
  Object,
  Existential,
  ErrorExistential,
  Closure
} InstanceKind;
