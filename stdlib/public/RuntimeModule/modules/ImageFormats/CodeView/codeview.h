//===--- codeview.h - Definitions of CodwView structures for Swift --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Definitions of CodeView structures for import into Swift code
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CODEVIEW_H_
#define SWIFT_CODEVIEW_H_

#include <stdint.h>

#ifdef __cplusplus
namespace swift {
namespace runtime {
#endif

typedef uint32_t DWORD;
typedef char CHAR;

typedef struct {
  uint32_t Data1;
  uint16_t Data2;
  uint16_t Data3;
  uint8_t  Data4[8];
} GUID;

typedef struct {
  DWORD dwMagic;
  GUID  Signature;
  DWORD dwAge;
  CHAR  PdbFileName[1];
} CV_PDB70_INFO;

#define CV_PDB70_MAGIC 0x53445352

#ifdef __cplusplus
} // namespace runtime
} // namespace swift
#endif

#endif // SWIFT_CODEVIEW_H_
