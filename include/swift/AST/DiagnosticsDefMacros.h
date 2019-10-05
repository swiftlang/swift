//===--- DiagnosticDefMacros.h - Shared Diagnostic Macros -------*- C++ -*-===//
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
//
//  This file defines macros shared across diagnostic definition files.
//
//===----------------------------------------------------------------------===//

#if !(defined(DIAG) || (defined(ERROR) && defined(WARNING) && defined(NOTE)))
#  error Must define either DIAG or the set {ERROR,WARNING,NOTE}
#endif

#ifndef ERROR
#  define ERROR(ID,Options,Text,Signature)   \
  DIAG(ERROR,ID,Options,Text,Signature)
#endif

#ifndef WARNING
#  define WARNING(ID,Options,Text,Signature) \
  DIAG(WARNING,ID,Options,Text,Signature)
#endif

#ifndef NOTE
#  define NOTE(ID,Options,Text,Signature) \
  DIAG(NOTE,ID,Options,Text,Signature)
#endif

#ifndef REMARK
#  define REMARK(ID,Options,Text,Signature) \
  DIAG(REMARK,ID,Options,Text,Signature)
#endif

#ifndef FIXIT
#  define FIXIT(ID, Text, Signature)
#endif

#ifndef PUBLICERROR
#  define PUBLICERROR(ID, PUBLICID, OPTIONS, TEXT, SIGNATURE) \
  ERROR(ID, OPTIONS, TEXT, SIGNATURE)
#endif

#ifndef PUBLICWARNING
#  define PUBLICWARNING(ID, PUBLICID, OPTIONS, TEXT, SIGNATURE) \
  WARNING(ID, OPTIONS, TEXT, SIGNATURE)
#endif

#ifndef PUBLICNOTE
#  define PUBLICNOTE(ID, PUBLICID, OPTIONS, TEXT, SIGNATURE) \
  NOTE(ID, OPTIONS, TEXT, SIGNATURE)
#endif

#define INTERNALERROR(ID,Options,Text,Signature) \
ERROR(ID,Options,Text,Signature)

#define INTERNALWARNING(ID,Options,Text,Signature) \
  WARNING(ID,Options,Text,Signature)
