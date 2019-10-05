//===--- DiagnosticUndefMacros.h - Shared Diagnostic Macros -------*- C++ -*-===//
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
//  This file un-defines macros shared across diagnostic definition files.
//
//===----------------------------------------------------------------------===//

#ifndef DIAG_NO_UNDEF
# if defined(DIAG)
#  undef DIAG
# endif
# undef REMARK
# undef NOTE
# undef WARNING
# undef ERROR
# undef FIXIT
# undef PUBLICERROR
# undef PUBLICWARNING
# undef PUBLICNOTE
# undef INTERNALERROR
# undef INTERNALWARNING
#endif
