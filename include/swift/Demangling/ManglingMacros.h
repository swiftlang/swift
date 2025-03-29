//===--- ManglingMacros.h - Macros for Swift symbol mangling ----*- C++ -*-===//
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

#ifndef SWIFT_DEMANGLING_MANGLING_MACROS_H
#define SWIFT_DEMANGLING_MANGLING_MACROS_H

#define STRINGIFY_MANGLING(M) #M
#define MANGLE_AS_STRING(M) STRINGIFY_MANGLING(M)

/// The mangling prefix for the new mangling.
#if defined(__clang__)
_Pragma("clang diagnostic push")
_Pragma("clang diagnostic ignored \"-Wdollar-in-identifier-extension\"")
#endif
#define MANGLING_PREFIX $s
#define MANGLING_PREFIX_EMBEDDED $e
#if defined(__clang__)
_Pragma("clang diagnostic pop")
#endif

#define MANGLING_PREFIX_STR MANGLE_AS_STRING(MANGLING_PREFIX)
#define MANGLING_PREFIX_EMBEDDED_STR MANGLE_AS_STRING(MANGLING_PREFIX_EMBEDDED)

// The following macros help to create symbol manglings. They can be used
// if a mangled name is needed at compile-time, e.g. for variable names in the
// swift runtime libraries.

#define MANGLING_CONCAT2_IMPL(a, b) a##b
#define MANGLING_CONCAT3_IMPL(a, b, c) a##b##c

#define MANGLING_CONCAT2(a, b) MANGLING_CONCAT2_IMPL(a, b)
#define MANGLING_CONCAT3(a, b, c) MANGLING_CONCAT3_IMPL(a, b, c)
#define MANGLE_SYM(Ops) MANGLING_CONCAT2(MANGLING_PREFIX, Ops)
#define METADATA_MANGLING N
#define METATYPE_MANGLING m
#define EMPTY_TUPLE_MANGLING yt
#define ANY_MANGLING yp
#define ANYOBJECT_MANGLING yXl
#define NO_ARGS_MANGLING yy
#define FUNC_TYPE_MANGLING c
#define NOESCAPE_FUNC_TYPE_MANGLING XE
#define DIFF_FUNC_TYPE_MANGLING Yjrc
#define OBJC_PARTIAL_APPLY_THUNK_MANGLING Ta
#define OPTIONAL_MANGLING(Ty) MANGLING_CONCAT2_IMPL(Ty, Sg)

#define FUNCTION_MANGLING \
          MANGLING_CONCAT2(NO_ARGS_MANGLING, FUNC_TYPE_MANGLING)

#define DIFF_FUNCTION_MANGLING \
          MANGLING_CONCAT2(NO_ARGS_MANGLING, DIFF_FUNC_TYPE_MANGLING)

#define NOESCAPE_FUNCTION_MANGLING \
          MANGLING_CONCAT2(NO_ARGS_MANGLING, NOESCAPE_FUNC_TYPE_MANGLING)

#define THIN_FUNCTION_MANGLING \
          MANGLING_CONCAT2(NO_ARGS_MANGLING, Xf)

#define METADATA_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT2(Ty, METADATA_MANGLING))

#define STRUCT_METADATA_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT3(Ty, V, METADATA_MANGLING))

#define CLASS_METADATA_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT3(Ty, C, METADATA_MANGLING))

#define STRUCT_MD_ACCESSOR_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT3(Ty, V, Ma))

#define VALUE_WITNESS_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT2(Ty, WV))

#define METATYPE_VALUE_WITNESS_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT3(Ty, METATYPE_MANGLING, WV))

#define NOMINAL_TYPE_DESCR_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT2(Ty, Mn))

#define STRUCT_TYPE_DESCR_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT3(Ty, V, Mn))

#define PROTOCOL_DESCR_SYM(Ty) \
          MANGLE_SYM(MANGLING_CONCAT2(Ty, Mp))

#define OBJC_PARTIAL_APPLY_THUNK_SYM \
          MANGLE_SYM(OBJC_PARTIAL_APPLY_THUNK_MANGLING)

#endif // SWIFT_DEMANGLING_MANGLING_MACROS_H

