//===--- BuiltinMappedTypes.def - Mapped Type Database ----------*- C++ -*-===//
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
// This file defines the database of builtin C types that are imported as swift
// stdlib types.
//
// MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)
//  - CLANG_BUILTIN_KIND is the kind of builtin type, clang::BuiltinType
//  - SWIFT_TYPE_NAME is the name of the corresponding stdlib type.
//
// MAP_BUILTIN_INTEGER_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)
//  - CLANG_BUILTIN_KIND is the kind of builtin type, clang::BuiltinType
//  - SWIFT_TYPE_NAME is the name of the corresponding stdlib type.
//
//===----------------------------------------------------------------------===//

#ifndef MAP_BUILTIN_INTEGER_TYPE
#define MAP_BUILTIN_INTEGER_TYPE(CLANG, SWIFT) MAP_BUILTIN_TYPE(CLANG, SWIFT)
#endif

MAP_BUILTIN_TYPE(Bool,      CBool)
MAP_BUILTIN_INTEGER_TYPE(Char_U,    CChar)
MAP_BUILTIN_INTEGER_TYPE(Char_S,    CChar)
MAP_BUILTIN_INTEGER_TYPE(UChar,     CUnsignedChar)
MAP_BUILTIN_INTEGER_TYPE(UShort,    CUnsignedShort)
MAP_BUILTIN_INTEGER_TYPE(UInt,      CUnsignedInt)
MAP_BUILTIN_INTEGER_TYPE(ULong,     CUnsignedLong)
MAP_BUILTIN_INTEGER_TYPE(ULongLong, CUnsignedLongLong)
MAP_BUILTIN_INTEGER_TYPE(UInt128,   CUnsignedInt128)
MAP_BUILTIN_INTEGER_TYPE(WChar_S,   CWideChar)
MAP_BUILTIN_INTEGER_TYPE(WChar_U,   CWideChar)
MAP_BUILTIN_INTEGER_TYPE(Char16,    CChar16)
MAP_BUILTIN_INTEGER_TYPE(Char32,    CChar32)
MAP_BUILTIN_INTEGER_TYPE(SChar,     CSignedChar)
MAP_BUILTIN_INTEGER_TYPE(Short,     CShort)
MAP_BUILTIN_INTEGER_TYPE(Int,       CInt)
MAP_BUILTIN_INTEGER_TYPE(Long,      CLong)
MAP_BUILTIN_INTEGER_TYPE(LongLong,  CLongLong)
MAP_BUILTIN_INTEGER_TYPE(Int128,    CInt128)
MAP_BUILTIN_TYPE(Float,     CFloat)
MAP_BUILTIN_TYPE(Double,    CDouble)
MAP_BUILTIN_TYPE(LongDouble, CLongDouble)

#undef MAP_BUILTIN_TYPE
#undef MAP_BUILTIN_INTEGER_TYPE

