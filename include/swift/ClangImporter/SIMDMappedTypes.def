//===--- SIMDMappedTypes.def - Mapped Type Database -------------*- C++ -*-===//
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
// This file defines the database of builtin C types, vectors of which are
// imported as swift SIMD types.
//
//
// MAP_SIMD_TYPE(C_TYPE, SCALAR_TYPE, BUILTIN_KIND)
//
//   maps vectors of the Clang builtin type identified by BUILTIN_KIND to
//   Swift types in the SIMD module.
//
//===----------------------------------------------------------------------===//

MAP_SIMD_TYPE(float, float, Float)
MAP_SIMD_TYPE(double, double, Double)
MAP_SIMD_TYPE(int, int, Int)
MAP_SIMD_TYPE(uint, unsigned int, UInt)

#undef MAP_SIMD_TYPE
