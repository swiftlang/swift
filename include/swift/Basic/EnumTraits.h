//===--- EnumTraits.h - Traits for densely-packed enums ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  This file defines the EnumTraits concept, which can be used to
///  communicate information about an enum type's enumerators that currently
///  can't be recovered from the compiler.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ENUMTRAITS_H
#define SWIFT_BASIC_ENUMTRAITS_H

namespace swift {

/// A simple traits concept for recording the number of cases in an enum.
///
///  template <> class EnumTraits<WdigetKind> {
///    static constexpr size_t NumValues = NumWidgetKinds;
///  };
template <class E>
struct EnumTraits;

} // end namespace swift

#endif
