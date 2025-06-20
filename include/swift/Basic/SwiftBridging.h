//===--- Basic/SwiftBridging.h ----------------------------------*- C++ -*-===//
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
///
/// This is a wrapper around `<swift/bridging>` that redefines `SWIFT_NAME` to
/// accept a string literal, and some other macros in case the header is
/// unavailable (e.g. during bootstrapping). String literals enable us to
/// properly format the long Swift declaration names that many of our bridging
/// functions have.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SWIFT_BRIDGING_H
#define SWIFT_BASIC_SWIFT_BRIDGING_H

#include "swift/Basic/Compiler.h"
#if __has_include(<swift/bridging>)
#include <swift/bridging>
#else

#if __has_attribute(swift_attr)

/// Specifies that a C++ `class` or `struct` owns and controls the lifetime of
/// all of the objects it references. Such type should not reference any
/// objects whose lifetime is controlled externally. This annotation allows
/// Swift to import methods that return a `class` or `struct` type that is
/// annotated with this macro.
#define SWIFT_SELF_CONTAINED __attribute__((swift_attr("import_owned")))

/// Specifies that a specific C++ method should be imported as a computed
/// property. If this macro is specified on a getter, a getter will be
/// synthesized. If this macro is specified on a setter, both a getter and
/// setter will be synthesized.
///
/// For example:
///  ```
///    int getX() SWIFT_COMPUTED_PROPERTY;
///  ```
/// Will be imported as `var x: CInt {...}`.
#define SWIFT_COMPUTED_PROPERTY                                                \
  __attribute__((swift_attr("import_computed_property")))

#define _CXX_INTEROP_STRINGIFY(_x) #_x

/// Specifies that a specific C++ `class` or `struct` conforms to a
/// a specific Swift protocol.
///
/// This example shows how to use this macro to conform a class template to a
/// Swift protocol:
///  ```
///    template<class T>
///    class SWIFT_CONFORMS_TO_PROTOCOL(SwiftModule.ProtocolName)
///    CustomClass {};
///  ```
// clang-format off
#define SWIFT_CONFORMS_TO_PROTOCOL(_moduleName_protocolName)                   \
  __attribute__((swift_attr(                                                   \
      _CXX_INTEROP_STRINGIFY(conforms_to:_moduleName_protocolName))))
// clang-format on

#else // #if __has_attribute(swift_attr)

#define SWIFT_SELF_CONTAINED
#define SWIFT_COMPUTED_PROPERTY
#define SWIFT_CONFORMS_TO_PROTOCOL(_moduleName_protocolName)

#endif // #if __has_attribute(swift_attr)

#endif // #if __has_include(<swift/bridging>)

// Redefine SWIFT_NAME.
#ifdef SWIFT_NAME
#undef SWIFT_NAME
#endif

#if __has_attribute(swift_name)
/// Specifies a name that will be used in Swift for this declaration instead of
/// its original name.
#define SWIFT_NAME(_name) __attribute__((swift_name(_name)))
#else
#define SWIFT_NAME(_name)
#endif

#if __has_attribute(availability)
#define SWIFT_UNAVAILABLE(msg)                                                 \
  __attribute__((availability(swift, unavailable, message = msg)))
#else
#define SWIFT_UNAVAILABLE(msg)
#endif

#endif // SWIFT_BASIC_SWIFT_BRIDGING_H

