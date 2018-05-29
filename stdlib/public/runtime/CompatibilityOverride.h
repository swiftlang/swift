//===--- CompatibiltyOverride.h - Back-deploying compatibility fixes --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Support back-deploying compatibility fixes for newer apps running on older runtimes.
//
//===----------------------------------------------------------------------===//

#ifndef COMPATIBILITY_OVERRIDE_H
#define COMPATIBILITY_OVERRIDE_H

#include "Private.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Once.h"
#include <type_traits>

namespace swift {

#define COMPATIBILITY_UNPAREN(...) __VA_ARGS__

#define OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  typedef ret (*Original_ ## name) typedArgs;
#include "CompatibilityOverride.def"

#define OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  typedef ret (*Override_ ## name)(COMPATIBILITY_UNPAREN typedArgs, \
                                   Original_ ## name originalImpl);
#include "CompatibilityOverride.def"

#define OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  Override_ ## name getOverride_ ## name();
#include "CompatibilityOverride.def"


/// Used to define an override point. The override point #defines the appropriate
/// OVERRIDE macro from CompatibilityOverride.def to this macro, then includes
/// the file to generate the override points. The original implementation of the
/// functionality must be available as swift_funcNameHereImpl.
#define COMPATIBILITY_OVERRIDE(name, ret, attrs, namespace, typedArgs, namedArgs) \
  attrs ret namespace swift_ ## name typedArgs {                                  \
    static Override_ ## name Override;                                            \
    static swift_once_t Predicate;                                                \
    swift_once(&Predicate, [](void *) {                                           \
      Override = getOverride_ ## name();                                          \
    }, nullptr);                                                                  \
    if (Override != nullptr)                                                      \
      return Override(COMPATIBILITY_UNPAREN namedArgs, swift_ ## name ## Impl);   \
    return swift_ ## name ## Impl namedArgs; \
  }

} /* end namespace swift */

#endif /* COMPATIBILITY_OVERRIDE_H */
