//===--- CompatibilityOverride.h - Back-deployment patches ------*- C++ -*-===//
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
// The compatibility override system supports the back-deployment of
// bug fixes and ABI additions to existing Swift systems.  This is
// primarily of interest on Apple platforms, since other platforms don't
// ship with a stable Swift runtime that cannot simply be replaced.
//
// The compatibility override system works as follows:
//
// 1. Certain runtime functions are "hooked" so that they can be replaced
//    by the program at launch time.  The function at the public symbol
//    (we will use `swift_task_cancel` as a consistent example) is a
//    thunk that either calls the standard implementation or its dynamic
//    replacement.  If a dynamic replacement is called, it is passed
//    the standard implementation as an extra argument.
//
// 2. The public thunks are defined in different .cpp files throughout
//    the runtime by the COMPATIBILITY_OVERRIDE macro below, triggered
//    by an #include at the bottom of the file.  The list of definitions
//    to expand in a particular file is defined by the appropriate
//    CompatibilityOverride*.def file for the current runtime component
//    (i.e. either the main runtime or the concurrency runtime).
//    The standard implementation must be defined in that file as a
//    function with the suffix `Impl`, e.g. `swift_task_cancelImpl`.
//    Usually the standard implementation should be static, and
//    everywhere else in the runtime should call the public symbol.
//
// 3. The public thunks determine their replacement by calling an
//    override accessor for the symbol the first time they are called.
//    These accessors are named e.g. `swift::getOverride_swift_task_cancel`
//    and are defined by CompatibilityOverride.cpp, which is built
//    separately for each runtime component using different build
//    settings.
//
// 4. The override accessors check for a Mach-O section with a specific
//    name, and if it exists, they interpret the section as a struct
//    with one field per replaceable function.  The order of fields is
//    determined by the appropriate CompatibilityOverride*.def file.
//    The section name, the struct layout, and the function signatures of
//    the replacement functions are the only parts of this which are ABI;
//    everything else is an internal detail of the runtime which can be
//    changed if useful.
//
// 5. The name of the Mach-O section is specific to both the current
//    runtime component and the current version of the Swift runtime.
//    Therefore, a compatibility override library always targets a
//    specific runtime version and implicitly does nothing on other
//    versions.
//
// 6. Compatibility override libraries define a Mach-O section with the
//    appropriate name and layout for their target component and version
//    and initialize the appropriate fields within it to the replacement
//    functions.  This occurs in the Overrides.cpp file in the library.
//    Compatibility override libraries are linked against later override
//    libraries for the same component, so if a patch needs to be applied
//    to multiple versions, the last version can define public symbols
//    that the other versions can use (assuming that any internal runtime
//    structures are roughly compatible).
//
// 7. Compatibility override libraries are rebuilt with every Swift
//    release in case that release requires new patches to the target
//    runtime.  They are therefore live code, unlike e.g. the
//    back-deployment concurrency runtime.
//
// 8. The back-deployment concurrency runtime looks for the same section
//    name as the OS-installed 5.6 runtime and therefore will be patched
//    by the 5.6 compatibility override library.
//
//===----------------------------------------------------------------------===//

#ifndef COMPATIBILITY_OVERRIDE_H
#define COMPATIBILITY_OVERRIDE_H

#include "../runtime/Private.h"
#include "swift/Runtime/CMakeConfig.h"
#include "swift/Runtime/Concurrency.h"
#include "swift/Runtime/Metadata.h"
#include <atomic>
#include <type_traits>

namespace swift {

// Macro utilities.
#define COMPATIBILITY_UNPAREN(...) __VA_ARGS__
#define COMPATIBILITY_CONCAT2(x, y) x##y
#define COMPATIBILITY_CONCAT(x, y) COMPATIBILITY_CONCAT2(x, y)

// This ridiculous construct will remove the parentheses from the argument and
// add a trailing comma, or will produce nothing when passed no argument. For
// example:
// COMPATIBILITY_UNPAREN_WITH_COMMA((1, 2, 3)) -> 1, 2, 3,
// COMPATIBILITY_UNPAREN_WITH_COMMA((4)) -> 4,
// COMPATIBILITY_UNPAREN_WITH_COMMA() ->
#define COMPATIBILITY_UNPAREN_WITH_COMMA(x)                                    \
  COMPATIBILITY_CONCAT(COMPATIBILITY_UNPAREN_ADD_TRAILING_COMMA_,              \
                       COMPATIBILITY_UNPAREN_WITH_COMMA2 x)
#define COMPATIBILITY_UNPAREN_WITH_COMMA2(...) PARAMS(__VA_ARGS__)
#define COMPATIBILITY_UNPAREN_ADD_TRAILING_COMMA_PARAMS(...) __VA_ARGS__,
#define COMPATIBILITY_UNPAREN_ADD_TRAILING_COMMA_COMPATIBILITY_UNPAREN_WITH_COMMA2

// This ridiculous construct will preserve the parentheses around the argument,
// or will produce an empty pair of parentheses when passed no argument. For
// example:
// COMPATIBILITY_PAREN((1, 2, 3)) -> (1, 2, 3)
// COMPATIBILITY_PAREN((4)) -> (4)
// COMPATIBILITY_PAREN() -> ()
#define COMPATIBILITY_PAREN(x)                                                 \
  COMPATIBILITY_CONCAT(COMPATIBILITY_PAREN_, COMPATIBILITY_PAREN2 x)
#define COMPATIBILITY_PAREN2(...) PARAMS(__VA_ARGS__)
#define COMPATIBILITY_PAREN_PARAMS(...) (__VA_ARGS__)
#define COMPATIBILITY_PAREN_COMPATIBILITY_PAREN2 ()

// Include path computation. Code that includes this file can write `#include
// "..CompatibilityOverride/CompatibilityOverrideIncludePath.h"` to include the
// appropriate .def file for the current library.
#define COMPATIBILITY_OVERRIDE_INCLUDE_PATH_swiftRuntimeCore                   \
  "../CompatibilityOverride/CompatibilityOverrideRuntime.def"
#define COMPATIBILITY_OVERRIDE_INCLUDE_PATH_swift_Concurrency                  \
  "../CompatibilityOverride/CompatibilityOverrideConcurrency.def"

#define COMPATIBILITY_OVERRIDE_INCLUDE_PATH                                    \
  COMPATIBILITY_CONCAT(COMPATIBILITY_OVERRIDE_INCLUDE_PATH_,                   \
                       SWIFT_TARGET_LIBRARY_NAME)

// Compatibility overrides are only supported on Darwin.
#ifdef SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT
#if !(defined(__APPLE__) && defined(__MACH__))
#undef SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT
#endif
#endif

#ifndef SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT

// Call directly through to the original implementation when we don't support
// overrides.
#define COMPATIBILITY_OVERRIDE(name, ret, attrs, ccAttrs, namespace,           \
                               typedArgs, namedArgs)                           \
  attrs ccAttrs ret namespace swift_##name COMPATIBILITY_PAREN(typedArgs) {    \
    return swift_##name##Impl COMPATIBILITY_PAREN(namedArgs);                  \
  }

#else // #ifndef SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT

// Override section name computation. `COMPATIBILITY_OVERRIDE_SECTION_NAME` will
// resolve to string literal containing the appropriate section name for the
// current library.
// Turns into '__swift<major><minor>_hooks'
#define COMPATIBILITY_OVERRIDE_SECTION_NAME_swiftRuntimeCore "__swift" \
  SWIFT_VERSION_MAJOR \
  SWIFT_VERSION_MINOR \
  "_hooks"

// Turns into '__s<major><minor>async_hook'
#define COMPATIBILITY_OVERRIDE_SECTION_NAME_swift_Concurrency "__s" \
  SWIFT_VERSION_MAJOR \
  SWIFT_VERSION_MINOR \
  "async_hook"

#define COMPATIBILITY_OVERRIDE_SECTION_NAME                                    \
  COMPATIBILITY_CONCAT(COMPATIBILITY_OVERRIDE_SECTION_NAME_,                   \
                       SWIFT_TARGET_LIBRARY_NAME)

// Create typedefs for function pointers to call the original implementation.
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs)   \
  ccAttrs typedef ret(*Original_##name) COMPATIBILITY_PAREN(typedArgs);
#include "CompatibilityOverrideIncludePath.h"

// Create typedefs for override function pointers.
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs)   \
  ccAttrs typedef ret (*Override_##name)(COMPATIBILITY_UNPAREN_WITH_COMMA(     \
      typedArgs) Original_##name originalImpl);
#include "CompatibilityOverrideIncludePath.h"

// Create declarations for getOverride functions.
#define OVERRIDE(name, ret, attrs, ccAttrs, namespace, typedArgs, namedArgs)   \
  Override_##name getOverride_##name();
#include "CompatibilityOverrideIncludePath.h"

/// Used to define an override point. The override point #defines the appropriate
/// OVERRIDE macro from CompatibilityOverride.def to this macro, then includes
/// the file to generate the override points. The original implementation of the
/// functionality must be available as swift_funcNameHereImpl.
#define COMPATIBILITY_OVERRIDE(name, ret, attrs, ccAttrs, namespace,           \
                               typedArgs, namedArgs)                           \
  /* We are creating this separate function for the override case, */          \
  /* to prevent a stack frame from being created for the default case. */      \
  SWIFT_NOINLINE                                                               \
  ccAttrs static ret swift_##name##Slow(                                       \
      COMPATIBILITY_UNPAREN_WITH_COMMA(typedArgs)                              \
          std::atomic<uintptr_t> &Override,                                    \
      uintptr_t fn, Original_##name defaultImpl) {                             \
    constexpr uintptr_t DEFAULT_IMPL_SENTINEL = 0x1;                           \
    if (SWIFT_UNLIKELY(fn == 0x0)) {                                           \
      fn = (uintptr_t)getOverride_##name();                                    \
      if (fn == 0x0) {                                                         \
        Override.store(DEFAULT_IMPL_SENTINEL,                                  \
                       std::memory_order::memory_order_relaxed);               \
        return defaultImpl COMPATIBILITY_PAREN(namedArgs);                     \
      }                                                                        \
      Override.store(fn, std::memory_order::memory_order_relaxed);             \
    }                                                                          \
    return ((Override_##name)fn)(COMPATIBILITY_UNPAREN_WITH_COMMA(namedArgs)   \
                                     defaultImpl);                             \
  }                                                                            \
  attrs ccAttrs ret namespace swift_##name COMPATIBILITY_PAREN(typedArgs) {    \
    constexpr uintptr_t DEFAULT_IMPL_SENTINEL = 0x1;                           \
    static std::atomic<uintptr_t> Override;                                    \
    uintptr_t fn = Override.load(std::memory_order::memory_order_relaxed);     \
    if (SWIFT_LIKELY(fn == DEFAULT_IMPL_SENTINEL)) {                           \
      return swift_##name##Impl COMPATIBILITY_PAREN(namedArgs);                \
    } else if (SWIFT_UNLIKELY(fn == 0x0)) {                                    \
      return swift_##name##Slow(COMPATIBILITY_UNPAREN_WITH_COMMA(namedArgs)    \
                                    Override,                                  \
                                fn, &swift_##name##Impl);                      \
    }                                                                          \
    return ((Override_##name)fn)(COMPATIBILITY_UNPAREN_WITH_COMMA(namedArgs) & \
                                 swift_##name##Impl);                          \
  }

#endif // #else SWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT

} /* end namespace swift */

#endif /* COMPATIBILITY_OVERRIDE_H */
