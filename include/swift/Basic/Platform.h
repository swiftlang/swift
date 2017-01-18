//===--- Platform.h - Helpers related to target platforms -------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_PLATFORM_H
#define SWIFT_BASIC_PLATFORM_H

#include "swift/Basic/LLVM.h"
#include "swift/Config.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class Triple;
}

namespace swift {

  enum class DarwinPlatformKind : unsigned {
    MacOS,
    IPhoneOS,
    IPhoneOSSimulator,
    TvOS,
    TvOSSimulator,
    WatchOS,
    WatchOSSimulator
  };

  /// Returns true if the given triple represents iOS running in a simulator.
  bool tripleIsiOSSimulator(const llvm::Triple &triple);

  /// Returns true if the given triple represents AppleTV running in a simulator.
  bool tripleIsAppleTVSimulator(const llvm::Triple &triple);

  /// Returns true if the given triple represents watchOS running in a simulator.
  bool tripleIsWatchSimulator(const llvm::Triple &triple);

  /// Return true if the given triple represents any simulator.
  bool tripleIsAnySimulator(const llvm::Triple &triple);

  /// Returns the platform name for a given target triple.
  ///
  /// For example, the iOS simulator has the name "iphonesimulator", while real
  /// iOS uses "iphoneos". OS X is "macosx". (These names are intended to be
  /// compatible with Xcode's SDKs.)
  ///
  /// If the triple does not correspond to a known platform, the empty string is
  /// returned.
  StringRef getPlatformNameForTriple(const llvm::Triple &triple);

  /// Returns the platform Kind for Darwin triples.
  DarwinPlatformKind getDarwinPlatformKind(const llvm::Triple &triple);

  /// Returns the architecture component of the path for a given target triple.
  ///
  /// Typically this is used for mapping the architecture component of the
  /// path.
  ///
  /// For example, on Linux "armv6l" and "armv7l" are mapped to "armv6" and
  /// "armv7", respectively, within LLVM. Therefore the component path for the
  /// architecture specific objects will be found in their "mapped" paths.
  ///
  /// This is a stop-gap until full Triple support (ala Clang) exists within swiftc.
  StringRef getMajorArchitectureName(const llvm::Triple &triple);
} // end namespace swift

#endif // SWIFT_BASIC_PLATFORM_H

