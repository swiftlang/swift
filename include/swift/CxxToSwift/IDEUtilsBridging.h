//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// IMPORTANT: Every target that includes this header file needs to declare an
// explict dependency on `swiftIDEUtilsBridging-cxx-briding-header` to make sure
// that the C++ bridging header gets created before the C++ sources are
// compiled.

#include "swift/IDE/IDEBridging.h"
// Hack: The generated C++ header defines an OptionSet type (for the OptionSet
// in the Swift stdlib) in the `swift` namespace. This collides with the C++
// OptionSet type defined for the compiler.
// Since the Swift stdlib OptionSet can't be instantiated from C++ anyway,
// rename it to `SwiftOptionSet` using a `#define` to make it not collide.
#define OptionSet SwiftOptionSet
#include "swift/ASTGen/swiftIDEUtilsBridging-Swift.h"
#undef OptionSet
