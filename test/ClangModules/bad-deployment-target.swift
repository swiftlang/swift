// RUN: not %swift -parse -target x86_64-apple-macosx10.8 %clang-importer-sdk -I %S/Inputs/custom-modules %s
//
// This test ensures that a -target that is too old for the standard library
// will not crash in the ClangImporter.

// REQUIRES: OS=macosx

import Foundation
