// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -target x86_64-apple-macosx10.8 %s
//
// This test ensures that a -target that is too old for the standard library
// will not crash in the ClangImporter.

// REQUIRES: OS=macosx

import Foundation
