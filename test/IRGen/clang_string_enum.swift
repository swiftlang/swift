// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir %s -import-objc-header %S/Inputs/clang_string_enum.h > /dev/null

// REQUIRES: objc_interop

import Foundation

class PandaCub : NSObject {}

extension PandaCub {
  @objc func cuddle(_: PandaStyle) { }
}
