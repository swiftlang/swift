// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/Main.swift %t/Other.swift -enable-upcoming-feature MemberImportVisibility

//--- Main.swift

import Foundation

func test() -> CGSize {
  return CGSize(width: 60, height: 45)
}

//--- Other.swift

import CoreGraphics
