// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreFoundation
// Can't import StdlibUnittest; that brings in Foundation.

extension CFString {
  static func from(_ contents: String) -> CFString {
    return CFStringCreateWithCString(nil, contents, /*ascii*/0)
  }

  static func mutablyFrom(_ contents: String) -> CFMutableString {
    return CFStringCreateMutableCopy(nil, /*maxLength*/0, CFString.from(contents))
  }
}

do {
  print("Testing Array<CFString>") // CHECK-LABEL: Testing Array<CFString>
  var s = [CFString]()
  // Use long strings to avoid the tagged pointer optimization.
  s.append(.from("abcxxxxxxxxxxx"))
  s.append(.from("defxxxxxxxxxxx"))
  print(s.contains(.from("abcxxxxxxxxxxx"))) // CHECK-NEXT: true
  print(s.contains(.from("efxxxxxxxxxxx"))) // CHECK-NEXT: false
}

do {
  print("Testing Array<CFMutableString>") // CHECK-LABEL: Testing Array<CFMutableString>
  var s = [CFMutableString]()
  // Use long strings to avoid the tagged pointer optimization.
  s.append(.mutablyFrom("abcxxxxxxxxxxx"))
  s.append(.mutablyFrom("defxxxxxxxxxxx"))
  print(s.contains(.mutablyFrom("abcxxxxxxxxxxx"))) // CHECK-NEXT: true
  print(s.contains(.mutablyFrom("efxxxxxxxxxxx"))) // CHECK-NEXT: false

  let upcast = s as [CFString]
  print(upcast.contains(CFString.mutablyFrom("abcxxxxxxxxxxx"))) // CHECK-NEXT: true
  print(upcast.contains(CFString.from("abcxxxxxxxxxxx"))) // CHECK-NEXT: true
}

do {
  print("Testing Set<CFString>") // CHECK-LABEL: Testing Set<CFString>
  var s = Set<CFString>()
  // Use long strings to avoid the tagged pointer optimization.
  s.insert(.from("abcxxxxxxxxxxx"))
  s.insert(.from("defxxxxxxxxxxx"))
  print(s.contains(.from("abcxxxxxxxxxxx"))) // CHECK-NEXT: true
  print(s.contains(.from("efxxxxxxxxxxx"))) // CHECK-NEXT: false
}

do {
  print("Testing Set<CFMutableString>") // CHECK-LABEL: Testing Set<CFMutableString>
  // This is a horrible thing to do but we're just checking that the conformance works.
  var s = Set<CFMutableString>()
  s.insert(.mutablyFrom("abcxxxxxxxxxxx"))
  s.insert(.mutablyFrom("defxxxxxxxxxxx"))
  print(s.contains(.mutablyFrom("abcxxxxxxxxxxx"))) // CHECK-NEXT: true
  print(s.contains(.mutablyFrom("efxxxxxxxxxxx"))) // CHECK-NEXT: false
}
