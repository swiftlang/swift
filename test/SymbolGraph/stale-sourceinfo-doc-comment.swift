// Test that swift-symbolgraph-extract does not crash when a module's
// .swiftsourceinfo has doc comment byte offsets that are stale relative
// to the current source file on disk.
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/P_v1.swift -module-name P -emit-module -emit-module-path %t/P.swiftmodule -emit-module-doc-path %t/P.swiftdoc -emit-module-source-info-path %t/P.swiftsourceinfo
// RUN: cp %t/P_v2.swift %t/P_v1.swift
// RUN: %target-swift-frontend %t/Consumer.swift -module-name Consumer -emit-module -emit-module-path %t/Consumer.swiftmodule -emit-module-source-info-path %t/Consumer.swiftsourceinfo -I %t
// RUN: %target-swift-symbolgraph-extract -module-name Consumer -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Consumer.symbols.json

// P_v1.swift is compiled first to produce P.swiftsourceinfo, which records the
// byte offset of the '///' doc comment on the extension.  P_v1.swift is then
// replaced with P_v2.swift, which has different content at that same offset
// (@available rather than ///), making the stored offset stale.
// swift-symbolgraph-extract evaluates RawCommentRequest for P's extensions
// during synthesized-member processing.  Before the fix, it passed the
// non-comment text extracted from the stale offset to getCommentKind(), which
// asserted Comment[0] == '/'.  After the fix it falls through to .swiftdoc.

// CHECK: "identifier"
// CHECK: "swift.struct"

//--- P_v1.swift
public protocol P {
  func foo()
}

/// Documentation for the default extension.
extension P {
  public func bar() {}
}

//--- P_v2.swift
public protocol P {
  func foo()
}

@available(macOS 12.0, *)
extension P {
  public func bar() {}
}

//--- Consumer.swift
import P

public struct S: P {
  public func foo() {}
}
