// RUN: %target-swift-frontend -swift-version 5 %s -module-name lazy_typecheck -enable-library-evolution -parse-as-library -emit-module -emit-module-path /dev/null -experimental-lazy-typecheck -experimental-skip-non-inlinable-function-bodies -experimental-skip-non-exportable-decls

public protocol P {
  func req()
}

@available(macOS, renamed: "P")
@available(iOS, renamed: "P")
@available(tvOS, renamed: "P")
@available(watchOS, renamed: "P")
@available(visionOS, renamed: "P")
public typealias Q = P
