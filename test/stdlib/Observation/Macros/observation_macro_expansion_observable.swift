// REQUIRES: swift_swift_parser, asserts
//
// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: observation
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)

// RUN: %target-swift-frontend -swift-version 5 -typecheck -plugin-path %swift-plugin-dir -I %t -dump-macro-expansions %s 2>&1 | %FileCheck %s --color

import Observation

// Test cases for comment handling with Observable macro
_ = 0 // absorbs trivia so file check expectations don't leak into macro expansions

@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
@Observable
final class CommentAfterGlobalActorAnnotation {
  @MainActor // Innocent comment
  internal var it = 0
}

// CHECK-LABEL: @__swiftmacro{{.*}}CommentAfterGlobalActorAnnotation{{.*}}ObservationTracked{{.*}}.swift
// CHECK:       @MainActor // Innocent comment
// CHECK-NEXT:  @ObservationIgnored
// CHECK-NEXT:  private var _it = 0
_ = 0

@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
@Observable
final class CommentAfterAvailabilityAnnotation {
  @available(*, deprecated) // Innocent comment
  internal var it = 0
}

// CHECK-LABEL: @__swiftmacro{{.*}}CommentAfterAvailabilityAnnotation{{.*}}ObservationTracked{{.*}}.swift
// CHECK-NEXT:  {{-+}}
// CHECK-NEXT:  @available(*, deprecated) // Innocent comment
// CHECK-NEXT:  @ObservationIgnored
// CHECK-NEXT:  private  var _it  = 0
_ = 0

@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
@Observable
final class CommentOnSameLineAsOtherAnnotation {
  @MainActor /* Innocent comment */ public var it = 0
}

// CHECK-LABEL: @__swiftmacro{{.*}}CommentOnSameLineAsOtherAnnotation{{.*}}ObservationTracked{{.*}}.swift
// CHECK:       @MainActor /* Innocent comment */
// CHECK-NEXT:  @ObservationIgnored
// CHECK-NEXT:  private var _it = 0
_ = 0

@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
@Observable
final class CommentOnSameLineNoAnnotation {
  /* Innocent comment */ public /*1*/ final /*2*/ var /*3*/ it /*4*/ = 0
}

// Note: seems there's some weirdness with the existing macro eating/duplicating trivia in some
// cases but we'll just test for the current behavior since it doesn't seem to be covered elsewhere:

// CHECK-LABEL: @__swiftmacro{{.*}}CommentOnSameLineNoAnnotation{{.*}}ObservationTracked{{.*}}.swift
// CHECK:       /* Innocent comment */
// CHECK-NEXT:  @ObservationIgnored
// CHECK-NEXT:  private final /*2*/ var _it /*4*/ /*4*/ = 0
_ = 0

// `#sourceLocation` directives synthesized to forward the spelled location of
// `didSet`/`willSet` bodies must use an absolute filesystem path (`.filePath`)
// rather than `#fileID`-style `Module/file.swift` — the latter cannot be
// resolved by SourceKit/Xcode when navigating remapped diagnostics.
@available(macOS 14.0, iOS 17.0, watchOS 10.0, tvOS 17.0, *)
@Observable
final class DidSetSourceLocationUsesAbsolutePath {
  var value = 0 {
    didSet {
      _ = value
    }
  }
}

// CHECK-LABEL: @__swiftmacro{{.*}}DidSetSourceLocationUsesAbsolutePath{{.*}}ObservationTracked{{.*}}.swift
// CHECK:       #sourceLocation(file: "{{.*}}Macros{{.*}}observation_macro_expansion_observable.swift",
_ = 0
