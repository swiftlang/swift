// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DeprecatedReplaced -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DeprecatedReplaced -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DeprecatedReplaced.symbols.json --check-prefix=DEPRECATED
// RUN: %FileCheck %s --input-file %t/DeprecatedReplaced.symbols.json --check-prefix=NOTDEPRECATED

// REQUIRES: OS=macosx

@available(macOS, deprecated: 7.0)
@available(macOS, deprecated: 11.0)
public func foo() {}

// Last `deprecated` wins.
// DEPRECATED-LABEL: "precise": "s:18DeprecatedReplaced3fooyyF",
// DEPRECATED: "availability": [
// DEPRECATED-NEXT:   {
// DEPRECATED-NEXT:     "domain": "macOS",
// DEPRECATED-NEXT:     "deprecated": {
// DEPRECATED-NEXT:       "major": 11,
// DEPRECATED-NEXT:       "minor": 0
// DEPRECATED-NEXT:     }
// DEPRECATED-NEXT:   }
// DEPRECATED-NEXT: ]

@available(macOS, deprecated: 10.0)
@available(macOS, introduced: 10.0)
public func noLongerDeprecated() {}

// NOTDEPRECATED: "precise": "s:18DeprecatedReplaced08noLongerA0yyF",
// NOTDEPRECATED: "availability": [
// NOTDEPRECATED-NEXT:   {
// NOTDEPRECATED-NEXT:     "domain": "macOS",
// NOTDEPRECATED-NEXT:     "introduced": {
// NOTDEPRECATED-NEXT:       "major": 10,
// NOTDEPRECATED-NEXT:       "minor": 0
// NOTDEPRECATED-NEXT:     }
// NOTDEPRECATED-NEXT:   }
// NOTDEPRECATED-NEXT: ]
