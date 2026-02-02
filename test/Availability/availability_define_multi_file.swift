// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm \
// RUN:   %t/FileA.swift %t/FileB.swift \
// RUN:   -define-availability "_justAName" \
// RUN:   2>&1 | %FileCheck %s

// CHECK: -define-availability argument:1:11: error: expected ':' after '_justAName' in availability macro definition
// CHECK-NEXT: _justAName

/// It's parsed once so the diagnostic is produced once as well.
// CHECK-NOT: _justAName

//--- FileA.swift

@available(_triggerParsingMacros)
public func brokenPlatforms() {}

//--- FileB.swift

@available(_triggerParsingMacros)
public func brokenPlatforms() {}
