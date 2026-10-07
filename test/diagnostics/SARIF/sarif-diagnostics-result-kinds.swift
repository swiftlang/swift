// Results that are not a located failure.

// RUN: %empty-directory(%t)

// No source location: no locations, and no artifacts either.
// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=sarif -serialize-diagnostics-path %t/nolocation.sarif %t/nonexistent.swift
// RUN: %FileCheck --input-file=%t/nolocation.sarif %s -check-prefix=NO-LOCATION

// NO-LOCATION: "runs" : [
// NO-LOCATION-NOT: "artifacts"
// NO-LOCATION: "results" : [
// NO-LOCATION: "level" : "error"
// NO-LOCATION-NOT: "locations"
// NO-LOCATION: "message" : {
// NO-LOCATION-NEXT: "text" : "error opening input file
// NO-LOCATION: "ruleId" : "error_open_input_file"

// A remark is informational: a kind, and no level.
// RUN: %target-swift-frontend -typecheck -Rmodule-loading -module-cache-path %t/mcp -serialize-diagnostics=sarif -serialize-diagnostics-path %t/remark.sarif %s
// RUN: %FileCheck --input-file=%t/remark.sarif %s -check-prefix=REMARK

// REMARK: "kind" : "informational"
// REMARK-NOT: "level"

// REQUIRES: swift_sarif

let x = 1
