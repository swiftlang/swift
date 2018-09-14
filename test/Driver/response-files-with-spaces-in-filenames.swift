// RUN: %empty-directory(%t)
// RUN: touch "%t/f i l e.swift"
//
// RUN: %target-build-swift -driver-force-response-files -parse  "%t/f i l e.swift"
