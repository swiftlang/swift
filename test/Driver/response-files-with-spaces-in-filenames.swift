// RUN: %empty-directory(%t)
// RUN: touch "%t/f i l e.swift"
//
// RUN: %swiftc_driver -driver-force-response-files -typecheck  "%t/f i l e.swift"
