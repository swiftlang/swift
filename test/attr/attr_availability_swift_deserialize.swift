// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OldAndNew.swiftmodule -module-name OldAndNew %S/Inputs/OldAndNew.swift 
// RUN: not %target-swift-frontend -typecheck -I %t -swift-version 4 %s 2>&1 | %FileCheck -check-prefix THREE %s
// RUN: not %target-swift-frontend -typecheck -I %t -swift-version 5 %s 2>&1 | %FileCheck -check-prefix FOUR %s
// RUN: %target-swift-ide-test -print-module -module-to-print OldAndNew -source-filename x -I %t | %FileCheck %S/Inputs/OldAndNew.swift

import OldAndNew

// THREE: 'fiveOnly()' is unavailable
// THREE: 'fiveOnly()' was introduced in Swift 5.0
let _ = fiveOnly()

// FOUR: 'fourOnly()' is unavailable
// FOUR: 'fourOnly()' was obsoleted in Swift 5.0
let _ = fourOnly()
