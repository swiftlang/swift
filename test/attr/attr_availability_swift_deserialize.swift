// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OldAndNew.swiftmodule -module-name OldAndNew %S/Inputs/OldAndNew.swift 
// RUN: not %target-swift-frontend -typecheck -I %t -swift-version 3 %s 2>&1 | %FileCheck -check-prefix THREE %s
// RUN: not %target-swift-frontend -typecheck -I %t -swift-version 4 %s 2>&1 | %FileCheck -check-prefix FOUR %s

import OldAndNew

// THREE: 'fourOnly()' is unavailable
// THREE: 'fourOnly()' was introduced in Swift 4.0
let _ = fourOnly()

// FOUR: 'threeOnly()' is unavailable
// FOUR: 'threeOnly()' was obsoleted in Swift 4.0
let _ = threeOnly()
