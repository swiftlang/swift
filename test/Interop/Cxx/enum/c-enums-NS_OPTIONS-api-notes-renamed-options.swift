// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend %s -I %S/Inputs -typecheck -module-cache-path %t/cache -enable-experimental-cxx-interop 2>&1 | %FileCheck --allow-empty %s

// REQUIRES: objc_interop

import CenumsNSOptions

// CHECK-NOT: warning: imported declaration 'API_NOTES_NAMED_OptionOne' could not be mapped to 'SwiftOptionOneApiNotes'
// CHECK-NOT: warning: imported declaration 'API_NOTES_NAMED_OptionTwo' could not be mapped to 'SwiftOptionTwoApiNotes'
// CHECK-NOT: warning: imported declaration 'API_NOTES_NAMED_OptionThree' could not be mapped to 'SwiftOptionThreeApiNotes'
// CHECK-NOT: warning: imported declaration 'API_NOTES_NAMED_OptionFour' could not be mapped to 'SwiftOptionFourApiNotes'
