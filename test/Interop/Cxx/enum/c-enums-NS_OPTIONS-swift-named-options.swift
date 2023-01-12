// RUN: %empty-directory(%t/cache)
// RUN: %target-swift-frontend %s -I %S/Inputs -typecheck -module-cache-path %t/cache -enable-experimental-cxx-interop 2>&1 | %FileCheck --allow-empty %s

// REQUIRES: objc_interop

import CenumsNSOptions

// CHECK-NOT: warning: imported declaration 'NS_SWIFT_NAMED_OptionOne' could not be mapped to 'SwiftOptionOne'
// CHECK-NOT: warning: imported declaration 'NS_SWIFT_NAMED_OptionTwo' could not be mapped to 'SwiftOptionTwo'
// CHECK-NOT: warning: imported declaration 'NS_SWIFT_NAMED_OptionThree' could not be mapped to 'SwiftOptionThree'
// CHECK-NOT: warning: imported declaration 'NS_SWIFT_NAMED_OptionFour' could not be mapped to 'SwiftOptionFour'
