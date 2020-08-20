// Check that this doesn't crash due to a self-cycle. rdar://67435472
// RUN: %target-swift-frontend %s -emit-module -o /dev/null -emit-loaded-module-trace-path /dev/null -I %S/Inputs/imported_modules/SelfImport

import Outer
