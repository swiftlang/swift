// This file tests that the -Rcross-import option causes an appropriate remark to be emitted
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/lib-templates/* %t/
// RUN: %target-swift-frontend -typecheck %s -enable-cross-import-overlays -Rcross-import -I %t/include -I %t/lib/swift -F %t/Frameworks 2>&1 | %FileCheck %s -check-prefix IMPORT
// RUN: %target-swift-frontend -typecheck %s -disable-cross-import-overlay-search -enable-cross-import-overlays -Rcross-import -I %t/include -I %t/lib/swift -F %t/Frameworks 2>&1 \
// RUN:   | %FileCheck %s -check-prefix NO-IMPORT -allow-empty
// RUN: %target-swift-frontend -typecheck %s -disable-cross-import-overlay-search -enable-cross-import-overlays -Rcross-import -I %t/include -I %t/lib/swift -F %t/Frameworks 2>&1 \
// RUN:   -swift-module-cross-import DeclaringLibrary %t/lib/swift/DeclaringLibrary.swiftcrossimport/BystandingLibrary.swiftoverlay | %FileCheck %s -check-prefix IMPORT

import DeclaringLibrary
import BystandingLibrary

// IMPORT: import of 'DeclaringLibrary' and 'BystandingLibrary' triggered a cross-import of '_OverlayLibrary'
// NO-IMPORT-NOT: import of 'DeclaringLibrary' and 'BystandingLibrary' triggered a cross-import of '_OverlayLibrary'
