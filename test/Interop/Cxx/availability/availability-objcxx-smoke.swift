// RUN: %target-swift-ide-test -print-module -module-to-print=SomeModule -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck %s

import SomeModule

// CHECK: @available(swift, obsoleted: 3, renamed: "NSValueTransformerName.unarchiveFromDataTransformerName")