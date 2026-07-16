// RUN: %target-swift-ide-test -print-module -module-to-print=CFOptionsTypedefAlias -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default %s
// REQUIRES: objc_interop

import CFOptionsTypedefAlias

// CHECK: struct NSPropertyListMutabilityOptions : OptionSet

// CHECK-NOT: typealias NSPropertyListReadOptions = UInt
// CHECK: typealias NSPropertyListReadOptions = NSPropertyListMutabilityOptions
// CHECK: typealias NSPropertyListReadOptions2 = NSPropertyListMutabilityOptions
// CHECK: typealias NSPropertyListReadOptions3 = NSPropertyListMutabilityOptions

// CHECK: func read(withOptions options: NSPropertyListMutabilityOptions)

func test(_ reader: PlistReader) {
  let opts: NSPropertyListReadOptions = .mutableContainersAndLeaves
  let opts3: NSPropertyListReadOptions3 = [.mutableContainers]
  reader.read(withOptions: opts)
  reader.read(withOptions: opts3)
}
