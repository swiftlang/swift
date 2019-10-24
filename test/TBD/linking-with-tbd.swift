// REQUIRES: VENDOR=apple

// 1. Create a skeleton of a framework
// RUN: %empty-directory(%t/APIGrabBag.framework/Modules/APIGrabBag.swiftmodule)
// RUN: %empty-directory(%t/APIGrabBag.framework/Headers)

// 1. Compile api_grab_bag.swift to a .tbd and put it in %t

// RUN: %target-swift-frontend -emit-module -o %t/APIGrabBag.framework/Modules/APIGrabBag.swiftmodule/%target-cpu.swiftmodule -emit-tbd-path %t/APIGrabBag.framework/APIGrabBag.tbd %S/Inputs/api_grab_bag.swift -module-name APIGrabBag -tbd-install_name %t/APIGrabBag.framework/APIGrabBag

// 2. Compile the current file against the TBD

// RUN: %target-build-swift -emit-executable %s -o %t/executable -F %t -framework APIGrabBag

// 3. Install the actual dylib into the framework

// RUN: %target-build-swift -emit-library %S/Inputs/api_grab_bag.swift -module-name APIGrabBag -o %t/APIGrabBag.framework/APIGrabBag

// 4. Codesign the executable and run it

// RUN: %target-codesign %t/executable %t/APIGrabBag.framework/APIGrabBag
// RUN: %target-run %t/executable

import APIGrabBag

func useAPIs() {
  let c = PublicClass()
  c.method()

  let sub = PublicSubclass()
  sub.method()

  let s = PublicStruct()
  let t = s.publicStruct

  var e = PublicEnum.caseOne
  e = .caseTwo

  _ = e.publicStruct
}
