// RUN: %target-swiftc_driver -Xfrontend %clang-importer-sdk -emit-module -o %t.1.swiftmodule -emit-objc-header -emit-objc-header-path %t.1.h -module-name ThisModule %s %S/Inputs/main.swift %S/Inputs/lib.swift
// RUN: %target-swiftc_driver -Xfrontend %clang-importer-sdk -emit-module -o %t.2.swiftmodule -emit-objc-header -emit-objc-header-path %t.2.h -module-name ThisModule %s %S/Inputs/main.swift %S/Inputs/lib.swift -force-single-frontend-invocation
// RUN: diff %t.1.h %t.2.h

// REQUIRES: objc_interop

import Foundation

public class A: NSObject {
  func foo() {}
  @objc func bar(x: Int, baz y: Int) -> Int { return 1 }
}
public class B: A {
  func doSomething() {}
}
