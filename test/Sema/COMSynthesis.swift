// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

@com(interface: "00000000-0000-0000-0000-000000000000")
protocol IProtocol: IUnknown {
}

@com
class CClass1 {
}

// expected-note@+1 {{where 'COMType' = 'CClass4'}}
func com<COMType: IUnknown>(_ interface: COMType) {
}

func implicit(_ object: CClass1) {
  com(object)
}

@com
class CClass2: IUnknown {
}

func explicit(_ object: CClass2) {
  com(object)
}

@com(implementation: "00000000-0000-0000-0000-000000000000")
class CClass3: IProtocol {
}

func indirect(_ object: CClass3) {
  com(object)
}

class CClass4 {
}

func invalid(_ object: CClass4) {
  com(object)
  // expected-error@-1 {{global function 'com' requires that 'CClass4' conform to 'IUnknown'}}
}
