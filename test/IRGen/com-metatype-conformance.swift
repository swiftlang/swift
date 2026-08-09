// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -Osize -emit-ir %s | %FileCheck %s -check-prefix CHECK-OPT

// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/Interfaces.swiftmodule -module-name Interfaces %S/com-protocol-descriptor-iid.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Client -emit-ir %S/Inputs/com-metatype-conformance-client.swift | %FileCheck %s -check-prefix CHECK-XMODULE

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
protocol IWidget {
}

func __uuidof<Interface>(_: Interface.Type) -> IID
    where Interface.Type: COMInterface {
  Interface.IID
}

public func f() -> IID {
  __uuidof(IWidget.self)
}

// The interface's protocol descrip[tor is the compiler managed witness for its
// metatype conformance. There is no ordinary conformance descriptor, witness
// table, witness thunk, or runtime reigsteration record.

// CHECK-LABEL: define{{.*}} swiftcc {{.*}}@"$s{{.*}}8__uuidof
// CHECK-SAME:  ptr %Interface.Type.COMInterface
// CHECK:         load i64
// CHECK:         load i64
// CHECK-NOT:     IWidget_pm3COM12COMInterfaceACMc
// CHECK-NOT:     IWidget_pm3COM12COMInterfaceACWP
// CHECK-NOT:     IWidget_pm3COM12COMInterfaceA2cDP3IIDAC4GUIDVvgTW
// CHECK-NOT:     IWidget_pm3COM12COMInterfaceACHc
// CHECK-NOT:     section "__TEXT, __swift5_proto, regular"
// CHECK-NOT:     section "swift5_protocol_conformances"
// CHECK-NOT:     section ".sw5prtc$B"

// A concrete subsitution passes the IID in the interface descriptor.

// CHECK:      call swiftcc {{.*}} @"$s{{.*}}8__uuidof
// CHECK-SAME: ptr getelementptr inbounds (i8, ptr @"$s{{.*}}7IWidgetMp", i64 24))

// Optimizing a metatype-subject conformance must preserve the descriptor
// evidence rather than attempting to remap a synthesized conformance.
// CHECK-OPT: define{{.*}} swiftcc {{.*}}@"$s{{.*}}1f

// A concrete IID access in another module follows the same descriptor-backed
// requirement path without an interface-specific accessor or another GUID.

// CHECK-XMODULE-LABEL: define{{.*}} swiftcc {{.*}}@"$s6Client8__uuidof
// CHECK-XMODULE-NOT:     call swiftcc {{.*}}IWidgetPAA3E3IID3COM4GUIDVvg
// CHECK-XMODULE:         getelementptr inbounds (i8, ptr @"$s10Interfaces7IWidgetMp", i64 24)
// CHECK-XMODULE:         load i64
// CHECK-XMODULE:         load i64
