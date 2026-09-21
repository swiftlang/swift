// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -dump-ast %s 2>&1 | %FileCheck %s

@com(interface: "00000000-0000-0000-0000-000000000000")
protocol IInterface: IUnknown {
}

@com
class CClass1 {
}

// CHECK-LABEL: "CClass1" interface_type="CClass1.Type" access=internal non_resilient
// CHECK:     (com_attr {{.*}} threading=apartment)

@com(implementation: "00000000-0000-0000-0000-000000000000")
class CClass2: IUnknown {
}

// CHECK-LABEL: "CClass2" interface_type="CClass2.Type" access=internal non_resilient inherits="IUnknown"
// CHECK:     (com_attr {{.*}} CLSID="00000000-0000-0000-0000-000000000000" threading=apartment)

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .free)
class CClass3: IUnknown {
}

// CHECK-LABEL: "CClass3" interface_type="CClass3.Type" access=internal non_resilient inherits="IUnknown"
// CHECK:     (com_attr {{.*}} CLSID="00000000-0000-0000-0000-000000000000" threading=free)

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .apartment)
class CClass4: IUnknown {
}

// CHECK-LABEL: "CClass4" interface_type="CClass4.Type" access=internal non_resilient inherits="IUnknown"
// CHECK:     (com_attr {{.*}} CLSID="00000000-0000-0000-0000-000000000000" threading=apartment)

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .single)
class CClass5: IUnknown {
}

// CHECK-LABEL: "CClass5" interface_type="CClass5.Type" access=internal non_resilient inherits="IUnknown"
// CHECK:     (com_attr {{.*}} CLSID="00000000-0000-0000-0000-000000000000" threading=single)

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .both)
class CClass6: IUnknown {
}

// CHECK-LABEL: "CClass6" interface_type="CClass6.Type" access=internal non_resilient inherits="IUnknown"
// CHECK:     (com_attr {{.*}} CLSID="00000000-0000-0000-0000-000000000000" threading=both)

@com(implementation: "00000000-0000-0000-0000-000000000000", threading: .neutral)
class CClass7: IUnknown {
}

// CHECK-LABEL: "CClass7" interface_type="CClass7.Type" access=internal non_resilient inherits="IUnknown"
// CHECK:     (com_attr {{.*}} CLSID="00000000-0000-0000-0000-000000000000" threading=neutral)
