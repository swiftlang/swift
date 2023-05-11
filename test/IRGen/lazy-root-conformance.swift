// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DA -parse-as-library -parse-stdlib -module-name A %s -o %t/A.swiftmodule
// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-ir -DB -I %t -parse-as-library -parse-stdlib -module-name B -o - %s | %FileCheck %s -check-prefix CHECK-%target-import-type
// RUN: %target-swift-frontend -emit-ir -DB -I %t -parse-as-library -parse-stdlib -module-name B -o - %s

#if A
public protocol P {
}

public enum E : P {
}
#endif

#if B
import A

public protocol Q : P {
}

extension E : Q {
}
#endif

// CHECK-DIRECT: @"$s1A1EO1B1QADWP" ={{( dllexport)?}}{{( protected)?}} constant [2 x i8*] [i8* bitcast (%swift.protocol_conformance_descriptor* @"$s1A1EO1B1QADMc" to i8*), i8* bitcast (i8** @"$s1A1EOAA1PAAWP" to i8*)]
// CHECK-INDIRECT: @"$s1A1EO1B1QADWP" ={{( dllexport)?}}{{( protected)?}} constant [2 x i8*] [i8* bitcast ({{.*}}* @"$s1A1EO1B1QADMc" to i8*), i8* null]

