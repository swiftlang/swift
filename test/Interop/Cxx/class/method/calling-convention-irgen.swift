// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -parse-stdlib -module-name Swift -cxx-interoperability-mode=default -use-clang-function-types -target i686-unknown-windows-msvc -resource-dir %t -I %S/Inputs %s | %FileCheck %s

// REQUIRES: CODEGENERATOR=X86

// Use an empty resource directory so implicit Cxx and CxxStdlib lookups do not
// require runtime modules built for i686.
// Supply Void without requiring a standard library for the target.
public typealias Void = ()

import CallingConvention

// The default instance-method convention is thiscall, while an explicit
// cdecl method uses the platform C convention.
// CHECK-LABEL: define{{.*}} swiftcc void @"$ss7callAllyySo17CallingConventionVzF"
// CHECK: call x86_thiscallcc void @"?defaultMethod@CallingConvention@@QAEXXZ"(ptr {{%.*}})
// CHECK: call x86_stdcallcc void @"?stdcallMethod@CallingConvention@@QAGXXZ"(ptr {{%.*}})
// CHECK: call void @"?cdeclMethod@CallingConvention@@QAAXXZ"(ptr {{%.*}})
// CHECK: ret void
public func callAll(_ value: inout CallingConvention) {
  value.defaultMethod()
  value.stdcallMethod()
  value.cdeclMethod()
}
