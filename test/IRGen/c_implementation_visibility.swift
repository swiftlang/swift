// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The C entry point of an `@c @implementation` function is hidden if either
// the C declaration it implements is hidden or the Swift declaration is
// effectively less than public.

// RUN: %target-swift-frontend -emit-ir %t/main.swift -I %t \
// RUN:   -module-name main -parse-as-library \
// RUN:   -validate-tbd-against-ir=all \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,DEFAULT

// -enable-testing exports internal implementations, unless the C declaration
// is hidden.
// RUN: %target-swift-frontend -emit-ir %t/main.swift -I %t \
// RUN:   -module-name main -parse-as-library \
// RUN:   -validate-tbd-against-ir=all -enable-testing \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,TESTING

// -fvisibility=hidden makes unannotated C declarations hidden.
// RUN: %target-swift-frontend -emit-ir %t/main.swift -I %t \
// RUN:   -module-name main -parse-as-library \
// RUN:   -validate-tbd-against-ir=all -Xcc -fvisibility=hidden \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,FVIS-HIDDEN

// UNSUPPORTED: OS=windows-msvc

//--- module.modulemap
module Vis {
  header "Vis.h"
}

//--- Vis.h
void defaultPublic(void);
void defaultInternal(void);
void defaultFilePrivate(void);
__attribute__((visibility("hidden"))) void hiddenPublic(void);
__attribute__((visibility("hidden"))) void hiddenInternal(void);
__attribute__((visibility("default"))) void explicitDefaultInternal(void);
__attribute__((visibility("hidden"))) void cdeclHiddenPublic(void);

//--- main.swift
@_implementationOnly import Vis

// DEFAULT-DAG: define{{( protected)?}} void @defaultPublic()
// TESTING-DAG: define{{( protected)?}} void @defaultPublic()
// FVIS-HIDDEN-DAG: define hidden void @defaultPublic()
@c @implementation public func defaultPublic() {}

// DEFAULT-DAG: define hidden void @defaultInternal()
// TESTING-DAG: define{{( protected)?}} void @defaultInternal()
// FVIS-HIDDEN-DAG: define hidden void @defaultInternal()
@c @implementation func defaultInternal() {}

// Private implementations are still visible to C code in the same module.
// CHECK-DAG: define hidden void @defaultFilePrivate()
@c @implementation fileprivate func defaultFilePrivate() {}

// CHECK-DAG: define hidden void @hiddenPublic()
@c @implementation public func hiddenPublic() {}

// CHECK-DAG: define hidden void @hiddenInternal()
@c @implementation func hiddenInternal() {}

// DEFAULT-DAG: define hidden void @explicitDefaultInternal()
// TESTING-DAG: define{{( protected)?}} void @explicitDefaultInternal()
// FVIS-HIDDEN-DAG: define hidden void @explicitDefaultInternal()
@c @implementation func explicitDefaultInternal() {}

// `@_cdecl` goes through a native-to-foreign thunk, which must be hidden and
// not serialized.
// CHECK-DAG: define hidden void @cdeclHiddenPublic()
@_cdecl("cdeclHiddenPublic") @implementation public func cdeclHiddenPublic() {}
