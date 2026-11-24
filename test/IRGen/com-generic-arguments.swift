// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Library -enable-library-evolution -emit-module -emit-module-path %t/Library.swiftmodule %S/../Inputs/COMGenericArguments.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Library -enable-library-evolution -emit-ir -sil-verify-all %S/../Inputs/COMGenericArguments.swift | %FileCheck %s --check-prefix=LIBRARY
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir -sil-verify-all %s | %FileCheck %s

import Library

// LIBRARY-LABEL: define{{.*}} @"$s7Library7forward{{.*}}(ptr{{.*}}, ptr{{.*}}, i{{32|64}} %T.IExtended)
// LIBRARY: call swiftcc {{.*}} @"$s7Library4size{{.*}}(ptr{{.*}}, ptr %T, i{{32|64}} %T.IExtended)
// CHECK-LABEL: define{{.*}} @"$s{{.*}}4call
// CHECK: call swiftcc {{.*}} @"$s7Library7forward{{.*}}(ptr{{.*}}, ptr{{.*}}, i{{32|64}} 0)
public func call(_ value: any IExtended) -> Int {
  forward(value)
}

public func stored<T: IItem>(_ value: T) -> Int {
  Holder(value).size() + Owner(value).size() + capture(value)()
}

public func packs<T: IItem>(_ value: T) -> Int {
  forwardPack(value, value)
}
