// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -wmo -module-name A -emit-irgen -o %t/A1.ll %t/A.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedExistentials -parse-as-library
// RUN: %FileCheck --check-prefix=A1 %s < %t/A1.ll

// RUN: %target-swift-frontend -wmo -module-name A -num-threads 1 -emit-ir -o %t/A2.ll -o %t/B2.ll %t/A.swift %t/B.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedExistentials -parse-as-library
// RUN: %FileCheck --check-prefix=A2 %s < %t/A2.ll
// RUN: %FileCheck --check-prefix=B2 %s < %t/B2.ll

// RUN: %target-swift-frontend  -emit-module -emit-module-path %t/A.swiftmodule -wmo -module-name A -emit-ir -o %t/A3.ll  %t/A.swift %t/B.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedExistentials -parse-as-library
// RUN: %FileCheck --check-prefix=A3 %s < %t/A3.ll

// RUN: %target-swift-frontend -wmo -I %t -module-name C -emit-irgen -o %t/C4.ll  %t/C.swift -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedExistentials -parse-as-library
// RUN: %FileCheck --check-prefix=C4 %s < %t/C4.ll

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials


//--- A.swift
public class SomeClass {
}

public struct SomeStruct {
  var x = 1
  var y = 2
}

public enum SomeEnum {
   case a(Int)
   case b(Float)
}

//--- B.swift
public func getSomeClass() ->  SomeClass {
  return SomeClass()
}

public func getSomeStructAsAny() -> Any {
  return SomeStruct()
}

public func getSomeStruct() -> SomeStruct {
  return SomeStruct()
}

public func getSomeEnumAsAny() -> Any {
  return SomeEnum.a(1)
}

public func getSomeEnum()-> SomeEnum {
  return SomeEnum.b(2.0)
}

//--- C.swift
import A
public func useMetadata() -> Any {
  return getSomeClass()
}

public func useMetadata2() -> Any {
  return getSomeStruct()
}

public func useMetadata3() -> Any {
  return getSomeEnum()
}

// Test no reference of metadata.
// A1-NOT: CMf

// Test reference of metadata. Because we are the defining module metadata is
// visible outside of the image per current policy.
// In multiple llvm modules per SIL module mode "Private" SIL linkage becomes
// hidden linkonce_odr.
// A2: @"$e1A8SomeEnumOMf" = linkonce_odr hidden constant
// A2: @"$e1A10SomeStructVMf" = linkonce_odr hidden constant
// A2: @"$e1A9SomeClassCMf" = linkonce_odr hidden global

// A2: @"$e1A8SomeEnumON" = {{.*}}alias{{.*}}e1A8SomeEnumOMf
// A2: @"$e1A10SomeStructVN" = {{.*}}alias{{.*}}e1A10SomeStructVMf
// A2: @"$e1A9SomeClassCN" = {{.*}}alias{{.*}}e1A9SomeClassCMf

// B2: @"$e1A9SomeClassCN" = {{.*}}external{{.*}} global
// B2: @"$e1A10SomeStructVN" = {{.*}}external{{.*}} global
// B2: @"$e1A8SomeEnumON" = {{.*}}external{{.*}} global
// B2: call {{.*}} @"$e1A9SomeClassCACycfC"(ptr swiftself @"$e1A9SomeClassCN")
// B2: store{{.*}}e1A10SomeStructVN
// B2: store{{.*}}e1A8SomeEnumON

// Test reference of metadata. Because we are the defining module metadata is
// visible outside of the image per current policy.
// In single llvm module per SIL module "Private" SIL linkage makes more
// intuitive sense.
// A3: @"$e1A8SomeEnumOMf" = {{.*}}internal constant
// A3: @"$e1A10SomeStructVMf" = {{.*}}internal constant
// A3: @"$e1A9SomeClassCMf" = {{.*}}internal global
// A3: @"$e1A9SomeClassCN" = {{.*}}alias{{.*}}e1A9SomeClassCMf
// A3: call {{.*}} @"$e1A9SomeClassCACycfC"({{.*}}getelementptr{{.*}}@"$e1A9SomeClassCMf"


// Test "external" reference of metadata (defines metadata from another module
// in current module as linkonce).
// C4: @"$e1A8SomeEnumOMf" = linkonce_odr hidden constant
// C4: @"$e1A10SomeStructVMf" = linkonce_odr hidden constant
// C4: @"$e1A9SomeClassCMf" = linkonce_odr hidden global
