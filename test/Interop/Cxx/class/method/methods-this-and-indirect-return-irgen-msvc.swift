// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s

// REQUIRES: OS=windows-msvc

import Methods

public func use() -> CInt {
    var instance = HasMethods()
    let result = instance.nonConstPassThroughAsWrapper(42)
    return result.value
}

// CHECK: %[[instance:.*]] = alloca %TSo10HasMethodsV
// CHECK: %[[result:.*]] = alloca %TSo19NonTrivialInWrapperV
// CHECK: %[[CXX_THIS_PRE:.*]] = bitcast %TSo10HasMethodsV* %[[instance]] to %struct.HasMethods*
// CHECK: %[[CXX_THIS:.*]] = bitcast %TSo10HasMethodsV* %[[instance]] to %struct.HasMethods*
// CHECK: call void @"?nonConstPassThroughAsWrapper@HasMethods@@QEAA?AUNonTrivialInWrapper@@H@Z"(%struct.HasMethods* %[[CXX_THIS]], %struct.NonTrivialInWrapper* sret(%struct.NonTrivialInWrapper) %{{.*}}, i32 42)

// CHECK: define {{.*}} void @"?nonConstPassThroughAsWrapper@HasMethods@@QEAA?AUNonTrivialInWrapper@@H@Z"(%struct.HasMethods* {{.*}} %{{.*}}, %struct.NonTrivialInWrapper* noalias sret(%struct.NonTrivialInWrapper) {{.*}} %{{.*}}, i32 noundef %{{.*}})
