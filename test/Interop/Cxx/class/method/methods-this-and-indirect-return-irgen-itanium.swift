// RUN: %target-swift-emit-irgen -I %S/Inputs -enable-experimental-cxx-interop %s -Xcc -fignore-exceptions | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

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
// CHECK: call void @_ZN10HasMethods28nonConstPassThroughAsWrapperEi(%struct.NonTrivialInWrapper* sret(%struct.NonTrivialInWrapper) %{{.*}}, %struct.HasMethods* %[[CXX_THIS]], i32 42)

// CHECK: define {{.*}} void @_ZN10HasMethods28nonConstPassThroughAsWrapperEi(%struct.NonTrivialInWrapper* noalias sret(%struct.NonTrivialInWrapper) {{.*}} %{{.*}}, %struct.HasMethods* {{.*}} %{{.*}}, i32 noundef %{{.*}})
