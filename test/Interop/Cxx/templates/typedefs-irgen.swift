// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import Typedefs

public func foo() -> CInt {
  let banana = Banana()
  var peeled: PeeledBanana = PeeledBanana(t: banana)
  return peeled.doPeel()
}

// CHECK: $_ZNK4PeelI6BananaE6doPeelEv = comdat any

// CHECK: $_ZNK6Banana4peelEv = comdat any

// CHECK: define protected swiftcc i32 @"$s4main3foos5Int32VyF"() #0 {
// CHECK: %peeled = alloca %TSo4PeelV, align 1
// CHECK: %0 = bitcast %TSo4PeelV* %peeled to i8*
// CHECK: call void @llvm.memset.p0i8.i64(i8* align 1 %0, i8 0, i64 1, i1 false)
// CHECK: %1 = bitcast %TSo4PeelV* %peeled to i8*
// CHECK: %peeled.t = getelementptr inbounds %TSo4PeelV, %TSo4PeelV* %peeled, i32 0, i32 0
// CHECK: %2 = bitcast %TSo4PeelV* %peeled to %class.Peel*
// CHECK: %3 = call i32 @_ZNK4PeelI6BananaE6doPeelEv(%class.Peel* %2)
// CHECK: %4 = bitcast %TSo4PeelV* %peeled to i8*
// CHECK: ret i32 %3

// CHECK: define weak_odr i32 @_ZNK4PeelI6BananaE6doPeelEv(%class.Peel* %this) #3 comdat align 2
// CHECK: %this.addr = alloca %class.Peel*, align 8
// CHECK: store %class.Peel* %this, %class.Peel** %this.addr, align 8
// CHECK: %this1 = load %class.Peel*, %class.Peel** %this.addr, align 8
// CHECK: %t = getelementptr inbounds %class.Peel, %class.Peel* %this1, i32 0, i32 0
// CHECK: %call = call i32 @_ZNK6Banana4peelEv(%class.Banana* %t)
// CHECK: %add = add nsw i32 %call, 1
// CHECK: ret i32 %add

// CHECK: define linkonce_odr i32 @_ZNK6Banana4peelEv(%class.Banana* %this) #4 comdat align 2
// CHECK: %this.addr = alloca %class.Banana*, align 8
// CHECK: store %class.Banana* %this, %class.Banana** %this.addr, align 8
// CHECK: %this1 = load %class.Banana*, %class.Banana** %this.addr, align 8
// CHECK: ret i32 42
