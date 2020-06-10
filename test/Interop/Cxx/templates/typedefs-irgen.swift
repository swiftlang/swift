// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import Typedefs

public func foo() -> CInt {
  let banana = Banana()
  var peeled: PeeledBanana = PeeledBanana(fruit: banana)
  return peeled.peeledTaste()
}

// CHECK: $_ZNK4PeelI6BananaE11peeledTasteEv = comdat any

// CHECK: $_ZNK6Banana5tasteEv = comdat any

// CHECK: define protected swiftcc i32 @"$s4main3foos5Int32VyF"() #0 {
// CHECK: %peeled = alloca %TSo4PeelV, align 1
// CHECK: %0 = bitcast %TSo4PeelV* %peeled to i8*
// CHECK: call void @llvm.memset.p0i8.i64(i8* align 1 %0, i8 0, i64 1, i1 false)
// CHECK: %1 = bitcast %TSo4PeelV* %peeled to i8*
// CHECK: %peeled.fruit = getelementptr inbounds %TSo4PeelV, %TSo4PeelV* %peeled, i32 0, i32 0
// CHECK: %2 = bitcast %TSo4PeelV* %peeled to %struct.Peel*
// CHECK: %3 = call i32 @_ZNK4PeelI6BananaE11peeledTasteEv(%struct.Peel* %2)
// CHECK: %4 = bitcast %TSo4PeelV* %peeled to i8*
// CHECK: ret i32 %3

// CHECK: define weak_odr i32 @_ZNK4PeelI6BananaE11peeledTasteEv(%struct.Peel* %this) #3 comdat align 2
// CHECK: %this.addr = alloca %struct.Peel*, align 8
// CHECK: store %struct.Peel* %this, %struct.Peel** %this.addr, align 8
// CHECK: %this1 = load %struct.Peel*, %struct.Peel** %this.addr, align 8
// CHECK: %fruit = getelementptr inbounds %struct.Peel, %struct.Peel* %this1, i32 0, i32 0
// CHECK: %call = call i32 @_ZNK6Banana5tasteEv(%struct.Banana* %fruit)
// CHECK: %add = add nsw i32 %call, 5
// CHECK: ret i32 %add

// CHECK: define linkonce_odr i32 @_ZNK6Banana5tasteEv(%struct.Banana* %this) #4 comdat align 2
// CHECK: %this.addr = alloca %struct.Banana*, align 8
// CHECK: store %struct.Banana* %this, %struct.Banana** %this.addr, align 8
// CHECK: %this1 = load %struct.Banana*, %struct.Banana** %this.addr, align 8
// CHECK: ret i32 24
