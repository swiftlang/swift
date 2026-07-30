// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s
import PartiallyPreDefinedClassTemplate

public func getWrappedMagicInt() -> CInt {
  let myInt = IntWrapper(value: 7)
  var magicInt = PartiallyPreDefinedMagicallyWrappedInt(t: myInt)
  return magicInt.getValuePlusArg(13)
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main18getWrappedMagicInts5Int32VyF"()
// CHECK: %magicInt = alloca %TSo0030MagicWrapperIntWrapper_bHAFhxbV, align 4
// CHECK: %magicInt.t = getelementptr inbounds nuw %TSo0030MagicWrapperIntWrapper_bHAFhxbV, ptr %magicInt, i32 0, i32 0
// CHECK: {{(invoke|call)}} {{(noundef )?}}i32 @{{_ZNK12MagicWrapperI10IntWrapperE15getValuePlusArgEi|"\?getValuePlusArg@\?\$MagicWrapper@UIntWrapper@@@@QEBAHH@Z"}}(ptr {{[^,]*}}%magicInt, i32 {{(noundef )?}}13)

// CHECK: define {{(weak_odr|linkonce_odr)}}{{( dso_local)?}} {{(noundef )?}}i32 @{{_ZNK12MagicWrapperI10IntWrapperE15getValuePlusArgEi|"\?getValuePlusArg@\?\$MagicWrapper@UIntWrapper@@@@QEBAHH@Z"}}(ptr {{[^,]*}} %this, i32 {{(noundef )?}}%arg)
// CHECK: %this.addr = alloca ptr, align {{4|8}}
// CHECK: store ptr %this, ptr %this.addr, align {{4|8}}
// CHECK: %this1 = load ptr, ptr %this.addr, align {{4|8}}
// CHECK: %t = getelementptr inbounds nuw %struct.MagicWrapper, ptr %this1, i32 0, i32 0
// CHECK: %call = call {{(noundef )?}}i32 @{{_ZNK10IntWrapper8getValueEv|"\?getValue@IntWrapper@@QEBAHXZ"}}(ptr {{[^,]*}} %t)
// CHECK: [[ARG:%.*]] = load i32, ptr %arg.addr, align 4
// CHECK: %add = add nsw i32 %call, [[ARG]]
// CHECK: ret i32 %add

// CHECK: define linkonce_odr{{( dso_local)?}} {{(noundef )?}}i32 @{{_ZNK10IntWrapper8getValueEv|"\?getValue@IntWrapper@@QEBAHXZ"}}(ptr {{[^,]*}} %this)
// CHECK: %this.addr = alloca ptr, align {{4|8}}
// CHECK: store ptr %this, ptr %this.addr, align {{4|8}}
// CHECK: %this1 = load ptr, ptr %this.addr, align {{4|8}}
// CHECK: %value = getelementptr inbounds nuw %struct.IntWrapper, ptr %this1, i32 0, i32 0
// CHECK: [[VALUE:%.*]] = load i32, ptr %value, align 4
// CHECK: ret i32 [[VALUE]]
