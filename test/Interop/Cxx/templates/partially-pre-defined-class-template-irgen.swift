// RUN: %target-swift-emit-ir %s -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s
import PartiallyPreDefinedClassTemplate

public func getWrappedMagicInt() -> CInt {
  let myInt = IntWrapper(value: 7)
  var magicInt = PartiallyPreDefinedMagicallyWrappedInt(t: myInt)
  return magicInt.getValuePlusArg(13)
}

// CHECK-LABEL: define {{.*}}swiftcc i32 @"$s4main18getWrappedMagicInts5Int32VyF"()
// CHECK: i32 @{{_ZNK12MagicWrapperI10IntWrapperE15getValuePlusArgEi|"\?getValuePlusArg@\?\$MagicWrapper@UIntWrapper@@@@QEBAHH@Z"}}(ptr {{[^,]*}}, i32 {{(noundef )?}}13)
// CHECK: define {{.*}}i32 @{{_ZNK12MagicWrapperI10IntWrapperE15getValuePlusArgEi|"\?getValuePlusArg@\?\$MagicWrapper@UIntWrapper@@@@QEBAHH@Z"}}(
// CHECK: define {{.*}}i32 @{{_ZNK10IntWrapper8getValueEv|"\?getValue@IntWrapper@@QEBAHXZ"}}(
