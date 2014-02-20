// RUN: %swift -emit-silgen %s | FileCheck %s

protocol PropertyWithGetter {
  var a : Int { get }
}

protocol PropertyWithGetterSetter {
  var b : Int { get set }
}


class ClassWithGetter : PropertyWithGetter {
  var a : Int { get: return 42 }
}



class ClassWithGetterSetter : PropertyWithGetterSetter, PropertyWithGetter {
  var a : Int { get: return 1 set: }
  var b : Int { get: return 2 set: }
}


// CHECK: sil_witness_table ClassWithGetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWC9protocols15ClassWithGetterS_18PropertyWithGetterS_FS1_g1aSi
// CHECK-NEXT: }

// CHECK: sil_witness_table ClassWithGetterSetter: PropertyWithGetterSetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!getter.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterS_FS1_g1bSi
// CHECK-NEXT:  method #PropertyWithGetterSetter.b!setter.1: @_TTWC9protocols21ClassWithGetterSetterS_24PropertyWithGetterSetterS_FS1_s1bSi
// CHECK-NEXT: }

// CHECK: sil_witness_table ClassWithGetterSetter: PropertyWithGetter module protocols {
// CHECK-NEXT:  method #PropertyWithGetter.a!getter.1: @_TTWC9protocols21ClassWithGetterSetterS_18PropertyWithGetterS_FS1_g1aSi
// CHECK-NEXT: }
