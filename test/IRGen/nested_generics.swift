// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s --check-prefix=CHECK

// REQUIRES: CPU=x86_64

func blah<T>(_: T.Type) {}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T015nested_generics13makeAMetadatayyF()
public func makeAMetadata() {
  blah(OuterGenericStruct<Int>.InnerGenericStruct<String>.self)
  blah(OuterGenericStruct<Int>.InnerConcreteStruct.self)

  blah(OuterGenericClass<Int>.InnerGenericClass<String>.self)
  blah(OuterGenericClass<Int>.InnerConcreteClass.self)
}

// Type constructor for OuterGenericStruct<Int>.InnerGenericStruct<String>
// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_T015nested_generics18OuterGenericStructV05InnerdE0VySi_SSGMa()
// CHECK: call %swift.type* @_T015nested_generics18OuterGenericStructV05InnerdE0VMa(%swift.type* @_T0SiN, %swift.type* @_T0SSN)
// CHECK: ret %swift.type

// Type constructor for OuterGenericStruct<T>.InnerGenericStruct<U>
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015nested_generics18OuterGenericStructV05InnerdE0VMa(%swift.type*, %swift.type*)

// Type constructor for OuterGenericStruct<Int>.InnerConcreteStruct
// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_T015nested_generics18OuterGenericStructV013InnerConcreteE0VySi_GMa()
// CHECK: call %swift.type* @_T015nested_generics18OuterGenericStructV013InnerConcreteE0VMa(%swift.type* @_T0SiN)
// CHECK: ret %swift.type

// Type constructor for OuterGenericStruct<T>.InnerConcreteStruct
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015nested_generics18OuterGenericStructV013InnerConcreteE0VMa(%swift.type*)

public struct OuterGenericStruct<T> {
  public struct InnerGenericStruct<U> {
    public func method() {
      blah(T.self)
      blah(U.self)
    }
  }

  public struct InnerConcreteStruct {
    public func method() {
      blah(T.self)
    }
  }
}

// Type constructor for OuterGenericClass<Int>.InnerGenericClass<String>
// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_T015nested_generics17OuterGenericClassC05InnerdE0CySi_SSGMa()
// CHECK: call %swift.type* @_T015nested_generics17OuterGenericClassC05InnerdE0CMa(%swift.type* @_T0SiN, %swift.type* @_T0SSN)

// Type constructor for OuterGenericClass<T>.InnerGenericClass<U>
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015nested_generics17OuterGenericClassC05InnerdE0CMa(%swift.type*, %swift.type*)

// Type constructor for OuterGenericClass<Int>.InnerConcreteClass
// CHECK-LABEL: define linkonce_odr hidden %swift.type* @_T015nested_generics17OuterGenericClassC013InnerConcreteE0CySi_GMa()
// CHECK: call %swift.type* @_T015nested_generics17OuterGenericClassC013InnerConcreteE0CMa(%swift.type* @_T0SiN)
// CHECK: ret %swift.type

// Type constructor for OuterGenericClass<T>.InnerConcreteClass
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015nested_generics17OuterGenericClassC013InnerConcreteE0CMa(%swift.type*)

// Type constructor for OuterGenericStruct<T>
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015nested_generics18OuterGenericStructVMa(%swift.type*)

// Type constructor for OuterGenericClass<T>
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015nested_generics17OuterGenericClassCMa(%swift.type*)

public class OuterGenericClass<T> {
  public class InnerGenericClass<U> {
    public func method() {
      blah(T.self)
      blah(U.self)
    }
  }

  public class InnerConcreteClass {
    public func method() {
      blah(T.self)
    }
  }
}

// This used to crash while emitting value witnesses.

public struct Fish<Water> {}

public protocol Wet {}

extension Fish where Water : Wet {
  public enum Fillet {
    case grilled
    case fried
  }
}
