// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s --check-prefix=CHECK

// REQUIRES: CPU=x86_64

func blah<T>(_: T.Type) {}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s15nested_generics13makeAMetadatayyF"()
public func makeAMetadata() {
  blah(OuterGenericStruct<Int>.InnerGenericStruct<String>.self)
  blah(OuterGenericStruct<Int>.InnerConcreteStruct.self)

  blah(OuterGenericClass<Int>.InnerGenericClass<String>.self)
  blah(OuterGenericClass<Int>.InnerConcreteClass.self)
}

// Type constructor for OuterGenericStruct<Int>.InnerGenericStruct<String>
// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructV05InnerdE0VySi_SSGMa"(i64)
// CHECK: call swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructV05InnerdE0VMa"(i64 %0, %swift.type* @"$sSiN", %swift.type* @"$sSSN")
// CHECK: ret %swift.metadata_response

// Type constructor for OuterGenericStruct<T>.InnerGenericStruct<U>
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructV05InnerdE0VMa"(i64, %swift.type*, %swift.type*)

// Type constructor for OuterGenericStruct<Int>.InnerConcreteStruct
// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructV013InnerConcreteE0VySi_GMa"(i64)
// CHECK: call swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructV013InnerConcreteE0VMa"(i64 %0, %swift.type* @"$sSiN")
// CHECK: ret %swift.metadata_response

// Type constructor for OuterGenericStruct<T>.InnerConcreteStruct
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructV013InnerConcreteE0VMa"(i64, %swift.type*)

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
// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassC05InnerdE0CySi_SSGMa"(i64)
// CHECK: call swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassC05InnerdE0CMa"(i64 %0, %swift.type* @"$sSiN", %swift.type* @"$sSSN")

// Type constructor for OuterGenericClass<T>.InnerGenericClass<U>
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassC05InnerdE0CMa"(i64, %swift.type*, %swift.type*)

// Type constructor for OuterGenericClass<Int>.InnerConcreteClass
// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassC013InnerConcreteE0CySi_GMa"(i64)
// CHECK: call swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassC013InnerConcreteE0CMa"(i64 %0, %swift.type* @"$sSiN")
// CHECK: ret %swift.metadata_response

// Type constructor for OuterGenericClass<T>.InnerConcreteClass
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassC013InnerConcreteE0CMa"(i64, %swift.type*)

// Type constructor for OuterGenericStruct<T>
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s15nested_generics18OuterGenericStructVMa"(i64, %swift.type*)

// Type constructor for OuterGenericClass<T>
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s15nested_generics17OuterGenericClassCMa"(i64, %swift.type*)

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
