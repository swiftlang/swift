// RUN: %target-swift-frontend -emit-ir -disable-objc-interop %s | %FileCheck %s

// Note: -disable-objc-interop is used to give consistent results on Darwin
// and Linux, avoiding differences like %swift.refcounted -vs- %objc_object,
// etc.

public class SelfCasts {
  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc %T17dynamic_self_cast9SelfCastsC* @"$s17dynamic_self_cast9SelfCastsC02toD0yACXDACFZ"(%T17dynamic_self_cast9SelfCastsC*, %swift.type* swiftself)
  // CHECK: [[ARG:%.*]] = bitcast %T17dynamic_self_cast9SelfCastsC* %0 to i8*
  // CHECK: [[METATYPE:%.*]] = bitcast %swift.type* %1 to i8*
  // CHECK: call i8* @swift_dynamicCastClassUnconditional(i8* [[ARG]], i8* [[METATYPE]], i8* null, i32 0, i32 0)
  // CHECK: ret
  public static func toSelf(_ s: SelfCasts) -> Self {
    return s as! Self
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc %T17dynamic_self_cast9SelfCastsC* @"$s17dynamic_self_cast9SelfCastsC09genericToD0yACXDxlFZ"(%swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* swiftself)
  // CHECK: call i1 @swift_dynamicCast(%swift.opaque* {{%.*}}, %swift.opaque* {{%.*}}, %swift.type* %T, %swift.type* %1, {{.*}})
  // CHECK: ret
  public static func genericToSelf<T>(_ s: T) -> Self {
    return s as! Self
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc %T17dynamic_self_cast9SelfCastsC* @"$s17dynamic_self_cast9SelfCastsC014classGenericToD0yACXDxRlzClFZ"(%swift.refcounted*, %swift.type* %T, %swift.type* swiftself)
  // CHECK: [[ARG:%.*]] = bitcast %swift.refcounted* %0 to i8*
  // CHECK: [[METATYPE:%.*]] = bitcast %swift.type* %1 to i8*
  // CHECK: call i8* @swift_dynamicCastClassUnconditional(i8* [[ARG]], i8* [[METATYPE]], i8* null, i32 0, i32 0)
  // CHECK: ret
  public static func classGenericToSelf<T : AnyObject>(_ s: T) -> Self {
    return s as! Self
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s17dynamic_self_cast9SelfCastsC011genericFromD0xylFZ"(%swift.opaque* noalias nocapture sret, %swift.type* %T, %swift.type* swiftself)
  // CHECK: call i1 @swift_dynamicCast(%swift.opaque* {{%.*}}, %swift.opaque* {{%.*}}, %swift.type* %1, %swift.type* %T, {{.*}})
  // CHECK: ret
  public static func genericFromSelf<T>() -> T {
    let s = Self()
    return s as! T
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc %swift.refcounted* @"$s17dynamic_self_cast9SelfCastsC016classGenericFromD0xyRlzClFZ"(%swift.type* %T, %swift.type* swiftself)
  // CHECK: call i1 @swift_dynamicCast(%swift.opaque* {{%.*}}, %swift.opaque* {{%.*}}, %swift.type* %0, %swift.type* %T, {{.*}})
  // CHECK: ret
  public static func classGenericFromSelf<T : AnyObject>() -> T {
    let s = Self()
    return s as! T
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc {{i32|i64}} @"$s17dynamic_self_cast9SelfCastsC02toD11ConditionalyACXDSgACFZ"(%T17dynamic_self_cast9SelfCastsC*, %swift.type* swiftself)
  // CHECK: [[ARG:%.*]] = bitcast %T17dynamic_self_cast9SelfCastsC* %0 to i8*
  // CHECK: [[METATYPE:%.*]] = bitcast %swift.type* %1 to i8*
  // CHECK: call i8* @swift_dynamicCastClass(i8* [[ARG]], i8* [[METATYPE]])
  // CHECK: ret
  public static func toSelfConditional(_ s: SelfCasts) -> Self? {
    return s as? Self
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc {{i32|i64}} @"$s17dynamic_self_cast9SelfCastsC09genericToD11ConditionalyACXDSgxlFZ"(%swift.opaque* noalias nocapture, %swift.type* %T, %swift.type* swiftself)
  // CHECK: call i1 @swift_dynamicCast(%swift.opaque* {{%.*}}, %swift.opaque* {{%.*}}, %swift.type* %T, %swift.type* %1, {{.*}})
  // CHECK: ret
  public static func genericToSelfConditional<T>(_ s: T) -> Self? {
    return s as? Self
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc {{i32|i64}} @"$s17dynamic_self_cast9SelfCastsC014classGenericToD11ConditionalyACXDSgxRlzClFZ"(%swift.refcounted*, %swift.type* %T, %swift.type* swiftself)
  // CHECK: [[ARG:%.*]] = bitcast %swift.refcounted* %0 to i8*
  // CHECK: [[METATYPE:%.*]] = bitcast %swift.type* %1 to i8*
  // CHECK: call i8* @swift_dynamicCastClass(i8* [[ARG]], i8* [[METATYPE]])
  // CHECK: ret
  public static func classGenericToSelfConditional<T : AnyObject>(_ s: T) -> Self? {
    return s as? Self
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s17dynamic_self_cast9SelfCastsC011genericFromD11ConditionalxSgylFZ"(%TSq* noalias nocapture sret, %swift.type* %T, %swift.type* swiftself)
  // CHECK: call i1 @swift_dynamicCast(%swift.opaque* {{%.*}}, %swift.opaque* {{%.*}}, %swift.type* %1, %swift.type* %T, {{.*}})
  // CHECK: ret
  public static func genericFromSelfConditional<T>() -> T? {
    let s = Self()
    return s as? T
  }

  // CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc {{i32|i64}} @"$s17dynamic_self_cast9SelfCastsC016classGenericFromD11ConditionalxSgyRlzClFZ"(%swift.type* %T, %swift.type* swiftself)
  // CHECK: call i1 @swift_dynamicCast(%swift.opaque* {{%.*}}, %swift.opaque* {{%.*}}, %swift.type* %0, %swift.type* %T, {{.*}})
  // CHECK: ret
  public static func classGenericFromSelfConditional<T : AnyObject>() -> T? {
    let s = Self()
    return s as? T
  }

  public required init() {}
}
