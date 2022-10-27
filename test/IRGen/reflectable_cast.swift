// RUN: %target-swift-frontend -DOPTIN -enable-upcoming-feature OptInReflection -target %target-cpu-apple-macosx99.99 -emit-ir %s | %FileCheck %s --check-prefix=CHECK-OPT-IN
// RUN: %target-swift-frontend -DFULL -target %target-cpu-apple-macosx99.99 -emit-ir %s | %FileCheck %s --check-prefix=CHECK-FULL

#if OPTIN
public enum R: Reflectable {
	case A(Int)
	case B
}
#elseif FULL
public enum R {
	case A(Int)
	case B
}
#endif


public class ReflectableCasts {
	// CHECK-OPT-IN-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s16reflectable_cast16ReflectableCastsC15conditionalCastys0C0_pSgAA1ROFZ"(%Ts11Reflectable_pSg* noalias nocapture sret(%Ts11Reflectable_pSg) %0, i64 %1, i8 %2, %swift.type* nocapture readnone swiftself %3)
	// CHECK-OPT-IN-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* nonnull {{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-OPT-IN-DAG: ret void

	// CHECK-FULL-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s16reflectable_cast16ReflectableCastsC15conditionalCastys0C0_pSgAA1ROFZ"(%Ts11Reflectable_pSg* noalias nocapture sret(%Ts11Reflectable_pSg) %0, i64 %1, i8 %2, %swift.type* nocapture readnone swiftself %3)
	// CHECK-FULL-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* nonnull {{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-FULL-DAG: ret void
	public static func conditionalCast(_ s: R) -> Reflectable? {
    	return s as? Reflectable
  	}

	// CHECK-OPT-IN-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s16reflectable_cast16ReflectableCastsC9forceCastys0C0_pAA1ROFZ"(%Ts11ReflectableP* noalias nocapture sret(%Ts11ReflectableP) %0, i64 %1, i8 %2, %swift.type* nocapture readnone swiftself %3)
	// CHECK-OPT-IN-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* nonnull {{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-OPT-IN-DAG: ret void

	// CHECK-FULL-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s16reflectable_cast16ReflectableCastsC9forceCastys0C0_pAA1ROFZ"(%Ts11ReflectableP* noalias nocapture sret(%Ts11ReflectableP) %0, i64 %1, i8 %2, %swift.type* nocapture readnone swiftself %3)
	// CHECK-FULL-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* nonnull {{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-FULL-DAG: ret void
	public static func forceCast(_ s: R) -> Reflectable {
    	return s as! Reflectable
  	}

	// define swiftcc i1 @"$s16reflectable_cast16ReflectableCastsC6isTestySbAA1ROFZ"(i64 %0, i8 %1, %swift.type* nocapture readnone swiftself %2) #1 {

	// CHECK-OPT-IN-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc i1 @"$s16reflectable_cast16ReflectableCastsC6isTestySbAA1ROFZ"(i64 %0, i8 %1, %swift.type* nocapture readnone swiftself %2)
	// CHECK-OPT-IN-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* nonnull {{%.*}}, %swift.opaque* nonnull {{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-OPT-IN-DAG: ret i1 {{%[0-9]+}}

	// CHECK-FULL-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc i1 @"$s16reflectable_cast16ReflectableCastsC6isTestySbAA1ROFZ"(i64 %0, i8 %1, %swift.type* nocapture readnone swiftself %2)
	// CHECK-FULL-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* nonnull {{%.*}}, %swift.opaque* nonnull {{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-FULL-DAG: ret i1 {{%[0-9]+}}
	public static func isTest(_ s: R) -> Bool {
    	return s is Reflectable
  	}
}

