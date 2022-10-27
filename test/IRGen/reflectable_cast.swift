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
	// CHECK-OPT-IN-LABEL: define swiftcc void @"$s16reflectable_cast16ReflectableCastsC15conditionalCastys0C0_pSgAA1ROFZ"({{.*}})
	// CHECK-OPT-IN-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* {{.*}}{{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-OPT-IN-DAG: ret void

	// CHECK-FULL-LABEL: define swiftcc void @"$s16reflectable_cast16ReflectableCastsC15conditionalCastys0C0_pSgAA1ROFZ"({{.*}})
	// CHECK-FULL-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* {{.*}}{{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-FULL-DAG: ret void
	public static func conditionalCast(_ s: R) -> Reflectable? {
    	return s as? Reflectable
  	}

	// CHECK-OPT-IN-LABEL: define swiftcc void @"$s16reflectable_cast16ReflectableCastsC9forceCastys0C0_pAA1ROFZ"({{.*}})
	// CHECK-OPT-IN-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* {{.*}}{{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-OPT-IN-DAG: ret void

	// CHECK-FULL-LABEL: define swiftcc void @"$s16reflectable_cast16ReflectableCastsC9forceCastys0C0_pAA1ROFZ"({{.*}})
	// CHECK-FULL-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{%.*}}, %swift.opaque* {{.*}}{{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-FULL-DAG: ret void
	public static func forceCast(_ s: R) -> Reflectable {
    	return s as! Reflectable
  	}

	// CHECK-OPT-IN-LABEL: define swiftcc i1 @"$s16reflectable_cast16ReflectableCastsC6isTestySbAA1ROFZ"({{.*}})
	// CHECK-OPT-IN-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{.*}}{{%.*}}, %swift.opaque* {{.*}}{{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-OPT-IN-DAG: ret i1 {{%[0-9]+}}

	// CHECK-FULL-LABEL: define swiftcc i1 @"$s16reflectable_cast16ReflectableCastsC6isTestySbAA1ROFZ"({{.*}})
	// CHECK-FULL-DAG: call zeroext i1 @swift_reflectableCast(%swift.opaque* {{.*}}{{%.*}}, %swift.opaque* {{.*}}{{%.*}}, %swift.type* {{.*}}, %swift.type* {{.*}}, {{.*}})
  	// CHECK-FULL-DAG: ret i1 {{%[0-9]+}}
	public static func isTest(_ s: R) -> Bool {
    	return s is Reflectable
  	}
}

