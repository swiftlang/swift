// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -parse-as-library -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep ObjC.pcm
// RUN: %swift -constraint-checker -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-llvm -o - %s | FileCheck %s

import objc
import Foundation
import ScriptingBridge

struct SBTypedElementArray<T : NSObject> {
  var array : SBElementArray

  constructor(array : SBElementArray) { this.array = array }

  func [conversion] __conversion() -> SBElementArray { return array }

  func objectWithName(name : NSString) -> T {
    // CHECK: define void @_TV15ScriptingBridge19SBTypedElementArray14objectWithNameU__fRGS0_Q__FT4nameC8NSString_Q_
    // CHECK: [[THISTYPE:%.*]] = bitcast %swift.type* %This to %swift.type**
    // CHECK: [[TADDR:%.*]] = getelementptr inbounds %swift.type** [[THISTYPE]], i64 3
    // CHECK: [[T:%.*]] = load %swift.type** [[TADDR]], align 8
    // CHECK: [[SEL:%.*]] = load i8** @"selector(objectWithName:)", align 8
    // CHECK: [[ARG:%.*]] = load %_T8NSString** %{{.*}}, align 8
    // CHECK: [[RETARG:%.*]] = call %_T8NSString* bitcast (%objc_object* (%objc_object*)* @objc_retain to %_T8NSString* (%_T8NSString*)*)(%_T8NSString* [[ARG]]) nounwind
    // CHECK: [[OBJ:%.*]] = call %_T8NSObject* bitcast (void ()* @objc_msgSend to %_T8NSObject* (%_T14SBElementArray*, i8*, %_T8NSString*)*)(%_T14SBElementArray* %{{.*}}, i8* [[SEL]], %_T8NSString* [[ARG]])
    // CHECK: [[RETOBJ:%.*]] = call %_T8NSObject* bitcast (%objc_object* (%objc_object*)* @objc_retainAutoreleasedReturnValue to %_T8NSObject* (%_T8NSObject*)*)(%_T8NSObject* [[OBJ]]) nounwind
    // CHECK: [[OBJCAST:%.*]] = bitcast %_T8NSObject* [[RETOBJ]] to i8*
    // CHECK: [[TCAST:%.*]] = bitcast %swift.type* [[T]] to i8*
    // CHECK: call i8* @swift_dynamicCastUnconditional(i8* [[OBJCAST]], i8* [[TCAST]])
    return T(array.objectWithName(name))
  }
}

func checkHive(hive : SBHive, b : B, name : NSString) {
  var b2 = hive.bees.objectWithName(name)
}
