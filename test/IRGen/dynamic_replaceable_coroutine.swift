// RUN: %target-swift-frontend %use_no_opaque_pointers -module-name A -enable-implicit-dynamic -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name A -enable-implicit-dynamic -emit-ir %s


extension Int {
  public struct Thing {
    var _int: Int
    init(_ int: Int) {
      self._int = int
    }
  }

  public var thing: Thing {
    get { Thing(self) }
    // Make sure the initialization of `thing` is after the dynamic replacement
    // check. Coro splitting does not like memsets before the coro.begin.

    // CHECK: define{{.*}} swiftcc { i8*, %TSi1AE5ThingV* } @"$sSi1AE5thingSiAAE5ThingVvM"
    // CHECK: call i8* @swift_getFunctionReplacement
    // CHECK: br
    // CHECK: original_entry:
    // CHECK: [[FRAMEPTR:%.*]] = bitcast i8* %0 to
    // CHECK: [[THING:%.*]] = getelementptr inbounds {{.*}}* [[FRAMEPTR]], i32 0
    // CHECK: [[THING2:%.*]] = bitcast %TSi1AE5ThingV* [[THING]] to i8*
    // CHECK: call void @llvm.memset{{.*}}(i8* {{.*}} [[THING2]]
    // CHECK: ret
    _modify {
      var thing = Thing(self)
      yield &thing
    }
  }
}
