// RUN: %target-swift-frontend -Osize -import-objc-header %S/Inputs/Outliner.h %s -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend -Osize -g -import-objc-header %S/Inputs/Outliner.h %s -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

public class MyGizmo {
   private var gizmo : Gizmo
   private var optionalGizmo : Gizmo?

   init() {
     gizmo = Gizmo()
   }
	 // CHECK-LABEL: sil @_T08outliner7MyGizmoC11usePropertyyyF
	 // CHECK: [[FUN:%.*]] = function_ref @_T0So5GizmoC14stringPropertySQySSGvgToTeab_
	 // CHECK: apply [[FUN]]({{.*}}) : $@convention(thin) (@in_guaranteed Gizmo) -> @owned Optional<String>
	 // CHECK-NOT: return
	 // CHECK: apply [[FUN]]({{.*}}) : $@convention(thin) (@in_guaranteed Gizmo) -> @owned Optional<String>
	 // CHECK: return
   public func useProperty() {
     print(gizmo.stringProperty)
     print(optionalGizmo!.stringProperty)
	 }
}

// CHECK-LABEL: sil @_T08outliner13testOutliningyyF
// CHECK:  [[FUN:%.*]] = function_ref @_T0So5GizmoC14stringPropertySQySSGvgToTepb_
// CHECK:  apply [[FUN]](%{{.*}}) : $@convention(thin) (Gizmo) -> @owned Optional<String>
// CHECK:  apply [[FUN]](%{{.*}}) : $@convention(thin) (Gizmo) -> @owned Optional<String>
// CHECK:  [[FUN:%.*]] = function_ref @_T0So5GizmoC14stringPropertySQySSGvsToTembnn_
// CHECK:  apply [[FUN]]({{.*}}) : $@convention(thin) (@owned String, Gizmo) -> ()
// CHECK:  apply [[FUN]]({{.*}}) : $@convention(thin) (@owned String, Gizmo) -> ()
// CHECK:  [[FUN:%.*]] = function_ref @_T0So5GizmoC12modifyStringSQySSGAD_Si10withNumberSQyypG0D6FoobartFToTembnnnb_
// CHECK:  apply [[FUN]]({{.*}}) : $@convention(thin) (@owned String, Int, Optional<AnyObject>, Gizmo) -> @owned Optional<String>
// CHECK:  apply [[FUN]]({{.*}}) : $@convention(thin) (@owned String, Int, Optional<AnyObject>, Gizmo) -> @owned Optional<String>
// CHECK:  [[FUN:%.*]] = function_ref @_T0So5GizmoC11doSomethingSQyypGSQySaySSGGFToTembnn_
// CHECK:  apply [[FUN]]({{.*}}) : $@convention(thin) (@owned Array<String>, Gizmo) -> @owned Optional<AnyObject>
// CHECK:  apply [[FUN]]({{.*}}) : $@convention(thin) (@owned Array<String>, Gizmo) -> @owned Optional<AnyObject>
// CHECK: return
public func testOutlining() {
  let gizmo = Gizmo()
  let foobar = Gizmo()
  print(gizmo.stringProperty)
  print(gizmo.stringProperty)

  gizmo.stringProperty = "foobar"
  gizmo.stringProperty = "foobar2"
  gizmo.modifyString("hello", withNumber:1, withFoobar: foobar)
  gizmo.modifyString("hello", withNumber:1, withFoobar: foobar)
  let arr = [ "foo", "bar"]
  gizmo.doSomething(arr)
  gizmo.doSomething(arr)
}

// CHECK-LABEL: sil shared [noinline] @_T0So5GizmoC14stringPropertySQySSGvgToTeab_ : $@convention(thin) (@in_guaranteed Gizmo) -> @owned Optional<String>
// CHECK: bb0(%0 : $*Gizmo):
// CHECK:   %1 = load %0 : $*Gizmo
// CHECK:   %2 = objc_method %1 : $Gizmo, #Gizmo.stringProperty!getter.1.foreign : (Gizmo) -> () -> String!
// CHECK:   %3 = apply %2(%1) : $@convention(objc_method) (Gizmo) -> @autoreleased Optional<NSString>
// CHECK:   switch_enum %3 : $Optional<NSString>, case #Optional.some!enumelt.1: bb1, case #Optional.none!enumelt: bb2
// CHECK: bb1(%5 : $NSString):
// CHECK:   %6 = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
// CHECK:   %7 = metatype $@thin String.Type
// CHECK:   %8 = apply %6(%3, %7) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
// CHECK:   %9 = enum $Optional<String>, #Optional.some!enumelt.1, %8 : $String
// CHECK:   br bb3(%9 : $Optional<String>)
// CHECK: bb2:
// CHECK:   %11 = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br bb3(%11 : $Optional<String>)
// CHECK: bb3(%13 : $Optional<String>):
// CHECK:   return %13 : $Optional<String>

// CHECK-LABEL: sil shared [noinline] @_T0So5GizmoC14stringPropertySQySSGvgToTepb_ : $@convention(thin) (Gizmo) -> @owned Optional<String>
// CHECK: bb0(%0 : $Gizmo):
// CHECK:  %1 = objc_method %0 : $Gizmo, #Gizmo.stringProperty!getter.1.foreign : (Gizmo) -> () -> String!
// CHECK:  %2 = apply %1(%0) : $@convention(objc_method) (Gizmo) -> @autoreleased Optional<NSString>
// CHECK:  switch_enum %2 : $Optional<NSString>, case #Optional.some!enumelt.1: bb1, case #Optional.none!enumelt: bb2
// CHECK:bb1(%4 : $NSString):
// CHECK:  %5 = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
// CHECK:  %6 = metatype $@thin String.Type
// CHECK:  %7 = apply %5(%2, %6) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
// CHECK:  %8 = enum $Optional<String>, #Optional.some!enumelt.1, %7 : $String
// CHECK:  br bb3(%8 : $Optional<String>)
// CHECK:bb2:
// CHECK:  %10 = enum $Optional<String>, #Optional.none!enumelt
// CHECK:  br bb3(%10 : $Optional<String>)
// CHECK:bb3(%12 : $Optional<String>):
// CHECK:  return %12 : $Optional<String>

// CHECK-LABEL: sil shared [noinline] @_T0So5GizmoC14stringPropertySQySSGvsToTembnn_ : $@convention(thin) (@owned String, Gizmo) -> () {
// CHECK: bb0(%0 : $String, %1 : $Gizmo):
// CHECK:   %2 = objc_method %1 : $Gizmo, #Gizmo.stringProperty!setter.1.foreign : (Gizmo) -> (String!) -> ()
// CHECK:   %3 = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF : $@convention(method) (@guaranteed String) -> @owned NSString
// CHECK:   %4 = apply %3(%0) : $@convention(method) (@guaranteed String) -> @owned NSString
// CHECK:   release_value %0 : $String
// CHECK:   %6 = enum $Optional<NSString>, #Optional.some!enumelt.1, %4 : $NSString
// CHECK:   %7 = apply %2(%6, %1) : $@convention(objc_method) (Optional<NSString>, Gizmo) -> ()
// CHECK:   strong_release %4 : $NSString
// CHECK:   return %7 : $()

// CHECK-LABEL: sil shared [noinline] @_T0So5GizmoC12modifyStringSQySSGAD_Si10withNumberSQyypG0D6FoobartFToTembnnnb_ : $@convention(thin) (@owned String, Int, Optional<AnyObject>, Gizmo) -> @owned Optional<String> {
// CHECK: bb0(%0 : $String, %1 : $Int, %2 : $Optional<AnyObject>, %3 : $Gizmo):
// CHECK:   %4 = objc_method %3 : $Gizmo, #Gizmo.modifyString!1.foreign : (Gizmo) -> (String!, Int, Any!) -> String!
// CHECK:   %5 = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF : $@convention(method) (@guaranteed String) -> @owned NSString
// CHECK:   %6 = apply %5(%0) : $@convention(method) (@guaranteed String) -> @owned NSString
// CHECK:   release_value %0 : $String
// CHECK:   %8 = enum $Optional<NSString>, #Optional.some!enumelt.1, %6 : $NSString
// CHECK:   %9 = apply %4(%8, %1, %2, %3) : $@convention(objc_method) (Optional<NSString>, Int, Optional<AnyObject>, Gizmo) -> @autoreleased Optional<NSString>
// CHECK:   strong_release %6 : $NSString
// CHECK:   switch_enum %9 : $Optional<NSString>, case #Optional.some!enumelt.1: bb2, case #Optional.none!enumelt: bb1
//
// CHECK: bb1:
// CHECK:   %12 = enum $Optional<String>, #Optional.none!enumelt
// CHECK:   br bb3(%12 : $Optional<String>)
//
// CHECK: bb2(%14 : $NSString):
// CHECK:   %15 = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
// CHECK:   %16 = metatype $@thin String.Type
// CHECK:   %17 = apply %15(%9, %16) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
// CHECK:   %18 = enum $Optional<String>, #Optional.some!enumelt.1, %17 : $String
// CHECK:   br bb3(%18 : $Optional<String>)
//
// CHECK: bb3(%20 : $Optional<String>):
// CHECK:   return %20 : $Optional<String>

// CHECK-LABEL: sil shared [noinline] @_T0So5GizmoC11doSomethingSQyypGSQySaySSGGFToTembnn_ : $@convention(thin) (@owned Array<String>, Gizmo) -> @owned Optional<AnyObject> {
// CHECK: bb0(%0 : $Array<String>, %1 : $Gizmo):
// CHECK:   %2 = objc_method %1 : $Gizmo, #Gizmo.doSomething!1.foreign : (Gizmo) -> ([String]!) -> Any!
// CHECK:   %3 = function_ref @_T0Sa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF : $@convention(method) <{{.*}}> (@guaranteed Array<{{.*}}>) -> @owned NSArray
// CHECK:   %4 = apply %3<String>(%0) : $@convention(method) <{{.*}}> (@guaranteed Array<{{.*}}>) -> @owned NSArray
// CHECK:   release_value %0 : $Array<String>
// CHECK:   %6 = enum $Optional<NSArray>, #Optional.some!enumelt.1, %4 : $NSArray
// CHECK:   %7 = apply %2(%6, %1) : $@convention(objc_method) (Optional<NSArray>, Gizmo) -> @autoreleased Optional<AnyObject>
// CHECK:   strong_release %4 : $NSArray
// CHECK:   return %7 : $Optional<AnyObject>

public func dontCrash<T: Proto>(x : Gizmo2<T>) {
  let s = x.doSomething()
  print(s)

}

public func dontCrash2(_ c: SomeGenericClass) -> Bool {
  guard let str = c.version else {
      return false
  }
  guard let str2 = c.doSomething() else {
      return false
  }

  let arr = [ "foo", "bar"]
  c.doSomething2(arr)

  return true
}
