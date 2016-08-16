// RUN: %sourcekitd-test -req=expand-placeholder %s | %FileCheck %s

foo(x: <#T##() -> Void#>)
// CHECK:      foo {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

foo(x: <#T##() -> Void#>, y: <#T##Int#>)
// CHECK:      foo(x: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, y: Int)

anArr.indexOfObjectPassingTest(<#T##predicate: ((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?##((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?#>)
// CHECK:      anArr.indexOfObjectPassingTest { (<#AnyObject!#>, <#Int#>, <#UnsafePointer<ObjCBool>#>) -> Bool in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

anArr.indexOfObjectPassingTest(<#T##predicate: ((_ obj: AnyObject!, _ idx: Int, _ stop: UnsafePointer<ObjCBool>) -> Bool)?##((_ obj: AnyObject!, _ idx: Int, _ stop: UnsafePointer<ObjCBool>) -> Bool)?#>)
// CHECK:      anArr.indexOfObjectPassingTest { (obj, idx, stop) -> Bool in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

anArr.indexOfObjectAtIndexes(<#T##s: NSIndexSet?##NSIndexSet?#>, options: <#T##NSEnumerationOptions#>, passingTest: <#T##((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?#>)
// CHECK:      anArr.indexOfObjectAtIndexes(NSIndexSet?, options: NSEnumerationOptions) { (<#AnyObject!#>, <#Int#>, <#UnsafePointer<ObjCBool>#>) -> Bool in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

if anArr.indexOfObjectPassingTest(<#T##predicate: ((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?##((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?#>) {
}
// CHECK:      if anArr.indexOfObjectPassingTest({ (<#AnyObject!#>, <#Int#>, <#UnsafePointer<ObjCBool>#>) -> Bool in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }) {
// CHECK-NEXT: }

dispatch_after(<#T##when: dispatch_time_t##dispatch_time_t#>, <#T##queue: dispatch_queue_t?##dispatch_queue_t?#>, <#T##block: dispatch_block_t?##dispatch_block_t?##() -> Void#>)
// CHECK:      dispatch_after(dispatch_time_t, dispatch_queue_t?) {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

foo(x: <#T##Self.SegueIdentifier -> Void#>)
// CHECK:      foo { (<#Self.SegueIdentifier#>) in

store.requestAccessToEntityType(<#T##entityType: EKEntityType##EKEntityType#>, completion: <#T##EKEventStoreRequestAccessCompletionHandler##EKEventStoreRequestAccessCompletionHandler##(Bool, NSError?) -> Void#>)
// CHECK:      store.requestAccessToEntityType(EKEntityType) { (<#Bool#>, <#NSError?#>) in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

func f() {
  store.requestAccessToEntityType(<#T##entityType: EKEntityType##EKEntityType#>, completion: nil)
}
// CHECK: store.requestAccessToEntityType(EKEntityType, completion: nil)

func f1() {
  bar(<#T##d: () -> ()##() -> ()#>)
}
// CHECK-NOT: bar { () -> () in

func f1() {
  bar(<#T##d: () -> ()##() -> ()#>, <#T##d: () -> ()##() -> ()#>)
}
// CHECK:   bar({
// CHECK-NEXT:	<#code#>
// CHECK-NEXT:	}, {
// CHECK-NEXT:	<#code#>
// CHECK-NEXT:	})


func f1() {
  bar(a : <#T##d: () -> ()##() -> ()#>, b : <#T##d: () -> ()##() -> ()#>)
}
// CHECK: bar(a : {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, b : {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: })


func f1() {
  bar(a : {}}, <#T##d: () -> ()##() -> ()#>)
}
// CHECK: bar(a : {}}, <#T##d: () -> ()##() -> ()#>)
