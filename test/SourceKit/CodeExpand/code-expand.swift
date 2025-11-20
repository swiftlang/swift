// RUN: %sourcekitd-test -req=expand-placeholder %s | %FileCheck %s

foo(x: <#T##() -> Void#>)
// CHECK:      foo {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

foo(x: <#T##() -> Void#>, y: <#T##Int#>)
// CHECK:      foo(x: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, y: Int)

try foo(x: <#T##() -> Void#>)
// CHECK:      try foo {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

await foo(x: <#T##() -> Void#>)
// CHECK:      await foo {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

!foo(x: <#T##() -> Void#>)
// CHECK:      !foo {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

foo(bar(<#T##() -> Void#>))
// CHECK:      foo(bar({
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }))

anArr.indexOfObjectPassingTest(<#T##predicate: ((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?##((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?#>)
// CHECK:      anArr.indexOfObjectPassingTest { <#AnyObject!#>, <#Int#>, <#UnsafePointer<ObjCBool>#> in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

anArr.indexOfObjectPassingTest(<#T##predicate: ((_ obj: AnyObject!, _ idx: Int, _ stop: UnsafePointer<ObjCBool>) -> Bool)?##((_ obj: AnyObject!, _ idx: Int, _ stop: UnsafePointer<ObjCBool>) -> Bool)?#>)
// CHECK:      anArr.indexOfObjectPassingTest { obj, idx, stop in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

anArr.indexOfObjectAtIndexes(<#T##s: NSIndexSet?##NSIndexSet?#>, options: <#T##NSEnumerationOptions#>, passingTest: <#T##((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?#>)
// CHECK:      anArr.indexOfObjectAtIndexes(NSIndexSet?, options: NSEnumerationOptions) { <#AnyObject!#>, <#Int#>, <#UnsafePointer<ObjCBool>#> in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

if anArr.indexOfObjectPassingTest(<#T##predicate: ((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?##((AnyObject!, Int, UnsafePointer<ObjCBool>) -> Bool)?#>) {
}
// CHECK:      if anArr.indexOfObjectPassingTest({ <#AnyObject!#>, <#Int#>, <#UnsafePointer<ObjCBool>#> in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }) {
// CHECK-NEXT: }

dispatch_after(<#T##when: dispatch_time_t##dispatch_time_t#>, <#T##queue: dispatch_queue_t?##dispatch_queue_t?#>, <#T##block: dispatch_block_t?##dispatch_block_t?##() -> Void#>)
// CHECK:      dispatch_after(dispatch_time_t, dispatch_queue_t?) {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

@resultBuilder
struct MyBuilder {}
func acceptBuilder<Result>(@MyBuilder body: () -> Result) {}
do {
  acceptBuilder(body: <#T##() -> Result#>)
  // CHECK: acceptBuilder {
  // CHECK-NEXT: <#code#>
  // CHECK-NEXT: }
}

foo(x: <#T##Self.SegueIdentifier -> Void#>)
// CHECK:      foo { <#Self.SegueIdentifier#> in

store.requestAccessToEntityType(<#T##entityType: EKEntityType##EKEntityType#>, completion: <#T##EKEventStoreRequestAccessCompletionHandler##EKEventStoreRequestAccessCompletionHandler##(Bool, NSError?) -> Void#>)
// CHECK:      store.requestAccessToEntityType(EKEntityType) { <#Bool#>, <#NSError?#> in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

func f() {
  store.requestAccessToEntityType(<#T##entityType: EKEntityType##EKEntityType#>, completion: nil)
}
// CHECK: store.requestAccessToEntityType(EKEntityType, completion: nil)

func f1() {
  bar(<#T##d: () -> ()##() -> ()#>)
}
// CHECK:      bar {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

func f1() {
  bar(<#T##__skip__: () -> ()##() -> ()#>, <#T##d: () -> ()##() -> ()#>)
}
// CHECK:   bar {
// CHECK-NEXT:	<#code#>
// CHECK-NEXT:	} _: {
// CHECK-NEXT:  <#code#>
// CHECK-NEXT:  }

func f1() {
  bar(<#T##d: () -> ()##() -> ()#>, <#T##d: () -> ()##() -> ()#>)
}
// CHECK:   bar {
// CHECK-NEXT:  <#code#>
// CHECK-NEXT:  } _: {
// CHECK-NEXT:  <#code#>
// CHECK-NEXT:  }

func f1() {
  bar(a : <#T##__skip__: () -> ()##() -> ()#>, b : <#T##d: () -> ()##() -> ()#>)
}
// CHECK: bar {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: } b: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

func f1() {
  bar(a : <#T##d: () -> ()##() -> ()#>, b : <#T##d: () -> ()##() -> ()#>)
}
// CHECK: bar {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: } b: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

func f1() {
  bar(a : {}}, <#T##d: () -> ()##() -> ()#>)
}
// CHECK: bar(a : {}}, <#T##d: () -> ()##() -> ()#>)

foo(withDuration: 1, animations: <#T##() -> Void#>)

if true {
  withtrail(<#T##() -> ()#>)
// CHECK:   withtrail {
// CHECK-NEXT: <#code#>
}
}

foo(.foo(<#T##block: () -> Void##() -> Void#>))
// CHECK: foo(.foo({

braced1(x: {<#T##() -> Void#>})
// CHECK:   braced1 {
// CHECK-NEXT:  <#code#>
// CHECK-NEXT:  }

braced2(x: {<#T##() -> Void#>}, y: Int)
// CHECK:   braced2(x: {
// CHECK-NEXT:  <#code#>
// CHECK-NEXT:  }, y: Int)

braced3({
  #if true
  <#T##() -> Int#>
  #endif
})
// CHECK:      braced3 {
// CHECK-NEXT:   <#code#>
// CHECK-NEXT: }

func returnTrailing() -> Int {
  return withtrail(<#T##() -> ()#>)
// CHECK: return withtrail {
// CHECK-NEXT: <#code#>
}

var yieldTrailing: Int {
  _read {
    yield withtrail(<#T##() -> ()#>)
  // CHECK: yield withtrail {
  // CHECK-NEXT: <#code#>
  }
}

func caseTrailing() -> Int {
  switch true {
    case true: withtrail(<#T##() -> ()#>)
// CHECK: case true: withtrail {
// CHECK-NEXT: <#code#>
    default: withtrail(<#T##() -> ()#>)
// CHECK: default: withtrail {
// CHECK-NEXT: <#code#>
  }
}

func throwTrailing() -> Int {
   throw withtrail(<#T##() -> ()#>)
// CHECK: throw withtrail {
// CHECK-NEXT: <#code#>
}

func singleExprTrailing1() -> Int {
  withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
}
var singleExprTrailing2: Int {
  withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
}
var singleExprTrailing3: Int {
  get {
    withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
  }
}

closureTrailingMulti {
  bah()
  withtrail(<#T##() -> ()#>)
// CHECK: bah()
// CHECK-NEXT: withtrail {
// CHECK-NEXT: <#code#>
}

closureIf {
  if withtrail(<#T##() -> ()#>) {}
// CHECK: if withtrail({
// CHECK-NEXT: <#code#>
}

closureNonTrail {
  nonTrail(<#T##() -> ()#>, 1)
// CHECK: nonTrail({
// CHECK-NEXT: <#code#>
}

singleExprClosureTrailing {
  withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
}

singleExprClosureTrailingParens({
  withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
})

singleExprClosureMultiArg(1) {
  withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
}
singleExprClosureMultiArg(1) {
  withtrail(<#T##() -> ()#>)
// CHECK: withtrail {
// CHECK-NEXT: <#code#>
}

func active() {
  foo(<#T##value: Foo##Foo#>)
  // CHECK: foo(Foo)
}
func activeWithTrailing() {
  forEach(<#T##() -> ()#>)
  // CHECK: forEach {
  // CHECK-NEXT: <#code#>
}
#if false
func inactive() {
  foo(<#T##value: Foo##Foo#>)
  // CHECK: foo(Foo)
}
func inactiveWithTrailing() {
  forEach(<#T##() -> ()#>)
  // CHECK: forEach {
  // CHECK-NEXT: <#code#>
}
#endif

expandClosureWithInternalParameterNames {
  withtrail(<#T##callback: (Int, Int) -> Bool##(_ a: Int, _ b: Int) -> Bool#>)
// CHECK: withtrail { a, b in
// CHECK-NEXT: <#code#>
}

// CHECK-LABEL: func expandMacro()
func expandMacro() {
  #foo(<#T##() -> Int#>)
  // CHECK:      #foo {
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: }

  #foo(bar: <#T##() -> ()#>)
  // CHECK:      #foo {
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: }

  #foo(bar: <#T##() -> Int#>, baz: <#T##() -> ()#>)
  // CHECK:      #foo {
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: } baz: {
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: }
}

// CHECK-LABEL: struct ExpandDeclMacro
struct ExpandDeclMacro {
  #foo(<#T##() -> ()#>)
  // CHECK:      #foo {
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: }

  #foo(bar(<#T##() -> ()#>))
  // CHECK:      #foo(bar({
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: }))

  #foo(#bar(<#T##() -> ()#>))
  // CHECK:      #foo(#bar({
  // CHECK-NEXT:   <#code#>
  // CHECK-NEXT: }))
}

@Foo(<#Int#>)
func testDeclAttr1() {}
// CHECK: @Foo(<#Int#>)
// CHECK-NEXT: func testDeclAttr1() {}

@Foo(<#T##() -> ()#>)
func testDeclAttr2() {}
// CHECK:      @Foo({
// CHECK-NEXT:   <#code#>
// CHECK-NEXT: })
// CHECK-NEXT: func testDeclAttr2() {}

func testTypeAttr1(x: @Foo(<#Int#>) String) {}
// CHECK: func testTypeAttr1(x: @Foo(<#Int#>) String) {}

func testTypeAttr2(x: @Foo(<#T##() -> ()#>) Int) {}
// CHECK: func testTypeAttr2(x: @Foo({
// CHECK-NEXT:   <#code#>
// CHECK-NEXT: }) Int) {}
