protocol ProtoA {
  func method()
  static func staticMethod()
}

class ClassB: ProtoA {
  let immutableMember: Int = 1
  var mutableMember: Int = 1
  func method() {}
  static func staticMethod() {}
  class func classMethod() {}
  final func finalMethod() {}
}

class ClassC: ClassB {}

class ClassD: ClassC {
  override var mutableMember: Int {
    get {
      return super.mutableMember
    }
    set {
      super.mutableMember = newValue + 1
    }
  }

  override func method() {
    super.method()
  }

  override class func classMethod() {}
}

struct StructE: ProtoA {
  func method() {}

  static func staticMethod() {}
}

func direct() {
  ClassB.staticMethod()
  ClassB.classMethod()
  ClassD.classMethod()
  StructE.staticMethod()
}

func protoCalls(p: ProtoA) {
  p.method()
  type(of: p).staticMethod()
}

func classCalls(c: ClassC) {
  _ = c.immutableMember
  _ = c.mutableMember
  c.mutableMember = 1
  c.method()
  type(of: c).staticMethod()
  type(of: c).classMethod()
  c.finalMethod()
}

func structCalls(e: StructE) {
  e.method()
  type(of: e).staticMethod()
}

// RUN: %sourcekitd-test -req=cursor -pos=28:11 %s -- %s | %FileCheck -check-prefix=CHECK-SUPER %s
// CHECK-SUPER: s:14cursor_dynamic6ClassBC6methodyyF
// CHECK-SUPER-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=41:10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSSTATIC %s
// CHECK-CLASSSTATIC: s:14cursor_dynamic6ClassBC12staticMethodyyFZ
// CHECK-CLASSSTATIC-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=42:10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCLASS %s
// CHECK-CLASSCLASS: s:14cursor_dynamic6ClassBC11classMethodyyFZ
// CHECK-CLASSCLASS-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=43:10 %s -- %s | %FileCheck -check-prefix=CHECK-SUBCLASSCLASS %s
// CHECK-SUBCLASSCLASS: s:14cursor_dynamic6ClassDC11classMethodyyFZ
// CHECK-SUBCLASSCLASS-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=44:11 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTSTATIC %s
// CHECK-STRUCTSTATIC: s:14cursor_dynamic7StructEV12staticMethodyyFZ
// CHECK-STRUCTSTATIC-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=48:5 %s -- %s | %FileCheck -check-prefix=CHECK-PROTOMETHOD %s
// CHECK-PROTOMETHOD: s:14cursor_dynamic6ProtoAP6methodyyF
// CHECK-PROTOMETHOD: DYNAMIC
// CHECK-PROTOMETHOD: RECEIVERS BEGIN
// CHECK-PROTOMETHOD-NEXT: s:14cursor_dynamic6ProtoAP
// CHECK-PROTOMETHOD-NEXT: RECEIVERS END

// RUN: %sourcekitd-test -req=cursor -pos=49:15 %s -- %s | %FileCheck -check-prefix=CHECK-PROTOTOSTATIC %s
// CHECK-PROTOTOSTATIC: s:14cursor_dynamic6ProtoAP12staticMethodyyFZ
// CHECK-PROTOTOSTATIC: DYNAMIC
// CHECK-PROTOSTATIC: RECEIVERS BEGIN
// CHECK-PROTOSTATIC-NEXT: s:14cursor_dynamic6ProtoAP
// CHECK-PROTOSTATIC-NEXT: RECEIVERS END

// RUN: %sourcekitd-test -req=cursor -pos=53:9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSIMMEMBER %s
// CHECK-CLASSIMMEMBER: s:14cursor_dynamic6ClassBC15immutableMemberSivp
// rdar://75645572 should include getter without dynamic (or maybe we just skip returning in that case
// since it would only be used to find overrides)

// RUN: %sourcekitd-test -req=cursor -pos=54:9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSMEMBERREAD %s
// CHECK-CLASSMEMBERREAD: s:14cursor_dynamic6ClassBC13mutableMemberSivp
// rdar://75645572 should include getter with dynamic

// RUN: %sourcekitd-test -req=cursor -pos=55:5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSMEMBERWRITE %s
// CHECK-CLASSMEMBERWRITE: s:14cursor_dynamic6ClassBC13mutableMemberSivp
// rdar://75645572 should include setter with dynamic

// RUN: %sourcekitd-test -req=cursor -pos=56:5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSMETHOD %s
// CHECK-CLASSMETHOD: s:14cursor_dynamic6ClassBC6methodyyF
// CHECK-CLASSMETHOD: DYNAMIC
// CHECK-CLASSMETHOD: RECEIVERS BEGIN
// CHECK-CLASSMETHOD-NEXT: s:14cursor_dynamic6ClassCC
// CHECK-CLASSMETHOD-NEXT: RECEIVERS END

// RUN: %sourcekitd-test -req=cursor -pos=57:15 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSTOSTATIC %s
// CHECK-CLASSTOSTATIC: s:14cursor_dynamic6ClassBC12staticMethodyyFZ
// CHECK-CLASSTOSTATIC-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=58:15 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSTOCLASS %s
// CHECK-CLASSTOCLASS: s:14cursor_dynamic6ClassBC11classMethodyyFZ
// CHECK-CLASSTOCLASS: DYNAMIC
// CHECK-CLASSTOCLASS: RECEIVERS BEGIN
// CHECK-CLASSTOCLASS-NEXT: s:14cursor_dynamic6ClassCC
// CHECK-CLASSTOCLASS-NEXT: RECEIVERS END

// RUN: %sourcekitd-test -req=cursor -pos=59:5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSFINAL %s
// CHECK-CLASSFINAL: s:14cursor_dynamic6ClassBC11finalMethodyyF
// CHECK-CLASSFINAL-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=63:5 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTMETHOD %s
// CHECK-STRUCTMETHOD: s:14cursor_dynamic7StructEV6methodyyF
// CHECK-STRUCTMETHOD-NOT: DYNAMIC

// RUN: %sourcekitd-test -req=cursor -pos=64:15 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTTOSTATIC %s
// CHECK-STRUCTTOSTATIC: s:14cursor_dynamic7StructEV12staticMethodyyFZ
// CHECK-STRUCTOTSTATIC-NOT: DYNAMIC
