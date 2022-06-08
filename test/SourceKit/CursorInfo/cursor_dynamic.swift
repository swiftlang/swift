protocol ProtoA {
  var protoMember: Int { get }
  static var staticProtoMember: Int { get }
  func method()
  static func staticMethod()
}

extension ProtoA {
  static var staticProtoMember: Int { get { 1 } }
  static func staticMethod() {}
}

class ClassB: ProtoA {
  var protoMember: Int { get { 1 } }
  class var classMember: Int { get { 1 } }
  let immutableMember: Int = 1
  var mutableMember: Int = 1
  func method() {}
  class func classMethod() {}
  final func finalMethod() {}
}

class ClassC: ClassB {
  static var staticProtoMember: Int { get { 1 } }
  static func staticMethod() {}
}

class ClassD: ClassC {
  override var protoMember: Int { get { 1 } }
  override class var classMember: Int { get { 1 } }

  override var mutableMember: Int {
    get {
      return super.mutableMember
    }
    set {
      super.mutableMember = newValue + 1
    }
  }

  override func method() {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck -check-prefix=CHECK-SUPER %s
    super.method()
    // CHECK-SUPER: s:14cursor_dynamic6ClassBC6methodyyF
    // CHECK-SUPER-NOT: DYNAMIC
  }

  override class func classMethod() {}
}

struct StructE: ProtoA {
  let protoMember: Int = 1
  static let staticProtoMember: Int = 1

  func method() {}

  static func staticMethod() {}
}

func directRefs() {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):14 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSBSTATICMEM %s
  _ = ClassB.staticProtoMember
  // CHECK-CLASSBSTATICMEM: s:14cursor_dynamic6ProtoAPAAE06staticC6MemberSivpZ
  // CHECK-CLASSBSTATICMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):14 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSBCLASSMEM %s
  _ = ClassB.classMember
  // CHECK-CLASSBCLASSMEM: s:14cursor_dynamic6ClassBC11classMemberSivpZ
  // CHECK-CLASSBCLASSMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSBSTATIC %s
  ClassB.staticMethod()
  // CHECK-CLASSBSTATIC: s:14cursor_dynamic6ProtoAPAAE12staticMethodyyFZ
  // CHECK-CLASSBSTATIC-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSBCLASS %s
  ClassB.classMethod()
  // CHECK-CLASSBCLASS: s:14cursor_dynamic6ClassBC11classMethodyyFZ
  // CHECK-CLASSBCLASS-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):14 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCSTATICMEM %s
  _ = ClassC.staticProtoMember
  // CHECK-CLASSCSTATICMEM: s:14cursor_dynamic6ClassCC17staticProtoMemberSivpZ
  // CHECK-CLASSCSTATICMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):14 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCCLASSMEM %s
  _ = ClassC.classMember
  // CHECK-CLASSCCLASSMEM: s:14cursor_dynamic6ClassBC11classMemberSivpZ
  // CHECK-CLASSCCLASSMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCSTATIC %s
  ClassC.staticMethod()
  // CHECK-CLASSCSTATIC: s:14cursor_dynamic6ClassCC12staticMethodyyFZ
  // CHECK-CLASSCSTATIC-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCCLASS %s
  ClassC.classMethod()
  // CHECK-CLASSCCLASS: s:14cursor_dynamic6ClassBC11classMethodyyFZ
  // CHECK-CLASSCCLASS-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):14 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDCLASSMEM %s
  _ = ClassD.classMember
  // CHECK-CLASSDCLASSMEM: s:14cursor_dynamic6ClassDC11classMemberSivpZ
  // CHECK-CLASSDCLASSMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):10 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDCLASS %s
  ClassD.classMethod()
  // CHECK-CLASSDCLASS: s:14cursor_dynamic6ClassDC11classMethodyyFZ
  // CHECK-CLASSDCLASS-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):15 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTESTATICMEM %s
  _ = StructE.staticProtoMember
  // CHECK-STRUCTESTATICMEM: s:14cursor_dynamic7StructEV17staticProtoMemberSivpZ
  // CHECK-STRUCTESTATICMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):11 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTESTATIC %s
  StructE.staticMethod()
  // CHECK-STRUCTESTATIC: s:14cursor_dynamic7StructEV12staticMethodyyFZ
  // CHECK-STRUCTESTATIC-NOT: DYNAMIC
}

func protoRefs(p: ProtoA) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-PROTOMEM %s
  _ = p.protoMember
  // CHECK-PROTOMEM: s:14cursor_dynamic6ProtoAP11protoMemberSivp
  // CHECK-PROTOMEM: DYNAMIC
  // CHECK-PROTOMEM: RECEIVERS BEGIN
  // CHECK-PROTOMEM-NEXT: s:14cursor_dynamic6ProtoAP
  // CHECK-PROTOMEM-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-PROTOMETHOD %s
  p.method()
  // CHECK-PROTOMETHOD: s:14cursor_dynamic6ProtoAP6methodyyF
  // CHECK-PROTOMETHOD: DYNAMIC
  // CHECK-PROTOMETHOD: RECEIVERS BEGIN
  // CHECK-PROTOMETHOD-NEXT: s:14cursor_dynamic6ProtoAP
  // CHECK-PROTOMETHOD-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-PROTOSTATICMEM %s
  _ = type(of: p).staticProtoMember
  // CHECK-PROTOSTATICMEM: s:14cursor_dynamic6ProtoAP06staticC6MemberSivpZ
  // CHECK-PROTOSTATICMEM: DYNAMIC
  // CHECK-PROTOSTATICMEM: RECEIVERS BEGIN
  // CHECK-PROTOSTATICMEM-NEXT: s:14cursor_dynamic6ProtoAP
  // CHECK-PROTOSTATICMEM-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-PROTOSTATIC %s
  type(of: p).staticMethod()
  // CHECK-PROTOSTATIC: s:14cursor_dynamic6ProtoAP12staticMethodyyFZ
  // CHECK-PROTOSTATIC: DYNAMIC
  // CHECK-PROTOSTATIC: RECEIVERS BEGIN
  // CHECK-PROTOSTATIC-NEXT: s:14cursor_dynamic6ProtoAP
  // CHECK-PROTOSTATIC-NEXT: RECEIVERS END
}

func protoGenericRefs<T>(p: T) where T: ProtoA {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-GENPROTOMEM %s
  _ = p.protoMember
  // CHECK-GENPROTOMEM: s:14cursor_dynamic6ProtoAP11protoMemberSivp
  // CHECK-GENPROTOMEM: DYNAMIC
  // TODO: Return receiver USRs for generics

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-GENPROTOMETHOD %s
  p.method()
  // CHECK-GENPROTOMETHOD: s:14cursor_dynamic6ProtoAP6methodyyF
  // CHECK-GENPROTOMETHOD: DYNAMIC
  // TODO: Return receiver USRs for generics

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-GENPROTOSTATICMEM %s
  _ = type(of: p).staticProtoMember
  // CHECK-GENPROTOSTATICMEM: s:14cursor_dynamic6ProtoAP06staticC6MemberSivpZ
  // CHECK-GENPROTOSTATICMEM: DYNAMIC
  // TODO: Return receiver USRs for generics

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):15 %s -- %s | %FileCheck -check-prefix=CHECK-GENPROTOTOSTATIC %s
  type(of: p).staticMethod()
  // CHECK-GENPROTOTOSTATIC: s:14cursor_dynamic6ProtoAP12staticMethodyyFZ
  // CHECK-GENPROTOTOSTATIC: DYNAMIC
  // TODO: Return receiver USRs for generics
}

func classRefs(c: ClassC, d: ClassD) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCPROTOMEM %s
  _ = c.protoMember
  // CHECK-CLASSCPROTOMEM: s:14cursor_dynamic6ClassBC11protoMemberSivp
  // CHECK-CLASSCPROTOMEM: DYNAMIC
  // CHECK-CLASSCPROTOMEM: RECEIVERS BEGIN
  // CHECK-CLASSCPROTOMEM-NEXT: s:14cursor_dynamic6ClassCC
  // CHECK-CLASSCPROTOMEM-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCIMMEMBER %s
  _ = c.immutableMember
  // CHECK-CLASSCIMMEMBER: s:14cursor_dynamic6ClassBC15immutableMemberSivp

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCMEMBERREAD %s
  _ = c.mutableMember
  // CHECK-CLASSCMEMBERREAD: s:14cursor_dynamic6ClassBC13mutableMemberSivp

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCMEMBERWRITE %s
  c.mutableMember = 1
  // CHECK-CLASSCMEMBERWRITE: s:14cursor_dynamic6ClassBC13mutableMemberSivp

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCMETHOD %s
  c.method()
  // CHECK-CLASSCMETHOD: s:14cursor_dynamic6ClassBC6methodyyF
  // CHECK-CLASSCMETHOD: DYNAMIC
  // CHECK-CLASSCMETHOD: RECEIVERS BEGIN
  // CHECK-CLASSCMETHOD-NEXT: s:14cursor_dynamic6ClassCC
  // CHECK-CLASSCMETHOD-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCTOSTATICMEM %s
  _ = type(of: c).staticProtoMember
  // CHECK-CLASSCTOSTATICMEM: s:14cursor_dynamic6ClassCC17staticProtoMemberSivpZ
  // CHECK-CLASSCTOSTATICMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCTOCLASSMEM %s
  _ = type(of: c).classMember
  // CHECK-CLASSCTOCLASSMEM: s:14cursor_dynamic6ClassBC11classMemberSivpZ
  // CHECK-CLASSCTOCLASSMEM: DYNAMIC
  // CHECK-CLASSCTOCLASSMEM: RECEIVERS BEGIN
  // CHECK-CLASSCTOCLASSMEM-NEXT: s:14cursor_dynamic6ClassCC
  // CHECK-CLASSCTOCLASSMEM-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):15 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCTOSTATIC %s
  type(of: c).staticMethod()
  // CHECK-CLASSCTOSTATIC: s:14cursor_dynamic6ClassCC12staticMethodyyFZ
  // CHECK-CLASSCTOSTATIC-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):15 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCTOCLASS %s
  type(of: c).classMethod()
  // CHECK-CLASSCTOCLASS: s:14cursor_dynamic6ClassBC11classMethodyyFZ
  // CHECK-CLASSCTOCLASS: DYNAMIC
  // CHECK-CLASSCTOCLASS: RECEIVERS BEGIN
  // CHECK-CLASSCTOCLASS-NEXT: s:14cursor_dynamic6ClassCC
  // CHECK-CLASSCTOCLASS-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSCFINAL %s
  c.finalMethod()
  // CHECK-CLASSCFINAL: s:14cursor_dynamic6ClassBC11finalMethodyyF
  // CHECK-CLASSCFINAL-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDPROTOMEM %s
  _ = d.protoMember
  // CHECK-CLASSDPROTOMEM: s:14cursor_dynamic6ClassDC11protoMemberSivp
  // CHECK-CLASSDPROTOMEM: DYNAMIC
  // CHECK-CLASSDPROTOMEM: RECEIVERS BEGIN
  // CHECK-CLASSDPROTOMEM-NEXT: s:14cursor_dynamic6ClassDC
  // CHECK-CLASSDPROTOMEM-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDMEMBERREAD %s
  _ = d.mutableMember
  // CHECK-CLASSDMEMBERREAD: s:14cursor_dynamic6ClassBC13mutableMemberSivp

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDMEMBERWRITE %s
  d.mutableMember = 1
  // CHECK-CLASSDMEMBERWRITE: s:14cursor_dynamic6ClassBC13mutableMemberSivp

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDMETHOD %s
  d.method()
  // CHECK-CLASSDMETHOD: s:14cursor_dynamic6ClassDC6methodyyF
  // CHECK-CLASSDMETHOD: DYNAMIC
  // CHECK-CLASSDMETHOD: RECEIVERS BEGIN
  // CHECK-CLASSDMETHOD-NEXT: s:14cursor_dynamic6ClassDC
  // CHECK-CLASSDMETHOD-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDTOCLASSMEM %s
  _ = type(of: d).classMember
  // CHECK-CLASSDTOCLASSMEM: s:14cursor_dynamic6ClassDC11classMemberSivpZ
  // CHECK-CLASSDTOCLASSMEM: DYNAMIC
  // CHECK-CLASSDTOCLASSMEM: RECEIVERS BEGIN
  // CHECK-CLASSDTOCLASSMEM-NEXT: s:14cursor_dynamic6ClassDC
  // CHECK-CLASSDTOCLASSMEM-NEXT: RECEIVERS END

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):15 %s -- %s | %FileCheck -check-prefix=CHECK-CLASSDTOCLASS %s
  type(of: d).classMethod()
  // CHECK-CLASSDTOCLASS: s:14cursor_dynamic6ClassDC11classMethodyyFZ
  // CHECK-CLASSDTOCLASS: DYNAMIC
  // CHECK-CLASSDTOCLASS: RECEIVERS BEGIN
  // CHECK-CLASSDTOCLASS-NEXT: s:14cursor_dynamic6ClassDC
  // CHECK-CLASSDTOCLASS-NEXT: RECEIVERS END
}

func structRefs(e: StructE) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):9 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTMEM %s
  _ = e.protoMember
  // CHECK-STRUCTMEM: s:14cursor_dynamic7StructEV11protoMemberSivp
  // CHECK-STRUCTMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTMETHOD %s
  e.method()
  // CHECK-STRUCTMETHOD: s:14cursor_dynamic7StructEV6methodyyF
  // CHECK-STRUCTMETHOD-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):19 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTTOSTATICMEM %s
  _ = type(of: e).staticProtoMember
  // CHECK-STRUCTTOSTATICMEM: s:14cursor_dynamic7StructEV17staticProtoMemberSivpZ
  // CHECK-STRUCTTOSTATICMEM-NOT: DYNAMIC

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):15 %s -- %s | %FileCheck -check-prefix=CHECK-STRUCTTOSTATIC %s
  type(of: e).staticMethod()
  // CHECK-STRUCTTOSTATIC: s:14cursor_dynamic7StructEV12staticMethodyyFZ
  // CHECK-STRUCTOTSTATIC-NOT: DYNAMIC
}
