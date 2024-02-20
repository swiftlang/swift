// RUN: %batch-code-completion


// Use the greater access between the protocol requirement and the witness. In this test 
// case the completed declaration is 'B.foo()' which is implicitly 'internal'. 
// But as the overriding declaration, the user needs to write both 'public' and 'override'.
public protocol P { func foo() }
public class B { internal func foo() {} }
public class C: B, P {
  #^COMPLETE^#
  // COMPLETE-DAG: Decl[InstanceMethod]/Super:         public override func foo() {|}; name=foo()
  // COMPLETE-DAG: Decl[Constructor]/Super:            override init() {|}; name=init()
}
