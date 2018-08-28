// REQUIRES: objc_interop

import Foundation

@objc enum Designated: Int { case X }
@objc enum RequiredDesignated: Int { case X }
@objc enum ObjCDesignated: Int { case X }
@objc enum DynamicDesignated: Int { case X }

@objc enum Convenience: Int { case X }
@objc enum DoubleConvenience: Int { case X }
@objc enum RequiredConvenience: Int { case X }
@objc enum RequiredDoubleConvenience: Int { case X }
@objc enum ObjCConvenience: Int { case X }
@objc enum ObjCDoubleConvenience: Int { case X }
@objc enum DynamicConvenience: Int { case X }
@objc enum DynamicDoubleConvenience: Int { case X }

class C: NSObject {
  init(_: Designated) {}
  required init(_: RequiredDesignated) {}
  @objc(initObjCDesignated:) init(_: ObjCDesignated) {}
  @objc(initDynamicDesignated:) dynamic init(_: DynamicDesignated) {}

  convenience init(_: Convenience) {
    self.init(Designated.X)
  }
  convenience init(_: DoubleConvenience) {
    self.init(Convenience.X)
  }
  required convenience init(_: RequiredConvenience) {
    self.init(RequiredDesignated.X)
  }
  required convenience init(_: RequiredDoubleConvenience) {
    self.init(RequiredConvenience.X)
  }
  @objc(initObjCConvenience:) convenience init(_: ObjCConvenience) {
    self.init(ObjCDesignated.X)
  }
  @objc(initObjCDoubleConvenience:) convenience init(_: ObjCDoubleConvenience) {
    self.init(ObjCDesignated.X)
  }
  @objc(initDynamicConvenience:) dynamic convenience init(_: DynamicConvenience) {
    self.init(DynamicDesignated.X)
  }
  @objc(initDynamicDoubleConvenience:) dynamic convenience init(_: DynamicDoubleConvenience) {
    self.init(DynamicConvenience.X)
  }
}

class D: C {
  override init(_: Designated) { super.init(Designated.X) }
  required init(_: RequiredDesignated) { super.init(RequiredDesignated.X) }
  override init(_: ObjCDesignated) { super.init(ObjCDesignated.X) }
  override init(_: DynamicDesignated) { super.init(DynamicDesignated.X) }
}

func foo(ct: C.Type, dt: D.Type) {
  _ = C(Convenience.X)
  _ = C(RequiredConvenience.X)
  _ = D(Convenience.X)
  _ = D(RequiredConvenience.X)
  _ = ct.init(RequiredConvenience.X)
  _ = dt.init(RequiredConvenience.X)
}
