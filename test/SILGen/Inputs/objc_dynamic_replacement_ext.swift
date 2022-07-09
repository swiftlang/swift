import Foundation

public class Generic<ItemType>: NSObject {
  @objc public dynamic func foo() {}

  @objc public dynamic var x: Int {
    get {
      return 0;
    }
    set {
      print("noop")
    }
  }

  @objc public dynamic var y: Int = 0
}

@objc
public protocol MyProto {
  func doIt()
}

public final class MyGeneric<Item>: NSObject, MyProto {
  public dynamic func doIt() {
  }
}
