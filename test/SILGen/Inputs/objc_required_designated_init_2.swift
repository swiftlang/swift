import Foundation

open class Boom: NSObject {
  public override required init() {
    super.init()
  }
}

open class Badaboom<U>: NSBoom<NSString> {
  public override required init() {
    super.init()
  }
}

