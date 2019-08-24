import Foundation
import ObjCStuff

open class ButtHolder {
  public final var x: Int
  public final var y: [OJCCloud.Butt: String]
  public final var z: String

  open func virtual() {
    print("\(x) \(y) \(z)")
  }

  public init() {  x = 0; y = [:]; z = "" }
}
