import Foundation
import ObjCStuff

open class ButtHolder {
  public final var x: Int
  public final var y: [OJCCloud.Butt: String]
  @NSManaged public var magic: [OJCCloud.Butt: String]
  public final var z: String

  open func virtual() {}

  public init() { fatalError() }
}
