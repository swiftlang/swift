@_exported import LowLevel

public func printMessage() {
  printMessageMoved()
}
public class CandyBox: Box {
  public typealias Item = Candy
  public var ItemKind: String { return getItem().kind }
  let itemInBox: Item
  public init(_ itemInBox: Item) { self.itemInBox = itemInBox }
  public func getItem() -> Item { return itemInBox }
}
