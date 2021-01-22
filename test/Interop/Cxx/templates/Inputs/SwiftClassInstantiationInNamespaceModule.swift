import ClassTemplateInNamespace

public func receiveShip(_ i: inout Space.Ship<Yolo>) {}

public func returnShip() -> Space.Ship<Yolo> {
  return Space.Ship<Yolo>()
}

// public func receiveShipWithEngine(_ i: inout Space.Ship<Engine.Turbojet>) {}

// public func returnShipWithEngine() -> Space.Ship<Engine.Turbojet> {
//   return Space.Ship<Engine.Turbojet>()
// }
