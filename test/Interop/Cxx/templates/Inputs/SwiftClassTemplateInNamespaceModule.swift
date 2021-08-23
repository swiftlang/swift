import ClassTemplateInNamespaceForSwiftModule

public func receiveShip(_ i: inout Space.Ship<Bool>) {}

public func returnShip() -> Space.Ship<Bool> {
  return Space.Ship<Bool>()
}

public func receiveShipWithEngine(_ i: inout Space.Ship<Engine.Turbojet>) {}

public func returnShipWithEngine() -> Space.Ship<Engine.Turbojet> {
  return Space.Ship<Engine.Turbojet>()
}
