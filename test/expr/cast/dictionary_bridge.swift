// RUN: %swift -parse %s -verify

class Root : Hashable { 
  var hashValue: Int {
    return 0
  }
}

func ==(x: Root, y: Root) -> Bool { return true }

class ObjC : Root {
  var x = 0
}

class DerivesObjC : ObjC { }

struct BridgedToObjC : Hashable, _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return ObjC.self
  }
  func bridgeToObjectiveC() -> ObjC {
    return ObjC()
  }
  static func bridgeFromObjectiveC(x: ObjC) -> BridgedToObjC? {
    return nil
  }

  var hashValue: Int {
    return 0
  }
}

func ==(x: BridgedToObjC, y: BridgedToObjC) -> Bool { return true }

func testUpcast() {
  var dictRR = Dictionary<Root, Root>()
  var dictRO = Dictionary<Root, ObjC>()
  var dictOR = Dictionary<ObjC, Root>()
  var dictOO = Dictionary<ObjC, ObjC>()
  var dictOD = Dictionary<ObjC, DerivesObjC>()
  var dictDO = Dictionary<DerivesObjC, ObjC>()
  var dictDD = Dictionary<DerivesObjC, DerivesObjC>()

  dictRR = dictRO
  dictRR = dictOR
  dictRR = dictOO
  dictRR = dictOD
  dictRR = dictDO
  dictRR = dictDD

  dictRO = dictOO
  dictRO = dictOD
  dictRO = dictDO
  dictRO = dictDD

  dictOR = dictOO
  dictOR = dictOD
  dictOR = dictDO
  dictOR = dictDD

  dictOO = dictOD
  dictOO = dictDO
  dictOO = dictDD

  dictOD = dictDD

  dictDO = dictDD

  dictDO = dictDD // error
  dictOD = dictDD // error
}

func testUpcastBridged() {
}


