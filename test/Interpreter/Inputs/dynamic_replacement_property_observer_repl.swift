@_private(sourceFile: "dynamic_replacement_property_observer_orig.swift") import TestDidWillSet

extension Stored {
  @_dynamicReplacement(for: i)
  var _replacement_i: Int {
    didSet {
      print("Stored.i.didSet from \(oldValue) to \(i) replacement")
    }
  }
  @_dynamicReplacement(for: y)
  var _replacement_y: Int {
    willSet {
      print("Stored.y.willSet from \(y) to \(newValue) replacement")
    }
  }

  @_dynamicReplacement(for: z)
  var _replacement_z: Int {
    willSet {
      print("Stored.z.willSet from \(z) to \(newValue) replacement")
    }
    didSet {
      print("Stored.z.didSet from \(oldValue) to \(z) replacement")
    }
  }
}

@_dynamicReplacement(for: myglobal)
public var _replacement_myglobal : Int = 1 {
  didSet {
    print("myglobal.didSet from \(oldValue) to \(myglobal) replacement")
  }
}

@_dynamicReplacement(for: myglobal2)
var _replacement_myglobal2 : Int = 1 {
  willSet {
    print("myglobal2.willSet from \(myglobal2) to \(newValue) replacement")
  }
}

@_dynamicReplacement(for: myglobal3)
var _replacement_myglobal3 : Int = 1 {
  willSet {
    print("myglobal3.willSet from \(myglobal3) to \(newValue) replacement")
  }
  didSet {
    print("myglobal3.didSet from \(oldValue) to \(myglobal3) replacement")
  }
}

extension HeapStored {
  @_dynamicReplacement(for: z)
  var _replacement_z: Int {
    willSet {
      print("HeapStored.z.willSet from \(z) to \(newValue) replacement")
    }
    didSet {
      print("HeapStored.z.didSet from \(oldValue) to \(z) replacement")
    }
  }
}
