struct Stored {
  var i: Int {
    didSet {
      print("Stored.i.didSet from \(oldValue) to \(i) original")
    }
  }

  var y: Int {
    willSet {
      print("Stored.y.willSet from \(y) to \(newValue) original")
    }
  }

  var z: Int {
    willSet {
      print("Stored.z.willSet from \(z) to \(newValue) original")
    }
    didSet {
      print("Stored.z.didSet from \(oldValue) to \(z) original")
    }
  }
}

var myglobal : Int = 1 {
  didSet {
    print("myglobal.didSet from \(oldValue) to \(myglobal) original")
  }
}

var myglobal2 : Int = 1 {
  willSet {
    print("myglobal2.willSet from \(myglobal2) to \(newValue) original")
  }
}

var myglobal3 : Int = 1 {
  willSet {
    print("myglobal3.willSet from \(myglobal3) to \(newValue) original")
  }
  didSet {
    print("myglobal3.didSet from \(oldValue) to \(myglobal3) original")
  }
}

class HeapStored {
  var z: Int = 5{
    willSet {
      print("HeapStored.z.willSet from \(z) to \(newValue) original")
    }
    didSet {
      print("HeapStored.z.didSet from \(oldValue) to \(z) original")
    }
  }
}
