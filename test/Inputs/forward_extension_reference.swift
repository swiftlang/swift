extension Foo : Bar {
  var count: Int {
    get {
      var x = Int(_countAndFlags >> 1)
      var y = Int(_countAndFlags >> 1)
      var z = _countAndFlags >> 1
      return x
    }
    set {
      let growth = newValue - count
      if growth == 0 {
        return
      }
      _countAndFlags = (UInt(newValue) << 1) | (_countAndFlags & 1)
    }
  }
}
