// RUN: %target-swift-frontend -emit-ir %s

public class C {
  private var _count: Int

  var count: Int {
    @storageRestrictions(initializes: _count)
    init {
      print(Self.self)  // crash here
      _count = newValue
    }
    get { _count }
    set { }
  }

  init() {
    count = 0
  }
}

let c = C()
