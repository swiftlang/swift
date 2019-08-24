struct DefaultInitializable {
  var a: Int?
  var b: Int = 3
  var c: Int?, d: Int?
}

class DefaultInitializableClass {
  var a: Int?
  @NSManaged var b: Int
}
