class DummyClass {}

// Used by the weak.mm runtime tests. All that matters is that the returned
// object is an instance of a pure Swift class.
@_silgen_name("make_swift_object")
public func make_swift_object() -> AnyObject {
  return DummyClass()
}
