import COM

// A @com protocol with a user-written protocol metatype extension.  The
// extension and its members must round-trip through serialization: the client
// recovers that this is a metatype extension from the (deserialized) extended
// type, since the flag is no longer stored.
@com(interface: "30000000-0000-0000-0000-000000000003")
public protocol IWidget: IUnknown { }

public extension IWidget.Protocol {
  var tag: Int { 7 }
  func describe() -> String { "widget" }
}
