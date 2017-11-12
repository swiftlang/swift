public class BeforeAndAfterOther {
  @available(swift, obsoleted: 4.0)
  public init(foo: ()) {}

  @available(swift 4.0)
  public init?(foo: ()) {}

  @available(swift, obsoleted: 4.0)
  public init() {}

  @available(swift 4.0)
  public init() throws {}

  @available(swift, obsoleted: 4.0)
  public static func foo() {}

  @available(swift 4.0)
  public static func foo() throws {}

  @available(swift 4.0)
  public var computed: Int16 { get { return 0 } set { } }

  @available(swift, obsoleted: 4.0)
  public var computed: Int8 { get { return 0 } set { } }

  @available(swift 4.0)
  public static var computed: Int16 { get { return 0 } set { } }

  @available(swift, obsoleted: 4.0)
  public static var computed: Int8 { get { return 0 } set { } }
}
