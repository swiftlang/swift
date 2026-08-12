import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget: IUnknown { }

@com(implementation: "20000000-0000-0000-0000-000000000002")
public class Widget: IWidget { }
