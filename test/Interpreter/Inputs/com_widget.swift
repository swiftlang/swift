import COM

@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
public protocol IWidget: IUnknown {}

@com(implementation: "01020304-0506-0708-090a-0b0c0d0e0f10")
public class CWidget: IWidget {}
