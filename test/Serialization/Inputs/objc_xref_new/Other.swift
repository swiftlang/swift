import ObjCModule

public class Sub: Base {
  public override func testBaseNameChanges() {}
  public override func testArgumentNameChanges(_: Any, next _: Any) {}
  public override func testNullabilityArgChanges(_: Any) {}
  public override func testNullabilityReturnChanges() -> Any { return () }
}
