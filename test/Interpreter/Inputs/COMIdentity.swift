import COMIdentity

@com(interface: "10000000-0000-0000-0000-000000000006")
public protocol ISource: AnyObject {}

public protocol NativeMarker: AnyObject {}
public class NativeBase {}
public final class NativeObject: NativeBase, NativeMarker {
  public static var destructions = 0
  deinit { NativeObject.destructions += 1 }
}
public final class Unrelated {}

func destroyNativeObject(_ pointer: UnsafeMutableRawPointer?) {
  Unmanaged<NativeBase>.fromOpaque(pointer!).release()
}

@inline(never)
func makeIdentity(_ object: NativeBase, supportsIdentity: Bool = true) -> any ISource {
  let metadata = unsafeBitCast(Swift.type(of: object), to: UnsafeRawPointer.self)
  let owner = COMIdentityObject_Create(Unmanaged.passRetained(object).toOpaque(),
      metadata, destroyNativeObject, supportsIdentity ? 1 : 0)!
  let source = COMIdentityObject_GetStorage(owner)!.load(as: (any ISource).self)
  COMIdentityObject_Release(owner)
  return source
}

func checkIdentityDestruction(_ count: Int) {
  precondition(COMIdentityObject_GetReferences() == 0)
  precondition(COMIdentityObject_GetDestructions() == 1)
  precondition(NativeObject.destructions == count)
}
