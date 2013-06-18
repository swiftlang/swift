// This function should be opaque to the optimizer.
func [asmname="swift_keepAlive"] swift_keepAlive(objPtr : Builtin.ObjectPointer)

/// \brief An instance of this struct keeps the references registered with it
/// at +1 reference count until the call to \c release().
///
/// It is absolutely necessary to call \c release().  Forgetting to call
/// \c release() will not cause a memory leak.  Instead, the managed objects will be
/// released earlier than expected.
///
/// This class can be used to extend lifetime of objects to pass UnsafePointers
/// to them to C APIs.
class LifetimeManager {
  var _managedRefs : Vector<Builtin.ObjectPointer>
  var _releaseCalled : Bool

  constructor() {
    _managedRefs = Vector<Builtin.ObjectPointer>()
    _releaseCalled = false
  }

  destructor {
    alwaysTrap(_releaseCalled, "release() should have been called")
  }

  func put(objPtr : Builtin.ObjectPointer) {
    _managedRefs.append(objPtr)
  }

  func put<T>(obj : T) {
    put(Builtin.castToObjectPointer(obj))
  }

  /// \brief Call this function to end the forced lifetime extension.
  func release() {
    swift_keepAlive(Builtin.castToObjectPointer(_managedRefs))
    _releaseCalled = true
  }
}

