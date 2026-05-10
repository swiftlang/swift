import Foundation

#if REVERSE

public final class AsyncValueBlockOperation: AsyncOperation {
  public override var isAsynchronous: Bool { return false }
}

open class AsyncOperation: Operation {
  open override var isAsynchronous: Bool { return true }
}

#else

open class AsyncOperation: Operation {
  open override var isAsynchronous: Bool { return true }
}

public final class AsyncValueBlockOperation: AsyncOperation {
  public override var isAsynchronous: Bool { return false }
}

#endif
