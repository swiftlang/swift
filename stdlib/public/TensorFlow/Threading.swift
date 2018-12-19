#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif

// A thread that runs the provided body.
class Thread {
  var thread: pthread_t
  init(perform body: @escaping () -> ()) {
    class ThreadArg {
      var body : () -> ()
      init(body : @escaping () -> ()) {
        self.body = body
      }
    }
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    typealias ThreadBody = @convention(c)
      (UnsafeMutableRawPointer) -> UnsafeMutableRawPointer?
#else
    typealias ThreadBody = @convention(c)
      (UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer?
#endif
    let threadBody: ThreadBody = { arg in
      // Set the cancelability of the detached thread.
      pthread_setcanceltype(Int32(PTHREAD_CANCEL_DEFERRED), nil)
      // Execute the tensor computation.
#if !(os(macOS) || os(iOS) || os(watchOS) || os(tvOS))
      let arg = arg!
#endif
      let param: ThreadArg = Unmanaged.fromOpaque(arg).takeRetainedValue()
      param.body()
      return nil
    }
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    var newThread: pthread_t!
#else
    var newThread = pthread_t()
#endif
    let creationStatus = pthread_create(
      &newThread, nil, threadBody,
      Unmanaged.passRetained(ThreadArg(body: body)).toOpaque()
    )
    internalConsistencyCheck(creationStatus == 0)
    self.thread = newThread
  }
  func join() {
    internalConsistencyCheck(pthread_join(thread, nil) == 0)
  }
}

public func _runOnNDevices(_ n: Int, perform body: @escaping (Int) -> ()) {
  var threads = [] as [Thread]
  for i in 0..<n {
    threads.append(Thread {
      body(i)
    })
  }
  for t in threads {
    t.join()
  }
}
