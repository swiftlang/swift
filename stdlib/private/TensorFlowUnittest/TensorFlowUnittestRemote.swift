#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif
import CTensorFlow
import CTensorFlowUnittest
import TensorFlow
import StdlibUnittest

typealias CTFServer = OpaquePointer

@usableFromInline
func debugLog(_ message: @autoclosure () -> String,
              file: StaticString = #file,
              line: UInt = #line) {
  if _RuntimeConfig.printsDebugLog {
    print("[\(file):\(line)] \(message())")
    // This helps dump more log before a crash.
    fflush(stdout)
  }
}


private class TestCluster {
  var num_tasks:Int = 0
  var servers: [CTFServer] = []
  public var serverDef:String = ""

  public init(num_tasks: Int) {
    self.num_tasks = num_tasks
    self.serverDef = startServers()
  }

  deinit {
    // TODO: There is no clean way to shutdown GrpcServers yet.
  }

  private func getServerDef(tasks: String, task_index: Int) -> String {
    return  """
      cluster {
        job {
          name: "localhost"
          \(tasks)
        }
      }
      job_name: "localhost"
      task_index: \(task_index)
      protocol: "grpc"
      """
  }

  private func getTasks() -> String {
    var tasks = ""
    for i in 0..<num_tasks {
      let port = TF_PickUnusedPortOrDie()
      let task = """
          tasks {
            key: \(i)
            value: "localhost:\(port)"
          }
      """
      debugLog("Picking port \(port) for \(i)...");
      tasks += task
    }
    return tasks;
  }

  private func startServers() -> String {
    let tasks = getTasks()
    let status = TF_NewStatus()
    // Start everything except the 0-th task, which will
    // be started when we set TFE_ContextSetServerDef.
    for i in 1..<num_tasks {
      debugLog("Starting task \(i)...");
      let serverDefText = getServerDef(tasks: tasks, task_index: i)
      let serverDef = TFE_GetServerDef(serverDefText, status)
      checkOk(status)
      let server = TF_NewServer(
          serverDef!.pointee.data, serverDef!.pointee.length, status)
      checkOk(status)
      TF_ServerStart(server, status)
      checkOk(status)
      servers.append(server!)
      let target = TF_ServerTarget(server!)
      debugLog("Started task \(i) at \(String(cString: target!))")
    }
    TF_DeleteStatus(status)
    // Return the server def for task 0 for use in compiler runtime.
    return getServerDef(tasks: tasks, task_index: 0)
  }
}

public func runAllTestsWithRemoteSession(num_tasks: Int = 2) {
  // This only works if we use the TFEager API.
  _RuntimeConfig.usesTFEagerAPI = true

  // In the remote test mode, TF level debug log can be printed by changing the
  // params below, instead of changing _RuntimeConfig.printsDebugLog and
  // _RuntimeConfig.tensorflowVerboseLogLevel on a per-test basis. This is
  // because InitTensorFlowRuntime() sets up such config, and it must be called
  // before TF_StartTensorFlowProcessCluster().
  InitTensorFlowRuntime(/* enable_debug_logging */ 1, /* verbose_level */ 0)
  _RuntimeConfig.tensorFlowRuntimeInitialized = true

  let testCluster = TestCluster(num_tasks: num_tasks)
  _RuntimeConfig.session = .remote(grpcAddress: testCluster.serverDef)
  runAllTests()
}
