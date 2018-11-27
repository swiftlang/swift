#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif
import CTensorFlow
import TensorFlow
import StdlibUnittest

typealias CTFServer = OpaquePointer

private class TestCluster {
  var taskCount: Int = 0
  var servers: [CTFServer] = []
  var serverDef: String = ""

  init(taskCount: Int) {
    self.taskCount = taskCount
    self.serverDef = startServers()
  }

  deinit {
    // TODO: There is no clean way to shutdown GrpcServers yet.
  }
  
  func debugLog(_ message: @autoclosure () -> String,
                file: StaticString = #file,
                line: UInt = #line) {
    if _RuntimeConfig.printsDebugLog {
      print("[\(file):\(line)] \(message())")
      // This helps dump more log before a crash.
      fflush(stdout)
    }
  }

  /// Create and return a text representation of a ServerDef proto.
  /// 
  /// - Parameters:
  ///   - forTasksConfig: Text representation of tasks config.
  ///   - withTaskIndex: The task index for this server def.
  ///
  func createServerDef(forTasksConfig: String, withTaskIndex: Int) -> String {
    return  """
      cluster {
        job {
          name: "localhost"
          \(forTasksConfig)
        }
      }
      job_name: "localhost"
      task_index: \(withTaskIndex)
      protocol: "grpc"
      """
  }

  /// Create and return a text representation of the tasks config.
  func createTasksConfig() -> String {
    var tasks = ""
    for i in 0..<taskCount {
      let port = TF_PickUnusedPortOrDie()
      let task = """
          tasks {
            key: \(i)
            value: "localhost:\(port)"
          }
      """
      debugLog("Picking port \(port) for \(i)...")
      tasks += task
    }
    return tasks;
  }

  /// Start GRPC servers for the testing with a remote session.
  func startServers() -> String {
    let tasks = createTasksConfig()
    let status = TF_NewStatus()
    // Start everything except the 0-th task, which will
    // be started when we set TFE_ContextSetServerDef.
    for i in 1..<taskCount {
      debugLog("Starting task \(i)...")
      let serverDefText = createServerDef(forTasksConfig: tasks, withTaskIndex: i)
      let serverDef = TFE_GetServerDef(serverDefText, status)
      checkOk(status)
      let server: CTFServer! = TF_NewServer(
          serverDef!.pointee.data, serverDef!.pointee.length, status)
      checkOk(status)
      TF_ServerStart(server, status)
      checkOk(status)
      servers.append(server)
      let target = TF_ServerTarget(server)
      debugLog("Started task \(i) at \(String(cString: target!))")
    }
    TF_DeleteStatus(status)
    // Return the server def for task 0 for use in compiler runtime.
    return createServerDef(forTasksConfig: tasks, withTaskIndex: 0)
  }
}

public func runAllTestsWithRemoteSession(taskCount: Int = 2) {
  // This only works if we use the TFEager API.
  _RuntimeConfig.usesTFEagerAPI = true

  // In the remote test mode, TF level debug log can be printed by changing the
  // params below, instead of changing _RuntimeConfig.printsDebugLog and
  // _RuntimeConfig.tensorflowVerboseLogLevel on a per-test basis. This is
  // because InitTensorFlowRuntime() sets up such config, and it must be called
  // before TF_StartTensorFlowProcessCluster().
  InitTensorFlowRuntime(/* enable_debug_logging */ 1, /* verbose_level */ 0)
  _RuntimeConfig.tensorFlowRuntimeInitialized = true

  let testCluster = TestCluster(taskCount: taskCount)
  _RuntimeConfig.session = .remote(grpcAddress: testCluster.serverDef)
  runAllTests()
}
