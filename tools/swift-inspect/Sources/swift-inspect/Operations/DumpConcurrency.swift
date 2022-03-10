//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(iOS) || os(macOS) || os(tvOS) || os(watchOS)

import ArgumentParser
import SwiftRemoteMirror

struct DumpConcurrency: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "Print information about the target's concurrency runtime.")

  @OptionGroup()
  var options: UniversalOptions

  func run() throws {
    try inspect(process: options.nameOrPid) { process in
      let dumper = ConcurrencyDumper(context: process.context,
                                     process: process as! DarwinRemoteProcess)
      dumper.dumpTasks()
      dumper.dumpActors()
      dumper.dumpThreads()
    }
  }
}

fileprivate class ConcurrencyDumper {
  let context: SwiftReflectionContextRef
  let process: DarwinRemoteProcess
  let jobMetadata: swift_reflection_ptr_t?
  let taskMetadata: swift_reflection_ptr_t?

  struct TaskInfo {
    var address: swift_reflection_ptr_t
    var jobFlags: UInt32
    var taskStatusFlags: UInt64
    var id: UInt64
    var runJob: swift_reflection_ptr_t
    var allocatorSlabPtr: swift_reflection_ptr_t
    var allocatorTotalSize: Int
    var allocatorTotalChunks: Int
    var childTasks: [swift_reflection_ptr_t]
    var asyncBacktrace: [swift_reflection_ptr_t]
    var parent: swift_reflection_ptr_t?
  }

  struct HeapInfo {
    var tasks: [swift_reflection_ptr_t] = []
    var jobs: [swift_reflection_ptr_t] = []
    var actors: [swift_reflection_ptr_t] = []
  }

  lazy var heapInfo: HeapInfo = gatherHeapInfo()

  lazy var threadCurrentTasks = process.currentTasks.filter{ $0.currentTask != 0 }

  lazy var tasks: [swift_reflection_ptr_t: TaskInfo] = gatherTasks()

  var actors: [swift_reflection_ptr_t] {
    heapInfo.actors
  }

  var metadataIsActorCache: [swift_reflection_ptr_t: Bool] = [:]
  var metadataNameCache: [swift_reflection_ptr_t: String?] = [:]

  init(context: SwiftReflectionContextRef, process: DarwinRemoteProcess) {
    self.context = context
    self.process = process

    func getMetadata(symbolName: String) -> swift_reflection_ptr_t? {
      let addr = process.getAddr(symbolName: symbolName)
      if let ptr = process.read(address: addr, size: MemoryLayout<UInt>.size) {
        return swift_reflection_ptr_t(ptr.load(as: UInt.self))
      }
      return nil
    }
    jobMetadata = getMetadata(symbolName: "_swift_concurrency_debug_jobMetadata")
    taskMetadata = getMetadata(symbolName: "_swift_concurrency_debug_asyncTaskMetadata")
  }

  func gatherHeapInfo() -> HeapInfo {
    var result = HeapInfo()
    
    process.iterateHeap { (pointer, size) in
      let metadata = swift_reflection_ptr_t(swift_reflection_metadataForObject(context, UInt(pointer)))
      if metadata == jobMetadata {
        result.jobs.append(swift_reflection_ptr_t(pointer))
      } else if metadata == taskMetadata {
        result.tasks.append(swift_reflection_ptr_t(pointer))
      } else if isActorMetadata(metadata) {
        result.actors.append(swift_reflection_ptr_t(pointer))
      }
    }

    return result
  }

  func gatherTasks() -> [swift_reflection_ptr_t: TaskInfo] {
    var map: [swift_reflection_ptr_t: TaskInfo] = [:]
    var tasksToScan: Set<swift_reflection_ptr_t> = []
    tasksToScan.formUnion(heapInfo.tasks)
    tasksToScan.formUnion(threadCurrentTasks.map{ swift_reflection_ptr_t($0.currentTask) }.filter{ $0 != 0 })

    while !tasksToScan.isEmpty {
      let taskToScan = tasksToScan.removeFirst()
      if let info = info(forTask: taskToScan) {
        map[taskToScan] = info
        for child in info.childTasks {
          let childMetadata = swift_reflection_metadataForObject(context, UInt(child))
          if let taskMetadata = taskMetadata, childMetadata != taskMetadata {
            print("Inconsistent data detected! Child task \(hex: child) has unknown metadata \(hex: taskMetadata)")
          }
          if map[child] == nil {
            tasksToScan.insert(child)
          }
        }
      }
    }

    for (task, info) in map {
      for child in info.childTasks {
        map[child]?.parent = task
      }
    }

    return map
  }

  func isActorMetadata(_ metadata: swift_reflection_ptr_t) -> Bool {
    if let cached = metadataIsActorCache[metadata] {
      return cached
    }
    let result = swift_reflection_metadataIsActor(context, metadata) != 0
    metadataIsActorCache[metadata] = result
    return result
  }

  func name(metadata: swift_reflection_ptr_t) -> String? {
    if let cached = metadataNameCache[metadata] {
      return cached
    }

    let name = context.name(type: metadata)
    metadataNameCache[metadata] = name
    return name
  }

  func info(forTask task: swift_reflection_ptr_t) -> TaskInfo? {
    let reflectionInfo = swift_reflection_asyncTaskInfo(context, task)
    if let error = reflectionInfo.Error {
      print("Error getting info for async task \(hex: task): \(String(cString: error))")
      return nil
    }

    // These arrays are temporary pointers which we must copy out before we call
    // into Remote Mirror again.
    let children = Array(UnsafeBufferPointer(
        start: reflectionInfo.ChildTasks,
        count: Int(reflectionInfo.ChildTaskCount)))
    let asyncBacktraceFrames = Array(UnsafeBufferPointer(
        start: reflectionInfo.AsyncBacktraceFrames,
        count: Int(reflectionInfo.AsyncBacktraceFramesCount)))

    var allocatorSlab = reflectionInfo.AllocatorSlabPtr
    var allocatorTotalSize = 0
    var allocatorTotalChunks = 0
    while allocatorSlab != 0 {
      let allocations = swift_reflection_asyncTaskSlabAllocations(context,
                                                                  allocatorSlab)
      guard allocations.Error == nil else { break }
      allocatorTotalSize += Int(allocations.SlabSize)
      allocatorTotalChunks += Int(allocations.ChunkCount)

      allocatorSlab = allocations.NextSlab
    }

    return TaskInfo(
      address: task,
      jobFlags: reflectionInfo.JobFlags,
      taskStatusFlags: reflectionInfo.TaskStatusFlags,
      id: reflectionInfo.Id,
      runJob: reflectionInfo.RunJob,
      allocatorSlabPtr: reflectionInfo.AllocatorSlabPtr,
      allocatorTotalSize: allocatorTotalSize,
      allocatorTotalChunks: allocatorTotalChunks,
      childTasks: children,
      asyncBacktrace: asyncBacktraceFrames
    )
  }

  func taskHierarchy() -> [(level: Int, lastChild: Bool, task: TaskInfo)] {
    var hierarchy: [(level: Int, lastChild: Bool, task: TaskInfo)] = []

    let topLevelTasks = tasks.values.filter{ $0.parent == nil }
    for top in topLevelTasks.sorted(by: { $0.id < $1.id }) {
      var stack: [(index: Int, task: TaskInfo)] = [(0, top)]
      hierarchy.append((0, true, top))

      while let (index, task) = stack.popLast() {
        if index < task.childTasks.count {
          stack.append((index + 1, task))
          let childPtr = task.childTasks[index]
          let childTask = tasks[childPtr]!
          hierarchy.append((stack.count, index == task.childTasks.count - 1, childTask))
          stack.append((0, childTask))
        }
      }
    }
    return hierarchy
  }

  func remove(from: String, upTo: String) -> String {
    from.withCString {
      if let found = strstr($0, upTo) {
        return String(cString: found + strlen(upTo))
      }
      return from
    }
  }

  func symbolicateBacktracePointer(ptr: swift_reflection_ptr_t) -> String {
    guard let name = process.symbolicate(swift_addr_t(ptr)).symbol else {
      return "<\(hex: ptr)>"
    }

    return remove(from: name, upTo: " resume partial function for ")
  }

  func flagsStrings<T: BinaryInteger>(flags: T, strings: [T: String]) -> [String] {
    return strings.sorted{ $0.key < $1.key }
                  .filter({ ($0.key & flags) != 0})
                  .map{ $0.value }
  }

  func flagsString<T: BinaryInteger>(flags: T, strings: [T: String]) -> String {
    let flagStrs = flagsStrings(flags: flags, strings: strings)
    if flagStrs.isEmpty {
      return "0"
    }

    let flagsStr = flagStrs.joined(separator: "|")
    return flagsStr
  }

  func decodeTaskFlags(_ info: TaskInfo) -> (
    priority: UInt32,
    flags: String
  ) {
    let priority = (info.jobFlags >> 8) & 0xff
    let jobFlags = flagsStrings(flags: info.jobFlags, strings: [
      1 << 24: "childTask",
      1 << 25: "future",
      1 << 26: "groupChildTask",
      1 << 28: "asyncLetTask"
    ])
    let taskFlags = flagsStrings(flags: info.taskStatusFlags, strings: [
      0x100: "cancelled",
      0x200: "locked",
      0x400: "escalated",
      0x800: "running"
    ])
    let allFlags = jobFlags + taskFlags
    let flagsStr = allFlags.isEmpty ? "0" : allFlags.joined(separator: "|")
    return (priority, flagsStr)
  }

  func decodeActorFlags(_ flags: UInt64) -> (
    status: String,
    flags: String,
    maxPriority: UInt64
  ) {
    let statuses: [UInt64: String] = [
      0: "idle",
      1: "scheduled",
      2: "running",
      3: "zombie-latching",
      4: "zombie-ready-for-deallocation"
    ]
    let flagsString = flagsString(flags: flags, strings: [
      1 << 3: "hasActiveInlineJob",
      1 << 4: "isDistributedRemote"
    ])

    let status = flags & 0x7
    let maxPriority = (flags >> 8) & 0xff
    return (
      status: statuses[status] ?? "unknown(\(status))",
      flags: flagsString,
      maxPriority: maxPriority
    )
  }

  func dumpTasks() {
    print("TASKS")

    var lastChilds: [Bool] = []

    let hierarchy = taskHierarchy()
    for (i, (level, lastChild, task)) in hierarchy.enumerated() {
      lastChilds.removeSubrange(level...)
      lastChilds.append(lastChild)

      let prevEntry = i > 0 ? hierarchy[i - 1] : nil

      let levelDidIncrease = level > (prevEntry?.level ?? -1)

      var prefix = ""
      for lastChild in lastChilds {
        prefix += lastChild ? "     " : "   | "
      }
      prefix += "   "
      let firstPrefix = String(prefix.dropLast(5) + (
          level == 0 ? "   " :
          lastChild  ? "`--" :
                       "+--"))
      if levelDidIncrease {
        print(prefix)
      }

      var firstLine = true
      func output(_ str: String) {
        print((firstLine ? firstPrefix : prefix) + str)
        firstLine = false
      }

      let runJobSymbol = process.symbolicate(swift_addr_t(task.runJob))
      let runJobLibrary = runJobSymbol.module ?? "<unknown>"

      let symbolicatedBacktrace = task.asyncBacktrace.map(symbolicateBacktracePointer)

      let flags = decodeTaskFlags(task)

      output("Task \(hex: task.id) - flags=\(flags.flags) priority=\(hex: flags.priority) address=\(hex: task.address)")
      if let parent = task.parent {
        output("parent: \(hex: parent)")
      }

      if let first = symbolicatedBacktrace.first {
        output("async backtrace: \(first)")
        for entry in symbolicatedBacktrace.dropFirst() {
          output("                 \(entry)")
        }
      }

      output("resume function: \(symbolicateBacktracePointer(ptr: task.runJob)) in \(runJobLibrary)")
      output("task allocator: \(task.allocatorTotalSize) bytes in \(task.allocatorTotalChunks) chunks")

      if task.childTasks.count > 0 {
        let s = task.childTasks.count > 1 ? "s" : ""
        output("* \(task.childTasks.count) child task\(s)")
      }

      if (task.childTasks.isEmpty) && i < hierarchy.count - 1 {
        print(prefix)
      }
    }

    print("")
  }

  func dumpActors() {
    print("ACTORS")

    for actor in actors {
      let metadata = swift_reflection_metadataForObject(context, UInt(actor))
      let metadataName = name(metadata: swift_reflection_ptr_t(metadata)) ?? "<unknown class name>"
      let info = swift_reflection_actorInfo(context, actor);

      let flags = decodeActorFlags(info.Flags)

      print("  \(hex: actor) \(metadataName) status=\(flags.status) flags=\(flags.flags) maxPriority=\(hex: flags.maxPriority)")

      func jobStr(_ job: swift_reflection_ptr_t) -> String {
        if let task = tasks[job] {
          return "Task \(hex: task.id) \(symbolicateBacktracePointer(ptr: task.runJob))"
        }
        return "<internal job \(hex: job)>"
      }

      var job = info.FirstJob
      if job == 0 {
        print("    no jobs queued")
      } else {
        print("    job queue: \(jobStr(job))")
        while job != 0 {
          job = swift_reflection_nextJob(context, job);
          if job != 0 {
            print("               \(jobStr(job))")
          }
        }
      }
        print("")
    }
  }

  func dumpThreads() {
    print("THREADS")
    if threadCurrentTasks.isEmpty {
      print("  no threads with active tasks")
      return
    }

    for (thread, task) in threadCurrentTasks {
      let taskStr: String
      if let info = tasks[swift_reflection_ptr_t(task)] {
        taskStr = "\(hex: info.id)"
      } else {
        taskStr = "<unknown task \(hex: task)>"
      }
      print("  Thread \(hex: thread) - current task: \(taskStr)")
    }
  }
}

#endif
