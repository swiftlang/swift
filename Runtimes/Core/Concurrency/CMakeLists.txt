add_subdirectory(InternalShims)

gyb_expand(Task+init.swift.gyb Task+init.swift)
gyb_expand(TaskGroup+addTask.swift.gyb TaskGroup+addTask.swift)
gyb_expand(Task+immediate.swift.gyb Task+immediate.swift)

add_library(swift_Concurrency
  Actor.cpp
  AsyncLet.cpp
  AsyncStream.cpp
  CFExecutor.cpp
  Clock.cpp
  ConcurrencyHooks.cpp
  EmbeddedSupport.cpp
  Error.cpp
  ExecutorBridge.cpp
  ExecutorImpl.cpp
  ExecutorChecks.cpp
  GlobalExecutor.cpp
  Setup.cpp
  Task.cpp
  TaskAlloc.cpp
  TaskGroup.cpp
  TaskLocal.cpp
  TaskStatus.cpp
  ThreadingError.cpp
  TracingSignpost.cpp
  "${PROJECT_SOURCE_DIR}/CompatibilityOverride/CompatibilityOverride.cpp"
  "${PROJECT_SOURCE_DIR}/linker-support/magic-symbols-for-install-name.c"
  Actor.swift
  AsyncCompactMapSequence.swift
  AsyncDropFirstSequence.swift
  AsyncDropWhileSequence.swift
  AsyncFilterSequence.swift
  AsyncFlatMapSequence.swift
  AsyncIteratorProtocol.swift
  AsyncLet.swift
  AsyncMapSequence.swift
  AsyncPrefixSequence.swift
  AsyncPrefixWhileSequence.swift
  AsyncSequence.swift
  AsyncStream.swift
  AsyncStreamBuffer.swift
  AsyncThrowingCompactMapSequence.swift
  AsyncThrowingDropWhileSequence.swift
  AsyncThrowingFilterSequence.swift
  AsyncThrowingFlatMapSequence.swift
  AsyncThrowingMapSequence.swift
  AsyncThrowingPrefixWhileSequence.swift
  AsyncThrowingStream.swift
  CheckedContinuation.swift
  Clock.swift
  ContinuousClock.swift
  CooperativeExecutor.swift
  Deque/_DequeBuffer.swift
  Deque/_DequeBufferHeader.swift
  Deque/_DequeSlot.swift
  Deque/_UnsafeWrappedBuffer.swift
  Deque/Compatibility.swift
  Deque/Deque.swift
  Deque/Deque+Codable.swift
  Deque/Deque+Collection.swift
  Deque/Deque+CustomDebugStringConvertible.swift
  Deque/Deque+CustomReflectable.swift
  Deque/Deque+CustomStringConvertible.swift
  Deque/Deque+Equatable.swift
  Deque/Deque+ExpressibleByArrayLiteral.swift
  Deque/Deque+Extras.swift
  Deque/Deque+Hashable.swift
  Deque/Deque+Storage.swift
  Deque/Deque+Testing.swift
  Deque/Deque+UnsafeHandle.swift
  Deque/UnsafeMutableBufferPointer+Utilities.swift
  DiscardingTaskGroup.swift
  Errors.swift
  Executor.swift
  ExecutorAssertions.swift
  ExecutorBridge.swift
  GlobalActor.swift
  GlobalConcurrentExecutor.swift
  MainActor.swift
  PartialAsyncTask.swift
  PlatformExecutorDarwin.swift
  PlatformExecutorLinux.swift
  PlatformExecutorFreeBSD.swift
  PlatformExecutorOpenBSD.swift
  PlatformExecutorWindows.swift
  PriorityQueue.swift
  SourceCompatibilityShims.swift
  SuspendingClock.swift
  Task.swift
  Task+PriorityEscalation.swift
  Task+TaskExecutor.swift
  TaskCancellation.swift
  TaskGroup.swift
  TaskLocal.swift
  TaskSleep.swift
  TaskSleepDuration.swift
  UnimplementedExecutor.swift
  "${CMAKE_CURRENT_BINARY_DIR}/Task+init.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/TaskGroup+addTask.swift"
  "${CMAKE_CURRENT_BINARY_DIR}/Task+immediate.swift")

include(${SwiftCore_CONCURRENCY_GLOBAL_EXECUTOR}.cmake)
target_compile_definitions(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_RUNTIME>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_TARGET_LIBRARY_NAME=swift_Concurrency>
  # NOTE: VS2017 <15.8 would round clamp alignment to alignof(max_align_t) which
  # was non-conformant. Indicate that we wish to use extended alignment.
  $<$<AND:$<PLATFORM_ID:Windows>,$<COMPILE_LANGUAGE:C,CXX>>:-D_ENABLE_EXTENDED_ALIGNED_STORAGE=1>
  $<$<COMPILE_LANGUAGE:C,CXX>:-D__STDC_WANT_LIB_EXT1__=1>)
target_compile_options(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  # NOTE: do not remove until `IsolatedAny` is on by default in all supported
  # compilers.
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature IsolatedAny>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Extern>"
  # NOTE: enable the async frame pointer on Darwin to faciliate debugging.
  $<$<AND:$<PLATFORM_ID:Darwin>,$<COMPILE_LANGUAGE:C,CXX>>:-fswift-async-fp=always>
  "$<$<AND:$<PLATFORM_ID:Darwin>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -swift-async-frame-pointer=always>"
  "$<$<AND:$<BOOL:${BUILD_SHARED_LIBS}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xcc -Dswift_Concurrency_EXPORTS>")
target_include_directories(swift_Concurrency PRIVATE
  # FIXME: Use of `swift/Runtime/...`, `swift/ABI/...`, and `swift/Demangling/...`
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include"
  # FIXME: grant access to `runtime/CMakeConfig.h` which should be available
  # through the swiftRuntime target.
  "${PROJECT_BINARY_DIR}/include")
# FIXME: Why is this not implicitly in the interface flags?
target_include_directories(swift_Concurrency INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")
target_link_libraries(swift_Concurrency PRIVATE
  swiftShims
  swiftConcurrencyInternalShims
  $<$<BOOL:${BUILD_SHARED_LIBS}>:swiftThreading>
  $<$<PLATFORM_ID:Windows>:Synchronization>
  $<$<PLATFORM_ID:Windows>:mincore>
  # Link to the runtime that we are just building.
  swiftCore)
set_target_properties(swift_Concurrency PROPERTIES
  Swift_MODULE_NAME _Concurrency
  LINKER_LANGUAGE CXX)
if(NOT BUILD_SHARED_LIBS AND CMAKE_STATIC_LIBRARY_PREFIX_Swift)
  set_target_properties(swift_Concurrency PROPERTIES
    PREFIX ${CMAKE_STATIC_LIBRARY_PREFIX_Swift})
endif()

install(TARGETS swift_Concurrency
  EXPORT SwiftCoreTargets
  COMPONENT SwiftCore_runtime
  ARCHIVE DESTINATION "${SwiftCore_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftCore_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_Concurrency)
install_swift_interface(swift_Concurrency)
