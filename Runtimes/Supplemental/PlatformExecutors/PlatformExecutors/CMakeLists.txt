# The executors of the platforms other than the Apple platforms, such as the
# `pthread` and Win32 executors, and their `OperationScheduler`
# implementations. It re-exports `_PlatformExecutorsCore`.
#
# Resync.cmake copies the sources from the `PlatformExecutors` and
# `CPlatformExecutors` targets of the swift-platform-executors checkout. The
# sources guard the code of each platform with `#if`, so all of them are built
# on every platform, except for the Dispatch executors, which are only for the
# Apple platforms.

# The package's C shim `CPlatformExecutors` is built here as the module
# `_PlatformExecutorsCShims`, so that it doesn't clash with the package's own
# module when the package is built with this toolchain. The package's sources
# import and qualify names with `CPlatformExecutors`, so they're compiled with
# `-module-alias CPlatformExecutors=_PlatformExecutorsCShims`.
#
# `PlatformExecutors` imports the shim with `internal import`. Clients of the
# builds with library evolution don't load it, but clients of the builds
# without library evolution, such as the static SDKs, do. So, like Foundation's
# `_FoundationCShims`, its headers and a module map go to
# `lib/swift/_PlatformExecutorsCShims` (`lib/swift_static` for the static
# SDKs), where the compiler finds C modules.
set(_platform_executors_c_include_dir "${PROJECT_SOURCE_DIR}/CPlatformExecutors/include")
set(_platform_executors_c_headers
  "${_platform_executors_c_include_dir}/CPlatformExecutors.h")
set(_platform_executors_c_module_dir "${CMAKE_CURRENT_BINARY_DIR}/_PlatformExecutorsCShims")
foreach(header ${_platform_executors_c_headers})
  get_filename_component(header_name "${header}" NAME)
  configure_file("${header}" "${_platform_executors_c_module_dir}/${header_name}" COPYONLY)
endforeach()
file(CONFIGURE OUTPUT "${_platform_executors_c_module_dir}/module.modulemap" CONTENT [[
module _PlatformExecutorsCShims {
  header "CPlatformExecutors.h"
  export *
}
]])

add_library(swift_PlatformExecutorsCShims OBJECT
  "${PROJECT_SOURCE_DIR}/CPlatformExecutors/shim.c")
target_compile_definitions(swift_PlatformExecutorsCShims PRIVATE
  _GNU_SOURCE)
target_include_directories(swift_PlatformExecutorsCShims PRIVATE
  "${_platform_executors_c_include_dir}")

add_library(swiftPlatformExecutors
  Exports.swift
  IO/HighLevel/File.swift
  IO/HighLevel/FilePermissions.swift
  IO/HighLevel/TCPConnection.swift
  IO/HighLevel/TCPListener.swift
  IO/HighLevel/UDPConnection.swift
  IO/HighLevel/UDPCore.swift
  IO/HighLevel/UDPSocket.swift
  IO/HighLevel/withOperationScheduler.swift
  Internal/AsyncTemporaryAllocation.swift
  Internal/AsyncUnsafePointer.swift
  Internal/ExecutorJob+Sequence.swift
  Internal/JobQueue.swift
  Internal/PriorityQueue.swift
  Internal/TemporaryAllocation.swift
  Internal/UniqueArray.swift
  Internal/UnownedJob+Compare.swift
  PThread/Internal/AsyncDo.swift
  PThread/Internal/BlockingWorkQueue.swift
  PThread/Internal/Box.swift
  PThread/Internal/ConditionLock.swift
  PThread/Internal/IO/ConditionSelectorBackend.swift
  PThread/Internal/IO/EpollReadinessBackend.swift
  PThread/Internal/IO/EventFileDescriptor.swift
  PThread/Internal/IO/FileSyscall.swift
  PThread/Internal/IO/IOBackend.swift
  PThread/Internal/IO/IOError+SyscallError.swift
  PThread/Internal/IO/IOOperation.swift
  PThread/Internal/IO/IOUringBackend.swift
  PThread/Internal/IO/IOWaitStrategy.swift
  PThread/Internal/IO/KQueueReadinessBackend.swift
  PThread/Internal/IO/PThreadExecutor+FileOperations.swift
  PThread/Internal/IO/PThreadExecutor+TCPSocketOperations.swift
  PThread/Internal/IO/PThreadExecutor+UDPSocketOperations.swift
  PThread/Internal/IO/PThreadTaskExecutor+FileOperations.swift
  PThread/Internal/IO/PThreadTaskExecutor+TCPSocketOperations.swift
  PThread/Internal/IO/PThreadTaskExecutor+UDPSocketOperations.swift
  PThread/Internal/IO/PlatformIOBackend.swift
  PThread/Internal/IO/ReadinessEvent.swift
  PThread/Internal/IO/ReadinessOperations.swift
  PThread/Internal/IO/SocketAddress+POSIX.swift
  PThread/Internal/IO/SocketSyscall.swift
  PThread/Internal/IO/TimerFileDescriptor.swift
  PThread/Internal/IO/_IntegerBitPacking.swift
  PThread/Internal/IO/syscall.swift
  PThread/Internal/IO/timespec+duration.swift
  PThread/Internal/PThread.swift
  PThread/Internal/SystemCoreCount.swift
  PThread/Internal/Thread.swift
  PThread/PThreadExecutor.swift
  PThread/PThreadFile.swift
  PThread/PThreadIOBackend.swift
  PThread/PThreadMainExecutor.swift
  PThread/PThreadSerialExecutor.swift
  PThread/PThreadTCPConnection.swift
  PThread/PThreadTCPListener.swift
  PThread/PThreadTaskExecutor.swift
  PThread/PThreadUDPSocket.swift
  PlatformExecutorFactory.swift
  PlatformSerialExecutor.swift
  PlatformTaskExecutor.swift
  Win32Executor/Win32NativeExecutors.swift)
set_target_properties(swiftPlatformExecutors PROPERTIES
  Swift_MODULE_NAME PlatformExecutors
  Swift_LANGUAGE_VERSION 6)
# The Swift settings of the Package.swift of swift-platform-executors, with the
# `ExperimentalIO` trait enabled, the same as for `_PlatformExecutorsCore`.
# `BuiltinModule` is for `Builtin.stackAlloc`, and
# `AllowRuntimeSymbolDeclarations` for the `@_silgen_name` declarations of
# runtime functions. The sources rely on the implicit import of `_Concurrency`
# for `async`, so it's imported explicitly, like for `_PlatformExecutorsCore`.
target_compile_options(swiftPlatformExecutors PRIVATE
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-package-name swift-platform-executors>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature ExistentialAny>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature InternalImportsByDefault>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature NonisolatedNonsendingByDefault>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature BuiltinModule>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AllowRuntimeSymbolDeclarations>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-D ExperimentalIO>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -import-module -Xfrontend _Concurrency>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-module-alias CPlatformExecutors=_PlatformExecutorsCShims>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${_platform_executors_c_module_dir}/module.modulemap>")
if(NOT BUILD_SHARED_LIBS)
  # Explicitly autolink the private collections module, because it is only
  # imported with `internal import`.
  target_compile_options(swiftPlatformExecutors PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -public-autolink-library -Xfrontend swift_PlatformExecutorsCollections>")
endif()
# The experimental io_uring backend, the `ExperimentalIOUring` trait of the
# package. It imports `SystemPackage` and `SystemSockets` of swift-system, which
# are built as the private modules `_PlatformExecutorsSystem` and
# `_PlatformExecutorsSystemSockets`, with `CSystem` as
# `_PlatformExecutorsSystemCShims`.
if(SwiftPlatformExecutors_ENABLE_IO_URING)
  target_compile_options(swiftPlatformExecutors PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-D ExperimentalIOUring>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-module-alias SystemPackage=_PlatformExecutorsSystem>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-module-alias SystemSockets=_PlatformExecutorsSystemSockets>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-module-alias CSystem=_PlatformExecutorsSystemCShims>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${SwiftPlatformExecutors_SYSTEM_C_MODULE_MAP}>")
  if(NOT BUILD_SHARED_LIBS)
    target_compile_options(swiftPlatformExecutors PRIVATE
      "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -public-autolink-library -Xfrontend swift_PlatformExecutorsSystem>"
      "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -public-autolink-library -Xfrontend swift_PlatformExecutorsSystemSockets>")
  endif()
  target_link_libraries(swiftPlatformExecutors PRIVATE
    swift_PlatformExecutorsSystem
    swift_PlatformExecutorsSystemSockets)
endif()
target_link_libraries(swiftPlatformExecutors PRIVATE
  swift_PlatformExecutorsCShims
  swift_PlatformExecutorsCollections
  swift_PlatformExecutorsCore
  swiftSynchronization
  swiftCore
  swift_Concurrency
  $<$<PLATFORM_ID:Android>:swiftAndroid>
  $<$<PLATFORM_ID:Linux>:swiftGlibc>
  $<$<PLATFORM_ID:Windows>:swiftCRT>
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swiftPlatformExecutors
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftPlatformExecutors)
install_swift_interface(swiftPlatformExecutors)

if(BUILD_SHARED_LIBS)
  set(_platform_executors_c_install_dir "${CMAKE_INSTALL_LIBDIR}/swift/_PlatformExecutorsCShims")
else()
  set(_platform_executors_c_install_dir "${CMAKE_INSTALL_LIBDIR}/swift_static/_PlatformExecutorsCShims")
endif()
install(FILES
  ${_platform_executors_c_headers}
  "${_platform_executors_c_module_dir}/module.modulemap"
  DESTINATION "${_platform_executors_c_install_dir}"
  COMPONENT ${PROJECT_NAME}_development)

embed_manifest(swiftPlatformExecutors)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftPlatformExecutors.cmake" OPTIONAL)
