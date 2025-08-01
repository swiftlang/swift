
find_package(dispatch QUIET REQUIRED)

check_symbol_exists("dispatch_async_swift_job" "dispatch/private.h"
  SwiftConcurrency_HAS_DISPATCH_ASYNC_SWIFT_JOB)

target_sources(swift_Concurrency PRIVATE
  DispatchGlobalExecutor.cpp
  DispatchExecutor.swift
  CFExecutor.swift
  ExecutorImpl.swift)
target_compile_definitions(swift_Concurrency PRIVATE
  $<$<BOOL:${SwiftConcurrency_HAS_DISPATCH_ASYNC_SWIFT_JOB}>:-DSwiftConcurrency_HAS_DISPATCH_ASYNC_SWIFT_JOB=1>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_CONCURRENCY_USES_DISPATCH=1>)
target_compile_options(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-DSWIFT_CONCURRENCY_USES_DISPATCH>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -DSWIFT_CONCURRENCY_USES_DISPATCH>")
target_link_libraries(swift_Concurrency PRIVATE
  dispatch)
