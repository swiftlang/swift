# The multithreaded default executor for wasm32-unknown-wasip1-threads: a pool
# of wasi-threads worker threads (WASIGlobalExecutor.cpp) fronted by the Swift
# executors in WASIExecutor.swift. Only meaningful for the threads triple —
# real threads and a runtime that is not single-threaded.
if(NOT WASI)
  message(SEND_ERROR "The wasi global executor is only supported when targeting WASI")
endif()
if(NOT SwiftCore_THREADING_PACKAGE STREQUAL "PTHREADS")
  message(SEND_ERROR "The wasi global executor requires SwiftCore_THREADING_PACKAGE=PTHREADS (wasm32-unknown-wasip1-threads)")
endif()
if(SwiftCore_SINGLE_THREADED_CONCURRENCY)
  message(SEND_ERROR "Cannot enable the wasi global executor with SwiftCore_SINGLE_THREADED_CONCURRENCY")
endif()

target_sources(swift_Concurrency PRIVATE
  WASIGlobalExecutor.cpp
  ExecutorImpl.swift
  WASIExecutor.swift
  PlatformExecutorWASI.swift)
