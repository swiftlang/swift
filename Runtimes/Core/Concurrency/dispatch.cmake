
find_package(dispatch QUIET REQUIRED)

target_sources(swift_Concurrency PRIVATE
  DispatchGlobalExecutor.cpp
  DispatchExecutor.swift
  CFExecutor.swift
  ExecutorImpl.swift)
target_compile_definitions(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_CONCURRENCY_USES_DISPATCH=1>)
target_compile_options(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-DSWIFT_CONCURRENCY_USES_DISPATCH>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -DSWIFT_CONCURRENCY_USES_DISPATCH>")
target_link_libraries(swift_Concurrency PRIVATE
  dispatch)
