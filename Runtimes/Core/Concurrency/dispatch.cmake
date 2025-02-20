
find_package(dispatch QUIET)

target_sources(swift_Concurrency PRIVATE
  DispatchGlobalExecutor.cpp)
target_compile_definitions(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_CONCURRENCY_USES_DISPATCH=1>)
target_compile_options(swift_Concurrency PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-DSWIFT_CONCURRENCY_USES_DISPATCH>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -DSWIFT_CONCURRENCY_USES_DISPATCH>")
target_link_libraries(swift_Concurrency PRIVATE
  dispatch)
