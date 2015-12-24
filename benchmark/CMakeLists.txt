if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  # Performance test harness only builds on Darwin.
  set(PERF_TESTSUITE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/PerfTestSuite" CACHE STRING "Path to swift performance testsuite repo.")
  if(EXISTS ${PERF_TESTSUITE_DIR}/CMakeLists.txt)
    add_subdirectory(${PERF_TESTSUITE_DIR})
  else()
    message(FATAL_ERROR "Can't find the Swift performance suite at ${PERF_TESTSUITE_DIR}/.")
  endif()
endif()

