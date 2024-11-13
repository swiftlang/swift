include(CheckSourceCompiles)
include(CheckCompilerFlag)

# Use C+17
set(SwiftCore_MIN_CXX_STANDARD 17)
# Unset CMAKE_CXX_STANDARD if it's too low and in the CMakeCache.txt
if($CACHE{CMAKE_CXX_STANDARD} AND $CACHE{CMAKE_CXX_STANDARD} LESS ${SwiftCore_MIN_CXX_STANDARD})
  message(WARNING "Resetting cache value for CMAKE_CXX_STANDARD to ${SwiftCore_MIN_CXX_STANDARD}")
  unset(CMAKE_CXX_STANDARD CACHE)
endif()

# Allow manually specified CMAKE_CXX_STANDARD if it's greater than the minimum
# required C++ version
if(DEFINED CMAKE_CXX_STANDARD AND CMAKE_CXX_STANDARD LESS ${SwiftCore_MIN_CXX_STANDARD})
  message(FATAL_ERROR "Requested CMAKE_CXX_STANDARD=${CMAKE_CXX_STANDARD} which is less than the minimum C++ standard ${SwiftCore_MIN_CXX_STANDARD}")
endif()

set(CMAKE_CXX_STANDARD ${SwiftCore_MIN_CXX_STANDARD} CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

check_source_compiles(CXX
"#if !(__has_attribute(swiftcall) && \
  __has_attribute(swift_context) && \
  __has_attribute(swift_error_result) && \
  __has_attribute(swift_indirect_result))
#error CXX compiler must support Swift calling conventions
#endif
int main(void) { return 0; }"
HAVE_SWIFTCALL)

if(NOT HAVE_SWIFTCALL)
  message(SEND_ERROR "CXX Compiler must support Swift calling conventions")
endif()

check_source_compiles(CXX
"#if !(__has_attribute(swiftasynccall) && \
  __has_attribute(swift_async_context))
#error CXX compiler must support Swift async calling conventions
#endif
int main(void) { return 0; }"
HAVE_SWIFT_ASYNC_CALL)

if(NOT HAVE_SWIFT_ASYNC_CALL)
  message(SEND_ERROR "CXX Compiler must support Swift async calling conventions")
endif()

check_compiler_flag(Swift "-color-diagnostics" HAVE_SWIFT_COLOR_DIAGNOSTICS)
if(HAVE_SWIFT_COLOR_DIAGNOSTICS)
  add_compile_options($<$<COMPILE_LANGUAGE:Swift>:-color-diagnostics>)
endif()

check_compiler_flag(Swift "-diagnostic-style swift" HAVE_SWIFT_DIAGNOSTIC_STYLE)
if(HAVE_SWIFT_DIAGNOSTIC_STYLE)
  add_compile_options($<$<COMPILE_LANGUAGE:Swift>:-diagnostic-style$<SEMICOLON>swift>)
endif()
