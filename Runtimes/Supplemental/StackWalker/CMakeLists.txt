cmake_minimum_required(VERSION 3.29)

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftBacktrace
  LANGUAGES CXX Swift
  VERSION 6.1.0${BUILD_NUMBER})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Backtrace must build as a standalone project")
endif()

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

find_package(SwiftCore REQUIRED)
find_package(SwiftOverlay REQUIRED)
find_package(SwiftCxxOverlay REQUIRED)
find_package(SwiftRuntime REQUIRED)

if(APPLE)
find_package(SwiftDarwin)
endif()

include(GNUInstallDirs)

# NOTE: we disable availability checking because we need to use code from the
# RuntimeModule, but we also need to build with an availability version below
# that defined by Backtracing 6.2 (for now).
#
# We can't use the same trick that we use in the RuntimeModule or stdlib,
# because we're importing those modules, but because we build *with* them, we
# want to be able to use new things that are in them.  We don't have a great
# solution to this, other than just turning off availability checking for this
# code.
add_compile_options(
  "$<$<COMPILE_LANGUAGE:Swift>:-parse-as-library>"
  "$<$<COMPILE_LANGUAGE:Swift>:-cxx-interoperability-mode=default>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-disable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-cxx-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-availability-checking>")

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontend and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)
# Ensure all symbols are fully resolved on Linux
add_link_options($<$<PLATFORM_ID:Android,Linux>:LINKER:-z,defs>)

add_executable(swift-backtrace
  main.swift
  AnsiColor.swift
  JSON.swift
  TargetMacOS.swift
  TargetLinux.swift
  TargetWindows.swift
  Themes.swift
  Timing.swift
  Utils.swift)
set_target_properties(swift-backtrace PROPERTIES
  Swift_MODULE_NAME swift_backtrace)
target_compile_options(swift-backtrace PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -I${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/include>")
target_link_libraries(swift-backtrace PRIVATE
  swiftShims
  swiftCore
  swift_Builtin_float
  swiftRuntime
  swiftCxx
  $<$<PLATFORM_ID:Darwin>:swiftDarwin>
  $<$<PLATFORM_ID:Linux>:swiftGlibc>
  $<$<PLATFORM_ID:Windows>:swiftCRT>
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>)

install(TARGETS swift-backtrace
  RUNTIME DESTINATION "${CMAKE_INSTALL_LIBEXECDIR}/swift")
