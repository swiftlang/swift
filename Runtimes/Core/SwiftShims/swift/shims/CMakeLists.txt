# Swift shim header files
install(FILES
  AssertionReporting.h
  CoreFoundationShims.h
  EmbeddedShims.h
  FoundationShims.h
  GlobalObjects.h
  HeapObject.h
  KeyPath.h
  LibcOverlayShims.h
  LibcShims.h
  MetadataSections.h
  Random.h
  RefCount.h
  Reflection.h
  RuntimeShims.h
  RuntimeStubs.h
  SwiftStdbool.h
  SwiftStddef.h
  SwiftStdint.h
  System.h
  Target.h
  ThreadLocalStorage.h
  UnicodeData.h
  Visibility.h
  _SwiftConcurrency.h
  _SwiftDistributed.h
  _SynchronizationShims.h
  module.modulemap
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/shims")

add_library(swiftShims INTERFACE)
target_include_directories(swiftShims INTERFACE
  $<$<COMPILE_LANGUAGE:C,CXX>:${CMAKE_CURRENT_SOURCE_DIR}/../../>
  $<$<COMPILE_LANGUAGE:Swift>:${CMAKE_CURRENT_SOURCE_DIR}>)
target_compile_options(swiftShims INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap>")
