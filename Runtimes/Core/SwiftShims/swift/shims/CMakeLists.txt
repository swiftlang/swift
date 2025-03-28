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
  ObjCShims.h
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
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/swift/shims"
  COMPONENT SwiftCore_development)

add_library(swiftShims INTERFACE)
target_include_directories(swiftShims INTERFACE
  $<$<COMPILE_LANGUAGE:C,CXX>:$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}>>
  $<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}/swift/shims>>)
target_compile_definitions(swiftShims INTERFACE
  $<$<AND:$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>,$<COMPILE_LANGUAGE:C,CXX>>:SWIFT_STATIC_STDLIB>)
target_compile_options(swiftShims INTERFACE
  "$<$<AND:$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xcc -DSWIFT_STATIC_STDLIB>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=\"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/module.modulemap>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}/swift/shims/module.modulemap>\">")

install(TARGETS swiftShims
  EXPORT SwiftCoreTargets
  COMPONENT SwiftCore_development)
