# FIXME: Refactor demangling library so that we aren't pulling sources from
#        the compiler.
add_library(swiftDemangling OBJECT
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/Context.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/Demangler.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/ManglingUtils.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/NodePrinter.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/Punycode.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/Remangler.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/NodeDumper.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/Errors.cpp")
target_compile_definitions(swiftDemangling PRIVATE swiftCore_EXPORTS
  $<$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>:-DSWIFT_SUPPORT_OLD_MANGLING>
  $<$<BOOL:${SwiftCore_ENABLE_TYPE_PRINTING}>:-DSWIFT_STDLIB_HAS_TYPE_PRINTING>
  $<$<BOOL:${SwiftCore_ENABLE_CRASH_REPORTER_CLIENT}>:-DSWIFT_HAVE_CRASHREPORTERCLIENT>)

target_include_directories(swiftDemangling
  PRIVATE
    "${SwiftCore_SWIFTC_SOURCE_DIR}/include"
    "${PROJECT_BINARY_DIR}/include")

target_link_libraries(swiftDemangling PRIVATE swiftShims)

if(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT)
  # We could likely pull the copy from the runtime sources
  add_library(swiftDemanglingCR OBJECT
    "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/CrashReporter.cpp")
  target_link_libraries(swiftDemanglingCR PRIVATE swiftShims)
endif()

if(SwiftCore_ENABLE_OBJC_INTEROP)
  target_sources(swiftDemangling PRIVATE
    "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/OldDemangler.cpp"
    "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Demangling/OldRemangler.cpp")
endif()

if(LINUX OR BSD)
  target_compile_options(swiftDemangling PRIVATE -fno-lto)
endif()

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftDemangling)
endif()
