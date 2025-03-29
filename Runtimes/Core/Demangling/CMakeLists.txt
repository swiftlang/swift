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
target_compile_definitions(swiftDemangling PRIVATE
  $<$<BOOL:${BUILD_SHARED_LIBS}>:-DswiftCore_EXPORTS>
  $<$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>:-DSWIFT_SUPPORT_OLD_MANGLING>
  $<$<BOOL:${SwiftCore_ENABLE_TYPE_PRINTING}>:-DSWIFT_STDLIB_HAS_TYPE_PRINTING>
  $<$<BOOL:${SwiftCore_ENABLE_CRASH_REPORTER_CLIENT}>:-DSWIFT_HAVE_CRASHREPORTERCLIENT>
  $<$<BOOL:${SwiftCore_HAS_ASL}>:-DSWIFT_STDLIB_HAS_ASL>)

# Target libraries that include libDemangling must define the name to use for
# the inline namespace to distinguish symbols from those built for the
# compiler, in order to avoid possible ODR violations if both are statically
# linked into the same binary. (see also commit message for 5b1daa9055c99904c84862ecc313641fd9b26e63)
target_compile_definitions(swiftDemangling PUBLIC
    $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_INLINE_NAMESPACE=__runtime>)

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
  install(TARGETS swiftDemangling
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()
