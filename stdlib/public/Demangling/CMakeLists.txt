# TODO: due to the use of `add_swift_target_library` rather than `add_library`
# we cannot use `target_sources` and thus must resort to list manipulations to
# adjust the source list.
set(swiftDemanglingSources
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Context.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Demangler.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/ManglingUtils.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/NodePrinter.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Punycode.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Remangler.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/NodeDumper.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Errors.cpp")
set(swiftDemanglingCRSources
  "${SWIFT_SOURCE_DIR}/lib/Demangling/CrashReporter.cpp")

set(swift_demangling_cflags)

if(SWIFT_RUNTIME_CRASH_REPORTER_CLIENT)
  list(APPEND swift_demangling_cflags
    "-DSWIFT_HAVE_CRASHREPORTERCLIENT=1")
endif()

# The old mangling support is only needed on platforms with ObjC.
if(SWIFT_STDLIB_ENABLE_OBJC_INTEROP)
  list(APPEND swiftDemanglingSources
    "${SWIFT_SOURCE_DIR}/lib/Demangling/OldDemangler.cpp"
    "${SWIFT_SOURCE_DIR}/lib/Demangling/OldRemangler.cpp"
  )
  list(APPEND swift_demangling_cflags -DSWIFT_SUPPORT_OLD_MANGLING=1)
else()
  list(APPEND swift_demangling_cflags -DSWIFT_SUPPORT_OLD_MANGLING=0)
endif()

if(SWIFT_STDLIB_HAS_TYPE_PRINTING)
  list(APPEND swift_demangling_cflags -DSWIFT_STDLIB_HAS_TYPE_PRINTING)
endif()

# Gold LTO is unsupported. To prevent tests from failing when building
# with LTO, force swiftDemangling library to compile without LTO for Linux.
add_swift_target_library(swiftDemangling OBJECT_LIBRARY
  ${swiftDemanglingSources}
  C_COMPILE_FLAGS
    -DswiftCore_EXPORTS
    ${swift_demangling_cflags}
  C_COMPILE_FLAGS_LINUX -fno-lto
  INSTALL_IN_COMPONENT never_install)

add_swift_target_library(swiftDemanglingCR OBJECT_LIBRARY
  ${swiftDemanglingCRSources}
  C_COMPILE_FLAGS
    -DswiftCore_EXPORTS
    ${swift_demangling_cflags}
  INSTALL_IN_COMPONENT never_install)
