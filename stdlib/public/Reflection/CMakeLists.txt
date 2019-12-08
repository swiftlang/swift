set(swiftReflection_SOURCES
  MetadataSource.cpp
  TypeLowering.cpp
  TypeRef.cpp
  TypeRefBuilder.cpp
  "${SWIFT_SOURCE_DIR}/stdlib/public/runtime/LLVMSupport.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Context.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/OldDemangler.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Demangler.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/NodePrinter.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/ManglingUtils.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Punycode.cpp"
  "${SWIFT_SOURCE_DIR}/lib/Demangling/Remangler.cpp")

# When we're building with assertions, include the demangle node dumper to aid
# in debugging.
if (LLVM_ENABLE_ASSERTIONS)
  list(APPEND swiftReflection_SOURCES
    "${SWIFT_SOURCE_DIR}/lib/Demangling/NodeDumper.cpp")
endif(LLVM_ENABLE_ASSERTIONS)

if(SWIFT_BUILD_STDLIB)
  add_swift_target_library(swiftReflection STATIC
    ${swiftReflection_SOURCES}
    C_COMPILE_FLAGS ${SWIFT_RUNTIME_CXX_FLAGS} -DswiftCore_EXPORTS
    LINK_FLAGS ${SWIFT_RUNTIME_LINK_FLAGS}
    SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    INSTALL_IN_COMPONENT dev)
endif()

