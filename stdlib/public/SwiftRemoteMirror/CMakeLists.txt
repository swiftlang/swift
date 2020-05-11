# libswiftRemoteMirror.dylib should not have runtime dependencies; it's
# always built as a shared library.
if(SWIFT_BUILD_DYNAMIC_STDLIB)
  add_swift_target_library(swiftRemoteMirror
                           SHARED DONT_EMBED_BITCODE NOSWIFTRT
                           SwiftRemoteMirror.cpp
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/ErrorHandling.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/Hashing.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/MemAlloc.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/SmallPtrSet.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/SmallVector.cpp"
                           "${SWIFT_SOURCE_DIR}/stdlib/public/LLVMSupport/StringRef.cpp"
                           LINK_LIBRARIES
                             swiftReflection
                           C_COMPILE_FLAGS
                             ${SWIFT_RUNTIME_CXX_FLAGS} -DswiftRemoteMirror_EXPORTS
                           LINK_FLAGS
                             ${SWIFT_RUNTIME_LINK_FLAGS}
                           SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
                           INSTALL_IN_COMPONENT
                             swift-remote-mirror)
endif()
