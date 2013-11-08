add_swift_executable(swift
  Helpers.cpp
  Immediate.cpp
  swift.cpp
  DEPENDS swiftIRGen swiftSIL swiftSILGen swiftSILPasses swiftSerialization
          swiftFrontend swiftClangImporter swiftIDE
  COMPONENT_DEPENDS bitreader bitwriter codegen ipo jit linker mcjit asmparser
                    irreader selectiondag ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(swift edit ${CORE_FOUNDATION})

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}"
                   -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE_PATH}" )
endif()

if (SWIFT_SUBMIT_VERSION)
  add_definitions(-DSWIFT_VERSION_STRING="swift-${SWIFT_SUBMIT_VERSION}.${SWIFT_SUBMIT_SUBVERSION}")
endif()

# Add a run-path so that the interpreter can find the swift runtime libraries.
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set_target_properties(swift
    PROPERTIES
    LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib/swift")
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)

# Add a symlink to clang's includes so the built swift 
# can find its resource dir when run in place.
add_custom_command(TARGET swift POST_BUILD
  COMMAND mkdir "-p" "${SWIFTLIB_DIR}/swift"
  COMMAND ln "-fns" "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang" "${SWIFTLIB_DIR}/swift/clang")

# If we are building both the swift compiler and its stdlib, also add symlinks 
# so the built compiler can find the built stdlib when run in place.
if (SWIFT_BUILD_TOOLS AND SWIFT_BUILD_STDLIB)
  add_custom_command(TARGET swift POST_BUILD
    COMMAND ln "-fns" "${SWIFTLIB_DIR}" "${CMAKE_BINARY_DIR}/bin/lib")
endif()
