set(sources
  AssertionReporting.h
  CoreFoundationShims.h
  FoundationShims.h
  GlobalObjects.h
  HeapObject.h
  KeyPath.h
  LibcOverlayShims.h
  LibcShims.h
  Random.h
  RefCount.h
  RuntimeShims.h
  RuntimeStubs.h
  SwiftStdbool.h
  SwiftStddef.h
  SwiftStdint.h
  System.h
  Target.h
  ThreadLocalStorage.h
  UnicodeShims.h
  Visibility.h

  DispatchOverlayShims.h
  NetworkOverlayShims.h
  OSOverlayShims.h
  ObjectiveCOverlayShims.h
  SafariServicesOverlayShims.h
  UIKitOverlayShims.h
  XCTestOverlayShims.h
  XPCOverlayShims.h

  CoreFoundationOverlayShims.h
  CFCharacterSetShims.h
  CFHashingShims.h

  FoundationOverlayShims.h
  FoundationShimSupport.h
  NSCalendarShims.h
  NSCharacterSetShims.h
  NSCoderShims.h
  NSDataShims.h
  NSDictionaryShims.h
  NSErrorShims.h
  NSFileManagerShims.h
  NSIndexPathShims.h
  NSIndexSetShims.h
  NSKeyedArchiverShims.h
  NSLocaleShims.h
  NSTimeZoneShims.h
  NSUndoManagerShims.h

  module.modulemap
  )

if ("${LLVM_PACKAGE_VERSION}" STREQUAL "")
  message(FATAL_ERROR
          "LLVM_PACKAGE_VERSION must be set before including subdirectories")
endif()

# Symlink in the Clang headers.
# First extract the "version" used for Clang's resource directory.
string(REGEX MATCH "[0-9]+\\.[0-9]+(\\.[0-9]+)?" CLANG_VERSION
  "${LLVM_PACKAGE_VERSION}")

# Function to find LLVM or Clang headers
function(find_llvm_clang_headers suffix description out_var)
  set(headers_locations
      "${LLVM_LIBRARY_DIR}${suffix}"

      "${SWIFT_PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib${suffix}"
      "${SWIFT_PATH_TO_CLANG_BUILD}/${LLVM_BUILD_TYPE}/lib${suffix}")

  set(headers_location)
  foreach(loc ${headers_locations})
    if(EXISTS "${loc}")
      set(headers_location "${loc}")
      set(${out_var} "${loc}" PARENT_SCOPE)
      break()
    endif()
  endforeach()
  if("${headers_location}" STREQUAL "")
    message(FATAL_ERROR "${description} headers were not found in any of the following locations: ${headers_locations}")
  endif()
endfunction()

if(SWIFT_BUILT_STANDALONE)
  find_llvm_clang_headers("/clang/${CLANG_VERSION}" "Clang" clang_headers_location)
else() # NOT SWIFT_BUILT_STANDALONE
  set(clang_headers_location "${LLVM_LIBRARY_DIR}/clang/${CLANG_VERSION}")
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  set(cmake_symlink_option copy_directory)
else()
  set(cmake_symlink_option create_symlink)
endif()

add_custom_command_target(unused_var
                          OUTPUT
                            ${SWIFT_LIBDIR}/clang
                          CUSTOM_TARGET_NAME
                            swift-clang-resource-dir-symlink
                          COMMAND
                            ${CMAKE_COMMAND} -E make_directory ${SWIFTLIB_DIR}
                          COMMAND
                            ${CMAKE_COMMAND} -E ${cmake_symlink_option} ${clang_headers_location} ${SWIFTLIB_DIR}/clang)

add_custom_command_target(unused_var
                          OUTPUT
                            ${SWIFT_LIBDIR}/shims
                          CUSTOM_TARGET_NAME
                            swift-shim-headers
                          COMMAND
                            ${CMAKE_COMMAND} -E make_directory ${SWIFTLIB_DIR}/shims
                          COMMAND
                            ${CMAKE_COMMAND} -E copy_if_different ${sources} ${SWIFTLIB_DIR}/shims/
                          DEPENDS
                            ${sources})

swift_install_in_component(compiler
                           FILES ${sources}
                           DESTINATION "lib/swift/shims"
                           COMPONENT swift-shim-headers)
add_llvm_install_targets(install-swift-shim-headers
                         COMPONENT swift-shim-headers)

# Install Clang headers under the Swift library so that an installed Swift's
# module importer can find the compiler headers corresponding to its Clang.
swift_install_in_component(clang-builtin-headers
    DIRECTORY "${clang_headers_location}/"
    DESTINATION "lib/swift/clang"
    PATTERN "*.h")

swift_install_symlink(clang ../clang/${CLANG_VERSION}
                      DESTINATION "lib/swift"
                      COMPONENT clang-resource-dir-symlink)

# Possibly install Clang headers under Clang's resource directory in case we
# need to use a different version of the headers than the installed Clang. This
# should be used in conjunction with clang-resource-dir-symlink.
swift_install_in_component(clang-builtin-headers-in-clang-resource-dir
    DIRECTORY "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang"
    DESTINATION "lib"
    PATTERN "*.h")
