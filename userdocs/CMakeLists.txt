swift_install_in_component(DIRECTORY diagnostics
                           DESTINATION "share/doc/swift"
                           COMPONENT compiler)

# 'update-doc-index' regenerates the auto-generated files
# 'userdocs/diagnostics/diagnostic-groups.md' and
# 'userdocs/diagnostics/upcoming-language-features.md' from the current
# 'DiagnosticGroups.def' and the diagnostic markdown files. 
#
# This target writes back into the source tree, so it is intentionally not
# part of the default build.
if(SWIFT_NATIVE_SWIFT_TOOLS_PATH)
  set(_swift_doc_index_swiftc "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swiftc")
elseif(CMAKE_Swift_COMPILER)
  set(_swift_doc_index_swiftc "${CMAKE_Swift_COMPILER}")
else()
  set(_swift_doc_index_swiftc "swiftc")
endif()

# 'generate-doc-index.swift' imports Foundation, so we need an SDK on Apple
# platforms; otherwise swiftc fails with "no such module 'Foundation'".
if(APPLE AND CMAKE_OSX_SYSROOT)
  set(_swift_doc_index_sdk_args -sdk "${CMAKE_OSX_SYSROOT}")
else()
  set(_swift_doc_index_sdk_args "")
endif()

add_custom_command(
  OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/generate-doc-index
  COMMAND ${_swift_doc_index_swiftc}
            ${_swift_doc_index_sdk_args}
            -swift-version 5
            -enable-upcoming-feature BareSlashRegexLiterals
            ${SWIFT_SOURCE_DIR}/utils/generate-doc-index.swift
            -o ${CMAKE_CURRENT_BINARY_DIR}/generate-doc-index
  DEPENDS ${SWIFT_SOURCE_DIR}/utils/generate-doc-index.swift
  COMMENT "Building generate-doc-index")

add_custom_target(update-doc-index
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/generate-doc-index
            ${SWIFT_SOURCE_DIR}
            ${SWIFT_SOURCE_DIR}/userdocs/diagnostics
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/generate-doc-index
  COMMENT "Regenerating userdocs/diagnostics index files"
  USES_TERMINAL)
