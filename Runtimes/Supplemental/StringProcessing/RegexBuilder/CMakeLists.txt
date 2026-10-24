add_library(swiftRegexBuilder
  CharacterClass.swift
  Anchor.swift
  Builder.swift
  Algorithms.swift
  Variadics.swift
  DSL.swift)

set_target_properties(swiftRegexBuilder PROPERTIES
  Swift_MODULE_NAME RegexBuilder)

target_link_libraries(swiftRegexBuilder PRIVATE
  swift_RegexParser
  swift_StringProcessing
  swiftCore
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

emit_swift_interface(swiftRegexBuilder)
install(TARGETS swiftRegexBuilder
  ARCHIVE DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  FILE_SET swift_modules
    COMPONENT ${PROJECT_NAME}_development
    DESTINATION "${${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR}")

generate_plist(swiftRegexBuilder "${CMAKE_PROJECT_VERSION}" swiftRegexBuilder)
embed_manifest(swiftRegexBuilder)
