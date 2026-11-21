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

install(TARGETS swiftRegexBuilder
  ARCHIVE DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftRegexBuilder)
if(${PROJECT_NAME}_ENABLE_SYMBOL_GRAPHS)
  target_link_libraries(swiftRegexBuilder PRIVATE SwiftCore_SymbolGraph)
  set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/symbol-graphs")
endif()
install_swift_interface(swiftRegexBuilder)

generate_plist(swiftRegexBuilder "${CMAKE_PROJECT_VERSION}" swiftRegexBuilder)
embed_manifest(swiftRegexBuilder)
