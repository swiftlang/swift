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
  swiftCore)
