add_library(swift_StringProcessing
  _CUnicode/UnicodeData.c
  _CUnicode/UnicodeScalarProps.c
  _CharacterClassModel.swift
  Executor.swift
  Capture.swift
  Unicode/NumberParsing.swift
  Unicode/ASCII.swift
  Unicode/CharacterProps.swift
  Unicode/Transcoding.swift
  Unicode/ScalarProps.swift
  Unicode/Comparison.swift
  Unicode/NecessaryEvils.swift
  Unicode/Validation.swift
  Unicode/UCD.swift
  Unicode/NFC.swift
  Unicode/Encodings.swift
  Unicode/Formatting.swift
  Unicode/WordBreaking.swift
  Unicode/Decoding.swift
  Unicode/CaseConversion.swift
  Algorithms/Matching/FirstMatch.swift
  Algorithms/Matching/MatchingCollectionSearcher.swift
  Algorithms/Matching/MatchResult.swift
  Algorithms/Matching/MatchReplace.swift
  Algorithms/Matching/Matches.swift
  Algorithms/Algorithms/FirstRange.swift
  Algorithms/Algorithms/Contains.swift
  Algorithms/Algorithms/Split.swift
  Algorithms/Algorithms/Trim.swift
  Algorithms/Algorithms/Ranges.swift
  Algorithms/Algorithms/StartsWith.swift
  Algorithms/Algorithms/Replace.swift
  Algorithms/Algorithms/SubstringSearcher.swift
  Algorithms/Searchers/CollectionSearcher.swift
  Algorithms/Searchers/ZSearcher.swift
  Algorithms/Consumers/CollectionConsumer.swift
  Algorithms/Consumers/FixedPatternConsumer.swift
  Regex/Options.swift
  Regex/Match.swift
  Regex/CustomComponents.swift
  Regex/Core.swift
  Regex/ASTConversion.swift
  Regex/DSLTree.swift
  Regex/AnyRegexOutput.swift
  LiteralPrinter.swift
  MatchingOptions.swift
  Compiler.swift
  ConsumerInterface.swift
  PrintAsPattern.swift
  Engine/MEBuiltins.swift
  Engine/Processor.swift
  Engine/MEProgram.swift
  Engine/MEQuantify.swift
  Engine/InstPayload.swift
  Engine/Backtracking.swift
  Engine/Instruction.swift
  Engine/Registers.swift
  Engine/MEBuilder.swift
  Engine/Structuralize.swift
  Engine/Tracing.swift
  Engine/Metrics.swift
  Engine/MECapture.swift
  ByteCodeGen.swift
  Utility/AsciiBitset.swift
  Utility/Traced.swift
  Utility/TypedIndex.swift
  Utility/Misc.swift
  Utility/TypedInt.swift
  Utility/Protocols.swift
  Utility/TypeVerification.swift
  Utility/RegexFactory.swift
  Utility/ASTBuilder.swift)

set_target_properties(swift_StringProcessing PROPERTIES
  Swift_MODULE_NAME _StringProcessing)

target_link_libraries(swift_StringProcessing PRIVATE
    swift_RegexParser
    swiftCore)

install(TARGETS swift_StringProcessing
  ARCHIVE DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_StringProcessing)
install_swift_interface(swift_StringProcessing)

embed_manifest(swift_StringProcessing)
