add_library(swift_RegexParser
  Regex/TreeProtocols.swift
  Regex/Parse/DelimiterLexing.swift
  Regex/Parse/Source.swift
  Regex/Parse/CharacterPropertyClassification.swift
  Regex/Parse/LexicalAnalysis.swift
  Regex/Parse/SyntaxOptions.swift
  Regex/Parse/CaptureList.swift
  Regex/Parse/CompilerInterface.swift
  Regex/Parse/CaptureStructure.swift
  Regex/Parse/Parse.swift
  Regex/Parse/SourceLocation.swift
  Regex/Parse/Diagnostics.swift
  Regex/Parse/Sema.swift
  Regex/Printing/RenderRanges.swift
  Regex/Printing/PrettyPrinter.swift
  Regex/Printing/PrintAsCanonical.swift
  Regex/Printing/DumpAST.swift
  Regex/AST/ASTAction.swift
  Regex/AST/AST.swift
  Regex/AST/CustomCharClass.swift
  Regex/AST/Quantification.swift
  Regex/AST/Conditional.swift
  Regex/AST/MatchingOptions.swift
  Regex/AST/ASTProtocols.swift
  Regex/AST/Group.swift
  Regex/AST/Atom.swift
  Utility/TypeConstruction.swift
  Utility/Misc.swift
  Utility/AllScalars.swift
  Utility/Errors.swift
  Utility/MissingUnicode.swift)

target_link_libraries(swift_RegexParser PRIVATE
  swiftCore)

set_target_properties(swift_RegexParser PROPERTIES
  Swift_MODULE_NAME _RegexParser)

install(TARGETS swift_RegexParser
  ARCHIVE DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftStringProcessing_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_RegexParser)
install_swift_interface(swift_RegexParser)

embed_manifest(swift_RegexParser)
