add_swift_executable(swift
  Frontend.cpp
  Immediate.cpp
  PrintingDiagnosticConsumer.cpp
  swift.cpp
  DEPENDS swiftIRGen swiftParse swiftSema swiftClangImporter swiftAST swiftSIL
          swiftSILGen
  COMPONENT_DEPENDS bitwriter codegen ipo jit linker mcjit asmparser
                                        selectiondag ${LLVM_TARGETS_TO_BUILD})

target_link_libraries(swift edit)

install(TARGETS swift
  RUNTIME DESTINATION bin)
