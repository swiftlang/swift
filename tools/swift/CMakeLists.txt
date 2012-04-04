add_swift_executable(swift
  Immediate.cpp
  Immediate.h
  PrintingDiagnosticConsumer.cpp
  PrintingDiagnosticConsumer.h
  swift.cpp
  DEPENDS swiftIRGen swiftParse swiftSema swiftAST
  COMPONENT_DEPENDS bitwriter codegen ipo mcjit jit linker
                                        selectiondag ${LLVM_TARGETS_TO_BUILD})

install(TARGETS swift
  RUNTIME DESTINATION bin)
