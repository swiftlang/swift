add_swift_executable(swift
  swift.cpp
  DEPENDS swiftParse swiftSema swiftAST 
  COMPONENT_DEPENDS bitwriter codegen ipo selectiondag ${LLVM_TARGETS_TO_BUILD})

install(TARGETS swift
  RUNTIME DESTINATION bin)
