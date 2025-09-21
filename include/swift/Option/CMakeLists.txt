set(LLVM_TARGET_DEFINITIONS Options.td)
swift_tablegen(Options.inc -gen-opt-parser-defs)
swift_add_public_tablegen_target(SwiftOptionDefinitions)
