
add_swift_host_library(LibSwiftSIL
  SHARED
  PURE_SWIFT
  Argument.swift
  BasicBlock.swift
  Function.swift
  GlobalVariable.swift
  Instruction.swift
  Location.swift
  Operand.swift
  Registration.swift
  Type.swift
  Utils.swift
  Value.swift)

target_link_libraries(LibSwiftSIL
  PRIVATE
  swiftSIL)
