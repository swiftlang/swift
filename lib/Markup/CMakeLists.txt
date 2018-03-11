add_swift_library(swiftMarkup STATIC
  AST.cpp
  LineList.cpp
  Markup.cpp
  
  LLVM_COMPONENT_DEPENDS
    support

  LINK_LIBRARIES
    swiftBasic
    libcmark_static)

