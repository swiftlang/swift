
gyb_expand(tgmath.swift.gyb tgmath.swift)
gyb_expand(WASILibc.swift.gyb WASILibc.swift)

add_library(swiftWASILibc
  Platform.swift
  POSIXError.swift
  tgmath.swift
  TiocConstants.swift
  WASILibc.swift)
set_target_properties(swiftWASILibc 
  Swift_MODULE_NAME WASILibc)
target_compile_definitions(swiftWASILibc 
  $<$<.   :${SwiftOverlay_ENABLE_REFLECTION}>:SWIFT_ENABLE_REFLECTION>)
target_link_libraries(swiftWASILibc 
  SwiftWASILibc)
target_link_libraries(swiftWASILibc 
  swiftCore)

     target_include_directories(swiftWASILibc 
  "$<$<.              :Swift>:$<.              :${CMAKE_CURRENT_BINARY_DIR}>$<.              :$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")

install(.     swiftWASILibc
         SwiftOverlayTargets
   "${SwiftOverlay_INSTALL_LIBDIR}"
   "${SwiftOverlay_INSTALL_LIBDIR}"
   "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftWASILibc)
install_swift_interface(swiftWASILibc)
