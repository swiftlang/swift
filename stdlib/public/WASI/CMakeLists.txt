add_swift_target_library(swiftWasiPthread STATIC IS_STDLIB
  Pthread.cpp
  INSTALL_IN_COMPONENT stdlib)
