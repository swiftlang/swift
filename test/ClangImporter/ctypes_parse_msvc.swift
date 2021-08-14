// RUN: %target-swift-frontend -Xcc -fms-extensions -import-objc-header %S/Inputs/ctypes_msvc.h -typecheck -verify %s

_ = T().uc
_ = T(S(uc: 0))
