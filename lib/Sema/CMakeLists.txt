add_swift_library(swiftSema
  NameBinding.cpp
  Scope.cpp
  Sema.cpp
  SemaBase.cpp
  SemaDecl.cpp
  SemaExpr.cpp
  SemaType.cpp
  TypeChecking.cpp
  DEPENDS swiftAST)
