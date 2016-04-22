// RUN: %target-swift-frontend -emit-ir -disable-incremental-llvm-codegen %s

enum Singleton {
  case F(Singleton -> ())
}

enum Single {
  case F(Single -> ())
  case X
  case Y
}

enum Multi {
  case F(Multi -> ())
  case G(Multi -> ())
}
