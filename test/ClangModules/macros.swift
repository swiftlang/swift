// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -constraint-checker -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep macros.pcm
import macros

// FIXME: Decide the type macros should map to.

func circle_area(radius:Double) -> Double {
  return M_PI * radius * radius
}

func convertGLBool(b:Int) -> Bool {
  return b != GL_FALSE
}

func pixelFormat(alpha:Bool) -> Int {
  if alpha {
    return GL_RGBA
  } else {
    return GL_RGB
  }
}

func boundsCheckU32(x:Int) -> Bool {
  return x >= 0 && x <= UINT32_MAX
}

func boundsCheckS64(x:Int) -> Bool {
  return x >= INT64_MIN && x <= INT64_MAX
}

func isEOF(c:Int) -> Bool {
  return c == EOF
}
