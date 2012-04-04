// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 123ABC

// FIXME: The "import" shouldn't be necessary
import swift

// FIXME: main() should be implicitly defined.
func main() {
  if (true) {
    print(123)
    printChar(65)
    printChar(66)
    printChar(67)
    printChar(10)
  }
}
