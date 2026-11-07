// RUN: %empty-directory(%t)
// RUN: %{python} -c "print('public let table: [UInt64] = [' + ', '.join(map(str, range(2048))) + ']')" > %t/ImageBase.swift
// RUN: %target-build-swift -emit-library -module-name ImageBase %t/ImageBase.swift -emit-module-path %t/ImageBase.swiftmodule -o %t/libImageBase.so -use-ld=lld -Xlinker --image-base=0x1000 -Xlinker -z -Xlinker max-page-size=0x1000 -Xlinker --no-rosegment -Xlinker --build-id=0x0123456789abcdef0123456789abcdef01234567
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -Onone -I %t -L %t -lImageBase -Xlinker -rpath -Xlinker %t -o %t/image-base.exe
// RUN: %target-run %t/image-base.exe | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=linux-gnu

// libImageBase.so is linked at 0x1000, so its lowest loadable segment doesn't
// start at virtual address 0.  The image map must still find its build ID.
// The table makes the library big enough that reading its notes from the
// wrong offset doesn't find them by chance.

import Runtime
import ImageBase

@main
struct ImageMapImageBase {
  static func main() {
    _ = table.count

    // CHECK: {{0x[0-9a-f]*-0x[0-9a-f]*}} 0123456789abcdef0123456789abcdef01234567 libImageBase.so {{.*[\\/]}}libImageBase.so
    for image in ImageMap.capture() where image.name == "libImageBase.so" {
      print(image)
    }
  }
}
