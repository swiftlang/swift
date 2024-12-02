// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/resources/linux/armv7 %t/sdk/usr/include %t/sdk/usr/lib/swift/linux/armv7
// RUN: touch %t/sdk/usr/include/{inttypes,stdint,unistd}.h

// RUN: touch %t/resources/linux/armv7/{SwiftGlibc.h,glibc.modulemap}
// RUN: touch %t/sdk/usr/lib/swift/linux/armv7/{SwiftGlibc.h,glibc.modulemap}
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target armv7-unknown-linux-gnueabihf -sdk %t/sdk -resource-dir %t/resources 2>&1 | %FileCheck -check-prefix=CHECK-LINUX %s

// RUN: cp %S/../../stdlib/public/Cxx/{cxxshim/libcxxshim.modulemap,libstdcxx/libstdcxx.h,libstdcxx/libstdcxx.modulemap} %t/resources/linux
// RUN: %target-swift-frontend %s -typecheck -parse-stdlib -dump-clang-diagnostics -resource-dir %t/resources -cxx-interoperability-mode=default 2>&1 | %FileCheck -check-prefix=CHECK-CXX -check-prefix=CHECK-%target-os-CXX %s

// RUN: mkdir -p %t/resources/android/aarch64 %t/sdk/usr/lib/swift/android/aarch64
// RUN: cp %S/../../stdlib/public/Platform/{SwiftAndroidNDK.h,SwiftBionic.h,android.modulemap} %t/resources/android/aarch64
// RUN: cp %S/../../stdlib/public/Platform/{SwiftAndroidNDK.h,SwiftBionic.h,android.modulemap} %t/sdk/usr/lib/swift/android/aarch64
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target aarch64-unknown-linux-android -sdk %t/sdk -resource-dir %t/resources 2>&1 | %FileCheck -check-prefix=CHECK-ANDROID %s

// RUN: cp %S/../../stdlib/public/Cxx/cxxshim/libcxxshim.modulemap %t/resources/android
// RUN: cp %S/../../stdlib/public/Cxx/cxxshim/libcxxshim.modulemap %t/sdk/usr/lib/swift/android
// RUN: %swift %s -typecheck -parse-stdlib -dump-clang-diagnostics -target aarch64-unknown-linux-android -sdk %t/sdk -resource-dir %t/resources -cxx-interoperability-mode=default 2>&1 | %FileCheck -check-prefix=CHECK-ANDROID-CXX %s

// CHECK-LINUX: clang importer redirected file mappings:
// CHECK-LINUX-NEXT: mapping real file '{{.*}}{{/|\\}}resources{{/|\\}}linux{{/|\\}}armv7{{/|\\}}glibc.modulemap' to virtual file '{{.*}}{{/|\\}}sdk{{/|\\}}usr{{/|\\}}include{{/|\\}}module.modulemap'
// CHECK-LINUX-NEXT: mapping real file '{{.*}}{{/|\\}}resources{{/|\\}}linux{{/|\\}}armv7{{/|\\}}SwiftGlibc.h' to virtual file '{{.*}}{{/|\\}}sdk{{/|\\}}usr{{/|\\}}include{{/|\\}}SwiftGlibc.h'

// CHECK-CXX: clang importer redirected file mappings:
// CHECK-linux-gnu-CXX: mapping real file '{{.*}}/resources/linux/libstdcxx.h' to virtual file '{{.*}}/usr/include/c++/{{.*}}/libstdcxx.h'
// CHECK-linux-gnu-CXX: clang importer overriding file '{{.*}}/usr/include/c++/{{.*}}/module.modulemap' with the following contents:
// CHECK-linux-gnu-CXX-NEXT: --- libstdcxx.modulemap ---
// CHECK-linux-gnu-CXX: Currently libstdc++ does not have a module map. To work around
// CHECK-linux-gnu-CXX-NEXT: this, Swift provides its own module map for libstdc++.
// CHECK-linux-gnu-CXX: header "libstdcxx.h"
// CHECK-linux-gnu-CXX: clang importer driver args: {{.*}}'-fmodule-map-file={{.*}}resources/linux/libcxxshim.modulemap'

// CHECK-ANDROID: clang importer redirected file mappings:
// CHECK-ANDROID-NEXT: mapping real file '{{.*}}{{/|\\}}resources{{/|\\}}android{{/|\\}}aarch64{{/|\\}}android.modulemap' to virtual file '{{.*}}{{/|\\}}sdk{{/|\\}}usr{{/|\\}}include{{/|\\}}module.modulemap'
// CHECK-ANDROID-NEXT: mapping real file '{{.*}}{{/|\\}}resources{{/|\\}}android{{/|\\}}aarch64{{/|\\}}SwiftAndroidNDK.h' to virtual file '{{.*}}{{/|\\}}sdk{{/|\\}}usr{{/|\\}}include{{/|\\}}SwiftAndroidNDK.h'
// CHECK-ANDROID-NEXT: mapping real file '{{.*}}{{/|\\}}resources{{/|\\}}android{{/|\\}}aarch64{{/|\\}}SwiftBionic.h' to virtual file '{{.*}}{{/|\\}}sdk{{/|\\}}usr{{/|\\}}include{{/|\\}}SwiftBionic.h'

// CHECK-ANDROID-CXX: clang importer driver args: {{.*}}'-fmodule-map-file={{.*}}resources{{/|\\}}android{{/|\\}}libcxxshim.modulemap'
