// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftinterface -enable-library-evolution -disable-objc-attr-requires-foundation-module -typecheck -module-name Swift -parse-stdlib -enable-experimental-cxx-interop -emit-clang-header-path %t/Swift.h  -experimental-skip-all-function-bodies
// RUN: %FileCheck %s < %t/Swift.h

// RUN: %check-interop-cxx-header-in-clang(%t/Swift.h -Wno-unused-private-field -Wno-unused-function)

// CHECK: namespace Swift {

// CHECK: class String final {
// CHECK-NEXT: public:
// CHECK-NEXT: inline ~String() {
// CHECK:  }
// CHECK-NEXT:  inline String(const String &other) {
// CHECK:  }
// CHECK-NEXT:  inline String(String &&) = default;
// CHECK-NEXT:  static inline String init();
// CHECK-NEXT:  #if defined(__OBJC__)
// CHECK-NEXT:  inline __attribute__((always_inline)) operator NSString * _Nonnull () const noexcept {
// CHECK-NEXT:    return (__bridge_transfer NSString *)(_impl::$sSS23_bridgeToObjectiveCImplyXlyF(_impl::swift_interop_passDirect_Swift_String(_getOpaquePointer())));
// CHECK-NEXT:   }
// CHECK-EMPTY:
// CHECK-NEXT:  #endif
// CHECK-NEXT: private:

// CHECK: } // namespace Swift
