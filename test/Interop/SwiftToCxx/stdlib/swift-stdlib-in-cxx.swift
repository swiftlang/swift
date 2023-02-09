// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftinterface -enable-library-evolution -disable-objc-attr-requires-foundation-module -typecheck -module-name Swift -parse-stdlib -enable-experimental-cxx-interop -emit-clang-header-path %t/Swift.h  -experimental-skip-all-function-bodies
// RUN: %FileCheck %s < %t/Swift.h

// RUN: %check-interop-cxx-header-in-clang(%t/Swift.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-private-field -Wno-unused-function -Wc++98-compat-extra-semi)

// CHECK: namespace Swift __attribute__((swift_private)) SWIFT_SYMBOL_MODULE("Swift") {

// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif
// CHECK-NEXT: class SWIFT_SYMBOL("s:Sq") Optional;

// CHECK: class SWIFT_SYMBOL("s:SS") String;

// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif
// CHECK-NEXT: class SWIFT_SYMBOL("s:Sa") Array;
// CHECK: template<class T_0_0>
// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif
// CHECK-NEXT: class SWIFT_SYMBOL("s:Sa") Array final {
// CHECK-NEXT: public:
// CHECK-NEXT: #ifndef __cpp_concepts
// CHECK-NEXT: static_assert(swift::isUsableInGenericContext<T_0_0>, "type cannot be used in a Swift generic context");
// CHECK-NEXT: #endif
// CHECK-NEXT: inline ~Array() {
// CHECK: }
// CHECK-NEXT: inline Array(const Array &other) {
// CHECK: }
// CHECK: static inline Array<T_0_0> init() SWIFT_SYMBOL({{.*}});
// CHECK: inline void append(const T_0_0& newElement) SWIFT_SYMBOL({{.*}});
// CHECK: inline T_0_0 remove(swift::Int index) SWIFT_SYMBOL({{.*}});
// CHECK: inline T_0_0 operator [](swift::Int index) const SWIFT_SYMBOL({{.*}});
// CHECK: inline swift::Int getCount() const SWIFT_SYMBOL({{.*}});
// CHECK: inline swift::Int getCapacity() const SWIFT_SYMBOL({{.*}});

// CHECK: template<class T_0_0>
// CHECK: template<class T_0_0>

// CHECK: template<class T_0_0>
// CHECK: template<class T_0_0>
// CHECK-NEXT: #ifdef __cpp_concepts
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0>
// CHECK-NEXT: #endif
// CHECK-NEXT: class SWIFT_SYMBOL({{.*}}) Optional final {
// CHECK-NEXT: public:
// CHECK-NEXT: #ifndef __cpp_concepts
// CHECK-NEXT: static_assert(swift::isUsableInGenericContext<T_0_0>, "type cannot be used in a Swift generic context");
// CHECK-NEXT: #endif
// CHECK-NEXT: inline ~Optional() {
// CHECK: }
// CHECK-NEXT: inline Optional(const Optional &other) {
// CHECK: }
// CHECK:   enum class cases {
// CHECK-NEXT: some SWIFT_SYMBOL({{.*}}),
// CHECK-NEXT: none
// CHECK-NEXT: };
// CHECK: inline bool isSome() const;
// CHECK: inline bool isNone() const;
// CHECK: inline T_0_0 getUnsafelyUnwrapped() const SWIFT_SYMBOL({{.*}});

// CHECK: class SWIFT_SYMBOL({{.*}}) String final {
// CHECK-NEXT: public:
// CHECK-NEXT: inline ~String() {
// CHECK:  }
// CHECK-NEXT:  inline String(const String &other) {
// CHECK:  }
// CHECK-NEXT:  inline String(String &&) { abort(); }
// CHECK-NEXT:  static inline String init() SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:  inline UTF8View getUtf8() const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:  inline void setUtf8(const UTF8View& newValue) SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:  inline bool isContiguousUTF8() const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:  #if defined(__OBJC__)
// CHECK-NEXT:  inline __attribute__((always_inline)) operator NSString * _Nonnull () const noexcept {
// CHECK-NEXT:    return (__bridge_transfer NSString *)(_impl::$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF(_impl::swift_interop_passDirect_Swift_String(_getOpaquePointer())));
// CHECK-NEXT:   }
// CHECK-NEXT:  static inline __attribute__((always_inline)) String init(NSString * _Nonnull nsString) noexcept {
// CHECK-NEXT:  auto result = _make();
// CHECK-NEXT:  auto res = _impl::$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ((__bridge void *)nsString);
// CHECK-NEXT:  memcpy(result._getOpaquePointer(), &res, sizeof(res));
// CHECK-NEXT:  return result;
// CHECK-NEXT:  }
// CHECK-EMPTY:
// CHECK-NEXT:  #endif
// CHECK-NEXT: #define SWIFT_CXX_INTEROP_STRING_MIXIN
// CHECK-NEXT: // Look for the C++ interop support header relative to clang's resource dir:
// CHECK-NEXT: //  '<toolchain>/usr/lib/clang/<version>/include/../../../swift/swiftToCxx'.
// CHECK-NEXT: #if __has_include(<../../../swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>)
// CHECK-NEXT: #include <../../../swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>
// CHECK-NEXT: #elif __has_include(<../../../../../lib/swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>)
// CHECK-NEXT: //  '<toolchain>/usr/local/lib/clang/<version>/include/../../../../../lib/swift/swiftToCxx'.
// CHECK-NEXT: #include <../../../../../lib/swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>
// CHECK-NEXT: // Alternatively, allow user to find the header using additional include path into '<toolchain>/lib/swift'.
// CHECK-NEXT: #elif __has_include(<swiftToCxx/_SwiftStdlibCxxOverlay.h>)
// CHECK-NEXT: #include <swiftToCxx/_SwiftStdlibCxxOverlay.h>
// CHECK-NEXT: #endif
// CHECK-NEXT: private:

// CHECK: class SWIFT_SYMBOL({{.*}}) UTF8View final {
// CHECK:  inline UTF8View(UTF8View &&) { abort(); }
// CHECK-NEXT: inline String_Index getStartIndex() const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:   inline String_Index getEndIndex() const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:   inline String_Index index(const String_Index& i, swift::Int n) const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:   inline Swift::Optional<String_Index> index(const String_Index& i, swift::Int n, const String_Index& limit) const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT:   inline swift::Int distance(const String_Index& i, const String_Index& j) const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT: inline uint8_t operator [](const String_Index& i) const SWIFT_SYMBOL({{.*}});
// CHECK:   inline String getDescription() const SWIFT_SYMBOL({{.*}});
// CHECK:   inline swift::Int getCount() const SWIFT_SYMBOL({{.*}});
// CHECK-NEXT: private:

// CHECK: #if __has_include(<../../../swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>)
// CHECK-NEXT: #include <../../../swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>
// CHECK-NEXT: #elif __has_include(<../../../../../lib/swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>)
// CHECK-NEXT: //  '<toolchain>/usr/local/lib/clang/<version>/include/../../../../../lib/swift/swiftToCxx'.
// CHECK-NEXT: #include <../../../../../lib/swift/swiftToCxx/_SwiftStdlibCxxOverlay.h>
// CHECK-NEXT: // Alternatively, allow user to find the header using additional include path into '<toolchain>/lib/swift'.
// CHECK-NEXT: #elif __has_include(<swiftToCxx/_SwiftStdlibCxxOverlay.h>)
// CHECK-NEXT: #include <swiftToCxx/_SwiftStdlibCxxOverlay.h>
// CHECK-NEXT: #endif

// CHECK: } // namespace Swift
