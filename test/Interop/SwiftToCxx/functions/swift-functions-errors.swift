// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -enable-experimental-cxx-interop -clang-header-expose-decls=has-expose-attr-or-stdlib -enable-experimental-feature GenerateBindingsForThrowingFunctionsInCXX -typecheck -verify -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h
// RUN: %FileCheck --check-prefix=ACCESSORS %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR -Wno-unused-function)

// REQUIRES: swift_feature_GenerateBindingsForThrowingFunctionsInCXX

// CHECK-LABEL: namespace Functions SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Functions") {

// CHECK-LABEL: namespace _impl {

// CHECK: SWIFT_EXTERN void $s9Functions18emptyThrowFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // emptyThrowFunction()
// CHECK: SWIFT_EXTERN void $s9Functions18testDestroyedErroryyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // testDestroyedError()
// CHECK: SWIFT_EXTERN void $s9Functions13throwFunctionyyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunction()
// CHECK: SWIFT_EXTERN void $s9Functions28throwFunctionWithNeverReturns0E0OyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithNeverReturn()
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions31throwFunctionWithPossibleReturnyS2iKF(ptrdiff_t a, SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithPossibleReturn(_:)
// CHECK: SWIFT_EXTERN ptrdiff_t $s9Functions23throwFunctionWithReturnSiyKF(SWIFT_CONTEXT void * _Nonnull _ctx, SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error) SWIFT_CALL; // throwFunctionWithReturn()


// CHECK: }

@_expose(Cxx)
public enum NaiveErrors : Error {
    case returnError
    case throwError

    public func getMessage() {
        print(self)
    }
}

@_expose(Cxx)
public func emptyThrowFunction() throws { print("passEmptyThrowFunction") }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> emptyThrowFunction() SWIFT_SYMBOL("s:9Functions18emptyThrowFunctionyyKF") {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18emptyThrowFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

class TestDestroyed {
  deinit {
    print("Test destroyed")
  }
}

@_expose(Cxx)
public struct DestroyedError : Error {
  let t = TestDestroyed()
}

@_expose(Cxx)
public func testDestroyedError() throws { throw DestroyedError() }

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> testDestroyedError() SWIFT_SYMBOL("s:9Functions18testDestroyedErroryyKF") {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions18testDestroyedErroryyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

@_expose(Cxx)
public struct ThrowingAccessors {
    let fail: Bool

    public init(fail: Bool) { self.fail = fail }

    public var value: Int {
        get throws {
            if fail { throw NaiveErrors.throwError }
            return 42
        }
    }

    public var isReady: Bool {
        get throws {
            if fail { throw NaiveErrors.throwError }
            return true
        }
    }

    public static var staticValue: Int {
        get throws { throw NaiveErrors.returnError }
    }

    public subscript(index: Int) -> Int {
        get throws {
            if fail { throw NaiveErrors.throwError }
            return index * 2
        }
    }
}

// ACCESSORS: class SWIFT_SYMBOL("s:9Functions17ThrowingAccessorsV") ThrowingAccessors final {
// ACCESSORS: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> getValue() const SWIFT_SYMBOL("s:9Functions17ThrowingAccessorsV5valueSivp");
// ACCESSORS-NEXT: SWIFT_INLINE_THUNK swift::ThrowingResult<bool> isReady() const SWIFT_SYMBOL("s:9Functions17ThrowingAccessorsV7isReadySbvp");
// ACCESSORS-NEXT: static SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> getStaticValue() SWIFT_SYMBOL("s:9Functions17ThrowingAccessorsV11staticValueSivpZ");
// ACCESSORS-NEXT: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> operator [](swift::Int index) const SWIFT_SYMBOL("s:9Functions17ThrowingAccessorsVyS2icig");

// ACCESSORS: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingAccessors::getValue() const {
// ACCESSORS-NEXT: void* opaqueError = nullptr;
// ACCESSORS: auto returnValue = Functions::_impl::$s9Functions17ThrowingAccessorsV5valueSivg(Functions::_impl::swift_interop_passDirect_Functions_bool_0_1(_getOpaquePointer()), _ctx, &opaqueError);
// ACCESSORS-NEXT: if (opaqueError != nullptr)
// ACCESSORS-NEXT: #ifdef __cpp_exceptions
// ACCESSORS-NEXT: throw (swift::Error(opaqueError));
// ACCESSORS-NEXT: #else
// ACCESSORS-NEXT: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// ACCESSORS-NEXT: #endif
// ACCESSORS-EMPTY:
// ACCESSORS-NEXT: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// ACCESSORS-NEXT: }

// ACCESSORS: SWIFT_INLINE_THUNK swift::ThrowingResult<bool> ThrowingAccessors::isReady() const {
// ACCESSORS: auto returnValue = Functions::_impl::$s9Functions17ThrowingAccessorsV7isReadySbvg({{.*}}, &opaqueError);
// ACCESSORS: return swift::Expected<bool>(swift::Error(opaqueError));

// ACCESSORS: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingAccessors::getStaticValue() {
// ACCESSORS: auto returnValue = Functions::_impl::$s9Functions17ThrowingAccessorsV11staticValueSivgZ(_ctx, &opaqueError);
// ACCESSORS-NEXT: if (opaqueError != nullptr)

// ACCESSORS: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> ThrowingAccessors::operator [](swift::Int index) const SWIFT_SYMBOL("s:9Functions17ThrowingAccessorsVyS2icig") {
// ACCESSORS: auto returnValue = Functions::_impl::$s9Functions17ThrowingAccessorsVyS2icig(index, {{.*}}, &opaqueError);
// ACCESSORS-NEXT: if (opaqueError != nullptr)

@_expose(Cxx)
public func throwFunction() throws {
    print("passThrowFunction")
    throw NaiveErrors.throwError
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> throwFunction() SWIFT_SYMBOL("s:9Functions13throwFunctionyyKF") {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: _impl::$s9Functions13throwFunctionyyKF(_ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithNeverReturn() throws -> Never {
    print("passThrowFunctionWithNeverReturn")
    throw NaiveErrors.returnError
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<void> throwFunctionWithNeverReturn() SWIFT_SYMBOL("s:9Functions28throwFunctionWithNeverReturns0E0OyKF") SWIFT_NORETURN_EXCEPT_ERRORS {
// CHECK-NEXT: void* opaqueError = nullptr;
// CHECK-NEXT: void* _ctx = nullptr;
// CHECK-NEXT: _impl::$s9Functions28throwFunctionWithNeverReturns0E0OyKF(_ctx, &opaqueError);
// CHECK-NEXT: if (opaqueError != nullptr)
// CHECK-NEXT: #ifdef __cpp_exceptions
// CHECK-NEXT: throw (swift::Error(opaqueError));
// CHECK-NEXT: #else
// CHECK-NEXT: return swift::Expected<void>(swift::Error(opaqueError));
// CHECK-NEXT: #endif
// CHECK-NEXT: abort();
// CHECK-NEXT: }

@_expose(Cxx)
public func throwFunctionWithPossibleReturn(_ a: Int) throws -> Int {
    print("passThrowFunctionWithPossibleReturn")
    if (a == 0) {
        throw NaiveErrors.returnError
    }
    return 0
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> throwFunctionWithPossibleReturn(swift::Int a) SWIFT_SYMBOL("s:9Functions31throwFunctionWithPossibleReturnyS2iKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = Functions::_impl::$s9Functions31throwFunctionWithPossibleReturnyS2iKF(a, _ctx, &opaqueError);
// CHECK: if (opaqueError != nullptr)
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK: }

@_expose(Cxx)
public func throwFunctionWithReturn() throws -> Int {
    print("passThrowFunctionWithReturn")
    throw NaiveErrors.returnError
    return 0
}

// CHECK: SWIFT_INLINE_THUNK swift::ThrowingResult<swift::Int> throwFunctionWithReturn() SWIFT_SYMBOL("s:9Functions23throwFunctionWithReturnSiyKF") SWIFT_WARN_UNUSED_RESULT {
// CHECK: void* opaqueError = nullptr;
// CHECK: void* _ctx = nullptr;
// CHECK: auto returnValue = Functions::_impl::$s9Functions23throwFunctionWithReturnSiyKF(_ctx, &opaqueError);
// CHECK: #ifdef __cpp_exceptions
// CHECK: throw (swift::Error(opaqueError));
// CHECK: #else
// CHECK: return swift::Expected<swift::Int>(swift::Error(opaqueError));
// CHECK: #endif
// CHECK: return SWIFT_RETURN_THUNK(swift::Int, returnValue);
// CHECK: }
