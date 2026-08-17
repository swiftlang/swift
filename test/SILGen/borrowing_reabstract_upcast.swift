// RUN: %target-swift-emit-silgen %s | %FileCheck %s

enum ColorParsingError: Error {
    case unknown(String)
}

enum Color {
    // CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast5ColorO3hexyACSSYKFZ : $@convention(method) (@guaranteed String, @thin Color.Type) -> (@owned Color, @error any Error)
    static func hex(_ hex: borrowing String) throws -> Self {
        // We ensure that the borrowing parameter is correctly copied/unwrapped when passed to the enum payload without crashing.
        // CHECK: bb0(%0 : @guaranteed $String, %1 : $@thin Color.Type):
        // CHECK: [[WRAPPER:%.*]] = guaranteed_copyable_to_moveonlywrapper [guaranteed] %0 : $String
        // CHECK: [[COPY:%.*]] = copy_value [[WRAPPER]] : $@sil_moveonlywrapped String
        // CHECK: [[UNWRAPPED:%.*]] = owned_moveonlywrapper_to_copyable [owned] [[COPY]] : $@sil_moveonlywrapped String
        // CHECK: [[ENUM:%.*]] = enum $ColorParsingError, #ColorParsingError.unknown!enumelt, [[UNWRAPPED]] : $String
        // CHECK: [[ERR:%.*]] = alloc_existential_box $any Error, $ColorParsingError
        // CHECK: [[ERR_PROJ:%.*]] = project_existential_box $ColorParsingError in [[ERR]] : $any Error
        // CHECK: store [[ENUM]] to [init] [[ERR_PROJ]] : $*ColorParsingError
        // CHECK: throw [[ERR]] : $any Error
        throw ColorParsingError.unknown(hex)
    }
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast18testBorrowingError5errorySS_tKF : $@convention(thin) (@guaranteed String) -> @error any Error
func testBorrowingError(error: borrowing String) throws {
    // CHECK: [[WRAPPER:%.*]] = guaranteed_copyable_to_moveonlywrapper [guaranteed] %0 : $String
    // CHECK: [[COPY:%.*]] = copy_value [[WRAPPER]] : $@sil_moveonlywrapped String
    // CHECK: [[UNWRAPPED:%.*]] = owned_moveonlywrapper_to_copyable [owned] [[COPY]] : $@sil_moveonlywrapped String
    // CHECK: [[ERR:%.*]] = alloc_existential_box $any Error, $String
    // CHECK: [[ERR_PROJ:%.*]] = project_existential_box $String in [[ERR]] : $any Error
    // CHECK: store [[UNWRAPPED]] to [init] [[ERR_PROJ]] : $*String
    // CHECK: throw [[ERR]] : $any Error
    throw error
}

enum SimpleEnum {
    case string(String)
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast24testBorrowingReturnEnumyAA06SimpleG0OSSF : $@convention(thin) (@guaranteed String) -> @owned SimpleEnum
func testBorrowingReturnEnum(_ str: borrowing String) -> SimpleEnum {
    return .string(str)
}

struct GenericStruct<T> {
    var val: T
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast26testBorrowingGenericStructyAA0fG0VySSGSSF : $@convention(thin) (@guaranteed String) -> @owned GenericStruct<String>
func testBorrowingGenericStruct(_ str: borrowing String) -> GenericStruct<String> {
    return GenericStruct(val: str)
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast18testBorrowingArrayySaySSGSSF : $@convention(thin) (@guaranteed String) -> @owned Array<String>
func testBorrowingArray(_ str: borrowing String) -> [String] {
    return [str]
}

protocol P {}
extension String: P {}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast23testBorrowingExistentialyAA1P_pSSF : $@convention(thin) (@guaranteed String) -> @out any P
func testBorrowingExistential(_ str: borrowing String) -> any P {
    return str
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast20testBorrowingTuple11ySS_SStSSF : $@convention(thin) (@guaranteed String) -> (@owned String, @owned String)
func testBorrowingTuple1(_ str: borrowing String) -> (String, String) {
    return (str, str)
}

enum NestedEnum {
    case inner(SimpleEnum)
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast22testBorrowingNestedEnumyAA0fG0OSSF : $@convention(thin) (@guaranteed String) -> @owned NestedEnum
func testBorrowingNestedEnum(_ str: borrowing String) -> NestedEnum {
    return .inner(.string(str))
}

indirect enum IndirectEnum {
    case string(String)
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast24testBorrowingIndirectEnumyAA0fG0OSSF : $@convention(thin) (@guaranteed String) -> @owned IndirectEnum
func testBorrowingIndirectEnum(_ str: borrowing String) -> IndirectEnum {
    return .string(str)
}

enum GenericEnum<T> {
    case val(T)
}

// CHECK-LABEL: sil hidden [ossa] @$s28borrowing_reabstract_upcast23testBorrowingGenericEnumyAA0fG0OySSGSSF : $@convention(thin) (@guaranteed String) -> @owned GenericEnum<String>
func testBorrowingGenericEnum(_ str: borrowing String) -> GenericEnum<String> {
    return .val(str)
}

// Ensure negative cases emit diagnostics and don't ICE
// RUN: not %target-swift-emit-silgen %s -D NEGATIVE 2>&1 | %FileCheck %s --check-prefix=NEGATIVE

#if NEGATIVE
func testConsumeBorrowing(_ str: borrowing String) {
    // NEGATIVE: error: 'str' is borrowed and cannot be consumed
    let _: String = consume str
}
#endif
