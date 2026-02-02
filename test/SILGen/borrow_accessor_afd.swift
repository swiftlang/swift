// RUN: %target-swift-emit-silgen -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature AddressableTypes %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_AddressableTypes

// When the `self` and/or result type of a borrow accessor is addressable-for-
// dependencies, the corresponding parameter and/or result is indirect.

@_addressableForDependencies
struct AFD { var x: Int }

struct DirectContainer {
    let _value: AFD

	let _nonAFDValue: AnyObject

	// `self` and `result` are both AFD
	// CHECK-LABEL: sil{{.*}} @$s{{.*}}15DirectContainerV5value{{.*}}vb :
	// CHECK:       bb0([[SELF:%.*]] : $*DirectContainer):
	// CHECK:         [[FIELD_ADDR:%.*]] = struct_element_addr [[SELF]]
	// CHECK:         return [[FIELD_ADDR]]
    var value: AFD {
       borrow { return _value }
    }

	// `self` is AFD, but `result` is not
	// CHECK-LABEL: sil{{.*}} @$s{{.*}}15DirectContainerV11nonAFDValue{{.*}}vb :
	// CHECK:       bb0([[SELF:%.*]] : $*DirectContainer):
	// CHECK:         [[FIELD_ADDR:%.*]] = struct_element_addr [[SELF]]
	// CHECK:         [[FIELD:%.*]] = load_borrow [[FIELD_ADDR]]
	// CHECK:         [[FIELD_UNCHECKED:%.*]] = unchecked_ownership [[FIELD]]
	// CHECK:         return [[FIELD_UNCHECKED]]
    var nonAFDValue: AnyObject {
       borrow { return _nonAFDValue }
    }
}

class Box {
    let _value: AFD
	let _nonAFDValue: AnyObject

    init() {}
}

struct IndirectContainer {
    let box: Box

    // `self` is non-AFD, but `result` is
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}17IndirectContainerV5value{{.*}}vb :
    // CHECK:       bb0([[SELF:%.*]] : @guaranteed $IndirectContainer):
    // CHECK:         [[BOX:%.*]] = struct_extract [[SELF]]
    // CHECK:         [[FIELD_ADDR:%.*]] = ref_element_addr [[BOX]]
    // CHECK:         return [[FIELD_ADDR]]
    var value: AFD {
        borrow { return box._value }
    }

    // neither `self` nor `result` is AFD
    // CHECK-LABEL: sil{{.*}} @$s{{.*}}17IndirectContainerV11nonAFDValue{{.*}}vb :
    // CHECK:       bb0([[SELF:%.*]] : @guaranteed $IndirectContainer):
    // CHECK:         [[BOX:%.*]] = struct_extract [[SELF]]
    // CHECK:         [[FIELD_ADDR:%.*]] = ref_element_addr [[BOX]]
	// CHECK:         [[FIELD:%.*]] = load_borrow [[FIELD_ADDR]]
	// CHECK:         [[FIELD_UNCHECKED:%.*]] = unchecked_ownership [[FIELD]]
	// CHECK:         return [[FIELD_UNCHECKED]]
	var nonAFDValue: AnyObject {
		borrow { return box._nonAFDValue }
	}
}

