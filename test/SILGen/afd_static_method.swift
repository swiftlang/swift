// RUN: %target-swift-emit-silgen -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -verify -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -module-name main %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes

// Ensure that address dependencies are properly lowered
// through static method calls in the same way they are for free function
// calls.

@_addressableForDependencies
struct Owner: ~Copyable {}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}10wrapStatic
// CHECK:       bb0([[OWNER:%.*]] : $*Owner):
@_lifetime(borrow owner)
func wrapStatic(_ owner: borrowing Owner) -> View {
	// CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[OWNER]]
	// CHECK:         [[METHOD:%.*]] = function_ref @$s{{.*}}4ViewV4into
	// CHECK:         apply [[METHOD]]([[MARK]],
    return View.into(owner) 
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}8wrapFree
// CHECK:       bb0([[OWNER:%.*]] : $*Owner):
@_lifetime(borrow owner)
func wrapFree(_ owner: borrowing Owner) -> View {
	// CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[OWNER]]
	// CHECK:         [[METHOD:%.*]] = function_ref @$s{{.*}}8viewInto
	// CHECK:         apply [[METHOD]]([[MARK]])
    return viewInto(owner) 
}

@_lifetime(borrow owner)
func viewInto(_ owner: borrowing Owner) -> View {
	fatalError()
}

struct View: ~Escapable {
	@_lifetime(borrow owner)
	static func into(_ owner: borrowing Owner) -> View {
		fatalError()
	}
}

