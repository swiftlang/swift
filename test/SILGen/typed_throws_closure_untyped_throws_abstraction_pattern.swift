// RUN: %target-swift-emit-silgen %s | %FileCheck %s

public enum MyError: Error {
	case myError
}

func assignTypedThrowsClosureInUntypedContext() {
	var callback: (() throws(Error) -> ())?
	// Even though the closure literal here throws a concrete error type,
	// since we are assigning it directly to a function type context throwing
	// untyped `Error`, we peephole away the conversion to avoid a thunk and
	// emit the closure literal as throwing untyped `Error`.
	// CHECK-LABEL: sil {{.*}}@$s{{.*}}assignTypedThrowsClosureInUntypedContext{{.*}}7MyErrorOYKcfU_ :
	// CHECK-SAME:    -> @error any Error
	// CHECK:         [[CONCRETE_ERROR_BUF:%.*]] = alloc_stack $MyError
	// CHECK:         [[CONCRETE_ERROR:%.*]] = load [trivial] [[CONCRETE_ERROR_BUF]]
	// CHECK:         [[ABSTRACT_ERROR:%.*]] = alloc_existential_box
	// CHECK:         [[ABSTRACT_ERROR_PAYLOAD:%.*]] = project_existential_box $MyError in [[ABSTRACT_ERROR]]
	// CHECK:         store [[ABSTRACT_ERROR]] to [init] [[ABSTRACT_ERROR_BUF:%[0-9]+]]
	// CHECK:         store [[CONCRETE_ERROR]] to [trivial] [[ABSTRACT_ERROR_PAYLOAD]]
	// CHECK:         [[ERROR_TO_THROW:%.*]] = load [take] [[ABSTRACT_ERROR_BUF]]
	// CHECK:         throw [[ERROR_TO_THROW]]
	callback = {() throws(MyError) in
			throw .myError
	}
}

