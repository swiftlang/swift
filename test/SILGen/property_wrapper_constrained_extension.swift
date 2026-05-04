// RUN: %target-swift-emit-silgen -module-name X %s | %FileCheck %s

// Exercises a SILGen code path that would trigger an assertion when emitting
// a property wrapper initializer inside a class nested in a constrained
// extension, where the generic parameter is fully substituted (rdar://174669500)

struct Projection<T> {
  var value: T
}

@propertyWrapper
struct Published<Value> {
    var wrappedValue: Value
    init(wrappedValue: Value) { self.wrappedValue = wrappedValue }

    var projectedValue: Projection<Value> {
        Projection(value: wrappedValue)
    }

    init(projectedValue: Projection<Value>) {
      self.wrappedValue = projectedValue.value
    }
}

struct Container<T: Hashable> {
    var entries: [T: Int] = [:]
}

extension Container where T == String {
    class Inner {
    // CHECK-LABEL: sil hidden [ossa] @$s1X9ContainerVAASSRszrlE5InnerCAEySS_Gycfc
    // CHECK: // function_ref property wrapped field init accessor of Container<>.Inner.data
    // CHECK: [[INIT:%[0-9]+]] = function_ref @$s1X9ContainerVAASSRszrlE5InnerC4dataACySSGvpfF : $@convention(thin) (@owned Container<String>, @thick Container<String>.Inner.Type) -> @out Published<Container<String>>
    // CHECK: [[INIT_PA:%[0-9]+]] = partial_apply [callee_guaranteed] [[INIT]]
    // CHECK: // function_ref Container<>.Inner.data.setter
    // CHECK: [[SET:%[0-9]+]] = function_ref @$s1X9ContainerVAASSRszrlE5InnerC4dataACySSGvs : $@convention(method) (@owned Container<String>, @guaranteed Container<String>.Inner) -> ()
    // CHECK: [[SET_PA:%[0-9]+]] = partial_apply [callee_guaranteed] [on_stack] [[SET]]
    // CHECK: assign_or_init #Container.Inner.data, {{.*}} init [[INIT_PA]], set [[SET_PA]]
        init() {
            data = Container<String>()
        }

        @Published var data: Container<String>

        // For extra coverage only; not part of original issue.
        func asProjected() -> Projection<Container<String>> {
          return $data
        }
    }
}

// This is the generic version of the above.
extension Container {
    class Inner2 {
        init(asGeneric t: T.Type) {
            data2 = Container<T>()
        }

        @Published var data2: Container<T>

        // For extra coverage only; not part of original issue.
        func asProjected2() -> Projection<Container<T>> {
          return $data2
        }
    }
}
