// RUN: %target-run-simple-swift(%S/Inputs/main.swift %S/Inputs/consume-logging-metadata-value.swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future) | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios
// Executing on the simulator within __abort_with_payload with "No ABI plugin located for triple x86_64h-apple-ios -- shared libraries will not be registered!"
// UNSUPPORTED: CPU=x86_64 && OS=ios
// UNSUPPORTED: CPU=x86_64 && OS=tvos
// UNSUPPORTED: CPU=x86_64 && OS=watchos
// UNSUPPORTED: CPU=i386 && OS=watchos
// UNSUPPORTED: use_os_stdlib
// rdar://87772056
// UNSUPPORTED: CPU=arm64e && OS=ios

class WeakMutableInstanceMethodBox<Input, Output> {
    let line: UInt

    init(line: UInt = #line) {
      self.line = line
    }

    private var work: ((Input) -> Output?)?

    func bind<T: AnyObject>(to object: T, method: @escaping (T) -> (Input) -> Output?) {
        self.work = { [weak object] input in
            guard let object = object else { return nil }
            return method(object)(input)
        }
    }

    func call(_ input: Input) -> Output? {
        return work?(input)
    }
}

extension WeakMutableInstanceMethodBox : CustomStringConvertible {

  var description: String {
    "\(type(of: self)) @ \(line)"
  }

}

class MyWeakMutableInstanceMethodBox<Input, Output> : WeakMutableInstanceMethodBox<Input, Output> {

    override init(line: UInt = #line) {
        super.init(line: line)
    }

}

@inline(never)
func consume<Input, Output>(base: WeakMutableInstanceMethodBox<Input, Output>) {
  consume(base)
}

func consume<Input, Output>(derived: MyWeakMutableInstanceMethodBox<Input,  Output>) {
  consume(derived)
}

func doit() {
    // CHECK: [[SUPERCLASS_METADATA_INT_BOOL_ADDRESS:[0-9a-f]+]] WeakMutableInstanceMethodBox<Int, Bool> @ [[@LINE+1]]
    consume(WeakMutableInstanceMethodBox<Int, Bool>())
    // CHECK: [[SUPERCLASS_METADATA_INT_BOOL_ADDRESS]] WeakMutableInstanceMethodBox<Int, Bool> @ [[@LINE+1]]
    consume(base: WeakMutableInstanceMethodBox<Int, Bool>())
    // CHECK: [[SUPERCLASS_METADATA_DOUBLE_FLOAT_ADDRESS:[0-9a-f]+]] WeakMutableInstanceMethodBox<Double, Float> @ [[@LINE+1]]
    consume(WeakMutableInstanceMethodBox<Double, Float>())
    // CHECK: [[SUPERCLASS_METADATA_DOUBLE_FLOAT_ADDRESS]] WeakMutableInstanceMethodBox<Double, Float> @ [[@LINE+1]]
    consume(base: WeakMutableInstanceMethodBox<Double, Float>())

    // CHECK: [[SUBCLASS_METADATA_INT_BOOL_ADDRESS:[0-9a-f]+]] MyWeakMutableInstanceMethodBox<Int, Bool> @ [[@LINE+1]]
    consume(MyWeakMutableInstanceMethodBox<Int, Bool>())
    // CHECK: [[SUPERCLASS_METADATA_INT_BOOL_ADDRESS]] MyWeakMutableInstanceMethodBox<Int, Bool> @ [[@LINE+1]]
    consume(base: MyWeakMutableInstanceMethodBox<Int, Bool>())
    // CHECK: [[SUBCLASS_METADATA_INT_BOOL_ADDRESS]] MyWeakMutableInstanceMethodBox<Int, Bool> @ [[@LINE+1]]
    consume(derived: MyWeakMutableInstanceMethodBox<Int, Bool>())

    // CHECK: [[SUBCLASS_METADATA_DOUBLE_FLOAT_ADDRESS:[0-9a-f]+]] MyWeakMutableInstanceMethodBox<Double, Float>
    consume(MyWeakMutableInstanceMethodBox<Double, Float>())
    // CHECK: [[SUPERCLASS_METADATA_DOUBLE_FLOAT_ADDRESS]] MyWeakMutableInstanceMethodBox<Double, Float>
    consume(base: MyWeakMutableInstanceMethodBox<Double, Float>())
    // CHECK: [[SUBCLASS_METADATA_DOUBLE_FLOAT_ADDRESS]] MyWeakMutableInstanceMethodBox<Double, Float>
    consume(derived: MyWeakMutableInstanceMethodBox<Double, Float>())
}

