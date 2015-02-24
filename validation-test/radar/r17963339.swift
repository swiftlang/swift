// RUN: %target-run-simple-swift

// XFAIL: linux

protocol MyClassDelegate: class {
    func didFindCureForCancer()
}


class MyClass {

    weak var delegate: MyClassDelegate?

    init() {}

    func findCure() {
        // Crashes here with EXC_BAD_ACCESS. Why? -- Note that it also crashes if I replace the following line with `if let d = delegate { d.didFindCureForCancer() }`
        delegate?.didFindCureForCancer()
    }

    deinit { println("MyClass died") }
}


class AppDelegate: MyClassDelegate {

    func application() -> Bool {
        println("starting")
        let cureFinder = MyClass()
        cureFinder.delegate = self
        cureFinder.findCure()

        return true
    }

    func didFindCureForCancer() {
        println("about time...")
    }

    deinit { println("AppDelegate died") }
}

_ = {
  AppDelegate().application()
}()
// CHECK-LABEL: starting
// CHECK-NEXT: about time...
// CHECK-NEXT: MyClass died
// CHECK-NEXT: AppDelegate died
