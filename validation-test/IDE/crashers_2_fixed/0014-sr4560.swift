// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

protocol MyDelegate: AnyObject {
	func mySweetDelegateFunction()
}

class Foo {
	weak var delegate: MyDelegate?
	
	func bar() {
		self.delegate.#^A^#
			// ^--- type "." here -> crash
	}
}
