// RUN: %batch-code-completion

func takeClosure(_ body: () -> Void) {}

func test() {
  let myVar = 1
  takeClosure {
    do {
      #^COMPLETE^#
// COMPLETE: Decl[LocalVar]/Local: myVar[#Int#];
    } catch let error as Int {
    } catch let error {
    }
  }
}
