// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/75845
// Make sure we don't crash.

struct Foo {
  init() {
    do {
    } catch {
      #^A^#self#^B^# = #^C^#error#^D^#
    }
  }
}
// A: Decl[LocalVar]/Local: error[#any Error#]; name=error
// B: Begin completions
// C: Decl[LocalVar]/Local: error[#any Error#]; name=error
// D: Begin completions

enum E {
  case e(Error)

  func foo() {
    var x = self
    do {
    } catch {
      x = .e(error)#^E^#
      // E: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: .foo()[#Void#]; name=foo()
    }
  }

  static func bar() {
    do {
    } catch {
      _ = foo(.e(error))#^F^#
      // F: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ()[#Void#]; name=()

      _ = foo(.e(error))()#^G^#
      // G: Begin completions, 1 items
      // G: Keyword[self]/CurrNominal: .self[#Void#]; name=self
    }
  }
}
