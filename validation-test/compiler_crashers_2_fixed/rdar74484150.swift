// RUN: %target-swift-frontend -typecheck %s

func foo() {
  enum No: Error {
    case no
  }

  defer {
    do {
      throw No.no
    } catch No.no {
    } catch {
    }
  }
  _ = ()
}
