public class C2 {
  public final func f() {
    func local() {
      _ = {
        class Local {
          lazy var a = {
            return b
          }()
          var b = 123
        }
      }
    }
  }
}
