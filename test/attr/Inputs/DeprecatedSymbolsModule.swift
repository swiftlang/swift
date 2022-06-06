@available(*, deprecated)
func importedDeprecatedFunc() {}

@available(*, deprecated)
func importedDeprecatedFuncWithReturn() -> String {
    return "Donda"
}

@available(*, deprecated)
var importedDeprecatedVar = "910210"

@available(*, deprecated)
struct importedDeprecatedType {
    var x = ""
    static let m = 22

    func memberFunc() -> Int {
        40
    }
}
