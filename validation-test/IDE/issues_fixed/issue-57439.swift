// RUN: %batch-code-completion

// https://github.com/apple/swift/issues/57439

struct TodayView {
    func foo() {
        Uext(verbatim: #^COMPLETE^#)
    }
    
    private func dateString2() -> String {
        return "abc"
    }
}

struct Uext {
    init(verbatim content: String)
    init<F : GormatStyle>(_ input: F.FormatInput) where F.FormatInput : Equatable
}

protocol GormatStyle {
    associatedtype FormatInput
}

// COMPLETE: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: dateString2()[#String#];
