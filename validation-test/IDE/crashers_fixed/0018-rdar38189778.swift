// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

struct Horse {
    init(saddle: @escaping (_ height: Float) -> Float) {}
}

func myHorse() -> Horse {
    return Horse(
        saddle: { (height) -> Float in
            let stirrups: #^A^#Float = 30
            return height - stirrups
    })
}
