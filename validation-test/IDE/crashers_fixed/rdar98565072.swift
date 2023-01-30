// RUN: %target-swift-ide-test -code-completion -code-completion-token COMPLETE -source-filename %s

protocol PreferenceKey {
    associatedtype Value
}
protocol View {}

func propagateHeight<K: PreferenceKey>() -> some View where K.Value == #^COMPLETE^#
