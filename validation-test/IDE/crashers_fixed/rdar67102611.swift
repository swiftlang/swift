// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s > /dev/null

for x in "foo" {
    if x.#^A^# {
    }
}
