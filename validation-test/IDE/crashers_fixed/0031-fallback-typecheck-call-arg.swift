// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token COMPLETE

func tryMap(_ x: (String) -> Void) {}

func fetch() {
    tryMap { data in
        doesNotExist(data: data, #^COMPLETE^#)
    }
}
