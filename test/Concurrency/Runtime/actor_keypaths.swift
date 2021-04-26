// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency %import-libdispatch)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

actor Page {
    nonisolated let initialNumWords : Int

    @actorIndependent(unsafe)
    var numWords : Int

    init(_ words : Int) {
        numWords = words
        initialNumWords = words
    }
}

actor Book {
    nonisolated let pages : [Page]

    init(_ numPages : Int) {
        var stack : [Page] = []
        for i in 0 ..< numPages {
            stack.append(Page(i))
        }
        pages = stack
    }

    @actorIndependent
    subscript(_ page : Int) -> Page {
        return pages[page]
    }
}

func bookHasChanged(_ b : Book,
                    currentGetter : KeyPath<Page, Int>,
                    initialGetter : (Page) -> Int) -> Bool {
    let numPages = b[keyPath: \.pages.count]

    for i in 0 ..< numPages {
        let pageGetter = \Book.[i]
        let currentWords = pageGetter.appending(path: currentGetter)

        if (b[keyPath: currentWords] != initialGetter(b[keyPath: pageGetter])) {
            return true
        }
    }

    return false
}

func enumeratePageKeys(from : Int, to : Int) -> [KeyPath<Book, Page>] {
    var keys : [KeyPath<Book, Page>] = []
    for i in from ..< to {
        keys.append(\Book.[i])
    }
    return keys
}

func erasePages(_ book : Book, badPages: [KeyPath<Book, Page>]) {
    for page in badPages {
        book[keyPath: page].numWords = 0
    }
}

let book = Book(100)

if bookHasChanged(book, currentGetter: \Page.numWords, initialGetter: \Page.initialNumWords) {
    fatalError("book should not be changed")
}

erasePages(book, badPages: enumeratePageKeys(from: 0, to: 100))

guard bookHasChanged(book, currentGetter: \Page.numWords, initialGetter: \Page.initialNumWords) else {
    fatalError("book should be wiped!")
}
