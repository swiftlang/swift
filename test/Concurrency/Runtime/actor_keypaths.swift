// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

actor Page {
    let initialNumWords : Int

    private let numWordsMem: UnsafeMutablePointer<Int>

    nonisolated
    var numWords : Int {
      get { numWordsMem.pointee }
      set { numWordsMem.pointee = newValue }
    }

    private init(withWords words : Int) {
        initialNumWords = words
        numWordsMem = .allocate(capacity: 1)
        numWordsMem.initialize(to: words)
    }

    convenience init(_ words: Int) {
        self.init(withWords: words)
        numWords = words
    }

    deinit {
      numWordsMem.deallocate()
    }
}

actor Book {
    let pages : [Page]

    init(_ numPages : Int) {
        var stack : [Page] = []
        for i in 0 ..< numPages {
            stack.append(Page(i))
        }
        pages = stack
    }

    nonisolated
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
