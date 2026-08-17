// RUN: %target-swift-frontend -swift-version 6 -emit-module -module-name Library -emit-object %s -c

import Foundation

@objc public class Author : NSObject {
    public var name: String
    public var books: [Book] = []
    public init(name: String) {
        self.name = name
    }
    public func addBook(_ bookName: String) {
        books.append(Book(title: bookName))
    }
}

@objc public class Book : NSObject {
    public var title: String
    public var author: Author?
    @objc public init(title: String, author: Author? = nil) {
        self.title = title
        self.author = author
    }
}

