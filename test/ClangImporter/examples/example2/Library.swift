// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -swift-version 6 -I %t -emit-module -module-name Author -emit-object %t/Author.swift -c

//--- Author.swift

import Foundation
import BookOpaqueInterface

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


//--- Book.swift

import Foundation
import Author

@objc public class Book : NSObject {
    public var title: String
    public var author: Author?
    @objc public init(title: String, author: Author? = nil) {
        self.title = title
        self.author = author
    }
}

//--- BookOpaqueInterface.h

#import <Foundation/Foundation.h>

@class Book;
Book* createABook(NSString* name);

//--- BookOpaqueInterface.mm

#import "BookOpaqueInterface.h"
#import "Book-Swift.h"

Book* createABook(NSString* name) {
	return [[Book alloc] initWithTitle: name];
}


//--- module.modulemap

module BookOpaqueInterface {
  header "BookOpaqueInterface.h"
}
