//
//  AppDelegate.swift
//  Multiline
//
//  Created by John Holdsworth on 09/04/2017.
//  Copyright © 2017 John Holdsworth. All rights reserved.
//

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {



    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application

//        print(""""hello world"""")

        var author = "john", xml =
            "<?xml version=\"1.0\"?>\n" +
            "<catalog>\n" +
            "   <book id=\"bk101\" empty=\"\">\n" +
            "       <author>\(author)</author>\n" +
            "       <title>XML Developer's Guide</title>\n" +
            "       <genre>Computer</genre>\n" +
            "       <price>44.95</price>\n" +
            "       <publish_date>2000-10-01</publish_date>\n" +
            "       <description>An in-depth look at creating applications with XML.</description>\n" +
            "   </book>\n" +
            "</catalog>"

        print( """
            <?xml version="1.0"?>
            <catalog>
               <book id="bk101" empty="">
                   <author>\(author)</author>
                   <title>XML Developer's Guide</title>
                   <genre>Computer</genre>
                   <price>44.95</price>
                   <publish_date>2000-10-01</publish_date>
                   <description>An in-depth look at creating applications with XML.</description>
               </book>
            </catalog>
            """ )

        assert( xml == """
            <?xml version="1.0"?>
            <catalog>
               <book id="bk101" empty="">
                   <author>\(author)</author>
                   <title>XML Developer's Guide</title>
                   <genre>Computer</genre>
                   <price>44.95</price>
                   <publish_date>2000-10-01</publish_date>
                   <description>An in-depth look at creating applications with XML.</description>
               </book>
            </catalog>
            """ )

//        assert( xml == """<?xml version="1.0"?>
//            <catalog>
//               <book id="bk101" empty="">
//                   <author>\(author)</author>
//                   <title>XML Developer's Guide</title>
//                   <genre>Computer</genre>
//                   <price>44.95</price>
//                   <publish_date>2000-10-01</publish_date>
//                   <description>An in-depth look at creating applications with XML.</description>
//               </book>
//            </catalog>
//            """ )
//
//        print( """
//            <?xml version="1.0"?>
//            <catalog>
//               <book id="bk101" empty="">
//                   <author>\(author)</author>
//                   <title>XML Developer's Guide</title>
//                   <genre>Computer</genre>
//                   <price>44.95</price>
//                   <publish_date>2000-10-01</publish_date>
//                   <description>An in-depth look at creating applications with XML.</description>
//               </book>
//            </catalog>
//            """ )

//        let text = """
//            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
//            tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, \
//            quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\
//            """
//        func delimit(_ str: String) -> String {
//            return "<\(str)>"
//        }
//        print(delimit("""Nineteen\
//Upsilon"""))

        print( """
            Usage: myapp <options>

            Run myapp to do mything

            Options:
            -myoption - an option
            """ )

//        let bookTuples = [("id", "john", "book", "novel", 9.99),
//                          ("id", "john", "book", "novel", 9.99)]
//
//        xml = """
//            <?xml version="1.0"?>
//            <catalog>\n
//            """
//        
//        for (id, author, title, genre, price) in bookTuples {
//            xml += """
//                    <book id="bk\(id)">
//                        <author>\(author)</author>
//                        <title>\(title)</title>
//                        <genre>\(genre)</genre>
//                        <price>\(price)</price>
//                    </book>\n
//                """
//        }
//        
//        xml += """
//            </catalog>
//            """

        print( xml )

      print("\((0..<10).map {"""
        \("a" + """
         \($0) valid
        """)
        """}.joined(separator:" ")) literal")


      let bookTuples = [(1, "john", "book", "novel", 9.99),
                        (2, "john", "book", "novel", 9.99)]

      print(""gsdf"
            <?xml version="1.0"?>
            <catalog>
            \(bookTuples.map { (id, author, title, genre, price) in """
                <book id="bk\(id)" empty="">
                    <author>\(author)</author>
                    <title>\(title)</title>
                    <genre>\(genre)</genre>
                    <price>\(price)</price>
                </book>
            """}.joined(separator:"\n"))
            </catalog>
            """)

    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }


}

