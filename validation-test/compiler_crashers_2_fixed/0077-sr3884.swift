// RUN: %target-swift-frontend %s -emit-ir

protocol DataSourceItem { }

protocol TableDataSourceItem : DataSourceItem { }


class DataSource<T : DataSourceItem> { }

class TableDataSource<T : TableDataSourceItem>: DataSource<T> { }


class DataSourceBuilder<T : TableDataSourceItem, U : TableDataSource<T>> { }

class TableDataSourceBuilder<T : TableDataSourceItem, U : TableDataSource<T>> : DataSourceBuilder<T, U> { }


enum MyItem: TableDataSourceItem { }

class MyBuilder : TableDataSourceBuilder<MyItem, TableDataSource<MyItem>> { }

let builder = MyBuilder()
