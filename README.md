# SQL_Compiler_Scala

Build a SQL-compatible compiler in Scala using LMS

Usage:

1.For sbt shell: sbt

2.For SQL interpreter

~~~
run unstaged "select * from ? schema Phrase, Year, MatchCount, VolumeCount delim \\t where Year='1999'" src/data/t1gram.csv
~~~

3.SQL Compiler to Scala
~~~
run scala "select * from ? schema Phrase, Year, MatchCount, VolumeCount delim \\t where Year='1999'" src/data/t1gram.csv
~~~

4.SQL Compiler to C
~~~
run c "select * from ? schema Phrase, Year, MatchCount, VolumeCount delim \\t where Year='1999'" src/data/t1gram.csv 
~~~