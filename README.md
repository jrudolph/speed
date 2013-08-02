import speed._
==============
:cake: from the hot shores of the black forest :cake:

Install
-------

```
resolvers += Opts.resolver.sonatypeReleases

libraryDependencies += "net.virtual-void" %% "speed" % "12" % "provided"
```

Use
---

```scala
import speed._
var counter = 0
for {
  i ← 1 to 100
  j ← 1 to 100
} counter += i * j
counter
```

or

```scala
import speed._
(1 to 1000).map(i ⇒ i * i).sum
```

How?
----

In the words of the master of enrichment:


![Yo dawg, we heard you like your scala fast, so we put code in yo' code so it loops while it loops](http://i.imgur.com/fAZMT1J.png)

Macros.


How much fasta?
---------------

Runtime (smaller is better, reference is an equivalent while loop)

|Description|While|Speedy Range|Scala Range|
|-----------|----:|-----------:|----------:|
|[foreach counting](#foreach-counting) | 100 % | 98.43 % | 143.02 %
|[nested counting](#nested-counting)   | 100 % | 99.55 % | 223.28 %
|[filtered summing](#filtered-summing) | 100 % | 98.30 % | 1744.03 %
|[mapped summing](#mapped-summing)     | 100 % | 96.12 % | 4974.68 %
|[array foreach counting](#array-foreach-counting) | 100 % | 99.48 % | 439.40 %


### Foreach counting

```scala
var counter = 0
for (i ← 1 to 1000) counter += i * i
counter
```

### Nested counting
```scala
var counter = 0
for {
  i ← 1 to 100
  j ← 1 to 100
} counter += i * j
counter
```

### Filtered summing
```scala
(1 to 1000).filter(_ % 3 == 0).sum
```

### Mapped summing
```scala
(1 to 1000).map(i ⇒ i * i).sum
```

### Array foreach counting
```scala
var counter = 0
for (x ← array) counter += x * x
counter
```

What's missing?
---------------

Much.

 * proper testing
 * support for more Array methods
 * support for other Array types than `Array[Int]`


Should I use it in production?
------------------------------

Look into the eyes of your favorite car dealer, again, and decide yo'self:

![Yo dawg, we heard you like your scala fast, so we put code in yo' code so it loops while it loops](http://i.imgur.com/fAZMT1J.png)


Extra goodies
-------------

 * example of using quasiquotes for 2.10.2 macros
 * a stub implementation for constant folding to simplify generated trees if possible


Previous work
-------------

 * [spire's][spire] cfor
 * [scalaxy][scalaxy] loops

[spire]: https://github.com/non/spire
[scalaxy]: https://github.com/ochafik/Scalaxy
