import speed._
==============
:cake: from the hot shores of the black forest :cake:

Install
-------

```
resolvers += Opts.resolver.sonatypeReleases

libraryDependencies += "net.virtual-void" %% "speed" % "13" % "provided"
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

|Description|While|Speedy|Scala|
|-----------|----:|------|----:|
|[foreach counting](#foreach-counting) | 100 % | 100.22 % | 135.97 %
|[nested counting](#nested-counting) | 100 % | 100.91 % | 218.19 %
|[filtered summing](#filtered-summing) | 100 % | 98.22 % | 1543.41 %
|[mapped summing](#mapped-summing) | 100 % | 102.32 % | 1585.62 %
|[array foreach counting](#array-foreach-counting) | 100 % | 99.47 % | 443.93 %
|[array summing](#array-summing) | 100 % | 99.73 % | 296.28 %
|[array filtered summing](#array-filtered-summing) | 100 % | 103.16 % | 973.61 %
|[array mapped summing](#array-mapped-summing) | 100 % | 100.69 % | 450.02 %
|[size of filtered ref array](#size-of-filtered-ref-array) | 100 % | 99.83 % | 351.10 %

(Don't be fooled, values < 100 % are most likely not significant but I'm including them here
 anyways just for the giggles. :laughing:)

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

### Array summing
```scala
array.sum
```

### Array filtered summing
```scala
array.filter(_ % 3 == 0).sum
```

### Array mapped summing
```scala
array.map(x ⇒ x * x).sum
```

### Size of Filtered ref Array
```scala
class Ref(var num: Int = 0)

val N = 1000
val refs = (0 until N).map(i ⇒ new Ref(i)).toArray

refs
  .filter(_.num % 5 == 0)
  .filter(_.num % 7 == 0)
  .size
```

What's missing?
---------------

Much.

 * support for putting our special ranges into vals and use them later
 * proper testing
 * support for more Array methods

Should I use it in production?
------------------------------

Look into the eyes of your favorite car dealer, again, and decide yo'self:

![Yo dawg, we heard you like your scala fast, so we put code in yo' code so it loops while it loops](http://i.imgur.com/fAZMT1J.png)


Extra goodies
-------------

 * example of using quasiquotes for 2.10.4 macros
 * a stub implementation for constant folding to simplify generated trees if possible

Previous work
-------------

 * [spire's][spire] cfor
 * [scalaxy][scalaxy] loops

[spire]: https://github.com/non/spire
[scalaxy]: https://github.com/ochafik/Scalaxy

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/fd91a42ebd2f69382ea34cfc2a09d9ed "githalytics.com")](http://githalytics.com/jrudolph/speed)
