import speed._
==============
from the hot shores of the black forest :cake:

![Yo dawg, we heard you like your scala fast, so we put code in yo' code so it loops while it loops](http://i.imgur.com/fAZMT1J.png)

Install
-------

```
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
